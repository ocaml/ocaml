(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Common subexpression elimination by value numbering over extended
   basic blocks. *)

open Mach

type valnum = int

(* We maintain sets of equations of the form
       valnums = operation(valnums)
   plus a mapping from registers to value numbers.  *)

type rhs = operation * valnum array

module Equations =
  Map.Make(struct type t = rhs let compare = Pervasives.compare end)

type numbering =
  { num_next: int;                      (* next fresh value number *)
    num_eqs: valnum array Equations.t;  (* mapping rhs -> valnums *)
    num_reg: valnum Reg.Map.t }         (* mapping register -> valnum *)

let empty_numbering =
  { num_next = 0; num_eqs = Equations.empty; num_reg = Reg.Map.empty }

(** [valnum_reg n r] returns the value number for the contents of
  register [r].  If none exists, a fresh value number is returned
  and associated with register [r].  The possibly updated numbering
  is also returned.  [valnum_regs] is similar, but for an array of
  registers. *)

let valnum_reg n r =
  try
    (n, Reg.Map.find r n.num_reg)
  with Not_found ->
    let v = n.num_next in
    ({n with num_next = v + 1; num_reg = Reg.Map.add r v n.num_reg}, v)

let valnum_regs n rs =
  let l = Array.length rs in
  let vs = Array.make l 0 in
  let n = ref n in
  for i = 0 to l-1 do
    let (ni, vi) = valnum_reg !n rs.(i) in
    vs.(i) <- vi;
    n := ni
  done;
  (!n, vs)

(* Look up the set of equations for an equation with the given rhs.
   Return [Some res] if there is one, where [res] is the lhs. *)

let find_equation n rhs =
  try
    Some(Equations.find rhs n.num_eqs)
  with Not_found ->
    None

(* Find a set of registers containing the given value numbers. *)

let find_regs_containing n vs =
  match Array.length vs with
  | 0 -> Some [||]
  | 1 -> let v = vs.(0) in
         Reg.Map.fold (fun r v' res -> if v' = v then Some [|r|] else res)
                      n.num_reg None
  | _ -> assert false

(* Associate the given value numbers to the given result registers,
   without adding new equations. *)

let set_known_regs n rs vs =
  match Array.length rs with
  | 0 -> n
  | 1 -> { n with num_reg = Reg.Map.add rs.(0) vs.(0) n.num_reg }
  | _ -> assert false

(* Record the effect of a move: no new equations, but the result reg
   maps to the same value number as the argument reg. *)

let set_move n src dst =
  let (n1, v) = valnum_reg n src in
  { n1 with num_reg = Reg.Map.add dst v n1.num_reg }

(* Record the equation [fresh valnums = rhs] and associate the given
   result registers [rs] to [fresh valnums]. *)

let set_fresh_regs n rs rhs =
  match Array.length rs with
  | 0 -> { n with num_eqs = Equations.add rhs [||] n.num_eqs }
  | 1 -> let v = n.num_next in
         { num_next = v + 1;
           num_eqs = Equations.add rhs [|v|] n.num_eqs;
           num_reg = Reg.Map.add rs.(0) v n.num_reg }
  | _ -> assert false

(* Forget everything we know about the given result registers,
   which are receiving unpredictable values at run-time. *)

let set_unknown_regs n rs =
  { n with num_reg = Array.fold_right Reg.Map.remove rs n.num_reg }

(* Keep only the equations satisfying the given predicate. *)

let filter_equations pred n =
  { n with num_eqs = Equations.filter (fun (op,_) res -> pred op) n.num_eqs }

(* Prepend a reg-reg move *)

let insert_move srcs dsts i =
  match Array.length srcs with
  | 0 -> i
  | 1 -> instr_cons (Iop Imove) srcs dsts i
  | _ -> assert false

(* Classification of operations *)

type op_class =
  | Op_pure     (* pure, produce one result *)
  | Op_checkbound     (* checkbound-style: no result, can raise an exn *)
  | Op_load           (* memory load *)
  | Op_store of bool  (* memory store, false = init, true = assign *)
  | Op_other          (* anything else that does not store in memory *)

class cse_generic = object (self)

(* Default classification of operations.  Can be overriden in
   processor-specific files to classify specific operations better. *)

method class_of_operation op =
  match op with
  | Imove | Ispill | Ireload -> assert false   (* treated specially *)
  | Iconst_int _ | Iconst_float _ | Iconst_symbol _
  | Iconst_blockheader _ -> Op_pure
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Iextcall _ -> assert false                 (* treated specially *)
  | Istackoffset _ -> Op_other
  | Iload(_,_) -> Op_load
  | Istore(_,_,asg) -> Op_store asg
  | Ialloc _ -> Op_other
  | Iintop(Icheckbound) -> Op_checkbound
  | Iintop _ -> Op_pure
  | Iintop_imm(Icheckbound, _) -> Op_checkbound
  | Iintop_imm(_, _) -> Op_pure
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat -> Op_pure
  | Ispecific _ -> Op_other

(* Operations that are so cheap that it isn't worth factoring them. *)

method is_cheap_operation op =
  match op with
  | Iconst_int _ | Iconst_blockheader _ -> true
  | _ -> false

(* Forget all equations involving memory loads.  Performed after a
   non-initializing store *)

method private kill_loads n =
  filter_equations (fun o -> self#class_of_operation o <> Op_load) n

(* Keep only equations involving checkbounds, and forget register values.
   Performed across a call. *)

method private keep_checkbounds n =
  filter_equations (fun o -> self#class_of_operation o = Op_checkbound)
                   {n with num_reg = Reg.Map.empty }

(* Perform CSE on the given instruction [i] and its successors.
   [n] is the value numbering current at the beginning of [i]. *)

method private cse n i =
  match i.desc with
  | Iend | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _)
  | Iexit _ | Iraise _ ->
      i
  | Iop (Imove | Ispill | Ireload) ->
      (* For moves, we associate the same value number to the result reg
         as to the argument reg. *)
      let n1 = set_move n i.arg.(0) i.res.(0) in
      {i with next = self#cse n1 i.next}
  | Iop (Icall_ind | Icall_imm _ | Iextcall _) ->
      (* We don't perform CSE across function calls, as it increases
         register pressure too much.  We do remember the checkbound
         instructions already performed, though, since their reuse
         cannot increase register pressure. *)
      let n1 = self#keep_checkbounds n in
      {i with next = self#cse n1 i.next}
  | Iop op ->
      begin match self#class_of_operation op with
      | Op_pure | Op_checkbound | Op_load ->
          assert (Array.length i.res <= 1);
          let (n1, varg) = valnum_regs n i.arg in
          begin match find_equation n1 (op, varg) with
          | Some vres ->
              (* This operation was computed earlier. *)
              let n2 = set_known_regs n1 i.res vres in            
              begin match find_regs_containing n1 vres with
              | Some res when not (self#is_cheap_operation op) ->
                  (* We can replace res <- op args with r <- move res.
                     If the operation is very cheap to compute, e.g.
                     an integer constant, don't bother. *)
                  insert_move res i.res (self#cse n2 i.next)                
              | _ ->
                  {i with next = self#cse n2 i.next}
              end
          | None ->
              (* This operation produces a result we haven't seen earlier. *)
              let n2 = set_fresh_regs n1 i.res (op, varg) in
              {i with next = self#cse n2 i.next}
          end
      | Op_store false | Op_other ->
          (* An initializing store or an "other" operation do not invalidate
             any equations, but we do not know anything about the results. *)
          let n1 = set_unknown_regs n i.res in
          {i with next = self#cse n1 i.next}
      | Op_store true ->
          (* A non-initializing store: it can invalidate
             anything we know about prior loads. *)
          let n1 = set_unknown_regs (self#kill_loads n) i.res in
          {i with next = self#cse n1 i.next}
      end        
  (* For control structures, we set the numbering to empty at every
     join point, but propagate the current numbering across fork points. *)
  | Iifthenelse(test, ifso, ifnot) ->
      {i with desc = Iifthenelse(test, self#cse n ifso, self#cse n ifnot);
              next = self#cse empty_numbering i.next}
  | Iswitch(index, cases) ->
      {i with desc = Iswitch(index, Array.map (self#cse n) cases);
              next = self#cse empty_numbering i.next}
  | Iloop(body) ->
      {i with desc = Iloop(self#cse empty_numbering body);
              next = self#cse empty_numbering i.next}
  | Icatch(nfail, body, handler) ->
      {i with desc = Icatch(nfail, self#cse n body, self#cse empty_numbering handler);
              next = self#cse empty_numbering i.next}
  | Itrywith(body, handler) ->
      {i with desc = Itrywith(self#cse n body, self#cse empty_numbering handler);
              next = self#cse empty_numbering i.next}

method fundecl f =
  {f with fun_body = self#cse empty_numbering f.fun_body}

end



