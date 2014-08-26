(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Transformation of Mach code into a list of pseudo-instructions. *)

open Reg
open Mach

type label = int

let label_counter = ref 99

let new_label() = incr label_counter; !label_counter

type frame_offsets = int

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    live: Reg.Set.t }

and instruction_desc =
    Lend
  | Lop of operation
  | Lreloadretaddr
  | Lreturn
  | Llabel of label
  | Lbranch_ind of frame_offsets
  | Lbranch of label * frame_offsets
  | Lcondbranch of test * label
  | Lcondbranch3 of label option * label option * label option
  | Lswitch of label array
  | Lsetuptrap of label
  | Lpushtrap
  | Lpoptrap
  | Lraise of Lambda.raise_kind

let has_fallthrough = function
  | Lreturn | Lbranch _ | Lswitch _ | Lraise _
  | Lop Itailcall_ind | Lop (Itailcall_imm _) -> false
  | _ -> true

type fundecl =
  { fun_name: string;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t }

(* Invert a test *)

let invert_integer_test = function
    Isigned cmp -> Isigned(Cmm.negate_comparison cmp)
  | Iunsigned cmp -> Iunsigned(Cmm.negate_comparison cmp)

let invert_test = function
    Itruetest -> Ifalsetest
  | Ifalsetest -> Itruetest
  | Iinttest(cmp) -> Iinttest(invert_integer_test cmp)
  | Iinttest_imm(cmp, n) -> Iinttest_imm(invert_integer_test cmp, n)
  | Ifloattest(cmp, neg) -> Ifloattest(cmp, not neg)
  | Ieventest -> Ioddtest
  | Ioddtest -> Ieventest

(* The "end" instruction *)

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty }

(* Cons an instruction (live, debug empty) *)

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty }

(* Cons a simple instruction (arg, res, live empty) *)

let cons_instr d n =
  { desc = d; next = n; arg = [||]; res = [||];
    dbg = Debuginfo.none; live = Reg.Set.empty }

(* Build an instruction with arg, res, dbg, live taken from
   the given Mach.instruction *)

let copy_instr d i n =
  { desc = d; next = n;
    arg = i.Mach.arg; res = i.Mach.res;
    dbg = i.Mach.dbg; live = i.Mach.live }

(*
   Label the beginning of the given instruction sequence.
   - If the sequence starts with a branch, jump over it.
   - If the sequence is the end, (tail call position), just do nothing
*)

let get_label n = match n.desc with
    Lbranch (lbl,_) -> (lbl, n)
  | Llabel lbl -> (lbl, n)
  | Lend -> (-1, n)
  | _ -> let lbl = new_label() in (lbl, cons_instr (Llabel lbl) n)

(* Check the fallthrough label *)
let check_label n = match n.desc with
  | Lbranch (lbl,_) -> lbl
  | Llabel lbl -> lbl
  | _ -> -1

(* Discard all instructions up to the next label.
   This function is to be called before adding a non-terminating
   instruction. *)

let rec discard_dead_code n =
  match n.desc with
    Lend -> n
  | Llabel _ -> n
(* Do not discard Lpoptrap or Istackoffset instructions,
   as this may cause a stack imbalance later during assembler generation. *)
  | Lpoptrap -> n
  | Lop(Istackoffset _) -> n
  | _ -> discard_dead_code n.next

(*
   Add a branch in front of a continuation.
   Discard dead code in the continuation.
   Does not insert anything if we're just falling through
   or if we jump to dead code after the end of function (lbl=-1)
*)

let add_branch ?(frame_offsets=0) lbl n =
  if lbl >= 0 then
    let n1 = discard_dead_code n in
    match n1.desc with
    | Llabel lbl1 when lbl1 = lbl -> n1
    | _ -> cons_instr (Lbranch (lbl,frame_offsets)) n1
  else
    discard_dead_code n

(* Current labels for exit handler *)

let exit_label : (int * (int * label)) list ref = ref []

let find_exit_label k =
  try
    List.assoc k !exit_label
  with
  | Not_found -> Misc.fatal_error "Linearize.find_exit_label"

let is_next_catch n = match !exit_label with
| (n0,_)::_  when n0=n -> true
| _ -> false

let fail_depth fail = fst (find_exit_label fail)

(* Linearize an instruction [i]: add it in front of the continuation [n] *)

let rec linear' depth i n =
  let linear = linear' depth in
  match i.Mach.desc with
    Iend -> n
  | Iop(Itailcall_ind | Itailcall_imm _ as op) ->
      copy_instr (Lop op) i (discard_dead_code n)
  | Iop(Imove | Ireload | Ispill)
    when i.Mach.arg.(0).loc = i.Mach.res.(0).loc ->
      linear i.Mach.next n
  | Iop(Iconst_sexn_addr nfail) ->
      let (_,lbl) = find_exit_label nfail in
      copy_instr (Lop (Iconst_sexn_addr lbl)) i (linear i.Mach.next n)
  | Iop op ->
      copy_instr (Lop op) i (linear i.Mach.next n)
  | Ireturn ->
      let n1 = copy_instr Lreturn i (discard_dead_code n) in
      if !Proc.contains_calls
      then cons_instr Lreloadretaddr n1
      else n1
  | Iifthenelse(test, ifso, ifnot) ->
      let n1 = linear i.Mach.next n in
      begin match (ifso.Mach.desc, ifnot.Mach.desc, n1.desc) with
        Iend, _, Lbranch (lbl,0) ->
          copy_instr (Lcondbranch(test, lbl)) i (linear ifnot n1)
      | _, Iend, Lbranch (lbl,0) ->
          copy_instr (Lcondbranch(invert_test test, lbl)) i (linear ifso n1)
      | Iexit nfail1, Iexit nfail2, _
            when is_next_catch nfail1
              && fail_depth nfail1 = depth
              && fail_depth nfail2 = depth ->
          let (_,lbl2) = find_exit_label nfail2 in
          copy_instr
            (Lcondbranch (invert_test test, lbl2)) i (linear ifso n1)
      | Iexit nfail, _, _
          when fail_depth nfail = depth ->
          let n2 = linear ifnot n1
          and (_,lbl) = find_exit_label nfail in
          copy_instr (Lcondbranch(test, lbl)) i n2
      | _,  Iexit nfail, _
          when fail_depth nfail = depth ->
          let n2 = linear ifso n1 in
          let (_,lbl) = find_exit_label nfail in
          copy_instr (Lcondbranch(invert_test test, lbl)) i n2
      | Iend, _, _ ->
          let (lbl_end, n2) = get_label n1 in
          copy_instr (Lcondbranch(test, lbl_end)) i (linear ifnot n2)
      | _,  Iend, _ ->
          let (lbl_end, n2) = get_label n1 in
          copy_instr (Lcondbranch(invert_test test, lbl_end)) i
                     (linear ifso n2)
      | _, _, _ ->
        (* Should attempt branch prediction here *)
          let (lbl_end, n2) = get_label n1 in
          let (lbl_else, nelse) = get_label (linear ifnot n2) in
          copy_instr (Lcondbranch(invert_test test, lbl_else)) i
            (linear ifso (add_branch lbl_end nelse))
      end
  | Iswitch(index, cases) ->
      let lbl_cases = Array.make (Array.length cases) 0 in
      let (lbl_end, n1) = get_label(linear i.Mach.next n) in
      let n2 = ref (discard_dead_code n1) in
      for i = Array.length cases - 1 downto 0 do
        let (lbl_case, ncase) =
                get_label(linear cases.(i) (add_branch lbl_end !n2)) in
        lbl_cases.(i) <- lbl_case;
        n2 := discard_dead_code ncase
      done;
      (* Switches with 1 and 2 branches have been eliminated earlier.
         Here, we do something for switches with 3 branches. *)
      if Array.length index = 3 then begin
        let fallthrough_lbl = check_label !n2 in
        let find_label n =
          let lbl = lbl_cases.(index.(n)) in
          if lbl = fallthrough_lbl then None else Some lbl in
        copy_instr (Lcondbranch3(find_label 0, find_label 1, find_label 2))
                   i !n2
      end else
        copy_instr (Lswitch(Array.map (fun n -> lbl_cases.(n)) index)) i !n2
  | Iloop body ->
      let lbl_head = new_label() in
      let n1 = linear i.Mach.next n in
      let n2 = linear body (cons_instr (Lbranch (lbl_head,0)) n1) in
      cons_instr (Llabel lbl_head) n2
  | Icatch(handlers, body) ->
      let (lbl_end, n1) = get_label(linear i.Mach.next n) in
      let labels = List.map (fun (_io, handler) ->
          match handler.Mach.desc with
          | Iend -> lbl_end
          | _ -> new_label ())
          handlers in
      let exit_label_add = List.map2
          (fun (io, _) lbl -> (io, (depth, lbl)))
          handlers labels in
      let previous_exit_label = !exit_label in
      exit_label := exit_label_add @ !exit_label;
      let n2 = List.fold_left2 (fun n (_io, handler) lbl_handler ->
          match handler.Mach.desc with
          | Iend -> n
          | _ -> cons_instr (Llabel lbl_handler) (linear handler n1))
          n1 handlers labels
      in
      let n3 = linear body (add_branch lbl_end n2) in
      exit_label := previous_exit_label;
      n3
  | Iexit nfail ->
      let n1 = linear i.Mach.next n in
      let (lbl_depth, lbl) = find_exit_label nfail in
      let frame_offsets = depth - lbl_depth in
      let rec poptraps count n =
        if count = 0
        then add_branch ~frame_offsets lbl n
        else cons_instr Lpoptrap (poptraps (count-1) n) in
      poptraps frame_offsets n1

  | Iexit_ind possible_fails ->
      let n1 = linear i.Mach.next n in
      let lbl_depth =
        match possible_fails with
        | [] -> Misc.fatal_error "Linearize.linear': Iexit_ind"
        | h :: t ->
            let depth = fail_depth h in
            List.iter
              (fun n ->
                 if fail_depth n <> depth
                 then Misc.fatal_error "Linearize.linear': Iexit_ind not matching depth")
              t;
            depth
      in
      let frame_offsets = depth - lbl_depth in
      let rec poptraps count n =
        if count = 0
        then copy_instr (Lbranch_ind frame_offsets) i n
        else cons_instr Lpoptrap (poptraps (count-1) n) in
      poptraps frame_offsets n1

  | Itrywith(body, handler) ->
      let (lbl_join, n1) = get_label (linear i.Mach.next n) in
      let (lbl_body, n2) =
        get_label (cons_instr Lpushtrap
                    (linear' (depth+1) body (cons_instr Lpoptrap n1))) in
      cons_instr (Lsetuptrap lbl_body)
        (linear handler (add_branch lbl_join n2))
  | Iraise k ->
      copy_instr (Lraise k) i (discard_dead_code n)

let linear i n =
  linear' 0 i n

let fundecl f =
  { fun_name = f.Mach.fun_name;
    fun_body = linear f.Mach.fun_body end_instr;
    fun_fast = f.Mach.fun_fast;
    fun_dbg  = f.Mach.fun_dbg }
