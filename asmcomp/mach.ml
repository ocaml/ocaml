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

(* Representation of machine code by sequences of pseudoinstructions *)

type integer_comparison =
    Isigned of Cmm.comparison
  | Iunsigned of Cmm.comparison

type integer_operation =
    Iadd | Isub | Imul | Imulh | Idiv | Imod
  | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr
  | Icomp of integer_comparison
  | Icheckbound

type test =
    Itruetest
  | Ifalsetest
  | Iinttest of integer_comparison
  | Iinttest_imm of integer_comparison * int
  | Ifloattest of Cmm.comparison * bool
  | Ioddtest
  | Ieventest

type operation =
    Imove
  | Ispill
  | Ireload
  | Iconst_int of nativeint
  | Iconst_float of float
  | Iconst_symbol of string
  | Iconst_blockheader of nativeint
  | Icall_ind
  | Icall_imm of string
  | Itailcall_ind
  | Itailcall_imm of string
  | Iextcall of string * bool
  | Istackoffset of int
  | Iload of Cmm.memory_chunk * Arch.addressing_mode
  | Istore of Cmm.memory_chunk * Arch.addressing_mode * bool
  | Ialloc of int
  | Iintop of integer_operation
  | Iintop_imm of integer_operation * int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Ifloatofint | Iintoffloat
  | Ispecific of Arch.specific_operation

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: Reg.t array;
    res: Reg.t array;
    dbg: Debuginfo.t;
    mutable live: Reg.Set.t }

and instruction_desc =
    Iend
  | Iop of operation
  | Ireturn
  | Iifthenelse of test * instruction * instruction
  | Iswitch of int array * instruction array
  | Ilabel of (int * instruction) list * instruction
  | Ijump of int
  | Ijump_ind of int list
  | Itrywith of instruction * instruction
  | Iraise of Lambda.raise_kind

type fundecl =
  { fun_name: string;
    fun_args: Reg.t array;
    fun_body: instruction;
    fun_fast: bool;
    fun_dbg : Debuginfo.t }

let rec dummy_instr =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty }

let end_instr () =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty }

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty }

let instr_cons_debug d a r dbg n =
  { desc = d; next = n; arg = a; res = r; dbg = dbg; live = Reg.Set.empty }

let rec instr_iter f i =
  match i.desc with
    Iend -> ()
  | _ ->
      f i;
      match i.desc with
        Iend -> ()
      | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Ilabel(handlers, body) ->
          instr_iter f body;
          List.iter (fun (_n, handler) -> instr_iter f handler) handlers;
          instr_iter f i.next
      | Ijump _ -> ()
      | Ijump_ind _ -> ()
      | Itrywith(body, handler) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise _ -> ()
      | _ ->
          instr_iter f i.next

module StExnSet = Set.Make(struct type t = int let compare x y = x-y end)

type result =
  { reachable_exits : StExnSet.t;
    recursive_handlers : StExnSet.t }

let empty_result =
  { reachable_exits = StExnSet.empty;
    recursive_handlers = StExnSet.empty }

let result_union r1 r2 =
  { reachable_exits =
      StExnSet.union r1.reachable_exits r2.reachable_exits;
    recursive_handlers =
      StExnSet.union r1.recursive_handlers r2.recursive_handlers }

let recursive_handlers i =
  let rec loop i =
    match i.desc with
      Iend -> empty_result
    | Ireturn | Iop(Itailcall_ind) | Iop(Itailcall_imm _) -> empty_result
    | Iraise _ -> empty_result
    | Iifthenelse(tst, ifso, ifnot) ->
        result_union
          (result_union (loop ifso) (loop ifnot))
          (loop i.next)
    | Iswitch(index, cases) ->
        Array.fold_left (fun acc case -> result_union acc (loop case))
          (loop i.next) cases
    | Ilabel(handlers, body) ->
        let r =
          List.fold_left (fun acc (nfail, handler) ->
              let acc = result_union acc (loop handler) in
              if StExnSet.mem nfail acc.reachable_exits
              then { acc with
                     recursive_handlers =
                       StExnSet.add nfail acc.recursive_handlers }
              else acc)
            empty_result handlers
        in
        result_union r
          (result_union (loop body) (loop i.next))
    | Ijump n ->
        { empty_result with
          reachable_exits = StExnSet.singleton n }
    | Ijump_ind l ->
        let reachable_exits = List.fold_right StExnSet.add l StExnSet.empty in
        { empty_result with reachable_exits }
    | Itrywith(body, handler) ->
        result_union
          (result_union (loop body) (loop handler))
          (loop i.next)
    | Iop _ ->
        loop i.next
  in
  (loop i).recursive_handlers

