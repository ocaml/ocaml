# 2 "asmcomp/loongarch64/proc.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                yala <zhaojunchao@loongson.cn>                          *)
(*                                                                        *)
(*               Copyright © 2008-2023 LOONGSON                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of the LoongArch *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map
   --------------------

    zero                   always zero
    ra                     return address
    sp, gp, tp             stack pointer, global pointer, thread pointer
    a0-a7        0-7       arguments/results
    s2-s6        8-12      arguments/results (preserved by C)
    t2-t6        13-17     temporary
    s0           18        general purpose (preserved by C)
    t0, t1       19-20     temporaries (used by call veneers)
    s1           21        trap pointer (preserved by C)
    s7           22        allocation pointer (preserved by C)
    s8           23        domain pointer (preserved by C)

  Floating-point register map
  ---------------------------

    f0-f7    100-107     arguments
    f0-f1    100-101     arguments/results
    f8-f23   108-123     temporary
    f24-f31  124-131     subroutine register variables

  Additional notes
  ----------------

    - t1 is used by the code generator, so not available for register
      allocation.

    - t0-t6 may be used by PLT stubs, so should not be used to pass
      arguments and may be clobbered by [Ialloc] in the presence of dynamic
      linking.
*)

let int_reg_name =
    [|"$a0"; "$a1"; "$a2"; "$a3"; "$a4"; "$a5"; "$a6"; "$a7";  (* 0- 7 *)
      "$s2"; "$s3"; "$s4"; "$s5"; "$s6";                       (* 8-12*)
      "$t2"; "$t3"; "$t4"; "$t5"; "$t6"; "$t7"; "$t8";          (*13-19*)
      "$s0";                                                   (*20*)
      "$t0"; "$t1";                                            (*21-22*)
      "$s1"; "$s7"; "$s8";                                      (*23-25*)
    |]

let float_reg_name =
  [| "$ft0"; "$ft1"; "$ft2"; "$ft3"; "$ft4"; "$ft5"; "$ft6";"$ft7";
     "$fs0"; "$fs1";
     "$fa0"; "$fa1"; "$fa2"; "$fa3"; "$fa4"; "$fa5"; "$fa6"; "$fa7";
     "$fs2"; "$fs3"; "$fs4"; "$fs5"; "$fs6"; "$fs7";
     "$ft8"; "$ft9"; "$ft10"; "$ft11";"$ft12";"$ft13";"$ft14";"$ft15"; |]
let num_register_classes = 2

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 1

(* first 19 int regs allocatable; all float regs allocatable *)
let num_available_registers = [| 21; 32 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 26 Reg.dummy in
  for i = 0 to 25 do
    v.(i) <- Reg.at_location Int (Reg i)
  done;
  v

let hard_float_reg =
  let v = Array.make 32 Reg.dummy in
  for i = 0 to 31 do
    v.(i) <- Reg.at_location Float (Reg(100 + i))
  done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let calling_conventions
    first_int last_int first_float last_float make_stack first_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref first_stack in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | Val | Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align (max 0 !ofs) 16) (* Keep stack 16-aligned. *)

let incoming ofs =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ = fatal_error "Proc.loc_results: cannot call"

let max_arguments_for_tailcalls = 13 (* in regs *) + 64 (* in domain state *)

(* OCaml calling convention:
     first integer args in a0 .. a7, s2 .. s6
     first float args in fa0 .. fa7, fs2 .. fs9
     remaining args in domain state area, then on stack.
   Return values in a0 .. a7, s2 .. s6 or fa0 .. fa7, fs2 .. fs9. *)

let loc_arguments arg =
  calling_conventions 0 12 110 121 outgoing (- size_domainstate_args) arg

let loc_parameters arg =
  let (loc, _ofs) =
    calling_conventions 0 12 110 121 incoming (- size_domainstate_args) arg
  in
  loc

let loc_results res =
  let (loc, _ofs) =
    calling_conventions 0 12 110 121 not_supported 0 res
  in
  loc

(* C calling convention:
     first integer args in a0 .. a7
     first float args in fa0 .. fa7
     remaining args on stack.
   A FP argument can be passed in an integer register if all FP registers
   are exhausted but integer registers remain.
   Return values in a0 .. a1 or fa0 .. fa1. *)

let external_calling_conventions
    first_int last_int first_float last_float make_stack arg =
  let loc = Array.make (Array.length arg) [| Reg.dummy |] in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | Val | Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- [| phys_reg !int |];
          incr int
        end else begin
          loc.(i) <- [| stack_slot (make_stack !ofs) ty |];
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- [| phys_reg !float |];
          incr float
        end else if !int <= last_int then begin
          loc.(i) <- [| phys_reg !int |];
          incr int
        end else begin
          loc.(i) <- [| stack_slot (make_stack !ofs) Float |];
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align !ofs 16) (* Keep stack 16-aligned. *)

let loc_external_arguments ty_args =
  let arg = Cmm.machtype_of_exttype_list ty_args in
  external_calling_conventions 0 7 110 117 outgoing arg

let loc_external_results res =
  let (loc, _ofs) = calling_conventions 0 1 110 111 not_supported 0 res
  in loc

(* Exceptions are in a0 *)

let loc_exn_bucket = phys_reg 0

(* Registers destroyed by operations *)

let destroyed_at_c_noalloc_call =
  (* s0-s8 and fs0-fs7 are callee-save, but s0 is
     used to preserve OCaml sp. *)
  Array.of_list(List.map phys_reg
    [0; 1; 2; 3; 4; 5; 6; 7; 13; 14; 15; 16; 17; 18; 19; 20;(*s0*)
     100; 101; 102; 103; 104; 105; 106; 107; 110; 111; 112; 113; 114; 115; 116;
     117; 124; 125; 126; 127; 128; 129; 130; 131])

let destroyed_at_alloc =
  (* t0-t6 are used for PLT stubs *)
    if !Clflags.dlcode then Array.map phys_reg [|13; 14; 15; 16; 17; 18; 19|]
  else [| phys_reg 13 |] (* t2 is used to pass the argument to caml_allocN *)

let destroyed_at_oper = function
  | Iop(Icall_ind | Icall_imm _) -> all_phys_regs
  | Iop(Iextcall{alloc; stack_ofs; _}) ->
      assert (stack_ofs >= 0);
      if alloc || stack_ofs > 0 then all_phys_regs
      else destroyed_at_c_noalloc_call
  | Iop(Ialloc _) | Iop(Ipoll _) -> destroyed_at_alloc
  | Iop(Istore(Single, _, _)) -> [| phys_reg 100 |]
  | Iop(Ifloatofint | Iintoffloat) -> [| phys_reg 100 |]
  | _ -> [| |]

let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| |]

(* Maximal register pressure *)

let safe_register_pressure = function
  | Iextcall _ -> 5  (*9-3 s0~s8 - s7 - s8 - s1 - s0*)
  | _ -> 21

let max_register_pressure = function
  | Iextcall _ -> [| 5; 8 |] (* 6 integer callee-saves, 8 FP callee-saves *)
  | _ -> [| 21; 30 |]

(* Layout of the stack *)

let frame_required fd =
  fd.fun_contains_calls
  || fd.fun_num_stack_slots.(0) > 0
  || fd.fun_num_stack_slots.(1) > 0

let prologue_required fd =
  frame_required fd

  (* FIXME *)
let int_dwarf_reg_numbers =
    [| 4; 5; 6; 7; 8; 9; 10; 11;
     23; 24; 25; 26; 27; 28; 29; 30;
     14; 15; 16; 17; 18;
     31;
     12; 13;
     19; 20;
  |]

let float_dwarf_reg_numbers =
  [| 32; 33; 34; 35; 36; 37; 38; 39;
     40; 41;
     42; 43; 44; 45; 46; 47; 48; 49;
     50; 51; 52; 53; 54; 55; 56; 57;
     58; 59;
     60; 61; 62; 63;
  |]

let dwarf_register_numbers ~reg_class =
  match reg_class with
  | 0 -> int_dwarf_reg_numbers
  | 1 -> float_dwarf_reg_numbers
  | _ -> Misc.fatal_errorf "Bad register class %d" reg_class

let stack_ptr_dwarf_register_number = 2

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command
    (Config.asm ^ " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let init () = ()
