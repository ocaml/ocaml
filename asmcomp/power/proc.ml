# 2 "asmcomp/power/proc.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of the Power PC *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    0                   temporary, null register for some operations
    1                   stack pointer
    2                   pointer to table of contents
    3 - 10              function arguments and results
    11 - 12             temporaries
    13                  pointer to small data area
    14 - 28             general purpose, preserved by C
    29                  trap pointer
    30                  domain state pointer
    31                  allocation pointer
  Floating-point register map:
    0                   temporary
    1 - 13              function arguments and results
    14 - 31             general purpose, preserved by C
*)

let int_reg_name =
  [| "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10";           (* 0 - 7 *)
     "14"; "15"; "16"; "17"; "18"; "19"; "20"; "21";    (* 8 - 15 *)
     "22"; "23"; "24"; "25"; "26"; "27"; "28" |]        (* 16 - 22 *)

let float_reg_name =
  [| "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7";
     "8"; "9"; "10"; "11"; "12"; "13"; "14"; "15";
     "16"; "17"; "18"; "19"; "20"; "21"; "22"; "23";
     "24"; "25"; "26"; "27"; "28"; "29"; "30"; "31" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
  | Val | Int | Addr -> 0
  | Float -> 1

let num_available_registers = [| 23; 32 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 23 Reg.dummy in
  for i = 0 to 22 do v.(i) <- Reg.at_location Int (Reg i) done; v

let hard_float_reg =
  let v = Array.make 32 Reg.dummy in
  for i = 0 to 31 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done; v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let size_domainstate_args = 64 * size_int

let loc_int last_int make_stack reg_use_stack int ofs =
  if !int <= last_int then begin
    let l = phys_reg !int in
    incr int;
    if reg_use_stack then ofs := !ofs + size_int;
    l
  end else begin
    let l = stack_slot (make_stack !ofs) Int in
    ofs := !ofs + size_int; l
  end

let loc_float last_float make_stack reg_use_stack int float ofs =
  if !float <= last_float then begin
    let l = phys_reg !float in
    incr float;
    (* Passing a float in a float register reserves a normal register as well *)
    incr int;
    if reg_use_stack then ofs := !ofs + size_float;
    l
  end else begin
    ofs := Misc.align !ofs size_float;
    let l = stack_slot (make_stack !ofs) Float in
    ofs := !ofs + size_float; l
  end

let calling_conventions first_int last_int first_float last_float
      make_stack first_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref first_stack in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | Val | Int | Addr ->
        loc.(i) <- loc_int last_int make_stack false int ofs
    | Float ->
        loc.(i) <- loc_float last_float make_stack false int float ofs
  done;
  (loc, Misc.align (max 0 !ofs) 16)  (* keep stack 16-aligned *)

let incoming ofs =
  if ofs >= 0
  then Incoming ofs
  else Domainstate (ofs + size_domainstate_args)
let outgoing ofs =
  if ofs >= 0
  then Outgoing ofs
  else Domainstate (ofs + size_domainstate_args)
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

let max_arguments_for_tailcalls = 16 (* in regs *) + 64 (* in domain state *)

let loc_arguments arg =
    calling_conventions 0 15 101 113 outgoing (- size_domainstate_args) arg

let loc_parameters arg =
  let (loc, _ofs) =
    calling_conventions 0 15 101 113 incoming (- size_domainstate_args) arg
  in loc

let loc_results res =
  let (loc, _ofs) = calling_conventions 0 15 101 113 not_supported 0 res
  in loc

(* C calling conventions for ELF64v2:
     Use GPR 3-10 for the first integer arguments.
     Use FPR 1-13 for the first float arguments.
     If all arguments fit in registers, don't reserve stack space.
     Otherwise, reserve stack space for all arguments.
     Always reserve 32 bytes at bottom of stack, plus whatever is needed
     to hold the arguments.
     The reserved 32 bytes are automatically added in emit.mlp
     and need not appear here.
*)

let external_calling_conventions
    first_int last_int first_float last_float
    make_stack stack_ofs reg_use_stack ty_args =
  let loc = Array.make (List.length ty_args) [| Reg.dummy |] in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref stack_ofs in
  List.iteri
    (fun i ty_arg ->
      match ty_arg with
      | XInt | XInt32 | XInt64 ->
        loc.(i) <-
          [| loc_int last_int make_stack reg_use_stack int ofs |]
      | XFloat ->
        loc.(i) <-
          [| loc_float last_float make_stack reg_use_stack int float ofs |])
    ty_args;
  (loc, Misc.align !ofs 16) (* Keep stack 16-aligned *)

let loc_external_arguments ty_args =
  let (loc, ofs) =
    external_calling_conventions 0 7 101 113 outgoing 0 true ty_args in
  if Array.exists
       (fun r ->
          assert (Array.length r = 1);
          match r.(0).loc with Stack _ -> true | _ -> false)
       loc
  then (loc, ofs)
  else (loc, 0)

(* Results are in GPR 3 and FPR 1 *)

let loc_external_results res =
  let (loc, _ofs) = calling_conventions 0 1 101 101 not_supported 0 res
  in loc

(* Exceptions are in GPR 3 *)

let loc_exn_bucket = phys_reg 0

(* For ELF64v2 see:
   "64-Bit ELF V2 ABI Specification -- Power Architecture"
   http://openpowerfoundation.org/wp-content/uploads/resources/leabi/
     content/dbdoclet.50655239___RefHeading___Toc377640569.html
*)

let int_dwarf_reg_numbers =
  [| 3; 4; 5; 6; 7; 8; 9; 10;
     14; 15; 16; 17; 18; 19; 20; 21;
     22; 23; 24; 25; 26; 27;
  |]

let float_dwarf_reg_numbers =
  [| 32; 33; 34; 35; 36; 37; 38; 39;
     40; 41; 42; 43; 44; 45; 46; 47;
     48; 49; 50; 51; 52; 53; 54; 55;
     56; 57; 58; 59; 60; 61; 62; 63;
  |]

let dwarf_register_numbers ~reg_class =
  match reg_class with
  | 0 -> int_dwarf_reg_numbers
  | 1 -> float_dwarf_reg_numbers
  | _ -> Misc.fatal_errorf "Bad register class %d" reg_class

let stack_ptr_dwarf_register_number = 1

(* Registers destroyed by operations *)

(* For direct C calls, all caller-save registers are destroyed,
   plus GPR28 because it is used to save the OCaml stack pointer. *)
let destroyed_at_c_call =
  Array.of_list(List.map phys_reg
    [0; 1; 2; 3; 4; 5; 6; 7; 22;
     100; 101; 102; 103; 104; 105; 106; 107; 108; 109; 110; 111; 112; 113])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall { alloc = true; _ }) ->
      all_phys_regs
  | Iop(Iextcall { alloc = false; _ }) ->
      destroyed_at_c_call
  | Iop(Iintoffloat | Istore(Single, _, _)) ->
      [| phys_reg 100 |] (* FPR0 destroyed *)
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| phys_reg 11 |]

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall _ -> 13
  | _ -> 23

let max_register_pressure = function
    Iextcall _ -> [| 13; 18 |]
  | Iintoffloat | Istore(Single, _, _) -> [| 23; 31 |]
  | _ -> [| 23; 32 |]

(* Layout of the stack *)

let frame_required _ = true

let prologue_required _ = true

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " " ^
                 (String.concat " " (Misc.debug_prefix_map_flags ())) ^
                 " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let init () = ()
