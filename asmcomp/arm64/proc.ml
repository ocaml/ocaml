(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of the ARM processor in 64-bit mode *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Integer register map:
    x0 - x15              general purpose (caller-save)
    x16, x17              temporaries (used by call veeners)
    x18                   platform register (reserved)
    x19 - x24             general purpose (callee-save)
    x25                   domain state pointer
    x26                   trap pointer
    x27                   alloc pointer
    x28                   alloc limit
    x29                   frame pointer
    x30                   return address
    sp / xzr              stack pointer / zero register
   Floating-point register map:
    d0 - d7               general purpose (caller-save)
    d8 - d15              general purpose (callee-save)
    d16 - d31             general purpose (caller-save)
*)

let int_reg_name =
  [| "x0";  "x1";  "x2";  "x3";  "x4";  "x5";  "x6";  "x7";
     "x8";  "x9";  "x10"; "x11"; "x12"; "x13"; "x14"; "x15";
     "x19"; "x20"; "x21"; "x22"; "x23"; "x24";
     "x25"; "x26"; "x27"; "x28"; "x16"; "x17" |]

let float_reg_name =
  [| "d0";  "d1";  "d2";  "d3";  "d4";  "d5";  "d6";  "d7";
     "d8";  "d9";  "d10"; "d11"; "d12"; "d13"; "d14"; "d15";
     "d16"; "d17"; "d18"; "d19"; "d20"; "d21"; "d22"; "d23";
     "d24"; "d25"; "d26"; "d27"; "d28"; "d29"; "d30"; "d31" |]

let num_register_classes = 2

let register_class r =
  match r.typ with
  | Val | Int | Addr  -> 0
  | Float -> 1

let num_available_registers =
  [| 22; 32 |] (* first 22 int regs allocatable; all float regs allocatable *)

let first_available_register =
  [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.make 28 Reg.dummy in
  for i = 0 to 27 do
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

let reg_x8 = phys_reg 8
let reg_d7 = phys_reg 107

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

let loc_spacetime_node_hole = Reg.dummy  (* Spacetime unsupported *)

(* Calling conventions *)

let loc_int last_int make_stack int ofs =
  if !int <= last_int then begin
    let l = phys_reg !int in
    incr int; l
  end else begin
    ofs := Misc.align !ofs size_int;
    let l = stack_slot (make_stack !ofs) Int in
    ofs := !ofs + size_int; l
  end

let loc_float last_float make_stack float ofs =
  if !float <= last_float then begin
    let l = phys_reg !float in
    incr float; l
  end else begin
    ofs := Misc.align !ofs size_float;
    let l = stack_slot (make_stack !ofs) Float in
    ofs := !ofs + size_float; l
  end

let loc_int32 last_int make_stack int ofs =
  if !int <= last_int then begin
    let l = phys_reg !int in
    incr int; l
  end else begin
    let l = stack_slot (make_stack !ofs) Int in
    ofs := !ofs + (if macosx then 4 else 8);
    l
  end

let calling_conventions
    first_int last_int first_float last_float make_stack arg =
  let loc = Array.make (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i) with
    | Val | Int | Addr ->
        loc.(i) <- loc_int last_int make_stack int ofs
    | Float ->
        loc.(i) <- loc_float last_float make_stack float ofs
  done;
  (loc, Misc.align !ofs 16)  (* keep stack 16-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported _ofs = fatal_error "Proc.loc_results: cannot call"

(* OCaml calling convention:
     first integer args in r0...r15
     first float args in d0...d15
     remaining args on stack.
   Return values in r0...r15 or d0...d15. *)

let max_arguments_for_tailcalls = 16
let last_int_register = if macosx then 7 else 15

let loc_arguments arg =
  calling_conventions 0 last_int_register 100 115 outgoing arg
let loc_parameters arg =
  let (loc, _) = calling_conventions 0 last_int_register 100 115 incoming arg in loc
let loc_results res =
  let (loc, _) = calling_conventions 0 last_int_register 100 115 not_supported res in loc

(* C calling convention:
     first integer args in r0...r7
     first float args in d0...d7
     remaining args on stack.
   macOS/iOS peculiarity: int32 arguments passed on stack occupy 4 bytes,
   while the AAPCS64 says 8 bytes.
   Return values in r0...r1 or d0. *)

let external_calling_conventions
    first_int last_int first_float last_float make_stack ty_args =
  let loc = Array.make (List.length ty_args) [| Reg.dummy |] in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  List.iteri (fun i ty_arg ->
    begin match ty_arg with
    | XInt | XInt64 ->
        loc.(i) <- [| loc_int last_int make_stack int ofs |]
    | XInt32 ->
        loc.(i) <- [| loc_int32 last_int make_stack int ofs |]
    | XFloat ->
        loc.(i) <- [| loc_float last_float make_stack float ofs |]
    end)
    ty_args;
  (loc, Misc.align !ofs 16)  (* keep stack 16-aligned *)

let loc_external_arguments ty_args =
  external_calling_conventions 0 7 100 107 outgoing ty_args

let loc_external_results res =
  let (loc, _) = calling_conventions 0 1 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 0

(* See "DWARF for the ARM 64-bit architecture (AArch64)" available from
   developer.arm.com. *)

let int_dwarf_reg_numbers =
  [| 0; 1; 2; 3; 4; 5; 6; 7;
     8; 9; 10; 11; 12; 13; 14; 15;
     19; 20; 21; 22; 23; 24;
     25; 26; 27; 28; 16; 17;
  |]

let float_dwarf_reg_numbers =
  [| 64; 65; 66; 67; 68; 69; 70; 71;
     72; 73; 74; 75; 76; 77; 78; 79;
     80; 81; 82; 83; 84; 85; 86; 87;
     88; 89; 90; 91; 92; 93; 94; 95;
  |]

let dwarf_register_numbers ~reg_class =
  match reg_class with
  | 0 -> int_dwarf_reg_numbers
  | 1 -> float_dwarf_reg_numbers
  | _ -> Misc.fatal_errorf "Bad register class %d" reg_class

let stack_ptr_dwarf_register_number = 31

(* Volatile registers: none *)

let regs_are_volatile _rs = false

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  (* x19-x28, d8-d15 preserved *)
  Array.of_list (List.map phys_reg
    [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;
     100;101;102;103;104;105;106;107;
     116;117;118;119;120;121;122;123;
     124;125;126;127;128;129;130;131])

let destroyed_at_oper = function
  | Iop(Icall_ind _ | Icall_imm _) | Iop(Iextcall { alloc = true; }) ->
      all_phys_regs
  | Iop(Iextcall { alloc = false; }) ->
      destroyed_at_c_call
  | Iop(Ialloc _) ->
      [| reg_x8 |]
  | Iop(Iintoffloat | Ifloatofint | Iload(Single, _) | Istore(Single, _, _)) ->
      [| reg_d7 |]            (* d7 / s7 destroyed *)
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

let destroyed_at_reloadretaddr = [| |]

(* Maximal register pressure *)

let safe_register_pressure = function
  | Iextcall _ -> 8
  | Ialloc _ -> 24
  | _ -> 25

let max_register_pressure = function
  | Iextcall _ -> [| 10; 8 |]
  | Ialloc _ -> [| 24; 32 |]
  | Iintoffloat | Ifloatofint
  | Iload(Single, _) | Istore(Single, _, _) -> [| 25; 31 |]
  | _ -> [| 25; 32 |]

(* Pure operations (without any side effect besides updating their result
   registers). *)

let op_is_pure = function
  | Icall_ind _ | Icall_imm _ | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _
  | Iintop(Icheckbound _) | Iintop_imm(Icheckbound _, _)
  | Ispecific(Ishiftcheckbound _) -> false
  | _ -> true

(* Layout of the stack *)
let frame_required fd =
  fd.fun_contains_calls
    || fd.fun_num_stack_slots.(0) > 0
    || fd.fun_num_stack_slots.(1) > 0

let prologue_required fd =
  frame_required fd

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " " ^
                 (String.concat " " (Misc.debug_prefix_map_flags ())) ^
                 " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)


let init () = ()
