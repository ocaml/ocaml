(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Description of the Alpha processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = true

(* Registers available for register allocation *)

(* Register map:
    $0 - $7     0 - 7       function results
    $8 - $12    8 - 12      general purpose ($9 - $15 are preserved by C)
    $13                     allocation pointer
    $14                     allocation limit
    $15                     trap pointer
    $16 - $22   13 - 19     function arguments
    $23 - $25               temporaries (for the code gen and for the asm)
    $26 - $30               stack ptr, global ptr, etc
    $31                     always zero

    $f0 - $f7   100 - 107   function results
    $f8 - $f15  108 - 115   general purpose ($f2 - $f9 preserved by C)
    $f16 - $f23 116 - 123   function arguments
    $f24 - $f30 124 - 129   general purpose
    $f28                    temporary
    $f31                    always zero *)

let int_reg_name = [|
  (* 0-7 *)    "$0"; "$1"; "$2"; "$3"; "$4"; "$5"; "$6"; "$7";
  (* 8-12 *)   "$8"; "$9"; "$10"; "$11"; "$12";
  (* 13-19 *)  "$16"; "$17"; "$18"; "$19"; "$20"; "$21"; "$22"
|]

let float_reg_name = [|
  (* 100-107 *) "$f0"; "$f1"; "$f2"; "$f3"; "$f4"; "$f5"; "$f6"; "$f7";
  (* 108-115 *) "$f8"; "$f9"; "$f10"; "$f11"; "$f12"; "$f13"; "$f14"; "$f15";
  (* 116-123 *) "$f16"; "$f17"; "$f18"; "$f19"; "$f20"; "$f21"; "$f22"; "$f23";
  (* 124-129 *) "$f24"; "$f25"; "$f26"; "$f27"; "$f29"; "$f30"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 20; 30 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 20 Reg.dummy in
  for i = 0 to 19 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.create 30 Reg.dummy in
  for i = 0 to 29 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float make_stack
                        arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
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
  (loc, Misc.align !ofs 16)         (* Keep stack 16-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 13 18 116 123 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 13 18 116 123 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 107 not_supported res in loc

(* On the Alpha, C functions have calling conventions similar to those
   for Caml functions, except that integer and floating-point registers
   for arguments are allocated "in sequence". E.g. a function
   taking a float f1 and two ints i2 and i3 will put f1 in the
   first float reg, i2 in the second int reg and i3 in the third int reg. *)

let ext_calling_conventions first_int last_int first_float last_float
                            make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int; incr int; incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float; incr int; incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, Misc.align !ofs 16)             (* Keep stack 16-aligned *)

let loc_external_arguments arg =
  ext_calling_conventions 13 18 116 121 outgoing arg
let loc_external_results res =
  let (loc, ofs) = ext_calling_conventions 0 0 100 100 not_supported res in loc
let extcall_use_push = false

let loc_exn_bucket = phys_reg 0         (* $0 *)

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* $9 - $12, $f2 - $f9 preserved *)
  Array.of_list(List.map phys_reg
    [0;1;2;3;4;5;6;7;8;13;14;15;16;17;18;19;
     100;101;110;111;112;113;114;115;116;117;118;119;120;121;122;123;124;
     125;126;127;128;129])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 4
  | _ -> 19
let max_register_pressure = function
    Iextcall(_, _) -> [| 4; 8 |]
  | _ -> [| 19; 29 |]

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  let as_cmd =
    if digital_asm && !Clflags.gprofile
    then Config.asm ^ " -pg"
    else Config.asm in
  Ccomp.command (as_cmd ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

open Clflags;;
open Config;;
