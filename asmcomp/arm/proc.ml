(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Description of the ARM processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Register map:
    r0 - r7                     general purpose (r4 - r7 preserved by C)
    r8                          allocation pointer (preserved by C)
    r9                          allocation limit (preserved by C)
    r10                         general purpose
    r11                         trap pointer (preserved by C)
    r12                         general purpose
    r13                         stack pointer
    r14                         return address
    r15                         program counter

    f0 - f6                     general purpose (f4 - f6 preserved by C)
    f7                          temporary
*)

let int_reg_name = [|
  "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r10"; "r12"
|]

let float_reg_name = [|
  "f0"; "f1"; "f2"; "f3"; "f4"; "f5"; "f6"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 10; 7 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 10 Reg.dummy in
  for i = 0 to 9 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.create 7 Reg.dummy in
  for i = 0 to 6 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float
                        make_stack arg =
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
  (loc, !ofs)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 7 100 103 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 7 100 103 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 103 not_supported res in loc

(* Calling conventions for C are as for Caml, except that float arguments
   are passed in pairs of integer registers. *)

let loc_external_arguments arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let reg = ref 0 in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !reg <= 3 then begin
          loc.(i) <- phys_reg !reg;
          incr reg
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !reg <= 2 then begin
          loc.(i) <- phys_reg !reg;
          reg := !reg + 2
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, !ofs)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 0

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* r4-r9, f4-f6 preserved *)
  Array.of_list(List.map phys_reg [0;1;2;3;8;9; 100;101;102;103])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Ialloc(_)) -> [|phys_reg 8|]    (* r10 destroyed *)
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 4
  | _ -> 7
let max_register_pressure = function
    Iextcall(_, _) -> [| 4; 4 |]
  | _ -> [| 10; 7 |]

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Sys.command ("as -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

open Clflags;;
open Config;;
