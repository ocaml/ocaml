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
    r0 - r3                     general purpose (not preserved by C)
    r4 - r7                     general purpose (preserved)
    r8                          allocation pointer (preserved)
    r9                          platform register, usually reserved
    r10                         allocation limit (preserved)
    r11                         trap pointer (preserved)
    r12                         general purpose (not preserved by C)
    r13                         stack pointer
    r14                         return address
    r15                         program counter
*)

let int_reg_name = [|
  "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r12"
|]

let num_register_classes = 1

let register_class r = assert (r.typ <> Float); 0

let num_available_registers = [| 9 |]

let first_available_register = [| 0 |]

let register_name r = int_reg_name.(r)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 9 Reg.dummy in
  for i = 0 to 8 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let all_phys_regs = hard_int_reg

let phys_reg n = all_phys_regs.(n)

let stack_slot slot ty =
  assert (ty <> Float);
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

(* XXX float types have already been expanded into pairs of integers.
   So we cannot align these floats.  See if that causes a problem. *)

let calling_conventions first_int last_int make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
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
        assert false
  done;
  (loc, Misc.align !ofs 8)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 0 7 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 7 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 not_supported res in loc

let loc_external_arguments arg =
  calling_conventions 0 3 outgoing arg
let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 1 not_supported res in loc

let loc_exn_bucket = phys_reg 0

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* r4-r7 preserved *)
  Array.of_list(List.map phys_reg [0;1;2;3;8])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Ialloc(_)) -> [|phys_reg 8|]    (* r12 destroyed *)
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 4
  | _ -> 9
let max_register_pressure = function
    Iextcall(_, _) -> [| 4 |]
  | _ -> [| 9 |]

(* Layout of the stack *)

let num_stack_slots = [| 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

open Clflags;;
open Config;;
