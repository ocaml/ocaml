(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Description of the Motorola 680x0 processor *)

open Misc
open Arch
open Format
open Cmm
open Reg
open Mach

(* Registers available for register allocation *)

(* Register map:
    A0 - A6     0-6     address registers (A2-A6 callee-save)
    A7                  stack pointer
    D0 - D4     7-11    data registers (D2 - D7 callee-save)
    D5                  temporary
    D6                  allocation pointer
    D7                  trap pointer
    FP0 - FP7   12-19   floating-point registers (FP2 - FP7 callee-save)
*)

let register_names =
  [| "a0"; "a1"; "a2"; "a3"; "a4"; "a5"; "a6";
     "d0"; "d1"; "d2"; "d3"; "d4";
     "fp0"; "fp1"; "fp2"; "fp3"; "fp4"; "fp5"; "fp6"; "fp7" |]

let num_register_classes = 3

let register_class r =
  match r.typ with
    Addr -> 0
  | Int -> 1
  | Float -> 2

let num_available_registers = [| 7; 5; 8 |]

let first_available_register = [| 0; 7; 12 |]

let register_name r = register_names.(r)

(* There is no scheduling, so just pack registers. *)

let rotate_registers = false

(* Representation of hard registers by pseudo-registers *)

let all_phys_regs =
  let v = Array.create 20 Reg.dummy in
  for i = 0 to 6 do v.(i) <- Reg.at_location Addr (Reg i) done;
  for i = 7 to 11 do v.(i) <- Reg.at_location Int (Reg i) done;
  for i = 12 to 19 do v.(i) <- Reg.at_location Float (Reg i) done;
  v

let phys_reg n = all_phys_regs.(n)

let stack_slot slot ty = Reg.at_location ty (Stack slot)

let reg_A0 = phys_reg 0
let reg_FP0 = phys_reg 12

(* Instruction selection *)

let word_addressed = false

(* Calling conventions *)

let calling_conventions first_addr last_addr first_float last_float
                        make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let addr = ref first_addr in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      (Addr | Int) as ty ->
        if !addr <= last_addr then begin
          loc.(i) <- phys_reg !addr;
          incr addr
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_addr
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
  calling_conventions 0 5 12 18 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 5 12 18 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 12 18 not_supported res in loc
let extcall_use_push = true
let loc_external_arguments arg =
  fatal_error "Proc.loc_external_arguments"
let loc_external_results res =
  let (loc, ofs) = calling_conventions 7 7 12 12 not_supported res in loc

let loc_exn_bucket = reg_A0

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  Array.of_list(List.map phys_reg [0; 1; 7; 8; 12; 13])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | Iop(Iintoffloat) -> [| reg_FP0 |]
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure op = 5

let max_register_pressure = function
    Iextcall(_, _) -> [| 5; 3; 6 |]
  | Iintoffloat -> [| 7; 5; 7 |]
  | _ -> num_available_registers

(* Layout of the stack frame *)

let num_stack_slots = [| 0; 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command ("as -o " ^ outfile ^ " " ^ infile)

