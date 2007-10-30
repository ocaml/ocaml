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

(* Description of the Mips processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Register map:
    $0                          always 0
    $1                          temporary for the assembler
    $2 - $7     0 - 5           function results
    $8 - $15    6 - 13          function arguments
    $16 - $21   14 - 19         general purpose (preserved by C)
    $22                         allocation pointer (preserved by C)
    $23                         allocation limit (preserved by C)
    $24 - $25                   temporaries
    $26 - $29                   kernel regs, stack pointer, global pointer
    $30                         trap pointer (preserved by C)
    $31                         return address

    $f0 - $f3   100 - 103       function results
    $f4 - $f11  104 - 111       general purpose
    $f12 - $f19 112 - 119       function arguments
    $f20 - $f30 120 - 130       general purpose (even numbered preserved by C)
    $f31                        temporary *)

let int_reg_name = [|
  (* 0-5 *)    "$2"; "$3"; "$4"; "$5"; "$6"; "$7";
  (* 6-13 *)   "$8"; "$9"; "$10"; "$11"; "$12"; "$13"; "$14"; "$15";
  (* 14-19 *)  "$16"; "$17"; "$18"; "$19"; "$20"; "$21"
|]
  
let float_reg_name = [|
  "$f0"; "$f1"; "$f2"; "$f3"; "$f4";
  "$f5"; "$f6"; "$f7"; "$f8"; "$f9";
  "$f10"; "$f11"; "$f12"; "$f13"; "$f14";
  "$f15"; "$f16"; "$f17"; "$f18"; "$f19";
  "$f20"; "$f21"; "$f22"; "$f23"; "$f24";
  "$f25"; "$f26"; "$f27"; "$f28"; "$f29"; "$f30"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 20; 31 |]

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
  let v = Array.create 31 Reg.dummy in
  for i = 0 to 30 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
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
  (loc, Misc.align !ofs 16)         (* Keep stack 16-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 6 13 112 119 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 6 13 112 119 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 100 103 not_supported res in loc

(* The C calling conventions are as follows:
   the first 8 arguments are passed either in integer regs $4...$11
   or float regs $f12...$f19.  Each argument "consumes" both one slot
   in the int register file and one slot in the float register file.
   Extra arguments are passed on stack, in a 64-bits slot, right-justified
   (i.e. at +4 from natural address). *)   

let loc_external_arguments arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref 2 in
  let float = ref 112 in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    if i < 8 then begin
      loc.(i) <- phys_reg (if arg.(i).typ = Float then !float else !int);
      incr int;
      incr float
    end else begin
      begin match arg.(i).typ with
        Float -> loc.(i) <- stack_slot (Outgoing !ofs) Float
      | ty    -> loc.(i) <- stack_slot (Outgoing (!ofs + 4)) ty
      end;
      ofs := !ofs + 8
    end
  done;
  (loc, Misc.align !ofs 16)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 0         (* $2 *)

(* Registers destroyed by operations *)

let destroyed_at_c_call =
  (* $16 - $21, $f20, $f22, $f24, $f26, $f28, $f30 preserved *)
  Array.of_list(List.map phys_reg
    [0;1;2;3;4;5;6;7;8;9;10;11;12;13;
     100;101;102;103;104;105;106;107;108;109;110;111;112;113;114;
     115;116;117;118;119;121;123;125;127;129])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 6
  | _ -> 20
let max_register_pressure = function
    Iextcall(_, _) -> [| 6; 6 |]
  | _ -> [| 20; 31 |]

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

open Clflags;;
open Config;;
