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

(* Description of the Mips processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Exceptions raised to signal cases not handled here *)

exception Use_default

(* Instruction selection *)

let select_addressing = function
    Cconst_symbol s ->
      (Ibased(s, 0), Ctuple [])
  | Cop(Cadda, [Cconst_symbol s; Cconst_int n]) ->
      (Ibased(s, n), Ctuple [])
  | Cop(Cadda, [arg; Cconst_int n]) ->
      (Iindexed n, arg)
  | Cop(Cadda, [arg1; Cop(Caddi, [arg2; Cconst_int n])]) ->
      (Iindexed n, Cop(Cadda, [arg1; arg2]))
  | arg ->
      (Iindexed 0, arg)

let select_oper op args = raise Use_default

let select_store addr exp = raise Use_default

let select_push exp = fatal_error "Proc: select_push"

let pseudoregs_for_operation op arg res = raise Use_default

let is_immediate (n:int) = true

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

    $f0 - $f8   100 - 104       function results
    $f10                        temporary
    $f12 - $f18 105 - 108       function arguments
    $f20 - $f30 109 - 114       general purpose (preserved by C) *)

let int_reg_name = [|
  (* 0-5 *)    "$2"; "$3"; "$4"; "$5"; "$6"; "$7";
  (* 6-13 *)   "$8"; "$9"; "$10"; "$11"; "$12"; "$13"; "$14"; "$15";
  (* 14-19 *)  "$16"; "$17"; "$18"; "$19"; "$20"; "$21"
|]
  
let float_reg_name = [|
  (* 100-104 *) "$f0"; "$f2"; "$f4"; "$f6"; "$f8";
  (* 105-108 *) "$f12"; "$f14"; "$f16"; "$f18";
  (* 109-114 *) "$f20"; "$f22"; "$f24"; "$f26"; "$f28"; "$f30"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 20; 15 |]

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
  let v = Array.create 15 Reg.dummy in
  for i = 0 to 14 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float
                        initial_stack_ofs make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref initial_stack_ofs in
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
  (loc, Misc.align !ofs 8)         (* Keep stack 8-aligned *)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

let loc_arguments arg =
  calling_conventions 6 13 105 108 0 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 6 13 105 108 0 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 5 100 104 0 not_supported res in loc

(* The C compiler calling conventions are peculiar, especially when
   a mix of ints and floats are passed. Here, we trap a few special
   cases and revert to [calling_conventions] when only scalars are
   passed. *)

let loc_external_arguments arg =
  try
    for i = 0 to Array.length arg - 1 do
      if arg.(i).typ = Float then raise Exit
    done;
    (* Always reserve 4 words at the bottom of the stack *)
    calling_conventions 2 5 0 0 16 outgoing arg
  with Exit ->
    match Array.to_list(Array.map register_class arg) with
      [1] ->                          (* $f12 *)
         [| phys_reg 105 |], 16
    | [1;1] ->                        (* $f12, $f14 *)
         [| phys_reg 105; phys_reg 106 |], 16
    | [1;0] ->                        (* $f12, $6 *)
         [| phys_reg 105; phys_reg 4 |], 16
    | [0;1] ->                        (* $4, $6 (needs fpreg -> $6,$7 move) *)
         [| phys_reg 2; phys_reg 4 |], 16
    | [0;0;1] ->                      (* $4, $5, $6 (ditto) *)
         [| phys_reg 2; phys_reg 3; phys_reg 4 |], 16
    | _ ->
         fatal_error "Proc_mips.loc_external_arguments"

let extcall_use_push = false

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 0 not_supported res in loc

let loc_exn_bucket = phys_reg 0         (* $2 *)

(* Registers destroyed by operations *)

let destroyed_at_c_call =               (* $16 - $21, $f20 - $f30 preserved *)
  Array.of_list(List.map phys_reg
    [0;1;2;3;4;5;6;7;8;9;10;11;12;13;
     100;101;102;103;104;105;106;107;108])

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
  | _ -> [| 20; 15 |]

(* Reloading *)

let reload_test makereg round tst args = raise Use_default
let reload_operation makereg round op args res = raise Use_default

(* No scheduling is needed, the assembler does it better than us. *)

let need_scheduling = false

let oper_latency _ = 1

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let asm_command =
  match Config.system with
    "ultrix" -> "as -O2 -nocpp -o "
  | "irix"   -> "as -32 -O2 -nocpp -o "
  | _ -> fatal_error "Proc_mips.asm_command"

let assemble_file infile outfile =
  Ccomp.command (asm_command ^ outfile ^ " " ^ infile)

