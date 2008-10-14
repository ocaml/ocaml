(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Description of the IA64 processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Register map:
    r0                      always 0
    r1                      global pointer (gp)
    r2 - r3                 temporaries (for the code generator)
    r4                      allocation pointer
    r5                      allocation limit
    r6                      trap pointer
    r7                      saved gp during C calls (preserved by C)
    r8 - r11       0 - 3    function results
    r12                     stack pointer
    r13                     reserved by C (thread-specific data)
    r14 - r15     80 - 81   temporaries (for accessing stack variables)
    r16 - r31      4 - 19   general purpose
    r32 - r63     20 - 51   function arguments
    r64 - r91     52 - 79   general purpose
    r92 - r95               used by C glue code

  We do not use register windows, but instead allocate 64 "out" registers
  (r32-r95) when entering Caml code.

    f0                        always 0.0
    f1                        always 1.0
    f2 - f5       100 - 103   general purpose (preserved by C)
    f6 - f7       104 - 105   general purpose
    f8 - f15      106 - 113   function results
    f16 - f31     114 - 129   function arguments (preserved by C)
    f32 - f63     130 - 161   general purpose
    f64 - f66                 temporaries
    f67 - f127                unused
*)

let int_reg_name = [|
  (* 0-3 *)    "r8"; "r9"; "r10"; "r11";
  (* 4-19 *)   "r16"; "r17"; "r18"; "r19"; "r20"; "r21"; "r22"; "r23";
               "r24"; "r25"; "r26"; "r27"; "r28"; "r29"; "r30"; "r31";
  (* 20-51 *)  "r32"; "r33"; "r34"; "r35"; "r36"; "r37"; "r38"; "r39";
               "r40"; "r41"; "r42"; "r43"; "r44"; "r45"; "r46"; "r47";
               "r48"; "r49"; "r50"; "r51"; "r52"; "r53"; "r54"; "r55";
               "r56"; "r57"; "r58"; "r59"; "r60"; "r61"; "r62"; "r63";
  (* 52-79 *)  "r64"; "r65"; "r66"; "r67"; "r68"; "r69"; "r70"; "r71";
               "r72"; "r73"; "r74"; "r75"; "r76"; "r77"; "r78"; "r79";
               "r80"; "r81"; "r82"; "r83"; "r84"; "r85"; "r86"; "r87";
               "r88"; "r89"; "r90"; "r91";
  (* 80-81 *)  "r14"; "r15"
|]
  
let float_reg_name = [|
  (* 0-13 *)   "f2"; "f3"; "f4"; "f5"; "f6"; "f7";
               "f8"; "f9"; "f10"; "f11"; "f12"; "f13"; "f14"; "f15"; 
  (* 14-29 *)  "f16"; "f17"; "f18"; "f19"; "f20"; "f21"; "f22"; "f23";
               "f24"; "f25"; "f26"; "f27"; "f28"; "f29"; "f30"; "f31";
  (* 30-61 *)  "f32"; "f33"; "f34"; "f35"; "f36"; "f37"; "f38"; "f39";
               "f40"; "f41"; "f42"; "f43"; "f44"; "f45"; "f46"; "f47";
               "f48"; "f49"; "f50"; "f51"; "f52"; "f53"; "f54"; "f55";
               "f56"; "f57"; "f58"; "f59"; "f60"; "f61"; "f62"; "f63"
|]

let num_register_classes = 2

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 80; 62 |]

let first_available_register = [| 0; 100 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else float_reg_name.(r - 100)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 82 Reg.dummy in
  for i = 0 to 81 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.create 62 Reg.dummy in
  for i = 0 to 61 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let all_phys_regs =
  Array.append hard_int_reg hard_float_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else hard_float_reg.(n - 100)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float
                        lockstep make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int;
          if lockstep then incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float;
          if lockstep then incr int
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
  calling_conventions 20 51 114 129 false outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 20 51 114 129 false incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 3 106 113 false not_supported res
  in loc
(* Arguments in r32...r39, f8...f15
   Results in r8...r11, f8...f15 *)
let loc_external_arguments arg =
  calling_conventions 20 27 106 113 true outgoing arg
let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 3 106 113 false not_supported res
  in loc
let extcall_use_push = false

let loc_exn_bucket = phys_reg 0         (* r8 *)

(* Registers destroyed by operations *)

let destroyed_at_c_call =    (* f2...f5, f16...f31 preserved by C *)
  Array.append
    hard_int_reg
    (Array.of_list(List.map phys_reg
        [100;101;102;103;104;105;106;107;108;109;110;111;112;113;
         130;131;132;133;134;135;136;137;138;139;
         140;141;142;143;144;145;146;147;148;149;
         150;151;152;153;154;155;156;157;158;159;
         160;161]))

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure *)

let safe_register_pressure = function
    Iextcall(_, _) -> 0
  | _ -> 62
let max_register_pressure = function
    Iextcall(_, _) -> [| 0; 20 |]
  | _ -> num_available_registers

(* Layout of the stack *)

let num_stack_slots = [| 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

open Clflags;;
open Config;;
