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

open Cmm
open Reg
open Arch
open Mach
open Proc
open Linearize

(* (Feeble attempt at) Instruction scheduling for the IA64 *)

(* Prior to scheduling, we rewrite the code to split reg -> stack
   and stack -> reg moves into two instructions: a "stackaddr" instruction
   that takes the address of the stack variable, and a "spill" or "reload"
   instruction that performs the stack access proper.  This way,
   the "stackaddr" instructions can be scheduled earlier. *)

let temp_counter = ref 0
let temporaries = [| phys_reg 80 (*r14*); phys_reg 81 (*r15*) |]
let new_temp () =
  let r = temporaries.(!temp_counter) in
  incr temp_counter;
  if !temp_counter >= Array.length temporaries then temp_counter := 0;
  r

let rec fixup_stack_accesses i =
  match i.desc with
    Lend -> i
  | Lop(Imove | Ireload | Ispill) ->
      let src = i.arg.(0) and dst = i.res.(0) in
      begin match (src.loc, dst.loc) with
        (Reg _, Reg _) ->
          { i with next = fixup_stack_accesses i.next }
      | (Stack _, Reg _) ->
          let tmp = new_temp() in
          instr_cons (Lop(Ispecific Istackaddr)) [|src|] [|tmp|]
              (instr_cons (Lop Ireload) [|tmp|] [|dst|]
                  (fixup_stack_accesses i.next))
      | (Reg _, Stack _) ->
          let tmp = new_temp() in
          instr_cons (Lop(Ispecific Istackaddr)) [|dst|] [|tmp|]
              (instr_cons (Lop Ispill) [|src; tmp|] [||]
                  (fixup_stack_accesses i.next))
      | (_, _) ->
          assert false
      end
  | _ ->
      { i with next = fixup_stack_accesses i.next }

(* The basic-block scheduler proper *)

class scheduler = object (self)

inherit Schedgen.scheduler_generic as super

(* Latencies (in cycles). Based on the Itanium, with considerable poetic
   licence.  All latencies are tripled in an attempt to favor dual- or
   triple-issue. *)

(* Most integer operations: 1 cycle --> 3
   Shifts with variable count: 2 cycles --> 6
   Float add, sub, mult, multadd: 5 cycles --> 15
   FP integer multiply: 7 cycles --> 21
   Int loads: 2 cycles --> 6
   Float loads: 9 cycles --> 27
   GP to FP register move: 7 cycles --> 21
   FP to GP register move: 2 cycles --> 6
*)

method oper_latency = function
    Ireload -> 6
  | Iload(kind, _) ->
      begin match kind with Single | Double | Double_u -> 27 | _ -> 6 end
  | Iconst_symbol _ -> 6 (* turned into a load *)
  | Iconst_float _ -> 21 (* ends up in a GP to FP register move *)
  | Iintop(Imul) -> 6 (* ends up in a FP to GP register move *)
  | Iintop(Ilsl | Ilsr | Iasr) -> 6
  | Iaddf -> 15
  | Isubf -> 15
  | Imulf -> 15
  | Idivf -> 15
  | Ispecific(Imultaddf | Imultsubf | Isubmultf) -> 15
  | _ -> 3

(* Issue cycles.  Rough approximations.  E.g. an operation that expands
   into 2 dependent one-cycle operations is considered to waste 3 issue slots.
   (Depending on the grouping with surrounding instructions, this could
   be as low as 2 or as high as 6.)  We adjust upward if the first operation
   has longer latency. *)

method oper_issue_cycles = function
    Iconst_float _ -> 3
  | Iconst_symbol _ -> 3
  | Iload((Byte_signed | Sixteen_signed | Thirtytwo_signed), _) -> 6
  | Ialloc _ -> 4
  | Iintop(Imul) -> 25
  | Iintop(Icomp _) -> 5
  | Iintop(Icheckbound) -> 3
  | Iintop_imm(Imul, _) -> 12
  | Iintop_imm(Idiv, _) -> 12
  | Iintop_imm(Imod, _) -> 12
  | Iintop_imm(Icheckbound, _) -> 3
  | Idivf -> 24
  | Ifloatofint -> 45
  | Iintoffloat -> 45
  | _ -> 1

(* Say that Istoreincr terminates a basic block *)

method oper_in_basic_block = function
    Ispecific(Istoreincr _) -> false
  | op -> super#oper_in_basic_block op

end

let schedule_fundecl = (new scheduler)#schedule_fundecl

let fundecl f =
  schedule_fundecl {f with fun_body = fixup_stack_accesses f.fun_body}
