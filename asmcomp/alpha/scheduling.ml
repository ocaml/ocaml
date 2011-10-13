(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Arch
open Mach

(* The Digital Unix assembler does scheduling better than us.
   However, the Linux-Alpha assembler does not do scheduling, so we do
   a feeble attempt here. *)

class scheduler = object (self)

inherit Schedgen.scheduler_generic as super

(* Latencies (in cycles). Based on the 21064, with some poetic license. *)

method oper_latency = function
    Ireload -> 3
  | Iload(_, _) -> 3
  | Iconst_symbol _ -> 3 (* turned into a load *)
  | Iconst_float _ -> 3 (* ends up in a load *)
  | Iintop(Imul) -> 23
  | Iintop_imm(Imul, _) -> 23
  | Iaddf -> 6
  | Isubf -> 6
  | Imulf -> 6
  | Idivf -> 63
  | _ -> 2
    (* Most arithmetic instructions can be executed back-to-back in 1 cycle.
       However, some combinations (arith; load or arith; store) require 2
       cycles.  Also, by claiming 2 cycles instead of 1, we might favor
       dual issue. *)

(* Issue cycles.  Rough approximations. *)

method oper_issue_cycles = function
    Iconst_float _ -> 4                 (* load from $gp, then load *)
  | Ialloc _ -> 4
  | Iintop(Icheckbound) -> 2
  | Iintop_imm(Idiv, _) -> 3
  | Iintop_imm(Imod, _) -> 5
  | Iintop_imm(Icheckbound, _) -> 2
  | Ifloatofint -> 10
  | Iintoffloat -> 10
  | _ -> 1

(* Say that reloadgp is not part of a basic block (prevents moving it
   past an operation that uses $gp) *)

method oper_in_basic_block = function
    Ispecific(Ireloadgp _) -> false
  | op -> super#oper_in_basic_block op

end

let fundecl =
  if digital_asm
  then (fun f -> f)
  else (new scheduler)#schedule_fundecl
