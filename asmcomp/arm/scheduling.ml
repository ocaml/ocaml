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

open Mach

(* Instruction scheduling for the Sparc *)

class scheduler = object

inherit Schedgen.scheduler_generic

(* Scheduling -- based roughly on the Strong ARM *)

method oper_latency = function
    Ireload -> 2
  | Iload(_, _) -> 2
  | Iconst_symbol _ -> 2                (* turned into a load *)
  | Iconst_float _ -> 2                 (* turned into a load *)
  | Iintop(Imul) -> 3
  | Iintop_imm(Imul, _) -> 3
  (* No data available for floatops, let's make educated guesses *)
  | Iaddf -> 3
  | Isubf -> 3
  | Imulf -> 5
  | Idivf -> 15
  | _ -> 1

(* Issue cycles.  Rough approximations *)

method oper_issue_cycles = function
    Ialloc _ -> 4
  | Iintop(Icomp _) -> 3
  | Iintop(Icheckbound) -> 2
  | Iintop_imm(Idiv, _) -> 4
  | Iintop_imm(Imod, _) -> 6
  | Iintop_imm(Icomp _, _) -> 3
  | Iintop_imm(Icheckbound, _) -> 2
  | _ -> 1

end

let fundecl f = (new scheduler)#schedule_fundecl f
