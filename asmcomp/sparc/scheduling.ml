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

open Mach

(* Instruction scheduling for the Sparc *)

class scheduler = object

inherit Schedgen.scheduler_generic

(* Latencies (in cycles). Wild guesses. *)

method oper_latency = function
    Ireload -> 3
  | Iload(_, _) -> 3
  | Iconst_float _ -> 3 (* turned into a load *)
  | Iaddf | Isubf -> 3
  | Imulf -> 5
  | Idivf -> 15
  | _ -> 1

(* Issue cycles.  Rough approximations. *)

method oper_issue_cycles = function
    Iconst_float _ -> 2
  | Iconst_symbol _ -> 2
  | Ialloc _ -> 6
  | Iintop(Icomp _) -> 4
  | Iintop(Icheckbound) -> 2
  | Iintop_imm(Idiv, _) -> 5
  | Iintop_imm(Imod, _) -> 5
  | Iintop_imm(Icomp _, _) -> 4
  | Iintop_imm(Icheckbound, _) -> 2
  | Inegf -> 2
  | Iabsf -> 2
  | Ifloatofint -> 6
  | Iintoffloat -> 6
  | _ -> 1

end

let fundecl f = (new scheduler)#schedule_fundecl f

