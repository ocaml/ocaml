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

(* Instruction scheduling for the HPPA *)

open Arch
open Mach

class scheduler () as self =

inherit Schedgen.scheduler_generic () as super

(* Latencies (in cycles). Roughly based on the ``Mustang'' chips. *)

method oper_latency = function
    Ireload -> 2
  | Iload(_, _) -> 2
  | Iconst_float _ -> 2                 (* turned into a load *)
  | Iintop Imul -> 2                    (* ends up with a load *)
  | Iaddf | Isubf | Imulf -> 3
  | Idivf -> 12
  | _ -> 1

(* Issue cycles.  Rough approximations. *)

method oper_issue_cycles = function
    Iconst_float _ -> 3
  | Iconst_symbol _ -> 2
  | Iload(_, Ibased(_, _)) -> 2
  | Istore(_, Ibased(_, _)) -> 2
  | Ialloc _ -> 5
  | Iintop Imul -> 10
  | Iintop Ilsl -> 3
  | Iintop Ilsr -> 2
  | Iintop Iasr -> 3
  | Iintop(Icomp _) -> 2
  | Iintop(Icheckbound) -> 2
  | Iintop_imm(Idiv, _) -> 4
  | Iintop_imm(Imod, _) -> 5
  | Iintop_imm(Icomp _, _) -> 2
  | Iintop_imm(Icheckbound, _) -> 2
  | Ifloatofint -> 4
  | Iintoffloat -> 4
  | _ -> 1

end

let fundecl f = (new scheduler ())#schedule_fundecl f

