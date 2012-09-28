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

(* $Id: liveness.mli 12858 2012-08-10 14:45:51Z maranget $ *)

(* Liveness analysis.
   Annotate mach code with the set of regs live at each point. *)

open Format

val fundecl: formatter -> Mach.fundecl -> unit
