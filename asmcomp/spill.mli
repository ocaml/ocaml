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

(* $Id: spill.mli 12858 2012-08-10 14:45:51Z maranget $ *)

(* Insertion of moves to suggest possible spilling / reloading points
   before register allocation. *)

val fundecl: Mach.fundecl -> Mach.fundecl
