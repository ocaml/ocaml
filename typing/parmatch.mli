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

(* Detection of partial matches and unused match cases. *)

open Typedtree

val omega_list : 'a list -> pattern list

val compat : pattern -> pattern -> bool
val compats : pattern list -> pattern list -> bool

val check_partial:
        Env.t -> Location.t -> (pattern * expression) list -> partial
val check_unused: Env.t -> (pattern * expression) list -> unit

