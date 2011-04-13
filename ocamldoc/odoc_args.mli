(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Analysis of the command line arguments. *)

(** The current module defining the generator to use. *)
val current_generator : Odoc_gen.generator option ref

(** To set the documentation generator. *)
val set_generator : Odoc_gen.generator -> unit

(** Add an option specification. *)
val add_option : string * Arg.spec * string -> unit

(** Parse the args.
   [byte] indicate if we are in bytecode mode (default is [true]).*)
val parse : unit -> unit
