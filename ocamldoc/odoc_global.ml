(***********************************************************************)
(*                               OCamldoc                              *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Global variables. *)

let errors = ref 0

let warn_error = ref false


(* Tell ocaml compiler not to generate files. *)
let _ = Clflags.dont_write_files := true
