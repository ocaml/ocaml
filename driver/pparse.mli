(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: pparse.mli 12858 2012-08-10 14:45:51Z maranget $ *)

open Format

exception Error

val preprocess : string -> string
val remove_preprocessed : string -> unit
val remove_preprocessed_if_ast : string -> unit
val file : formatter -> string -> (Lexing.lexbuf -> 'a) -> string -> 'a
