(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Compiler performance recording *)

type file = string

type part =
  | All
  | Parsing of file
  | Typing of file
  | Transl of file
  | Generate of file
  | Assemble of file
  | Flambda of file
  | Cmm of file
  | Compile_phrases of file

type duration

val reset : unit -> unit

val get : part -> duration option
val start : part -> unit
val stop : part -> unit
val start_id : part -> 'a -> 'a
val stop_id : part -> 'a -> 'a
val time : part -> ('a -> 'b) -> 'a -> 'b

val print : Format.formatter -> unit
