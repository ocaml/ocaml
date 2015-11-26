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
  | Clambda of file
  | Cmm of file
  | Compile_phrases of file
  | Regalloc

val reset : unit -> unit
(** erase all recorded times *)

val get : part -> float option
(** returns the runtime in seconds of a completed part *)

val time : part -> ('a -> 'b) -> 'a -> 'b
(** [time part f arg] Record the runtime of [f arg] *)

val accumulate_time : part -> ('a -> 'b) -> 'a -> 'b
(** Like time for parts that can run multiple times *)

val print : Format.formatter -> unit
(** Prints all recorded timings to the formatter. *)
