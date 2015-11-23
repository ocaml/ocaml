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

val start : part -> unit
(** Mark the beginning of a section *)

val start_id : part -> 'a -> 'a
(** Behave like [start] but is the identity on the second argument.
    Usefull for introducing timings in patterns like
    [ function1
      ++ start_id some_part
      ++ function2
      ++ stop_id some_part
      ...]
*)

val stop : part -> unit
(** Mark the end of a section. It is an error to call stop without
    having called start earlier *)

val stop_id : part -> 'a -> 'a
(** Behave like [stop] but is the identity in the second argument *)

val time : part -> ('a -> 'b) -> 'a -> 'b
(** [time part f arg] Record the runtime of [f arg] *)

val restart : part -> unit
(** Start a timer for a part that can run multiple times *)

val accumulate : part -> unit
(** Stop and accumulate a timer started with restart *)

val accumulate_time : part -> ('a -> 'b) -> 'a -> 'b
(** Like time for parts that can run multiple times *)

val print : Format.formatter -> unit
(** Prints all recorded timings to the formatter. *)
