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

type build_kind =
  | File of file
  | Pack of string
  | Startup

type compiler_pass =
  | All
  | Parsing of file
  | Preprocessing of file
  | Typing of file
  | Transl of file
  | Generate of file
  | Assemble of file
  | Clambda of file
  | Cmm of file
  | Compile_phrases of file
  | Selection of build_kind
  | Comballoc of build_kind
  | CSE of build_kind
  | Liveness of build_kind
  | Deadcode of build_kind
  | Spill of build_kind
  | Split of build_kind
  | Regalloc of build_kind
  | Linearize of build_kind
  | Scheduling of build_kind
  | Emit of build_kind

val reset : unit -> unit
(** erase all recorded times *)

val get : compiler_pass -> float option
(** returns the runtime in seconds of a completed pass *)

val time : compiler_pass -> ('a -> 'b) -> 'a -> 'b
(** [time pass f arg] Record the runtime of [f arg] *)

val accumulate_time : compiler_pass -> ('a -> 'b) -> 'a -> 'b
(** Like time for passes that can run multiple times *)

val print : Format.formatter -> unit
(** Prints all recorded timings to the formatter. *)
