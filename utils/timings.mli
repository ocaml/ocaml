(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Pierre Chambart, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Compiler performance recording *)

type file = string

type source_provenance =
  | File of file
  | Pack of string
  | Startup
  | Toplevel

type compiler_pass =
  | All
  | Parsing of file
  | Preprocessing of file
  | Typing of file
  | Transl of file
  | Generate of file
  | Assemble of source_provenance
  | Clambda of source_provenance
  | Cmm of source_provenance
  | Compile_phrases of source_provenance
  | Selection of source_provenance
  | Comballoc of source_provenance
  | CSE of source_provenance
  | Liveness of source_provenance
  | Deadcode of source_provenance
  | Spill of source_provenance
  | Split of source_provenance
  | Regalloc of source_provenance
  | Linearize of source_provenance
  | Scheduling of source_provenance
  | Emit of source_provenance
  | Flambda_pass of string * source_provenance

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
