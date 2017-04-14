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

val reset : unit -> unit
(** erase all recorded times *)

val time_call : ?accumulate:bool -> string -> (unit -> 'a) -> 'a
(** [time_call pass f] calls [f] and records its runtime. *)

val time : ?accumulate:bool -> string -> ('a -> 'b) -> 'a -> 'b
(** [time pass f arg] records the runtime of [f arg] *)

type column = [ `Time | `Alloc | `Top_heap | `Abs_top_heap ]

val print : Format.formatter -> column list -> unit
(** Prints all recorded timings to the formatter. *)

(** Command line flags *)

val options_doc : string
val all_columns : column list

(** A few pass names that are needed in several places, and shared to
    avoid typos. *)

val generate : string
val transl : string
val typing : string
