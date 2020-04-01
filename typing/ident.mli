(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Identifiers (unique names) *)

type t

include Identifiable.S with type t := t
(* Notes:
   - [equal] compares identifiers by name
   - [compare x y] is 0 if [same x y] is true.
   - [compare] compares identifiers by binding location
*)

val print_with_scope : Format.formatter -> t -> unit
        (** Same as {!print} except that it will also add a "[n]" suffix
            if the scope of the argument is [n]. *)


val create_scoped: scope:int -> string -> t
val create_local: string -> t
val create_persistent: string -> t
val create_predef: string -> t

val rename: t -> t
        (** Creates an identifier with the same name as the input, a fresh
            stamp, and no scope.
            @raises [Fatal_error] if called on a persistent / predef ident. *)

val name: t -> string
val unique_name: t -> string
val unique_toplevel_name: t -> string
val persistent: t -> bool
val same: t -> t -> bool
        (** Compare identifiers by binding location.
            Two identifiers are the same either if they are both
            non-persistent and have been created by the same call to
            [create_*], or if they are both persistent and have the same
            name. *)

val compare: t -> t -> int

val global: t -> bool
val is_predef: t -> bool

val scope: t -> int

val lowest_scope : int
val highest_scope: int

val reinit: unit -> unit

type 'a tbl
        (* Association tables from identifiers to type 'a. *)

val empty: 'a tbl
val add: t -> 'a -> 'a tbl -> 'a tbl
val find_same: t -> 'a tbl -> 'a
val find_name: string -> 'a tbl -> t * 'a
val find_all: string -> 'a tbl -> (t * 'a) list
val fold_name: (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
val fold_all: (t -> 'a -> 'b -> 'b) -> 'a tbl -> 'b -> 'b
val iter: (t -> 'a -> unit) -> 'a tbl -> unit
val remove: t -> 'a tbl -> 'a tbl

(* Idents for sharing keys *)

val make_key_generator : unit -> (t -> t)
