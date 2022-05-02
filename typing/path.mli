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

(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string
  | Pcstr_ty of t * string
  | Pext_ty of t
  | Papply of t * t

val same: t -> t -> bool
val compare: t -> t -> int
val find_free_opt: Ident.t list -> t -> Ident.t option
val exists_free: Ident.t list -> t -> bool
val scope: t -> int
val flatten : t -> [ `Contains_apply | `Ok of Ident.t * string list ]

val name: ?paren:(string -> bool) -> t -> string
    (* [paren] tells whether a path suffix needs parentheses *)
val head: t -> Ident.t

val print: Format.formatter -> t -> unit

val heads: t -> Ident.t list

val last: t -> string

val is_constructor_typath: t -> bool

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
