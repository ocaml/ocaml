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
  | Papply of t * t

val same: t -> t -> bool
val compare: t -> t -> int
val find_free_opt: Ident.t list -> t -> Ident.t option
val exists_free: Ident.t list -> t -> bool

val scope: t -> int
        (** @raise [No_scope] if the path contains an unscoped identifier. *)

val flatten : t -> [ `Contains_apply | `Ok of Ident.t * string list ]

val subst : (Ident.t * Ident.t) list -> t -> t
        (** Substitute identifiers i_1 for i_1', ... i_n for i_n' from the list
            of the form [[(i_1, i_1'); ...; (i_n; i_n')]].

            The returned value will be physically equal to the input if no
            substitutions are made. *)

val unsubst : (Ident.t * Ident.t) list -> t -> t
        (** Substitute identifiers i_1 for i_1', ... i_n for i_n' from the list
            of the form [[(i_1', i_1); ...; (i_n'; i_n)]].

            The returned value will be physically equal to the input if no
            substitutions are made. *)

val scope_subst : (Ident.t * Ident.t) list -> t -> int
        (** Non-allocating equivalent to [scope (subst id_pairs t)].
            @raise [No_scope] if the path contains an unscoped identifier. *)

val same_subst : (Ident.t * Ident.t) list ->
  (Ident.t * Ident.t) list -> t -> t -> bool
        (** Non-allocating equivalent to
            [same (subst id_pairs1 p1) (subst id_pairs2 p2)]. *)

val find_unscoped : t -> Ident.t option

val find_unscoped_subst : (Ident.t * Ident.t) list -> t -> Ident.t option
        (** Non-allocating equivalent to [find_unscoped (subst id_pairs p)]. *)

val name: ?paren:(string -> bool) -> t -> string
    (* [paren] tells whether a path suffix needs parentheses *)
val head: t -> Ident.t

val print: Format.formatter -> t -> unit

val heads: t -> Ident.t list

val last: t -> string

val is_uident: string -> bool

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string

val constructor_typath: t -> typath
val is_constructor_typath: t -> bool

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
