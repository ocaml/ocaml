(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The empty type.

    @since 5.0 *)

(** {1:empty The empty type} *)

type t = empty = |
(** The empty type that has no values. *)

val equal : t -> t -> bool
(** [equal e1 e2] never returns. *)

val compare : t -> t -> int
(** [compare e1 e2] never returns. *)

val to_string : t -> string
(** [to_string e] never returns. *)
