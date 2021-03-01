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

(** Unit values.

    @since 4.08 *)

(** {1:unit The unit type} *)

type t = unit = () (**)
(** The unit type.

    The constructor [()] is included here so that it has a path,
    but it is not intended to be used in user-defined data types.
 *)

val equal : t -> t -> bool
(** [equal u1 u2] is [true]. *)

val compare : t -> t -> int
(** [compare u1 u2] is [0]. *)

val to_string : t -> string
(** [to_string b] is ["()"]. *)
