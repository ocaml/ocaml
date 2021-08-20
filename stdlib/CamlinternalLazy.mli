(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Run-time support for lazy values.
    All functions in this module are for system use only, not for the
    casual user. *)

exception Undefined

type 'a t = 'a lazy_t

val force_lazy_block : 'a lazy_t -> 'a

val force_val_lazy_block : 'a lazy_t -> 'a

val force : 'a lazy_t -> 'a
val force_val : 'a lazy_t -> 'a
