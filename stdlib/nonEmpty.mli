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

type 'a t = Head of 'a * 'a list

val of_list : 'a list -> 'a t option

external to_list : 'a t -> 'a list = "%identity"

val length : 'a t -> int

val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t

val hd : 'a t -> 'a
val tl : 'a t -> 'a list

val map : ('a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit

val append : 'a t -> 'a t -> 'a t
val append_list_right : 'a t -> 'a list -> 'a t
val append_list_left : 'a list -> 'a t -> 'a t

val to_seq : 'a t -> 'a Seq.t
val of_seq : 'a Seq.t -> 'a t option
