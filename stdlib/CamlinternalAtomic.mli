(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*          Guillaume Munch-Maccagnoni, projet Gallinette, INRIA          *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The documentation is in atomic.mli. CamlinternalAtomic exists in
   order to be a dependency of Stdlib. More precisely, the option
   modules_before_stdlib used in stdlib/dune does not support the
   Stdlib__ prefix trick. *)

type !'a t
val make : 'a -> 'a t
val get : 'a t -> 'a
val set : 'a t -> 'a -> unit
val exchange : 'a t -> 'a -> 'a
val compare_and_set : 'a t -> 'a -> 'a -> bool
val fetch_and_add : int t -> int -> int
val incr : int t -> unit
val decr : int t -> unit
