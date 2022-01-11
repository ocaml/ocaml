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

(* CamlinternalAtomic is a dependency of Stdlib, so it is compiled with
   -nopervasives. *)
type !'a t

(* Atomic is a dependency of Stdlib, so it is compiled with
   -nopervasives. *)
external make : 'a -> 'a t = "%makemutable"
external get : 'a t -> 'a = "%atomic_load"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
external ignore : 'a -> unit = "%ignore"

let set r x = ignore (exchange r x)
let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))

let rec modify f r =
  let v_old = get r in
  let v_new = f v_old in
  if compare_and_set r v_old v_new
  then ()
  else modify f r

let rec modify_get f r =
  let v_old = get r in
  let res, v_new = f v_old in
  if compare_and_set r v_old v_new
  then res
  else modify_get f r
