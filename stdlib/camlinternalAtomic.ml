(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Scherer, projet Partout, INRIA Paris-Saclay        *)
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
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( + ) : int -> int -> int = "%addint"
external ignore : 'a -> unit = "%ignore"

(* We are not reusing ('a ref) directly to make it easier to reason
   about atomicity if we wish to: even in a sequential implementation,
   signals and other asynchronous callbacks might break atomicity. *)
type 'a t = {mutable v: 'a}

let make v = {v}
let get r = r.v
let set r v = r.v <- v

(* The following functions are set to never be inlined: Flambda is
   allowed to move surrounding code inside the critical section,
   including allocations. *)

let[@inline never] exchange r v =
  (* BEGIN ATOMIC *)
  let cur = r.v in
  r.v <- v;
  (* END ATOMIC *)
  cur

let[@inline never] compare_and_set r seen v =
  (* BEGIN ATOMIC *)
  let cur = r.v in
  if cur == seen then (
    r.v <- v;
    (* END ATOMIC *)
    true
  ) else
    false

let[@inline never] fetch_and_add r n =
  (* BEGIN ATOMIC *)
  let cur = r.v in
  r.v <- (cur + n);
  (* END ATOMIC *)
  cur

let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
