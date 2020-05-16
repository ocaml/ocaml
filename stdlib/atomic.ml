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

(* We are not reusing ('a ref) directly to make it easier to reason
   about atomicity if we wish to: even in a sequential implementation,
   signals and other asynchronous callbacks might break atomicity. *)
type 'a t = {mutable v: 'a}

let make v = {v}
let get r = r.v
let set r v = r.v <- v

let exchange r v =
  let cur = r.v in
  r.v <- v;
  cur

let compare_and_set r seen v =
  let cur = r.v in
  if cur == seen then
    (r.v <- v; true)
  else
    false

let fetch_and_add r n =
  let cur = r.v in
  r.v <- (cur + n);
  cur

let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
