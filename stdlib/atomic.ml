(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2017-2018 University of Cambridge.                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

external ignore : 'a -> unit = "%ignore"

module Loc = struct
  type 'a t = 'a atomic_loc

  external get : 'a t -> 'a = "%atomic_load_loc"
  external exchange : 'a t -> 'a -> 'a = "%atomic_exchange_loc"
  external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas_loc"
  external fetch_and_add : int t -> int -> int = "%atomic_fetch_add_loc"

  let set t v =
    ignore (exchange t v)
  let incr t =
    ignore (fetch_and_add t 1)
  let decr t =
    ignore (fetch_and_add t (-1))
end

type !'a t

external make : 'a -> 'a t = "%makemutable"
external make_contended : 'a -> 'a t = "caml_atomic_make_contended"
external get : 'a t -> 'a = "%atomic_load"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"

let set r x = ignore (exchange r x)
let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))
