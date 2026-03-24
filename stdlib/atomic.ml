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

type !'a t =
  { mutable contents: 'a [@atomic];
  }

let make v =
  { contents = v }

external make_contended : 'a -> 'a t = "caml_atomic_make_contended"

let get t =
  t.contents
let set t v =
  t.contents <- v

let exchange t v =
  Loc.exchange [%atomic.loc t.contents] v
let compare_and_set t old new_ =
  Loc.compare_and_set [%atomic.loc t.contents] old new_
let fetch_and_add t incr =
  Loc.fetch_and_add [%atomic.loc t.contents] incr
let incr t =
  Loc.incr [%atomic.loc t.contents]
let decr t =
  Loc.decr [%atomic.loc t.contents]

module Array = struct
  type !'a t =
    'a array

  external check_array_bound
    : 'a t -> int -> unit
    = "%check_array_bound"

  external unsafe_index
    : 'a t -> int -> 'a Loc.t
    = "%atomic_unsafe_index"

  external length
    : 'a array -> int
    = "%array_length"

  external uniform_array_make
    : int -> 'a -> 'a t
    = "caml_uniform_array_make"

  let[@inline] unsafe_get t i =
    Loc.get (unsafe_index t i)
  let[@inline] get t i =
    check_array_bound t i;
    unsafe_get t i

  let[@inline] unsafe_set t i v =
    (* using Loc.set works less well as Simplif misses the
       lambda-level elimination of the atomic-location pair. *)
    ignore (Loc.exchange (unsafe_index t i) v)
  let[@inline] set t i v =
    check_array_bound t i;
    unsafe_set t i v

  let[@inline] unsafe_exchange t i v =
    Loc.exchange (unsafe_index t i) v
  let[@inline] exchange t i v =
    check_array_bound t i;
    unsafe_exchange t i v

  let[@inline] unsafe_compare_and_set t i old new_ =
    Loc.compare_and_set (unsafe_index t i) old new_
  let[@inline] compare_and_set t i old new_ =
    check_array_bound t i;
    unsafe_compare_and_set t i old new_

  let[@inline] unsafe_fetch_and_add t i incr =
    Loc.fetch_and_add (unsafe_index t i) incr
  let[@inline] fetch_and_add t i incr =
    check_array_bound t i;
    unsafe_fetch_and_add t i incr

  let make len v =
    if len < 0 then
      invalid_arg "Atomic.Array.make" ;
    uniform_array_make len v

  let init len fn =
    if len < 0 then
      invalid_arg "Atomic_array.init"
    else if len = 0 then
      [||]
    else begin
      let t = uniform_array_make len (fn 0) in
      for i = 1 to len - 1 do
        unsafe_set t i (fn i)
      done ;
      t
    end
end
