(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Jean-Christophe Filliâtre                        *)
(*                                                                        *)
(*   Copyright 2023 CNRS                                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Priority queues over ordered types *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val create: unit -> t
    val length: t -> int
    val is_empty: t -> bool
    val add: t -> elt -> unit
    val add_seq: t -> elt Seq.t -> unit
    exception Empty
    val min_elt: t -> elt
    val min_elt_opt: t -> elt option
    val pop_min: t -> elt
    val pop_min_opt: t -> elt option
    val remove_min: t -> unit
    val clear: t -> unit
    val copy: t -> t
    val of_array: elt array -> t
    val of_list: elt list -> t
    val of_seq: elt Seq.t -> t
    val iter: (elt -> unit) -> t -> unit
    val fold: ('acc -> elt -> 'acc) -> 'acc -> t -> 'acc
    val to_seq: t -> elt Seq.t
  end

module Make(Ord: OrderedType) =
  struct
    type elt = Ord.t

    (* Our priority queues are implemented using the standard "min heap"
       data structure, a dynamic array representing a binary tree. *)
    type t = elt Dynarray.t

    let create =
      Dynarray.create

    let length =
      Dynarray.length

    let is_empty h =
      length h = 0

    let clear =
      Dynarray.clear

    (* The node at index [i] has children nodes at indices [2 * i + 1]
       and [2 * i + 2] -- if they are valid indices in the dynarray. *)
    let left_child i = 2 * i + 1
    let right_child i = 2 * i + 2
    let parent_node i = (i - 1) / 2

    (* We say that a heap respects the "heap ordering" if the value of
       each node is no greater than the value of its children. The
       algorithm manipulates arrays that respect the heap algorithm,
       except for one node whose value may be too small or too large.

       The auxiliary functions [sift_up] and [sift_down] take
       such a misplaced value, and move it "up" (respectively: "down")
       until the heap ordering is restored.

       Functions [sift_up] and [sift_down] do not perform swaps, but
       rather expect the value to be assigned in the heap as an
       additional parameter [x], resulting in twice less assignments. *)

    (* store [x] at index [i], moving it up if necessary *)
    let rec sift_up h i x =
      if i = 0 then Dynarray.set h 0 x else
      let p = parent_node i in
      let y = Dynarray.get h p in
      if Ord.compare x y < 0 then (
        Dynarray.set h i y;
        sift_up h p x
      ) else
        Dynarray.set h i x

    let add h x =
      let i = Dynarray.length h in
      Dynarray.add_last h x;
      if i > 0 then sift_up h i x

    let add_seq h s =
      Seq.iter (add h) s

    exception Empty

    let min_elt h =
      if Dynarray.length h = 0 then raise Empty;
      Dynarray.get h 0

    let min_elt_opt h =
      if Dynarray.length h = 0 then None else Some (Dynarray.get h 0)

    let lt h i j =
      Ord.compare (Dynarray.get h i) (Dynarray.get h j) < 0

    (* store [x] at index [i], moving it down if necessary *)
    let rec sift_down h ~len i x =
      let left = left_child i in
      if left >= len then Dynarray.set h i x (* no child, stop *) else
      let smallest =
        let right = right_child i in
        if right >= len then left (* no right child *) else
        if lt h left right then left else right
      in
      let y = Dynarray.get h smallest in
      if Ord.compare y x < 0 then (
        Dynarray.set h i y;
        sift_down h ~len smallest x
      ) else
        Dynarray.set h i x

    let pop_min h =
      let n = Dynarray.length h in
      if n = 0 then raise Empty;
      let x = Dynarray.pop_last h in
      if n = 1 then x else (
        let r = Dynarray.get h 0 in
        sift_down h ~len:(n - 1) 0 x;
        r
      )

    let pop_min_opt h =
      let n = Dynarray.length h in
      if n = 0 then None else
      let x = Dynarray.pop_last h in
      if n = 1 then Some x else (
        let r = Dynarray.get h 0 in
        sift_down h ~len:(n - 1) 0 x;
        Some r
      )

    let remove_min h =
      let n = Dynarray.length h in
      if n = 0 then raise Empty;
      let x = Dynarray.pop_last h in
      if n > 1 then sift_down h ~len:(n - 1) 0 x

    let copy =
      Dynarray.copy

    (* array to heap in linear time (Floyd, 1964)
       many elements travel a short distance, few travel longer distances
       and we can show that it adds to O(N) *)
    let heapify h =
      let n = Dynarray.length h in
      for i = n/2 - 1 downto 0 do
        sift_down h ~len:n i (Dynarray.get h i)
      done;
      h

    let of_array a =
      Dynarray.of_array a |> heapify

    let of_list l =
      Dynarray.of_list l |> heapify

    let of_seq s =
      Dynarray.of_seq s |> heapify

    let iter =
      Dynarray.iter

    let fold =
      Dynarray.fold_left

    let to_seq =
      Dynarray.to_seq

  end

(* Notes:

  - Too bad [Dynarray.unsafe_get/set] are not exported.

  - The exception [Empty] is declared in signature [S].
    But it could also be a single, global exception in module [Pqueue].

  - [fold] is [fold_left] to be consitent with module [Queue]

*)
