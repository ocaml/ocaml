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

(* Priority queues over ordered elements.

   We choose to have polymorphic elements here, so that we can later
   instantiate [MakePoly] on polymorphic pairs (see functor [Make] below).
*)

module MakePoly(E: sig
    type 'a t
    val compare: 'a t -> 'a t -> int
end) =
  struct

    type 'a elt = 'a E.t

    (* Our priority queues are implemented using the standard "min heap"
       data structure, a dynamic array representing a binary tree. *)
    type 'a t = 'a E.t Dynarray.t

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
      if E.compare x y < 0 then (
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
      E.compare (Dynarray.get h i) (Dynarray.get h j) < 0

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
      if E.compare y x < 0 then (
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

(* We provide a simpler API, where the functor only requires the priority
   type. This is readily obtained by using the functor above on pairs
   (priority, element). *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type prio
    type 'a t
    val create: unit ->'a t
    val length: 'a t -> int
    val is_empty: 'a t -> bool
    val add: 'a t -> prio -> 'a -> unit
    val add_seq: 'a t -> (prio * 'a) Seq.t -> unit
    exception Empty
    val min_elt: 'a t -> prio * 'a
    val min_elt_opt: 'a t -> (prio * 'a) option
    val pop_min: 'a t -> prio * 'a
    val pop_min_opt: 'a t -> (prio * 'a) option
    val remove_min: 'a t -> unit
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val of_array: (prio * 'a) array -> 'a t
    val of_list: (prio * 'a) list -> 'a t
    val of_seq: (prio * 'a) Seq.t -> 'a t
    val iter: (prio -> 'a -> unit) -> 'a t -> unit
    val fold: ('acc -> prio -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
    val to_seq: 'a t -> (prio * 'a) Seq.t
  end

module Make(Prio: OrderedType) =
  struct
    include MakePoly(struct
      type 'a t = Prio.t * 'a
      let compare (x,_) (y,_) = Prio.compare x y
    end)
    type prio = Prio.t
    (* currying a few functions to get the expected API *)
    let add h p x = add h (p, x)
    let iter f h = iter (fun (p, x) -> f p x) h
    let fold f acc h = fold (fun acc (p, x) -> f acc p x) acc h
  end

(* Finally, we provide two instances when the priority is an integer *)

module MinQueue = Make(Int)
module MaxQueue = Make(struct type t = int
                              let compare = Fun.flip Int.compare end)
