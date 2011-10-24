(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

(*
 * Heap: heaps implemented both functionally and imperatively
 * Copyright (C) 2003 Jean-Christophe FILLIATRE
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* Heaps *)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

exception EmptyHeap

(*S Imperative implementation. *)

module Imperative(X: Ordered) : sig

  (* Type of imperative heaps.
     (In the following [n] refers to the number of elements in the heap) *)

  type t 

  (* [create c] creates a new heap, with initial capacity of [c] *)
  val create : int -> t

  (* [is_empty h] checks the emptiness of [h] *)
  val is_empty : t -> bool

  (* [add x h] adds a new element [x] in heap [h]; size of [h] is doubled
     when maximum capacity is reached; complexity $O(log(n))$ *)
  val add : t -> X.t -> unit

  (* [maximum h] returns the maximum element of [h]; raises [EmptyHeap]
     when [h] is empty; complexity $O(1)$ *)
  val maximum : t -> X.t

  (* [remove h] removes the maximum element of [h]; raises [EmptyHeap]
     when [h] is empty; complexity $O(log(n))$ *)
  val remove : t -> unit

  (* [pop_maximum h] removes the maximum element of [h] and returns it;
     raises [EmptyHeap] when [h] is empty; complexity $O(log(n))$ *)
  val pop_maximum : t -> X.t

  (* usual iterators and combinators; elements are presented in
     arbitrary order *)
  val iter : (X.t -> unit) -> t -> unit

  val fold : (X.t -> 'a -> 'a) -> t -> 'a -> 'a

end

(*S Functional implementation. *)

module type FunctionalSig = sig

  (* heap elements *)
  type elt

  (* Type of functional heaps *)
  type t

  (* The empty heap *)
  val empty : t

  (* [add x h] returns a new heap containing the elements of [h], plus [x];
     complexity $O(log(n))$ *)
  val add : elt -> t -> t

  (* [maximum h] returns the maximum element of [h]; raises [EmptyHeap]
     when [h] is empty; complexity $O(1)$ *)
  val maximum : t -> elt

  (* [remove h] returns a new heap containing the elements of [h], except
     the maximum of [h]; raises [EmptyHeap] when [h] is empty; 
     complexity $O(log(n))$ *) 
  val remove : t -> t

  (* usual iterators and combinators; elements are presented in
     arbitrary order *)
  val iter : (elt -> unit) -> t -> unit

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

end

module Functional(X: Ordered) : FunctionalSig with type elt = X.t
