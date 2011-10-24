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

(*s Heaps *)

module type Ordered = sig
  type t
  val compare : t -> t -> int
end

exception EmptyHeap

(*s Imperative implementation *)

module Imperative(X : Ordered) = struct

  (* The heap is encoded in the array [data], where elements are stored
     from [0] to [size - 1]. From an element stored at [i], the left 
     (resp. right) subtree, if any, is rooted at [2*i+1] (resp. [2*i+2]). *)

  type t = { mutable size : int; mutable data : X.t array }

  (* When [create n] is called, we cannot allocate the array, since there is
     no known value of type [X.t]; we'll wait for the first addition to 
     do it, and we remember this situation with a negative size. *)

  let create n = 
    if n <= 0 then invalid_arg "create";
    { size = -n; data = [||] }

  let is_empty h = h.size <= 0

  (* [resize] doubles the size of [data] *)

  let resize h =
    let n = h.size in
    assert (n > 0);
    let n' = 2 * n in
    let d = h.data in
    let d' = Array.create n' d.(0) in
    Array.blit d 0 d' 0 n;
    h.data <- d'

  let add h x =
    (* first addition: we allocate the array *)
    if h.size < 0 then begin
      h.data <- Array.create (- h.size) x; h.size <- 0
    end;
    let n = h.size in
    (* resizing if needed *)
    if n == Array.length h.data then resize h;
    let d = h.data in
    (* moving [x] up in the heap *)
    let rec moveup i =
      let fi = (i - 1) / 2 in
      if i > 0 && X.compare d.(fi) x < 0 then begin
	d.(i) <- d.(fi);
	moveup fi
      end else
	d.(i) <- x
    in
    moveup n;
    h.size <- n + 1

  let maximum h =
    if h.size <= 0 then raise EmptyHeap;
    h.data.(0)

  let remove h =
    if h.size <= 0 then raise EmptyHeap;
    let n = h.size - 1 in
    h.size <- n;
    let d = h.data in
    let x = d.(n) in
    (* moving [x] down in the heap *)
    let rec movedown i =
      let j = 2 * i + 1 in
      if j < n then
	let j = 
	  let j' = j + 1 in 
	  if j' < n && X.compare d.(j') d.(j) > 0 then j' else j 
	in
	if X.compare d.(j) x > 0 then begin 
	  d.(i) <- d.(j); 
	  movedown j 
	end else
	  d.(i) <- x
      else
	d.(i) <- x
    in
    movedown 0

  let pop_maximum h = let m = maximum h in remove h; m

  let iter f h = 
    let d = h.data in
    for i = 0 to h.size - 1 do f d.(i) done

  let fold f h x0 =
    let n = h.size in
    let d = h.data in
    let rec foldrec x i =
      if i >= n then x else foldrec (f d.(i) x) (succ i)
    in
    foldrec x0 0

end


(*s Functional implementation *)

module type FunctionalSig = sig
  type elt
  type t
  val empty : t
  val add : elt -> t -> t
  val maximum : t -> elt
  val remove : t -> t
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module Functional(X : Ordered) = struct

  (* Heaps are encoded as complete binary trees, i.e., binary trees
     which are full expect, may be, on the bottom level where it is filled 
     from the left. 
     These trees also enjoy the heap property, namely the value of any node 
     is greater or equal than those of its left and right subtrees.

     There are 4 kinds of complete binary trees, denoted by 4 constructors:
     [FFF] for a full binary tree (and thus 2 full subtrees);
     [PPF] for a partial tree with a partial left subtree and a full
     right subtree;
     [PFF] for a partial tree with a full left subtree and a full right subtree
     (but of different heights);
     and [PFP] for a partial tree with a full left subtree and a partial
     right subtree. *)

  type elt = X.t

  type t = 
    | Empty
    | FFF of t * X.t * t (* full    (full,    full) *)
    | PPF of t * X.t * t (* partial (partial, full) *)
    | PFF of t * X.t * t (* partial (full,    full) *)
    | PFP of t * X.t * t (* partial (full,    partial) *)

  let empty = Empty
 
  (* smart constructors for insertion *)
  let p_f l x r = match l with
    | Empty | FFF _ -> PFF (l, x, r)
    | _ -> PPF (l, x, r)

  let pf_ l x = function
    | Empty | FFF _ as r -> FFF (l, x, r)
    | r -> PFP (l, x, r)

  let rec add x = function
    | Empty -> 
	FFF (Empty, x, Empty)
    (* insertion to the left *)
    | FFF (l, y, r) | PPF (l, y, r) ->
	if X.compare x y > 0 then p_f (add y l) x r else p_f (add x l) y r
    (* insertion to the right *)
    | PFF (l, y, r) | PFP (l, y, r) ->
	if X.compare x y > 0 then pf_ l x (add y r) else pf_ l y (add x r)

  let maximum = function
    | Empty -> raise EmptyHeap
    | FFF (_, x, _) | PPF (_, x, _) | PFF (_, x, _) | PFP (_, x, _) -> x

  (* smart constructors for removal; note that they are different
     from the ones for insertion! *)
  let p_f l x r = match l with
    | Empty | FFF _ -> FFF (l, x, r)
    | _ -> PPF (l, x, r)

  let pf_ l x = function
    | Empty | FFF _ as r -> PFF (l, x, r)
    | r -> PFP (l, x, r)

  let rec remove = function
    | Empty -> 
	raise EmptyHeap
    | FFF (Empty, _, Empty) -> 
	Empty
    | PFF (l, _, Empty) ->
	l
    (* remove on the left *)
    | PPF (l, x, r) | PFF (l, x, r) ->
        let xl = maximum l in
	let xr = maximum r in
	let l' = remove l in
	if X.compare xl xr >= 0 then 
	  p_f l' xl r 
	else 
	  p_f l' xr (add xl (remove r))
    (* remove on the right *)
    | FFF (l, x, r) | PFP (l, x, r) ->
        let xl = maximum l in
	let xr = maximum r in
	let r' = remove r in
	if X.compare xl xr > 0 then 
	  pf_ (add xr (remove l)) xl r'
	else 
	  pf_ l xr r'

  let rec iter f = function
    | Empty -> 
	()
    | FFF (l, x, r) | PPF (l, x, r) | PFF (l, x, r) | PFP (l, x, r) -> 
	iter f l; f x; iter f r

  let rec fold f h x0 = match h with
    | Empty -> 
	x0
    | FFF (l, x, r) | PPF (l, x, r) | PFF (l, x, r) | PFP (l, x, r) -> 
	fold f l (fold f r (f x x0))

end
