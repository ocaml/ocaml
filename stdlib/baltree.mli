(* Basic balanced binary trees *)

(* This module implements balanced ordered binary trees.
   All operations over binary trees are applicative (no side-effects).
   The [set] and [List.map] modules are based on this module.
   This modules gives a more direct access to the internals of the
   binary tree implementation than the [set] and [List.map] abstractions,
   but is more delicate to use and not as safe. For advanced users only. *)

type 'a t = Empty | Node of 'a t * 'a * 'a t * int
        (* The type of trees containing elements of type ['a].
           [Empty] is the empty tree (containing no elements). *)

type 'a contents = Nothing | Something of 'a
        (* Used with the functions [modify] and [List.split], to represent
           the presence or the absence of an element in a tree. *)

val add: ('a -> int) -> 'a -> 'a t -> 'a t
        (* [add f x t] inserts the element [x] into the tree [t].
           [f] is an ordering function: [f y] must return [0] if
           [x] and [y] are equal (or equivalent), a negative integer if
           [x] is smaller than [y], and a positive integer if [x] is
           greater than [y]. The tree [t] is returned unchanged if
           it already contains an element equivalent to [x] (that is,
           an element [y] such that [f y] is [0]).
           The ordering [f] must be consistent with the orderings used
           to build [t] with [add], [remove], [modify] or [List.split]
           operations. *)
val contains: ('a -> int) -> 'a t -> bool
        (* [contains f t] checks whether [t] contains an element
           satisfying [f], that is, an element [x] such
           that [f x] is [0]. [f] is an ordering function with the same
           constraints as for [add]. It can be coarser (identify more
           elements) than the orderings used to build [t], but must be
           consistent with them. *)
val find: ('a -> int) -> 'a t -> 'a
        (* Same as [contains], except that [find f t] returns the element [x]
           such that [f x] is [0], or raises [Not_found] if none has been
           found. *)
val remove: ('a -> int) -> 'a t -> 'a t
        (* [remove f t] removes one element [x] of [t] such that [f x] is [0].
           [f] is an ordering function with the same constraints as for [add].
           [t] is returned unchanged if it does not contain any element
           satisfying [f]. If several elements of [t] satisfy [f],
           only one is removed. *)
val modify: ('a -> int) -> ('a contents -> 'a contents) -> 'a t -> 'a t
        (* General insertion/modification/deletion function.
           [modify f g t] searchs [t] for an element [x] satisfying the
           ordering function [f]. If one is found, [g] is applied to
           [Something x]; if [g] returns [Nothing], the element [x]
           is removed; if [g] returns [Something y], the element [y]
           replaces [x] in the tree. (It is assumed that [x] and [y]
           are equivalent, in particular, that [f y] is [0].)
           If the tree does not contain any [x] satisfying [f],
           [g] is applied to [Nothing]; if it returns [Nothing],
           the tree is returned unchanged; if it returns [Something x],
           the element [x] is inserted in the tree. (It is assumed that
           [f x] is [0].) The functions [add] and [remove] are special cases
           of [modify], slightly more efficient. *)
val split: ('a -> int) -> 'a t -> 'a t * 'a contents * 'a t
        (* [split f t] returns a triple [(less, elt, greater)] where
           [less] is a tree containing all elements [x] of [t] such that
           [f x] is negative, [greater] is a tree containing all
           elements [x] of [t] such that [f x] is positive, and [elt]
           is [Something x] if [t] contains an element [x] such that
           [f x] is [0], and [Nothing] otherwise. *)
val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
        (* Compare two trees. The first argument [f] is a comparison function
           over the tree elements: [f e1 e2] is zero if the elements [e1] and 
           [e2] are equal, negative if [e1] is smaller than [e2],
           and positive if [e1] is greater than [e2]. [compare f t1 t2]
           compares the fringes of [t1] and [t2] by lexicographic extension
           of [f]. *)
(*--*)
val join: 'a t -> 'a -> 'a t -> 'a t
val concat: 'a t -> 'a t -> 'a t

