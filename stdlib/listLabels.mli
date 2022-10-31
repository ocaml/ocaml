(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* NOTE:
   If this file is listLabels.mli, run tools/sync_stdlib_docs after editing it
   to generate list.mli.

   If this file is list.mli, do not edit it directly -- edit
   listLabels.mli instead.
 *)

(** List operations.

   Some functions are flagged as not tail-recursive.  A tail-recursive
   function uses constant stack space, while a non-tail-recursive function
   uses stack space proportional to the length of its list argument, which
   can be a problem with very long lists.  When the function takes several
   list arguments, an approximate formula giving stack usage (in some
   unspecified constant unit) is shown in parentheses.

   The above considerations can usually be ignored if your lists are not
   longer than about 10000 elements.

   The labeled version of this module can be used as described in the
   {!StdLabels} module.
 *)

type 'a t = 'a list = [] | (::) of 'a * 'a list (**)
(** An alias for the type of lists. *)

val length : 'a list -> int
(** Return the length (number of elements) of the given list. *)

val compare_lengths : 'a list -> 'b list -> int
(** Compare the lengths of two lists. [compare_lengths l1 l2] is
   equivalent to [compare (length l1) (length l2)], except that
   the computation stops after reaching the end of the shortest list.
   @since 4.05.0
 *)

val compare_length_with : 'a list -> len:int -> int
(** Compare the length of a list to an integer. [compare_length_with l len] is
   equivalent to [compare (length l) len], except that the computation stops
   after at most [len] iterations on the list.
   @since 4.05.0
 *)

val is_empty : 'a list -> bool
(** [is_empty l] is true if and only if [l] has no elements. It is equivalent to
    [compare_length_with l 0 = 0].
    @since 5.1
 *)

val cons : 'a -> 'a list -> 'a list
(** [cons x xs] is [x :: xs]
    @since 4.03.0 (4.05.0 in ListLabels)
 *)

val hd : 'a list -> 'a
(** Return the first element of the given list.
   @raise Failure if the list is empty.
 *)

val tl : 'a list -> 'a list
(** Return the given list without its first element.
   @raise Failure if the list is empty.
 *)

val nth : 'a list -> int -> 'a
(** Return the [n]-th element of the given list.
   The first element (head of the list) is at position 0.
   @raise Failure if the list is too short.
   @raise Invalid_argument if [n] is negative.
 *)

val nth_opt : 'a list -> int -> 'a option
(** Return the [n]-th element of the given list.
    The first element (head of the list) is at position 0.
    Return [None] if the list is too short.
    @raise Invalid_argument if [n] is negative.
    @since 4.05
 *)

val rev : 'a list -> 'a list
(** List reversal. *)

val init : len:int -> f:(int -> 'a) -> 'a list
(** [init ~len ~f] is [[f 0; f 1; ...; f (len-1)]], evaluated left to right.
    @raise Invalid_argument if [len < 0].
    @since 4.06.0
 *)

val append : 'a list -> 'a list -> 'a list
(** Concatenate two lists. Same function as the infix operator [@].
   Not tail-recursive (length of the first argument). The [@]
   operator is not tail-recursive either.
 *)

val rev_append : 'a list -> 'a list -> 'a list
(** [rev_append l1 l2] reverses [l1] and concatenates it with [l2].
   This is equivalent to [(]{!rev}[ l1) @ l2], but [rev_append] is
   tail-recursive and more efficient.
 *)

val concat : 'a list list -> 'a list
(** Concatenate a list of lists. The elements of the argument are all
   concatenated together (in the same order) to give the result.
   Not tail-recursive
   (length of the argument + length of the longest sub-list).
 *)

val flatten : 'a list list -> 'a list
(** Same as {!concat}. Not tail-recursive
   (length of the argument + length of the longest sub-list).
 *)


(** {1 Comparison} *)

val equal : eq:('a -> 'a -> bool) -> 'a list -> 'a list -> bool
(** [equal eq [a1; ...; an] [b1; ..; bm]] holds when
    the two input lists have the same length, and for each
    pair of elements [ai], [bi] at the same position we have
    [eq ai bi].

    Note: the [eq] function may be called even if the
    lists have different length. If you know your equality
    function is costly, you may want to check {!compare_lengths}
    first.

    @since 4.12.0
*)

val compare : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> int
(** [compare cmp [a1; ...; an] [b1; ...; bm]] performs
    a lexicographic comparison of the two input lists,
    using the same ['a -> 'a -> int] interface as {!Stdlib.compare}:

    - [a1 :: l1] is smaller than [a2 :: l2] (negative result)
      if [a1] is smaller than [a2], or if they are equal (0 result)
      and [l1] is smaller than [l2]
    - the empty list [[]] is strictly smaller than non-empty lists

    Note: the [cmp] function will be called even if the lists have
    different lengths.

    @since 4.12.0
*)

(** {1 Iterators} *)


val iter : f:('a -> unit) -> 'a list -> unit
(** [iter ~f [a1; ...; an]] applies function [f] in turn to
   [[a1; ...; an]]. It is equivalent to
   [f a1; f a2; ...; f an].
 *)

val iteri : f:(int -> 'a -> unit) -> 'a list -> unit
(** Same as {!iter}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
   @since 4.00.0
 *)

val map : f:('a -> 'b) -> 'a list -> 'b list
(** [map ~f [a1; ...; an]] applies function [f] to [a1, ..., an],
   and builds the list [[f a1; ...; f an]]
   with the results returned by [f].
 *)

val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list
(** Same as {!map}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
   @since 4.00.0
 *)

val rev_map : f:('a -> 'b) -> 'a list -> 'b list
(** [rev_map ~f l] gives the same result as
   {!rev}[ (]{!map}[ f l)], but is more efficient.
 *)

val filter_map : f:('a -> 'b option) -> 'a list -> 'b list
(** [filter_map ~f l] applies [f] to every element of [l], filters
    out the [None] elements and returns the list of the arguments of
    the [Some] elements.
    @since 4.08.0
 *)

val concat_map : f:('a -> 'b list) -> 'a list -> 'b list
(** [concat_map ~f l] gives the same result as
    {!concat}[ (]{!map}[ f l)]. Tail-recursive.
    @since 4.10.0
*)

val fold_left_map :
  f:('a -> 'b -> 'a * 'c) -> init:'a -> 'b list -> 'a * 'c list
(** [fold_left_map] is  a combination of [fold_left] and [map] that threads an
    accumulator through calls to [f].
    @since 4.11.0
*)

val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a
(** [fold_left ~f ~init [b1; ...; bn]] is
   [f (... (f (f init b1) b2) ...) bn].
 *)

val fold_right : f:('a -> 'b -> 'b) -> 'a list -> init:'b -> 'b
(** [fold_right ~f [a1; ...; an] ~init] is
   [f a1 (f a2 (... (f an init) ...))]. Not tail-recursive.
 *)


(** {1 Iterators on two lists} *)


val iter2 : f:('a -> 'b -> unit) -> 'a list -> 'b list -> unit
(** [iter2 ~f [a1; ...; an] [b1; ...; bn]] calls in turn
   [f a1 b1; ...; f an bn].
   @raise Invalid_argument if the two lists are determined
   to have different lengths.
 *)

val map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [map2 ~f [a1; ...; an] [b1; ...; bn]] is
   [[f a1 b1; ...; f an bn]].
   @raise Invalid_argument if the two lists are determined
   to have different lengths.
 *)

val rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [rev_map2 ~f l1 l2] gives the same result as
   {!rev}[ (]{!map2}[ f l1 l2)], but is more efficient.
 *)

val fold_left2 :
  f:('a -> 'b -> 'c -> 'a) -> init:'a -> 'b list -> 'c list -> 'a
(** [fold_left2 ~f ~init [a1; ...; an] [b1; ...; bn]] is
   [f (... (f (f init a1 b1) a2 b2) ...) an bn].
   @raise Invalid_argument if the two lists are determined
   to have different lengths.
 *)

val fold_right2 :
  f:('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> init:'c -> 'c
(** [fold_right2 ~f [a1; ...; an] [b1; ...; bn] ~init] is
   [f a1 b1 (f a2 b2 (... (f an bn init) ...))].
   @raise Invalid_argument if the two lists are determined
   to have different lengths. Not tail-recursive.
 *)


(** {1 List scanning} *)


val for_all : f:('a -> bool) -> 'a list -> bool
(** [for_all ~f [a1; ...; an]] checks if all elements of the list
   satisfy the predicate [f]. That is, it returns
   [(f a1) && (f a2) && ... && (f an)] for a non-empty list and
   [true] if the list is empty.
 *)

val exists : f:('a -> bool) -> 'a list -> bool
(** [exists ~f [a1; ...; an]] checks if at least one element of
   the list satisfies the predicate [f]. That is, it returns
   [(f a1) || (f a2) || ... || (f an)] for a non-empty list and
   [false] if the list is empty.
 *)

val for_all2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!for_all}, but for a two-argument predicate.
   @raise Invalid_argument if the two lists are determined
   to have different lengths.
 *)

val exists2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!exists}, but for a two-argument predicate.
   @raise Invalid_argument if the two lists are determined
   to have different lengths.
 *)

val mem : 'a -> set:'a list -> bool
(** [mem a ~set] is true if and only if [a] is equal
   to an element of [set].
 *)

val memq : 'a -> set:'a list -> bool
(** Same as {!mem}, but uses physical equality instead of structural
   equality to compare list elements.
 *)


(** {1 List searching} *)


val find : f:('a -> bool) -> 'a list -> 'a
(** [find ~f l] returns the first element of the list [l]
   that satisfies the predicate [f].
   @raise Not_found if there is no value that satisfies [f] in the
   list [l].
 *)

val find_opt : f:('a -> bool) -> 'a list -> 'a option
(** [find ~f l] returns the first element of the list [l]
   that satisfies the predicate [f].
   Returns [None] if there is no value that satisfies [f] in the
   list [l].
   @since 4.05
 *)

val find_map : f:('a -> 'b option) -> 'a list -> 'b option
(** [find_map ~f l] applies [f] to the elements of [l] in order,
    and returns the first result of the form [Some v], or [None]
    if none exist.
    @since 4.10.0
*)

val filter : f:('a -> bool) -> 'a list -> 'a list
(** [filter ~f l] returns all the elements of the list [l]
   that satisfy the predicate [f]. The order of the elements
   in the input list is preserved.
 *)

val find_all : f:('a -> bool) -> 'a list -> 'a list
(** [find_all] is another name for {!filter}.
 *)

val filteri : f:(int -> 'a -> bool) -> 'a list -> 'a list
(** Same as {!filter}, but the predicate is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
   @since 4.11.0
*)

val partition : f:('a -> bool) -> 'a list -> 'a list * 'a list
(** [partition ~f l] returns a pair of lists [(l1, l2)], where
   [l1] is the list of all the elements of [l] that
   satisfy the predicate [f], and [l2] is the list of all the
   elements of [l] that do not satisfy [f].
   The order of the elements in the input list is preserved.
 *)

val partition_map : f:('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list
(** [partition_map f l] returns a pair of lists [(l1, l2)] such that,
    for each element [x] of the input list [l]:
    - if [f x] is [Left y1], then [y1] is in [l1], and
    - if [f x] is [Right y2], then [y2] is in [l2].

    The output elements are included in [l1] and [l2] in the same
    relative order as the corresponding input elements in [l].

    In particular, [partition_map (fun x -> if f x then Left x else Right x) l]
    is equivalent to [partition f l].

    @since 4.12.0
*)


(** {1 Association lists} *)


val assoc : 'a -> ('a * 'b) list -> 'b
(** [assoc a l] returns the value associated with key [a] in the list of
   pairs [l]. That is,
   [assoc a [ ...; (a,b); ...] = b]
   if [(a,b)] is the leftmost binding of [a] in list [l].
   @raise Not_found if there is no value associated with [a] in the
   list [l].
 *)

val assoc_opt : 'a -> ('a * 'b) list -> 'b option
(** [assoc_opt a l] returns the value associated with key [a] in the list of
    pairs [l]. That is,
    [assoc_opt a [ ...; (a,b); ...] = Some b]
    if [(a,b)] is the leftmost binding of [a] in list [l].
    Returns [None] if there is no value associated with [a] in the
    list [l].
    @since 4.05
 *)

val assq : 'a -> ('a * 'b) list -> 'b
(** Same as {!assoc}, but uses physical equality instead of
   structural equality to compare keys.
 *)

val assq_opt : 'a -> ('a * 'b) list -> 'b option
(** Same as {!assoc_opt}, but uses physical equality instead of
   structural equality to compare keys.
   @since 4.05.0
 *)

val mem_assoc : 'a -> map:('a * 'b) list -> bool
(** Same as {!assoc}, but simply return [true] if a binding exists,
   and [false] if no bindings exist for the given key.
 *)

val mem_assq : 'a -> map:('a * 'b) list -> bool
(** Same as {!mem_assoc}, but uses physical equality instead of
   structural equality to compare keys.
 *)

val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
(** [remove_assoc a l] returns the list of
   pairs [l] without the first pair with key [a], if any.
   Not tail-recursive.
 *)

val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
(** Same as {!remove_assoc}, but uses physical equality instead
   of structural equality to compare keys. Not tail-recursive.
 *)


(** {1 Lists of pairs} *)


val split : ('a * 'b) list -> 'a list * 'b list
(** Transform a list of pairs into a pair of lists:
   [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
   Not tail-recursive.
 *)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** Transform a pair of lists into a list of pairs:
   [combine [a1; ...; an] [b1; ...; bn]] is
   [[(a1,b1); ...; (an,bn)]].
   @raise Invalid_argument if the two lists
   have different lengths. Not tail-recursive.
 *)


(** {1 Sorting} *)


val sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Sort a list in increasing order according to a comparison
   function. The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see Array.sort for
   a complete specification). For example,
   {!Stdlib.compare} is a suitable comparison function.
   The resulting list is sorted in increasing order.
   {!sort} is guaranteed to run in constant heap space
   (in addition to the size of the result list) and logarithmic
   stack space.

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
 *)

val stable_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!sort}, but the sorting algorithm is guaranteed to
   be stable (i.e. elements that compare equal are kept in their
   original order).

   The current implementation uses Merge Sort. It runs in constant
   heap space and logarithmic stack space.
 *)

val fast_sort : cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!sort} or {!stable_sort}, whichever is
    faster on typical input.
 *)

val sort_uniq : cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!sort}, but also remove duplicates.
    @since 4.02.0 (4.03.0 in ListLabels)
 *)

val merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merge two lists:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge ~cmp l1 l2] will return a
    sorted list containing all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
    Not tail-recursive (sum of the lengths of the arguments).
 *)

(** {1 Lists and Sequences} *)

val to_seq : 'a list -> 'a Seq.t
(** Iterate on the list.
    @since 4.07
 *)

val of_seq : 'a Seq.t -> 'a list
(** Create a list from a sequence.
    @since 4.07
 *)
