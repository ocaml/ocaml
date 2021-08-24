(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Sequences (functional iterators).

    The type ['a Seq.t] is a {b delayed list}, i.e. a list where some
    evaluation is needed to access the next element. This makes it possible
    to build infinite sequences, to build sequences as we traverse them, and
    to transform them in a lazy fashion rather than upfront.

    @since 4.07
*)

type 'a t = unit -> 'a node
(** The type of delayed lists containing elements of type ['a].
    Note that the concrete list node ['a node] is delayed under a closure,
    not a [lazy] block, which means it might be recomputed every time
    we access it. *)

and +'a node =
  | Nil
  | Cons of 'a * 'a t (**)
(** A fully-evaluated list node, either empty or containing an element
    and a delayed tail. *)

val empty : 'a t
(** The empty sequence, containing no elements. *)

val return : 'a -> 'a t
(** The singleton sequence containing only the given element. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] is the sequence containing the element [x] followed by
    the sequence [xs] @since 4.11 *)

val append : 'a t -> 'a t -> 'a t
(** [append xs ys] is the sequence [xs] followed by the sequence [ys]
    @since 4.11 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f seq] returns a new sequence whose elements are the elements of
    [seq], transformed by [f].
    This transformation is lazy, it only applies when the result is traversed.

    If [seq = [1;2;3]], then [map f seq = [f 1; f 2; f 3]]. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** Remove from the sequence the elements that do not satisfy the
    given predicate.
    This transformation is lazy, it only applies when the result is
    traversed. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** Apply the function to every element; if [f x = None] then [x] is dropped;
    if [f x = Some y] then [y] is returned.
    This transformation is lazy, it only applies when the result is
    traversed. *)

val concat : 'a t t -> 'a t
(** concatenate a sequence of sequences.

    @since 4.13
 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Map each element to a subsequence, then return each element of this
    sub-sequence in turn.
    This transformation is lazy, it only applies when the result is
    traversed. *)

val concat_map : ('a -> 'b t) -> 'a t -> 'b t
(** Alias for {!flat_map}.

    @since 4.13
*)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** Traverse the sequence from left to right, combining each element with the
    accumulator using the given function.
    The traversal happens immediately and will not terminate on infinite
    sequences.

    Also see {!List.fold_left} *)

val iter : ('a -> unit) -> 'a t -> unit
(** Iterate on the sequence, calling the (imperative) function on every element.
    The traversal happens immediately and will not terminate on infinite
    sequences. *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** Build a sequence from a step function and an initial value.
    [unfold f u] returns [empty] if [f u] returns [None],
    or [fun () -> Cons (x, unfold f y)] if [f u] returns [Some (x, y)].

    For example, [unfold (function [] -> None | h::t -> Some (h,t)) l]
    is equivalent to [List.to_seq l].
    @since 4.11 *)

(** {1 Consuming sequences} *)

(**

   The functions in this section consume their argument, a sequence, either
   partially or completely:
   - [is_empty], [head], [tail], [uncons] consume the sequence down to depth 1.
     That is, they demand the first argument of the sequence, if there is one.
   - [iter], [fold_left], [length], etc., consume the sequence all the way to
     its end. The sequence must be finite; otherwise, they do not terminate.
   - [for_all], [exists], [find], etc. consume the sequence down to a certain
     depth, which is a priori unpredictable.

   The functions that consume two sequences can be organized in two subgroups:
   - [iter2], [fold_left2], [for_all2], etc., consume both sequences all the
     way to the end, provided they have the same length. Otherwise, they fully
     consume the shorter sequence, and ignore the remainder of the longer
     sequence.
   - [exists2], [equal], [compare] consume the sequences down to a certain
     depth, which is a priori unpredictable.

   None of the functions in this section is lazy. These functions
   are consumers: they force some computation to take place. *)

val is_empty:  'a t -> bool
(** [is_empty xs] determines whether the sequence [xs] is empty.

    @since 4.14 *)

val head : 'a t -> 'a
(** [head xs] is the first element of the sequence [xs].

    The sequence [xs] must be nonempty.

    @raise Invalid_argument if [xs] is empty.
    @since 4.14 *)

val tail : 'a t -> 'a t
(** [tail xs] is the sequence [xs], deprived of its first element.

    The sequence [xs] must be nonempty.

    @raise Invalid_argument if [xs] is empty.
    @since 4.14 *)

val uncons : 'a t -> ('a * 'a t) option
(** If [xs] is empty, then [uncons xs] is [None].

    If [xs] is nonempty, then [uncons xs] is
    [Some (head xs, tail xs)],
    that is, a pair of the head and tail of the sequence [xs].

    This equivalence holds if [xs] is persistent.
    If [xs] is ephemeral, then [uncons] must be preferred
    over separate calls to [head] and [tail],
    which would cause [xs] to be queried twice.

    @since 4.14 *)



val length : 'a t -> int
(** [length xs] is the length of the sequence [xs].

    The sequence [xs] must be finite.

    @since 4.14 *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f xs] invokes [f i x] successively
    for every element [x] located at index [i] in the sequence [xs].

    The sequence [xs] must be finite.

    [iteri f xs] is equivalent to
    [iter (fun (i, x) -> f i x) (zip (ints 0) xs)].

    @since 4.14 *)

val fold_left_i : (int -> 'b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold_left_i f _ xs] invokes [f _ i x] successively
    for every element [x] located at index [i] of the sequence [xs].

    An accumulator of type ['a] is threaded through the calls to [f].

    The sequence [xs] must be finite.

    [fold_left_i f accu xs] is equivalent to
    [fold_left (fun accu (i, x) -> f accu i x) accu (zip (ints 0) xs)].

    @since 4.14 *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all p xs] determines whether all elements [x] of the sequence [xs]
    satisfy [p x].

    The sequence [xs] must be finite.

    @since 4.14 *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists xs p] determines whether at least one element [x]
    of the sequence [xs] satisfies [p x].

    The sequence [xs] must be finite.

    @since 4.14 *)

val find : ('a -> bool) -> 'a t -> 'a option
(** [find p xs] returns [Some x], where [x] is the first element of the
    sequence [xs] that satisfies [p x], if there is such an element.

    It returns [None] if there is no such element,
    provided the sequence [xs] is finite.

    @since 4.14 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f xs] returns [Some y], where [x] is the first element of the
    sequence [xs] such that [f x = Some _], if there is such an element,
    and where [y] is defined by [f x = Some y].

    It returns [None] if there is no such element,
    provided the sequence [xs] is finite.

    @since 4.14 *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f xs ys] invokes [f x y] successively for every pair [(x, y)] of
    elements drawn synchronously from the sequences [xs] and [ys].

    If the sequences [xs] and [ys] have different lengths, then
    iteration stops as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.

    At least one of the sequences [xs] and [ys] must be finite.

    [iter2 f xs ys] is equivalent to
    [iter (fun (x, y) -> f x y) (zip xs ys)].

    @since 4.14 *)

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
(** [fold_left2 f _ xs ys] invokes [f _ x y] successively
    for every pair [(x, y)] of elements drawn synchronously
    from the sequences [xs] and [ys].

    An accumulator of type ['a] is threaded through the calls to [f].

    If the sequences [xs] and [ys] have different lengths, then
    iteration stops as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.

    At least one of the sequences [xs] and [ys] must be finite.

    [fold_left2 f accu xs ys] is equivalent to
    [fold_left (fun accu (x, y) -> f accu x y) (zip xs ys)].

    @since 4.14 *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [for_all2 p xs ys] determines whether all pairs [(x, y)] of elements
    drawn synchronously from the sequences [xs] and [ys] satisfy [p x y].

    If the sequences [xs] and [ys] have different lengths, then
    iteration stops as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.

    At least one of the sequences [xs] and [ys] must be finite.

    [for_all2 p xs ys] is equivalent to [for_all (fun b -> b) (map2 p xs ys)].

    @since 4.14 *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [exists2 p xs ys] determines whether some pair [(x, y)] of elements
    drawn synchronously from the sequences [xs] and [ys] satisfies [p x y].

    If the sequences [xs] and [ys] have different lengths, then
    iteration must stop as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.

    At least one of the sequences [xs] and [ys] must be finite.

    [exists2 p xs ys] is equivalent to [exists (fun b -> b) (map2 p xs ys)].

    @since 4.14 *)

val equal: ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Provided the function [eq] defines an equality on elements,
    [equal eq xs ys] determines whether the sequences [xs] and [ys]
    are pointwise equal.

    At least one of the sequences [xs] and [ys] must be finite.

    @since 4.14 *)

val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
(** Provided the function [cmp] defines a preorder on elements,
    [compare cmp xs ys] compares the sequences [xs] and [ys]
    according to the lexicographic preorder.

    For more details on comparison functions, see {!Array.sort}.

    At least one of the sequences [xs] and [ys] must be finite.

    @since 4.14 *)

(** {1 Constructing sequences} *)

(** The functions in this section are lazy: that is, they return sequences
    whose elements are computed only when demanded. *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] is the sequence [f 0; f 1; ...; f (n-1)].

    [n] must be nonnegative.

    [init n f] is equivalent to [map f (up 0 n)].

    @raise Invalid_argument if [n] is negative.
    @since 4.14 *)

val repeat : 'a -> 'a t
(** [repeat x] is the infinite sequence
    where the element [x] is repeated indefinitely.

    [repeat x] is equivalent to [cycle (return x)].

    @since 4.14 *)

val forever : (unit -> 'a) -> 'a t
(** [forever f] is an infinite sequence where every element is produced
    (on demand) by the function call [f()].

    For instance,
    [forever Random.bool] is an infinite sequence of random bits.

    [forever f] is equivalent to [map f (repeat ())].

    @since 4.14 *)

val cycle : 'a t -> 'a t
(** [cycle xs] is the infinite sequence which consists of an infinite
    number of repetitions of the sequence [xs].

    The sequence [xs] must be nonempty.

    Consuming (a prefix of) the sequence [cycle xs] once
    can cause the sequence [xs] to be consumed more than once.
    Therefore, [xs] must be persistent.

    @raise Invalid_argument if [xs] is empty.
    @since 4.14 *)

val iterate : ('a -> 'a) -> 'a -> 'a t
(** [iterate f x] is the infinite sequence whose elements are
    [x], [f x], [f (f x)], and so on.

    In other words, it is the orbit of the function [f] out of
    the element [x].

    @since 4.14 *)

(** {1 Transforming sequences} *)

(** The functions in this section are lazy: that is, they return sequences
    whose elements are computed only when demanded. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi] is analogous to [map], but applies the function [f] to
    an index and an element.

    [mapi f xs] is equivalent to [map2 f (ints 0) xs].

    @since 4.14 *)

val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t
(** If [xs] is a sequence [[x0; x1; x2; ...]], then
    [scan f a0 xs] is a sequence of accumulators
    [[a0; a1; a2; ...]]
    where [a1] is [f a0 x0], [a2] is [f a1 x1], and so on.

    Thus, [scan f a0 xs] is conceptually related to
    [fold_left f a0 xs]. However, instead of performing an
    eager iteration and immediately returning the final accumulator,
    it returns a sequence of accumulators.

    For instance, [scan (+) 0] transforms a sequence of integers
    into the sequence of its partial sums.

    If [xs] has length [n]
    then [scan f a0 xs] has length [n+1].

    @since 4.14 *)

val take : int -> 'a t -> 'a t
(** [take n xs] is the sequence of the first [n] elements of [xs].

    If [xs] has fewer than [n] elements,
    then [take n xs] is equivalent to [xs].

    [n] must be nonnegative.

    @raise Invalid_argument if [n] is negative.
    @since 4.14 *)

val drop : int -> 'a t -> 'a t
(** [drop n xs] is the sequence [xs], deprived of its first [n] elements.

    If [xs] has fewer than [n] elements,
    then [drop n xs] is empty.

    [n] must be nonnegative.

    [drop] is lazy: the first [n+1] elements of the sequence [xs]
    are demanded only when the first element of [drop n xs] is
    demanded. For this reason, [drop 1 xs] is {i not} equivalent
    to [tail xs], which queries [xs] immediately.

    @raise Invalid_argument if [n] is negative.
    @since 4.14 *)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** [take_while p xs] is the longest prefix of the sequence [xs]
    where every element [x] satisfies [p x].

    @since 4.14 *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
(** [drop_while p xs] is the sequence [xs], deprived of the prefix
    [take_while p xs].

    @since 4.14 *)

val uniq : ('a -> 'a -> bool) -> 'a t -> 'a t
(** Provided the function [eq] defines an equality on elements,
    [uniq eq xs] is the sequence [xs],
    deprived of its adjacent duplicate elements.

    Thus, [uniq eq xs] has no adjacent duplicate elements.
    Furthermore, if [xs] is sorted according to an ordering
    that is compatible with [eq],
    then [uniq eq xs] has no duplicate elements at all.

    [uniq eq xs] is equivalent to [map head (group eq xs)].

    @since 4.14 *)

val group : ('a -> 'a -> bool) -> 'a t -> 'a t t
(** Provided the function [eq] defines an equality on elements,
    [group eq xs] is the sequence of the maximal runs
    of adjacent duplicate elements of the sequence [xs].

    Every element of [group eq xs] is a nonempty sequence of equal elements.

    The concatenation [concat (group eq xs)] is equal to [xs].

    Consuming [group eq xs], and consuming the sequences that it contains,
    can cause [xs] to be consumed more than once. Therefore, [xs] must be
    persistent.

    @since 4.14 *)

val memoize : 'a t -> 'a t
(** The sequence [memoize xs] has the same elements as the sequence [xs].

    Regardless of whether [xs] is ephemeral or persistent,
    [memoize xs] is persistent: even if it is queried several times,
    [xs] is queried at most once.
    @since 4.14 *)

exception ForcedTwice
(** This exception is raised when a sequence returned by [once]
    (or a suffix of it) is queried more than once.
    @since 4.14 *)

val once: 'a t -> 'a t
(** The sequence [once xs] has the same elements as the sequence [xs].

    Regardless of whether [xs] is ephemeral or persistent,
    [once xs] is an ephemeral sequence: it can be queried at most once.
    If it (or a suffix of it) is queried more than once, then the exception
    [ForcedTwice] is raised. This can be useful, while debugging or testing,
    to ensure that a sequence is consumed at most once.

    @raise ForcedTwice if [once xs], or a suffix of it,
           is queried more than once.
    @since 4.14 *)

val transpose : 'a t t -> 'a t t
(** If [xss] is a matrix (a sequence of rows), then [transpose xss] is
    the sequence of the columns of the matrix [xss].

    The rows of the matrix [xss] are not required to have the same length.

    The matrix [xss] is not required to be finite (in either direction).

    The matrix [xss] must be persistent.

    @since 4.14 *)

val diagonals : 'a t t -> 'a t t
(** If [xss] is a matrix (a sequence of rows), then [diagonals xss] is
    the sequence of its diagonals.

    The first diagonal contains just the first element of the
    first row. The second diagonal contains the first element of the
    second row and the second element of the first row; and so on.

    Every diagonal is a finite sequence.

    The rows of the matrix [xss] are not required to have the same length.

    The matrix [xss] is not required to be finite (in either direction).

    The matrix [xss] must be persistent.

    @since 4.14 *)

(** {1 Combining sequences} *)

val zip : 'a t -> 'b t -> ('a * 'b) t
(** [zip xs ys] is the sequence of pairs [(x, y)]
    drawn synchronously from the sequences [xs] and [ys].

    If the sequences [xs] and [ys] have different lengths, then
    the sequence ends as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.

    [zip xs ys] is equivalent to [map2 (fun a b -> (a, b)) xs ys].

    @since 4.14 *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f xs ys] is the sequence of the elements [f x y],
    where the pairs [(x, y)] are drawn synchronously from the
    sequences [xs] and [ys].

    If the sequences [xs] and [ys] have different lengths, then
    the sequence ends as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.

    [map2 f xs ys] is equivalent to [map (fun (x, y) -> f x y) (zip xs ys)].

    @since 4.14 *)

val interleave: 'a t -> 'a t -> 'a t
(** [interleave xs ys] is the sequence that begins with the first element of
    [xs], continues with the first element of [ys], and so on.

    When one of the sequences [xs] and [ys] is exhausted,
    [interleave xs ys] continues with the rest of the other sequence.

    @since 4.14 *)

val sorted_merge: ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
(** If the sequences [xs] and [ys] are sorted according to the total preorder
    [cmp], then [sorted_merge cmp xs ys] is the sorted sequence obtained by
    merging the sequences [xs] and [ys].

    For more details on comparison functions, see {!Array.sort}.

    @since 4.14 *)

val product : 'a t -> 'b t -> ('a * 'b) t
(** [product xs ys] is the Cartesian product of the sequences [xs] and [ys].

    For every element [x] of [xs] and for every element [y] of [ys],
    the pair [(x, y)] appears once as an element of [product xs ys].

    The order in which the pairs appear is unspecified.

    The sequences [xs] and [ys] are not required to be finite.

    The sequences [xs] and [ys] must be persistent.

    @since 4.14 *)

val product_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** The sequence [product xs ys] is the image through [f] of the Cartesian
    product of the sequences [xs] and [ys].

    For every element [x] of [xs] and for every element [y] of [ys],
    the element [f x y] appears once as an element of [product_with f xs ys].

    The order in which these elements appear is unspecified.

    The sequences [xs] and [ys] are not required to be finite.

    The sequences [xs] and [ys] must be persistent.

    [product_with f xs ys] is equivalent to
    [map (fun (x, y) -> f x y) (product xs ys)].

    @since 4.14 *)

val ap : ('a -> 'b) t -> 'a t -> 'b t
(** [ap] is the application operator of the type constructor [_ t],
    viewed as an applicative functor.

    For every element [x] of [xs] and for every element [y] of [ys],
    the element [x y] appears once as an element of [ap xs ys].

    The sequences [xs] and [ys] must be persistent.

    [ap xs ys] is equivalent to [product_with (@@) xs ys].

    @since 4.14 *)

(** {1 Splitting a sequence into two sequences} *)

val unzip : ('a * 'b) t -> 'a t * 'b t
(** [unzip] transforms a sequence of pairs into a pair of sequences.

    [unzip xs] is equivalent to [(map fst xs, map snd xs)].

    Querying either of the sequences returned by [unzip xs]
    causes [xs] to be queried.
    Therefore, querying both of them
    causes [xs] to be queried twice.
    Thus, [xs] must be persistent and cheap.
    If that is not the case, use [unzip (memoize xs)].

    @since 4.14 *)

val dispatch : ('a, 'b) Either.t t -> 'a t * 'b t
(** [dispatch] transforms a sequence of fruit (either apples or oranges)
    into a pair of a sequence of apples and a sequence of oranges.

    [dispatch xs] is equivalent to
    [(filter_map Either.find_left xs, filter_map Either.find_right xs)].

    Querying either of the sequences returned by [dispatch xs]
    causes [xs] to be queried.
    Therefore, querying both of them
    causes [xs] to be queried twice.
    Thus, [xs] must be persistent and cheap.
    If that is not the case, use [dispatch (memoize xs)].

    @since 4.14 *)

val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
(** [partition p xs] returns a pair of the subsequence of the elements
    of [xs] that satisfy [p] and the subsequence of the elements of
    [xs] that do not satisfy [p].

    [partition p xs] is equivalent to
    [filter p xs, filter (fun x -> not (p x)) xs].

    Consuming both of the sequences returned by [partition p xs] causes
    [xs] to be consumed twice and causes the function [f] to be applied
    twice to each element of the list.
    Therefore, [f] should be pure and cheap.
    Furthermore, [xs] should be persistent and cheap.
    If that is not the case, use [partition p (memoize xs)].

    @since 4.14 *)

(** {1 Converting between sequences and iterators} *)

(** An iterator is a representation of a sequence as a function of type
    [unit -> 'a option]. Every time this function is invoked, it returns
    the next element of the sequence. When there are no more elements,
    it returns [None]. An iterator has mutable internal state, therefore
    is ephemeral: the sequence that it represents can be consumed at most
    once. *)

val of_iterator: (unit -> 'a option) -> 'a t
(** [of_iterator it] is the sequence of the elements produced by the iterator
    [it]. It is an ephemeral sequence: it can be consumed at most once. If a
    persistent sequence is needed, use [memoize (of_iterator it)].
    @since 4.14 *)

val to_iterator: 'a t -> (unit -> 'a option)
(** [to_iterator xs] is a fresh iterator on the sequence [xs].
    @since 4.14 *)

(** {1 Sequences of integers} *)

val ints: int -> int t
(** [ints i] is the infinite sequence of the integers, beginning at [i] and
    counting up.
    @since 4.14 *)

val up: int -> int -> int t
(** [up i j] is the finite sequence of the integers comprised between
    [i] included and [j] excluded, counting up. It is nonempty if and
    only if [i < j] holds.
    @since 4.14 *)

val down: int -> int -> int t
(** [down i j] is the finite sequence of the integers between [i]
    excluded and [j] included, counting down. It is nonempty if and
    only if [i > j] holds.
    @since 4.14 *)

(** {1 Infix operators} *)

(** Infix operators.
    @since 4.14 *)
module Infix : sig

  val (@) : 'a t -> 'a t -> 'a t
  (** [(@)] is an alias for {!append}.
      @since 4.14 *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** [(>>=)] is the monadic [bind] operator.
      [xs >>= f] is equivalent to [flat_map f xs].
      @since 4.14 *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** [(>|=)] is the monadic [map] operator.
      [xs >|= f] is equivalent to [map f xs].
      @since 4.14 *)

  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  (** [(<*>)] is an alias for {!ap}.
      @since 4.14 *)

end
