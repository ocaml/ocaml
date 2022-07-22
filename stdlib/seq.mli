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

(** Sequences.

   A sequence of type ['a Seq.t] can be thought of as a {b delayed list},
   that is, a list whose elements are computed only when they are demanded
   by a consumer. This allows sequences to be produced and transformed
   lazily (one element at a time) rather than eagerly (all elements at
   once). This also allows constructing conceptually infinite sequences.

   The type ['a Seq.t] is defined as a synonym for [unit -> 'a Seq.node].
   This is a function type: therefore, it is opaque. The consumer can {b
   query} a sequence in order to request the next element (if there is
   one), but cannot otherwise inspect the sequence in any way.

   Because it is opaque, the type ['a Seq.t] does {i not} reveal whether
   a sequence is:
   - {b persistent},
     which means that the sequence can be used as many times as desired,
     producing the same elements every time,
     just like an immutable list; or
   - {b ephemeral},
     which means that the sequence is not persistent.
     Querying an ephemeral sequence might have an observable side effect,
     such as incrementing a mutable counter.
     As a common special case, an ephemeral sequence can be {b affine},
     which means that it must be queried at most once.

   It also does {i not} reveal whether the elements of the sequence are:

   - {b pre-computed and stored} in memory,
     which means that querying the sequence is cheap;
   - {b computed when first demanded and then stored} in memory,
     which means that querying the sequence once can be expensive,
     but querying the same sequence again is cheap; or
   - {b re-computed every time they are demanded},
     which may or may not be cheap.

   It is up to the programmer to keep these distinctions in mind
   so as to understand the time and space requirements of sequences.

   For the sake of simplicity, most of the documentation that follows
   is written under the implicit assumption that the sequences at hand
   are persistent.
   We normally do not point out {i when} or {i how many times}
   each function is invoked, because that would be too verbose.
   For instance, in the description of [map], we write:
   "if [xs] is the sequence [x0; x1; ...]
    then [map f xs] is the sequence [f x0; f x1; ...]".
   If we wished to be more explicit,
   we could point out that the transformation takes place on demand:
   that is, the elements of [map f xs] are computed only when they
   are demanded. In other words,
   the definition [let ys = map f xs] terminates immediately and
   does not invoke [f]. The function call [f x0] takes place only when the
   first element of [ys] is demanded, via the function call [ys()].
   Furthermore, calling [ys()] twice causes [f x0] to be called twice
   as well. If one wishes for [f] to be applied at most once to each
   element of [xs], even in scenarios where [ys] is queried more than once,
   then one should use [let ys = memoize (map f xs)].

   As a general rule, the functions that build sequences, such as [map],
   [filter], [scan], [take], etc., produce sequences whose elements are
   computed only on demand. The functions that eagerly consume sequences,
   such as [is_empty], [find], [length], [iter], [fold_left],
   etc., are the functions that force computation to take place.

   When possible, we recommend using sequences rather than dispensers
   (functions of type [unit -> 'a option] that produce elements upon
   demand). Whereas sequences can be persistent or ephemeral, dispensers
   are always ephemeral, and are typically more difficult to work with
   than sequences. Two conversion functions, {!to_dispenser} and
   {!of_dispenser}, are provided.

    @since 4.07 *)

type 'a t = unit -> 'a node
(** A sequence [xs] of type ['a t] is a delayed list of elements of
    type ['a]. Such a sequence is queried by performing a function
    application [xs()]. This function application returns a node,
    allowing the caller to determine whether the sequence is empty
    or nonempty, and in the latter case, to obtain its head and tail. *)

and +'a node =
  | Nil
  | Cons of 'a * 'a t (**)
(** A node is either [Nil], which means that the sequence is empty,
    or [Cons (x, xs)], which means that [x] is the first element
    of the sequence and that [xs] is the remainder of the sequence. *)

(** {1 Consuming sequences} *)

(**

   The functions in this section consume their argument, a sequence, either
   partially or completely:
   - [is_empty] and [uncons] consume the sequence down to depth 1.
     That is, they demand the first argument of the sequence, if there is one.
   - [iter], [fold_left], [length], etc., consume the sequence all the way to
     its end. They terminate only if the sequence is finite.
   - [for_all], [exists], [find], etc. consume the sequence down to a certain
     depth, which is a priori unpredictable.

   Similarly, among the functions that consume two sequences,
   one can distinguish two groups:
   - [iter2] and [fold_left2] consume both sequences all the way
     to the end, provided the sequences have the same length.
   - [for_all2], [exists2], [equal], [compare] consume the sequences down
     to a certain depth, which is a priori unpredictable.

   The functions that consume two sequences can be applied to two sequences
   of distinct lengths: in that case, the excess elements in the longer
   sequence are ignored. (It may be the case that one excess element is
   demanded, even though this element is not used.)

   None of the functions in this section is lazy. These functions
   are consumers: they force some computation to take place. *)

val is_empty : 'a t -> bool
(** [is_empty xs] determines whether the sequence [xs] is empty.

    It is recommended that the sequence [xs] be persistent.
    Indeed, [is_empty xs] demands the head of the sequence [xs],
    so, if [xs] is ephemeral, it may be the case that [xs] cannot
    be used any more after this call has taken place.

    @since 4.14 *)

val uncons : 'a t -> ('a * 'a t) option
(** If [xs] is empty, then [uncons xs] is [None].

    If [xs] is nonempty, then [uncons xs] is [Some (x, ys)] where [x] is the
    head of the sequence and [ys] its tail.

    @since 4.14 *)

val length : 'a t -> int
(** [length xs] is the length of the sequence [xs].

    The sequence [xs] must be finite.

    @since 4.14 *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f xs] invokes [f x] successively
    for every element [x] of the sequence [xs],
    from left to right.

    It terminates only if the sequence [xs] is finite. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** [fold_left f _ xs] invokes [f _ x] successively
    for every element [x] of the sequence [xs],
    from left to right.

    An accumulator of type ['a] is threaded through the calls to [f].

    It terminates only if the sequence [xs] is finite. *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** [iteri f xs] invokes [f i x] successively
    for every element [x] located at index [i] in the sequence [xs].

    It terminates only if the sequence [xs] is finite.

    [iteri f xs] is equivalent to
    [iter (fun (i, x) -> f i x) (zip (ints 0) xs)].

    @since 4.14 *)

val fold_lefti : ('b -> int -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold_lefti f _ xs] invokes [f _ i x] successively
    for every element [x] located at index [i] of the sequence [xs].

    An accumulator of type ['b] is threaded through the calls to [f].

    It terminates only if the sequence [xs] is finite.

    [fold_lefti f accu xs] is equivalent to
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

    It returns [None] if there is no such element.

    The sequence [xs] must be finite.

    @since 4.14 *)

val find_map : ('a -> 'b option) -> 'a t -> 'b option
(** [find_map f xs] returns [Some y], where [x] is the first element of the
    sequence [xs] such that [f x = Some _], if there is such an element,
    and where [y] is defined by [f x = Some y].

    It returns [None] if there is no such element.

    The sequence [xs] must be finite.

    @since 4.14 *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f xs ys] invokes [f x y] successively for every pair [(x, y)] of
    elements drawn synchronously from the sequences [xs] and [ys].

    If the sequences [xs] and [ys] have different lengths, then
    iteration stops as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.

    Iteration terminates only if at least one of the sequences
    [xs] and [ys] is finite.

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

    Iteration terminates only if at least one of the sequences
    [xs] and [ys] is finite.

    [fold_left2 f accu xs ys] is equivalent to
    [fold_left (fun accu (x, y) -> f accu x y) (zip xs ys)].

    @since 4.14 *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** [for_all2 p xs ys] determines whether all pairs [(x, y)] of elements
    drawn synchronously from the sequences [xs] and [ys] satisfy [p x y].

    If the sequences [xs] and [ys] have different lengths, then
    iteration stops as soon as one sequence is exhausted;
    the excess elements in the other sequence are ignored.
    In particular, if [xs] or [ys] is empty, then
    [for_all2 p xs ys] is true. This is where
    [for_all2] and [equal] differ: [equal eq xs ys] can
    be true only if [xs] and [ys] have the same length.

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

val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
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

val empty : 'a t
(** [empty] is the empty sequence.
    It has no elements. Its length is 0. *)

val return : 'a -> 'a t
(** [return x] is the sequence whose sole element is [x].
    Its length is 1. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] is the sequence that begins with the element [x],
    followed with the sequence [xs].

    Writing [cons (f()) xs] causes the function call [f()]
    to take place immediately. For this call to be delayed until the
    sequence is queried, one must instead write
    [(fun () -> Cons(f(), xs))].

    @since 4.11 *)

val init : int -> (int -> 'a) -> 'a t
(** [init n f] is the sequence [f 0; f 1; ...; f (n-1)].

    [n] must be nonnegative.

    If desired, the infinite sequence [f 0; f 1; ...]
    can be defined as [map f (ints 0)].

    @raise Invalid_argument if [n] is negative.

    @since 4.14 *)

val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
(** [unfold] constructs a sequence
    out of a step function and an initial state.

    If [f u] is [None] then
    [unfold f u] is the empty sequence.
    If [f u] is [Some (x, u')] then
    [unfold f u] is the nonempty sequence [cons x (unfold f u')].

    For example, [unfold (function [] -> None | h :: t -> Some (h, t)) l]
    is equivalent to [List.to_seq l].

    @since 4.11 *)

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
(** [cycle xs] is the infinite sequence that consists of an infinite
    number of repetitions of the sequence [xs].

    If [xs] is an empty sequence,
    then [cycle xs] is empty as well.

    Consuming (a prefix of) the sequence [cycle xs] once
    can cause the sequence [xs] to be consumed more than once.
    Therefore, [xs] must be persistent.

    @since 4.14 *)

val iterate : ('a -> 'a) -> 'a -> 'a t
(** [iterate f x] is the infinite sequence whose elements are
    [x], [f x], [f (f x)], and so on.

    In other words, it is the orbit of the function [f],
    starting at [x].

    @since 4.14 *)

(** {1 Transforming sequences} *)

(** The functions in this section are lazy: that is, they return sequences
    whose elements are computed only when demanded. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f xs] is the image of the sequence [xs] through the
    transformation [f].

    If [xs] is the sequence [x0; x1; ...] then
    [map f xs] is the sequence [f x0; f x1; ...]. *)

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi] is analogous to [map], but applies the function [f] to
    an index and an element.

    [mapi f xs] is equivalent to [map2 f (ints 0) xs].

    @since 4.14 *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p xs] is the sequence of the elements [x] of [xs]
    that satisfy [p x].

    In other words, [filter p xs] is the sequence [xs],
    deprived of the elements [x] such that [p x] is false. *)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** [filter_map f xs] is the sequence of the elements [y] such that
    [f x = Some y], where [x] ranges over [xs].

    [filter_map f xs] is equivalent to
    [map Option.get (filter Option.is_some (map f xs))]. *)

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

    The construction of the sequence [memoize xs] internally relies on
    suspensions provided by the module {!Lazy}. These suspensions are
    {i not} thread-safe. Therefore, the sequence [memoize xs]
    must {i not} be queried by multiple threads concurrently.

    @since 4.14 *)

exception Forced_twice
(** This exception is raised when a sequence returned by {!once}
    (or a suffix of it) is queried more than once.

    @since 4.14 *)

val once : 'a t -> 'a t
(** The sequence [once xs] has the same elements as the sequence [xs].

    Regardless of whether [xs] is ephemeral or persistent,
    [once xs] is an ephemeral sequence: it can be queried at most once.
    If it (or a suffix of it) is queried more than once, then the exception
    [Forced_twice] is raised. This can be useful, while debugging or testing,
    to ensure that a sequence is consumed at most once.

    @raise Forced_twice if [once xs], or a suffix of it,
           is queried more than once.

    @since 4.14 *)

val transpose : 'a t t -> 'a t t
(** If [xss] is a matrix (a sequence of rows), then [transpose xss] is
    the sequence of the columns of the matrix [xss].

    The rows of the matrix [xss] are not required to have the same length.

    The matrix [xss] is not required to be finite (in either direction).

    The matrix [xss] must be persistent.

    @since 4.14 *)

(** {1 Combining sequences} *)

val append : 'a t -> 'a t -> 'a t
(** [append xs ys] is the concatenation of the sequences [xs] and [ys].

    Its elements are the elements of [xs], followed by the elements of [ys].

    @since 4.11 *)

val concat : 'a t t -> 'a t
(** If [xss] is a sequence of sequences,
    then [concat xss] is its concatenation.

    If [xss] is the sequence [xs0; xs1; ...] then
    [concat xss] is the sequence [xs0 @ xs1 @ ...].

    @since 4.13 *)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** [flat_map f xs] is equivalent to [concat (map f xs)]. *)

val concat_map : ('a -> 'b t) -> 'a t -> 'b t
(** [concat_map f xs] is equivalent to [concat (map f xs)].

    [concat_map] is an alias for [flat_map].

    @since 4.13 *)

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

val interleave : 'a t -> 'a t -> 'a t
(** [interleave xs ys] is the sequence that begins with the first element of
    [xs], continues with the first element of [ys], and so on.

    When one of the sequences [xs] and [ys] is exhausted,
    [interleave xs ys] continues with the rest of the other sequence.

    @since 4.14 *)

val sorted_merge : ('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
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

val map_product : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** The sequence [map_product f xs ys] is the image through [f]
    of the Cartesian product of the sequences [xs] and [ys].

    For every element [x] of [xs] and for every element [y] of [ys],
    the element [f x y] appears once as an element of [map_product f xs ys].

    The order in which these elements appear is unspecified.

    The sequences [xs] and [ys] are not required to be finite.

    The sequences [xs] and [ys] must be persistent.

    [map_product f xs ys] is equivalent to
    [map (fun (x, y) -> f x y) (product xs ys)].

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

val split : ('a * 'b) t -> 'a t * 'b t
(** [split] is an alias for [unzip].

    @since 4.14 *)

val partition_map : ('a -> ('b, 'c) Either.t) -> 'a t -> 'b t * 'c t
(** [partition_map f xs] returns a pair of sequences [(ys, zs)], where:

    - [ys] is the sequence of the elements [y] such that
      [f x = Left y], where [x] ranges over [xs];

    - [zs] is the sequence of the elements [z] such that
      [f x = Right z], where [x] ranges over [xs].

    [partition_map f xs] is equivalent to a pair of
    [filter_map Either.find_left (map f xs)] and
    [filter_map Either.find_right (map f xs)].

    Querying either of the sequences returned by [partition_map f xs]
    causes [xs] to be queried.
    Therefore, querying both of them
    causes [xs] to be queried twice.
    Thus, [xs] must be persistent and cheap.
    If that is not the case, use [partition_map f (memoize xs)].

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

(** {1 Converting between sequences and dispensers} *)

(** A dispenser is a representation of a sequence as a function of type
    [unit -> 'a option]. Every time this function is invoked, it returns
    the next element of the sequence. When there are no more elements,
    it returns [None]. A dispenser has mutable internal state, therefore
    is ephemeral: the sequence that it represents can be consumed at most
    once. *)

val of_dispenser : (unit -> 'a option) -> 'a t
(** [of_dispenser it] is the sequence of the elements produced by the
    dispenser [it]. It is an ephemeral sequence: it can be consumed at most
    once. If a persistent sequence is needed, use
    [memoize (of_dispenser it)].

    @since 4.14 *)

val to_dispenser : 'a t -> (unit -> 'a option)
(** [to_dispenser xs] is a fresh dispenser on the sequence [xs].

    This dispenser has mutable internal state,
    which is not protected by a lock;
    so, it must not be used by several threads concurrently.

    @since 4.14 *)

(** {1 Sequences of integers} *)

val ints : int -> int t
(** [ints i] is the infinite sequence of the integers beginning at [i] and
    counting up.

    @since 4.14 *)
