(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = floatarray
(** The type of float arrays with packed representation.
    @since 4.08
  *)

val length : t -> int
(** Return the length (number of elements) of the given floatarray. *)

val get : t -> int -> float
(** [get a n] returns the element number [n] of floatarray [a].
    @raise Invalid_argument if [n] is outside the range 0 to
    [(length a - 1)]. *)

val set : t -> int -> float -> unit
(** [set a n x] modifies floatarray [a] in place, replacing element
    number [n] with [x].
    @raise Invalid_argument if [n] is outside the range 0 to
    [(length a - 1)]. *)

val make : int -> float -> t
(** [make n x] returns a fresh floatarray of length [n], initialized with [x].
    @raise Invalid_argument if [n < 0] or [n > Sys.max_floatarray_length]. *)

val create : int -> t
(** [create n] returns a fresh floatarray of length [n],
    with uninitialized data.
    @raise Invalid_argument if [n < 0] or [n > Sys.max_floatarray_length]. *)

val init : int -> f:(int -> float) -> t
(** [init n ~f] returns a fresh floatarray of length [n],
    with element number [i] initialized to the result of [f i].
    In other terms, [init n ~f] tabulates the results of [f]
    applied to the integers [0] to [n-1].
    @raise Invalid_argument if [n < 0] or [n > Sys.max_floatarray_length]. *)

val append : t -> t -> t
(** [append v1 v2] returns a fresh floatarray containing the
    concatenation of the floatarrays [v1] and [v2].
    @raise Invalid_argument if
    [length v1 + length v2 > Sys.max_floatarray_length]. *)

val concat : t list -> t
(** Same as {!append}, but concatenates a list of floatarrays. *)

val sub : t -> pos:int -> len:int -> t
(** [sub a ~pos ~len] returns a fresh floatarray of length [len],
    containing the elements number [pos] to [pos + len - 1]
    of floatarray [a].
    @raise Invalid_argument if [pos] and [len] do not
    designate a valid subarray of [a]; that is, if
    [pos < 0], or [len < 0], or [pos + len > length a]. *)

val copy : t -> t
(** [copy a] returns a copy of [a], that is, a fresh floatarray
    containing the same elements as [a]. *)

val fill : t -> pos:int -> len:int -> float -> unit
(** [fill a ~pos ~len x] modifies the floatarray [a] in place,
    storing [x] in elements number [pos] to [pos + len - 1].
    @raise Invalid_argument if [pos] and [len] do not
    designate a valid subarray of [a]. *)

val blit : src:t -> src_pos:int -> dst:t -> dst_pos:int -> len:int -> unit
(** [blit ~src ~src_pos ~dst ~dst_pos ~len] copies [len] elements
    from floatarray [src], starting at element number [src_pos],
    to floatarray [dst], starting at element number [dst_pos].
    It works correctly even if
    [src] and [dst] are the same floatarray, and the source and
    destination chunks overlap.
    @raise Invalid_argument if [src_pos] and [len] do not
    designate a valid subarray of [src], or if [dst_pos] and [len] do not
    designate a valid subarray of [dst]. *)

val to_list : t -> float list
(** [to_list a] returns the list of all the elements of [a]. *)

val of_list : float list -> t
(** [of_list l] returns a fresh floatarray containing the elements
    of [l].
    @raise Invalid_argument if the length of [l] is greater than
    [Sys.max_floatarray_length].*)

(** {2 Iterators} *)

val iter : f:(float -> unit) -> t -> unit
(** [iter ~f a] applies function [f] in turn to all
    the elements of [a].  It is equivalent to
    [f a.(0); f a.(1); ...; f a.(length a - 1); ()]. *)

val iteri : f:(int -> float -> unit) -> t -> unit
(** Same as {!iter}, but the
    function is applied with the index of the element as first argument,
    and the element itself as second argument. *)

val map : f:(float -> float) -> t -> t
(** [map ~f a] applies function [f] to all the elements of [a],
    and builds a floatarray with the results returned by [f]. *)

val mapi : f:(int -> float -> float) -> t -> t
(** Same as {!map}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val fold_left : f:('a -> float -> 'a) -> init:'a -> t -> 'a
(** [fold_left ~f x ~init] computes
    [f (... (f (f x init.(0)) init.(1)) ...) init.(n-1)],
    where [n] is the length of the floatarray [init]. *)

val fold_right : f:(float -> 'a -> 'a) -> t -> init:'a -> 'a
(** [fold_right f a init] computes
    [f a.(0) (f a.(1) ( ... (f a.(n-1) init) ...))],
    where [n] is the length of the floatarray [a]. *)

(** {2 Iterators on two arrays} *)

val iter2 : f:(float -> float -> unit) -> t -> t -> unit
(** [Array.iter2 ~f a b] applies function [f] to all the elements of [a]
    and [b].
    @raise Invalid_argument if the floatarrays are not the same size. *)

val map2 : f:(float -> float -> float) -> t -> t -> t
(** [map2 ~f a b] applies function [f] to all the elements of [a]
    and [b], and builds a floatarray with the results returned by [f]:
    [[| f a.(0) b.(0); ...; f a.(length a - 1) b.(length b - 1)|]].
    @raise Invalid_argument if the floatarrays are not the same size. *)

(** {2 Array scanning} *)

val for_all : f:(float -> bool) -> t -> bool
(** [for_all ~f [|a1; ...; an|]] checks if all elements of the floatarray
    satisfy the predicate [f]. That is, it returns
    [(f a1) && (f a2) && ... && (f an)]. *)

val exists : f:(float -> bool) -> t -> bool
(** [exists f [|a1; ...; an|]] checks if at least one element of
    the floatarray satisfies the predicate [f]. That is, it returns
    [(f a1) || (f a2) || ... || (f an)]. *)

val mem : float -> set:t -> bool
(** [mem a ~set] is true if and only if there is an element of [set] that is
    structurally equal to [a], i.e. there is an [x] in [set] such
    that [compare a x = 0]. *)

val mem_ieee : float -> set:t -> bool
(** Same as {!mem}, but uses IEEE equality instead of structural equality. *)

(** {2 Sorting} *)

val sort : cmp:(float -> float -> int) -> t -> unit
(** Sort a floatarray in increasing order according to a comparison
    function.  The comparison function must return 0 if its arguments
    compare as equal, a positive integer if the first is greater,
    and a negative integer if the first is smaller (see below for a
    complete specification).  For example, {!Stdlib.compare} is
    a suitable comparison function.  After calling [sort], the
    array is sorted in place in increasing order.
    [sort] is guaranteed to run in constant heap space
    and (at most) logarithmic stack space.

    The current implementation uses Heap Sort.  It runs in constant
    stack space.

    Specification of the comparison function:
    Let [a] be the floatarray and [cmp] the comparison function. The following
    must be true for all [x], [y], [z] in [a] :
-      [cmp x y] > 0 if and only if [cmp y x] < 0
-      if [cmp x y] >= 0 and [cmp y z] >= 0 then [cmp x z] >= 0

    When [sort] returns, [a] contains the same elements as before,
    reordered in such a way that for all i and j valid indices of [a] :
-      [cmp a.(i) a.(j)] >= 0 if and only if i >= j
*)

val stable_sort : cmp:(float -> float -> int) -> t -> unit
(** Same as {!sort}, but the sorting algorithm is stable (i.e.
     elements that compare equal are kept in their original order) and
     not guaranteed to run in constant heap space.

     The current implementation uses Merge Sort. It uses a temporary
     floatarray of length [n/2], where [n] is the length of the floatarray.
     It is usually faster than the current implementation of {!sort}. *)

val fast_sort : cmp:(float -> float -> int) -> t -> unit
(** Same as {!sort} or {!stable_sort}, whichever is faster
    on typical input. *)

(** {2 Float arrays and Sequences} *)

val to_seq : t -> float Seq.t
(** Iterate on the floatarray, in increasing order. Modifications of the
    floatarray during iteration will be reflected in the sequence. *)

val to_seqi : t -> (int * float) Seq.t
(** Iterate on the floatarray, in increasing order, yielding indices along
    elements. Modifications of the floatarray during iteration will be
    reflected in the sequence. *)

val of_seq : float Seq.t -> t
(** Create an array from the generator. *)


val map_to_array : f:(float -> 'a) -> t -> 'a array
(** [map_to_array ~f a] applies function [f] to all the elements of [a],
    and builds an array with the results returned by [f]:
    [[| f a.(0); f a.(1); ...; f a.(length a - 1) |]]. *)

val map_from_array : f:('a -> float) -> 'a array -> t
(** [map_from_array ~f a] applies function [f] to all the elements of [a],
    and builds a floatarray with the results returned by [f]. *)

(** {1:floatarray_concurrency Arrays and concurrency safety}

    Care must be taken when concurrently accessing float arrays from multiple
    domains: accessing an array will never crash a program, but unsynchronized
    accesses might yield surprising (non-sequentially-consistent) results.

    {2:floatarray_atomicity Atomicity}

    Every float array operation that accesses more than one array element is
    not atomic. This includes iteration, scanning, sorting, splitting and
    combining arrays.

    For example, consider the following program:
{[let size = 100_000_000
let a = Float.ArrayLabels.make size 1.
let update a f () =
   Float.ArrayLabels.iteri ~f:(fun i x -> Float.Array.set a i (f x)) a
let d1 = Domain.spawn (update a (fun x -> x +. 1.))
let d2 = Domain.spawn (update a (fun x ->  2. *. x +. 1.))
let () = Domain.join d1; Domain.join d2
]}

    After executing this code, each field of the float array [a] is either
    [2.], [3.], [4.] or [5.]. If atomicity is required, then the user must
    implement their own synchronization (for example, using {!Mutex.t}).

    {2:floatarray_data_race Data races}

    If two domains only access disjoint parts of the array, then the
    observed behaviour is the equivalent to some sequential interleaving of
    the operations from the two domains.

    A data race is said to occur when two domains access the same array
    element without synchronization and at least one of the accesses is a
    write. In the absence of data races, the observed behaviour is equivalent
    to some sequential interleaving of the operations from different domains.

    Whenever possible, data races should be avoided by using synchronization
    to mediate the accesses to the array elements.

    Indeed, in the presence of data races, programs will not crash but the
    observed behaviour may not be equivalent to any sequential interleaving of
    operations from different domains. Nevertheless, even in the presence of
    data races, a read operation will return the value of some prior write to
    that location with a few exceptions.


    {2:floatarray_datarace_tearing Tearing }

    Float arrays have two supplementary caveats in the presence of data races.

    First, the blit operation might copy an array byte-by-byte. Data races
    between such a blit operation and another operation might produce
    surprising values due to tearing: partial writes interleaved with other
    operations can create float values that would not exist with a sequential
    execution.

    For instance, at the end of
{[let zeros = Float.Array.make size 0.
let max_floats = Float.Array.make size Float.max_float
let res = Float.Array.copy zeros
let d1 = Domain.spawn (fun () -> Float.Array.blit zeros 0 res 0 size)
let d2 = Domain.spawn (fun () -> Float.Array.blit max_floats 0 res 0 size)
let () = Domain.join d1; Domain.join d2
]}

    the [res] float array might contain values that are neither [0.]
    nor [max_float].

    Second, on 32-bit architectures, getting or setting a field involves two
    separate memory accesses. In the presence of data races, the user may
    observe tearing on any operation.
*)

(**/**)

(** {2 Undocumented functions} *)

(* These functions are for system use only. Do not call directly. *)
external unsafe_get : t -> int -> float = "%floatarray_unsafe_get"
external unsafe_set : t -> int -> float -> unit = "%floatarray_unsafe_set"
