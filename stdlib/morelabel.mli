(* $Id$ *)

(* Module [Morelabel] : more labelized functions to handle data *)

module Hashtbl : sig

(* Module [Hashtbl]: hash tables and hash functions *)

(* Hash tables are hashed association tables, with in-place modification. *)

(*** Generic interface *)

type ('a, 'b) t
        (* The type of hash tables from type ['a] to type ['b]. *)

val create : int -> ('a,'b) t
        (* [Hashtbl.create n] creates a new, empty hash table, with
           initial size [n].  The table grows as needed, so [n] is
           just an initial guess.  Better results are said to be
           achieved when [n] is a prime number.
           Raise [Invalid_argument] if [n] is less than 1. *)

val clear : ('a, 'b) t -> unit
        (* Empty a hash table. *)

val add : ('a, 'b) t -> key:'a -> data:'b -> unit
        (* [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
           Previous bindings for [x] are not removed, but simply
           hidden. That is, after performing [Hashtbl.remove tbl x],
           the previous binding for [x], if any, is restored.
           (Same behavior as with association lists.) *)

val find : ('a, 'b) t -> key:'a -> 'b
        (* [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
           or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> key:'a -> 'b list
        (* [Hashtbl.find_all tbl x] returns the list of all data
           associated with [x] in [tbl].
           The current binding is returned first, then the previous
           bindings, in reverse order of introduction in the table. *)

val mem :  ('a, 'b) t -> key:'a -> bool
        (* [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

val remove : ('a, 'b) t -> key:'a -> unit
        (* [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
           restoring the previous binding if it exists.
           It does nothing if [x] is not bound in [tbl]. *)

val iter : fun:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
        (* [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Each binding is presented exactly once
           to [f]. *)

(*** Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end
        (* The input signature of the functor [Hashtbl.Make].
           [t] is the type of keys.
           [equal] is the equality predicate used to compare keys.
           [hash] is a hashing function on keys, returning a non-negative
           integer. It must be such that if two keys are equal according
           to [equal], then they must have identical hash values as computed
           by [hash].
           Examples: suitable ([equal], [hash]) pairs for arbitrary key
           types include
           ([(=)], [Hashtbl.hash]) for comparing objects by structure, and
           ([(==)], [Hashtbl.hash]) for comparing objects by addresses
           (e.g. for mutable or cyclic keys). *)

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear: 'a t -> unit
    val add: 'a t -> key:key -> data:'a -> unit
    val remove: 'a t -> key:key -> unit
    val find: 'a t -> key:key -> 'a
    val find_all: 'a t -> key:key -> 'a list
    val mem: 'a t -> key:key -> bool
    val iter: fun:(key:key -> data:'a -> unit) -> 'a t -> unit
  end

module Make(H : HashedType): (S with type key = H.t)

        (* The functor [Hashtbl.Make] returns a structure containing
           a type [key] of keys and a type ['a t] of hash tables
           associating data of type ['a] to keys of type [key].
           The operations perform similarly to those of the generic
           interface, but use the hashing and equality functions
           specified in the functor argument [H] instead of generic
           equality and hashing. *)

(*** The polymorphic hash primitive *)

val hash : 'a -> int
        (* [Hashtbl.hash x] associates a positive integer to any value of
           any type. It is guaranteed that
                if [x = y], then [hash x = hash y]. 
           Moreover, [hash] always terminates, even on cyclic
           structures. *)

external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"
        (* [Hashtbl.hash_param n m x] computes a hash value for [x], with the
           same properties as for [hash]. The two extra parameters [n] and
           [m] give more precise control over hashing. Hashing performs a
           depth-first, right-to-left traversal of the structure [x], stopping
           after [n] meaningful nodes were encountered, or [m] nodes,
           meaningful or not, were encountered. Meaningful nodes are: integers;
           floating-point numbers; strings; characters; booleans; and constant
           constructors. Larger values of [m] and [n] means that more
           nodes are taken into account to compute the final hash
           value, and therefore collisions are less likely to happen.
           However, hashing takes longer. The parameters [m] and [n]
           govern the tradeoff between accuracy and speed. *)

end

module Buffer : sig

(* Module [Buffer]: extensible string buffers *)

(* This module implements string buffers that automatically expand
   as necessary.  It provides accumulative concatenation of strings
   in quasi-linear time (instead of quadratic time when strings are
   concatenated pairwise). *)

type t
     (* The abstract type of buffers. *)

val create : int -> t
     (* [create n] returns a fresh buffer, initially empty.
        The [n] parameter is the initial size of the internal string
        that holds the buffer contents.  That string is automatically
        reallocated when more than [n] characters are stored in the buffer,
        but shrinks back to [n] characters when [reset] is called.
        For best performance, [n] should be of the same order of magnitude
        as the number of characters that are expected to be stored in
        the buffer (for instance, 80 for a buffer that holds one output
        line).  Nothing bad will happen if the buffer grows beyond that
        limit, however.  In doubt, take [n = 16] for instance. *)
val contents : t -> string
     (* Return a copy of the current contents of the buffer.
        The buffer itself is unchanged. *)
val length : t -> int
     (* Return the number of characters currently contained in the buffer. *)
val clear : t -> unit
     (* Empty the buffer. *)
val reset : t -> unit
     (* Empty the buffer and deallocate the internal string holding the
        buffer contents, replacing it with the initial internal string
        of length [n] that was allocated by [create n].
        For long-lived buffers that may have grown a lot, [reset] allows
        faster reclaimation of the space used by the buffer. *)
val add_char : t -> char -> unit
     (* [add_char b c] appends the character [c] at the end of
        the buffer [b]. *)
val add_string : t -> string -> unit
     (* [add_string b s] appends the string [s] at the end of
        the buffer [b]. *)
val add_substring : t -> string -> pos:int -> len:int -> unit
     (* [add_substring b s ofs len] takes [len] characters from offset
        [ofs] in string [s] and appends them at the end of the buffer [b]. *)
val add_buffer : t -> t -> unit
     (* [add_buffer b1 b2] appends the current contents of buffer [b2]
        at the end of buffer [b1].  [b2] is not modified. *)
val add_channel : t -> in_channel -> len:int -> unit
     (* [add_channel b ic n] reads exactly [n] character from the
        input channel [ic] and stores them at the end of buffer [b].
        Raise [End_of_file] if the channel contains fewer than [n]
        characters. *)
val output_buffer : to:out_channel -> t -> unit
     (* [output_buffer oc b] writes the current contents of buffer [b]
        on the output channel [oc]. *)

end

module Map : sig

(* Module [Map]: association tables over ordered types *)

(* This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end
          (* The input signature of the functor [Map.Make].
             [t] is the type of the map keys.
             [compare] is a total ordering function over the keys.
             This is a two-argument function [f] such that
             [f e1 e2] is zero if the keys [e1] and [e2] are equal,
             [f e1 e2] is strictly negative if [e1] is smaller than [e2],
             and [f e1 e2] is strictly positive if [e1] is greater than [e2].
             Example: a suitable ordering function is
             the generic structural comparison function [compare]. *)

module type S =
  sig
    type key
          (* The type of the map keys. *)
    type 'a t
          (* The type of maps from type [key] to type ['a]. *)
    val empty: 'a t
          (* The empty map. *)
    val add: key:key -> data:'a -> 'a t -> 'a t
        (* [add x y m] returns a map containing the same bindings as
           [m], plus a binding of [x] to [y]. If [x] was already bound
           in [m], its previous binding disappears. *)
    val find: key:key -> 'a t -> 'a
        (* [find x m] returns the current binding of [x] in [m],
           or raises [Not_found] if no such binding exists. *)
    val remove: key:key -> 'a t -> 'a t
        (* [remove x m] returns a map containing the same bindings as
           [m], except for [x] which is unbound in the returned map. *)
    val mem:  key:key -> 'a t -> bool
        (* [mem x m] returns [true] if [m] contains a binding for [m],
           and [false] otherwise. *)
    val iter: fun:(key:key -> data:'a -> unit) -> 'a t -> unit
        (* [iter f m] applies [f] to all bindings in map [m].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Only current bindings are presented to [f]:
           bindings hidden by more recent bindings are not passed to [f]. *)
    val map: fun:('a -> 'b) -> 'a t -> 'b t
        (* [map f m] returns a map with same domain as [m], where the
           associated value [a] of all bindings of [m] has been
           replaced by the result of the application of [f] to [a].
           The order in which the associated values are passed to [f]
           is unspecified. *)
    val fold: fun:(key:key -> data:'a -> acc:'b -> 'b) -> 'a t -> acc:'b -> 'b
        (* [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
           where [k1 ... kN] are the keys of all bindings in [m],
           and [d1 ... dN] are the associated data.
           The order in which the bindings are presented to [f] is
           unspecified. *)
  end

module Make(Ord: OrderedType): (S with type key = Ord.t)
        (* Functor building an implementation of the map structure
           given a totally ordered type. *)

end


module Set : sig

(* Module [Set]: sets over ordered types *)

(* This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance. *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end
          (* The input signature of the functor [Set.Make].
             [t] is the type of the set elements.
             [compare] is a total ordering function over the set elements.
             This is a two-argument function [f] such that
             [f e1 e2] is zero if the elements [e1] and [e2] are equal,
             [f e1 e2] is strictly negative if [e1] is smaller than [e2],
             and [f e1 e2] is strictly positive if [e1] is greater than [e2].
             Example: a suitable ordering function is
             the generic structural comparison function [compare]. *)

module type S =
  sig
    type elt
          (* The type of the set elements. *)
    type t
          (* The type of sets. *)
    val empty: t
          (* The empty set. *)
    val is_empty: t -> bool
        (* Test whether a set is empty or not. *)
    val mem: elt:elt -> t -> bool
        (* [mem x s] tests whether [x] belongs to the set [s]. *)
    val add: let:elt -> t -> t
        (* [add x s] returns a set containing all elements of [s],
           plus [x]. If [x] was already in [s], [s] is returned unchanged. *)
    val singleton: elt -> t
        (* [singleton x] returns the one-element set containing only [x]. *)
    val remove: elt:elt -> t -> t
        (* [remove x s] returns a set containing all elements of [s],
           except [x]. If [x] was not in [s], [s] is returned unchanged. *)
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
        (* Union, intersection and set difference. *)
    val compare: t -> t -> int
        (* Total ordering between sets. Can be used as the ordering function
           for doing sets of sets. *)
    val equal: t -> t -> bool
        (* [equal s1 s2] tests whether the sets [s1] and [s2] are
           equal, that is, contain equal elements. *)
    val subset: t -> t -> bool
        (* [subset s1 s2] tests whether the set [s1] is a subset of
           the set [s2]. *)
    val iter: fun:(elt -> unit) -> t -> unit
        (* [iter f s] applies [f] in turn to all elements of [s].
           The order in which the elements of [s] are presented to [f]
           is unspecified. *)
    val fold: fun:(elt -> acc:'a -> 'a) -> t -> acc:'a -> 'a
        (* [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
           where [x1 ... xN] are the elements of [s].
           The order in which elements of [s] are presented to [f] is
           unspecified. *)
    val cardinal: t -> int
        (* Return the number of elements of a set. *)
    val elements: t -> elt list
        (* Return the list of all elements of the given set.
           The returned list is sorted in increasing order with respect
           to the ordering [Ord.compare], where [Ord] is the argument
           given to [Set.Make]. *)
    val min_elt: t -> elt
        (* Return the smallest element of the given set
           (with respect to the [Ord.compare] ordering), or raise
           [Not_found] if the set is empty. *)
    val max_elt: t -> elt
        (* Same as [min_elt], but returns the largest element of the
           given set. *)
    val choose: t -> elt
        (* Return one element of the given set, or raise [Not_found] if
           the set is empty. Which element is chosen is unspecified,
           but equal elements will be chosen for equal sets. *)
  end

module Make(Ord : OrderedType): (S with type elt = Ord.t)
        (* Functor building an implementation of the set structure
           given a totally ordered type. *)

end
