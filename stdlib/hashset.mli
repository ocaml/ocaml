(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Sacha-Élie Ayoun, Soteria Tools Ltd.                  *)
(*                                                                        *)
(*   Copyright 2026, Soteria Tools Ltd.                                   *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Hash sets.

   Hash sets are hashed sets of elements, with in-place modification.
   Because most operations on a hash set modify their input, they're
   more commonly used in imperative code. Membership testing (see {!mem})
   is normally very fast, often faster than the equivalent lookup in {!Set}.

   The functors {!Make} and {!MakeSeeded} can be used when
   performance or flexibility are key.
   The user provides custom equality and hash functions for the element type,
   and obtains a custom hash set type for this particular type of element.

   {b Warning} a hash set is only as good as the hash function. A bad hash
   function will turn the set into a degenerate list,
   with linear time membership testing instead of constant time.

   The polymorphic {!t} hash set is useful in simpler cases or
   in interactive environments. It uses the polymorphic {!Hashtbl.hash} function
   defined in the OCaml runtime (at the time of writing, it's SipHash),
   as well as the polymorphic equality [(=)].
*)

(** {b Unsynchronized accesses} *)

[@@@warning "-53"]
[@@@alert unsynchronized_access
    "Unsynchronized accesses to hash sets are a programming error."
]
[@@@warning "+53"]

(**
    Unsynchronized accesses to a hash set may lead to an invalid hash set
    state. Thus, concurrent accesses to a hash set must be synchronized
    (for instance with a {!Mutex.t}).
*)


(** {1 Generic interface} *)


type !'a t
(** The type of hash sets containing elements of type ['a]. *)

val create : ?random: (* thwart tools/sync_stdlib_docs *) bool ->
             int -> 'a t
(** [Hashset.create n] creates a new, empty hash set, with initial
   size greater or equal to the suggested size [n].  For best results,
   [n] should be on the order of the expected number of elements that
   will be in the set.  The set grows as needed, so [n] is just an
   initial guess.  If [n] is very small or negative then it is
   disregarded and a small default size is used.

   The optional [~random] parameter (a boolean) controls whether
   the internal organization of the hash set is randomized at each
   execution of [Hashset.create] or deterministic over all executions.

   A hash set that is created with [~random] set to [false] uses a
   fixed hash function ({!Hashtbl.hash}) to distribute elements among
   buckets.  As a consequence, collisions between elements happen
   deterministically.  In Web-facing applications or other
   security-sensitive applications, the deterministic collision
   patterns can be exploited by a malicious user to create a
   denial-of-service attack: the attacker sends input crafted to
   create many collisions in the set, slowing the application down.

   A hash set that is created with [~random] set to [true] uses the seeded
   hash function {!Hashtbl.seeded_hash} with a seed that is randomly chosen at
   hash set creation time.  In effect, the hash function used is randomly
   selected among [2^{30}] different hash functions.  All these hash
   functions have different collision patterns, rendering ineffective the
   denial-of-service attack described above.  However, because of
   randomization, enumerating all elements of the hash set using {!fold}
   or {!iter} is no longer deterministic: elements are enumerated in
   different orders at different runs of the program.

   If no [~random] parameter is given, hash sets are created
   in non-random mode by default.  This default can be changed
   either programmatically by calling {!randomize} or by
   setting the [R] flag in the [OCAMLRUNPARAM] environment variable. *)

val clear : 'a t -> unit
(** Empty a hash set. Use [reset] instead of [clear] to shrink the
    size of the bucket table to its initial size. *)

val reset : 'a t -> unit
(** Empty a hash set and shrink the size of the bucket table
    to its initial size. *)

val copy : 'a t -> 'a t
(** Return a copy of the given hash set. *)

val add : 'a t -> 'a -> unit
(** [Hashset.add s x] adds element [x] to set [s].
   If [x] is already present in [s], the set is unchanged. *)

val remove : 'a t -> 'a -> unit
(** [Hashset.remove s x] removes [x] from set [s].
   It does nothing if [x] is not present in [s]. *)

val mem : 'a t -> 'a -> bool
(** [Hashset.mem s x] checks if [x] is present in [s]. *)

val iter : ('a -> unit) -> 'a t -> unit
(** [Hashset.iter f s] applies [f] to all elements in set [s].
   Each element is presented exactly once to [f].

   The order in which the elements are passed to [f] is unspecified.

   If the hash set was created in non-randomized mode, the order
   in which the elements are enumerated is reproducible between
   successive runs of the program, and even between minor versions
   of OCaml.  For randomized hash sets, the order of enumeration
   is entirely random.

   The behavior is not specified if the hash set is modified
   by [f] during the iteration.
*)

val filter_inplace : ('a -> bool) -> 'a t -> unit
(** [Hashset.filter_inplace f s] removes from [s] all elements [x]
    for which [f x] returns [false].

    Other comments for {!iter} apply as well. *)

val fold : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [Hashset.fold f s init] computes
   [(f xN ... (f x1 init)...)],
   where [x1 ... xN] are the elements of [s].
   Each element is presented exactly once to [f].

   The order in which the elements are passed to [f] is unspecified.

   If the hash set was created in non-randomized mode, the order
   in which the elements are enumerated is reproducible between
   successive runs of the program, and even between minor versions
   of OCaml.  For randomized hash sets, the order of enumeration
   is entirely random.

   The behavior is not specified if the hash set is modified
   by [f] during the iteration.
*)

val length : 'a t -> int
(** [Hashset.length s] returns the number of elements in [s].
   It takes constant time. *)

val randomize : unit -> unit
(** After a call to [Hashset.randomize()], hash sets are created in
    randomized mode by default: {!create} returns randomized
    hash sets, unless the [~random:false] optional parameter is given.
    The same effect can be achieved by setting the [R] parameter in
    the [OCAMLRUNPARAM] environment variable.

    It is recommended that applications or Web frameworks that need to
    protect themselves against the denial-of-service attack described
    in {!create} call [Hashset.randomize()] at initialization
    time before any domains are created.

    Note that once [Hashset.randomize()] was called, there is no way
    to revert to the non-randomized default behavior of {!create}.
    This is intentional.  Non-randomized hash sets can still be
    created using [Hashset.create ~random:false]. *)

val is_randomized : unit -> bool
(** Return [true] if the sets are currently created in randomized mode
    by default, [false] otherwise. *)

val rebuild : ?random (* thwart tools/sync_stdlib_docs *) :bool ->
    'a t -> 'a t
(** Return a copy of the given hash set.  Unlike {!copy},
    {!rebuild}[ s] re-hashes all the elements of
    the original set [s].  The returned hash set is randomized if
    [s] was randomized, or the optional [random] parameter is true, or
    if the default is to create randomized hash sets; see
    {!create} for more information.

    {!rebuild} can safely be used to import a hash set built
    by an old version of the {!Hashset} module, then marshaled to
    persistent storage.  After unmarshaling, apply {!rebuild}
    to produce a hash set for the current version of the {!Hashset}
    module. *)

val stats : 'a t -> Hashtbl.statistics
(** [Hashset.stats s] returns statistics about the set [s]:
   number of buckets, size of the biggest bucket, distribution of
   buckets by size. *)

(** {1 Hash sets and Sequences} *)

val to_seq : 'a t -> 'a Seq.t
(** Iterate on the whole set.  The order in which the elements
    appear in the sequence is unspecified.

    The behavior is not specified if the hash set is modified
    during the iteration. *)

val add_seq : 'a t -> 'a Seq.t -> unit
(** Add the given elements to the set, using {!add}. *)

val of_seq : 'a Seq.t -> 'a t
(** Build a set from the given elements. *)

(** {1 Functorial interface} *)

(** The functorial interface allows the use of specific comparison
    and hash functions, either for performance/security concerns,
    or because elements are not hashable/comparable with the
    polymorphic builtins.

    For instance, one might want to specialize a set for integer elements:
    {[
      module IntHash =
        struct
          type t = int
          let equal i j = i=j
          let hash i = i land max_int
        end

      module IntHashset = Hashset.Make(IntHash)

      let s = IntHashset.create 17 in
      IntHashset.add s 12
    ]}

    This creates a new module [IntHashset], with a new type
    [IntHashset.t] of sets of integers.

    Note that the new type [IntHashset.t] is not compatible with
    the type ['a Hashset.t] of the generic interface. For
    example, [Hashset.length s] would not type-check, you must use
    [IntHashset.length].
*)

module type S =
  sig
    type elt
    (** The type of elements of the hash set. *)

    type t
    (** The type of hash sets. *)

    val create : int -> t
    val clear : t -> unit
    val reset : t -> unit
    val copy : t -> t
    val add : t -> elt -> unit
    val remove : t -> elt -> unit
    val mem : t -> elt -> bool
    val iter : (elt -> unit) -> t -> unit
    val filter_inplace : (elt -> bool) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val length : t -> int
    val stats : t -> Hashtbl.statistics
    val to_seq : t -> elt Seq.t
    val add_seq : t -> elt Seq.t -> unit
    val of_seq : elt Seq.t -> t
  end
(** The output signature of the functor {!Make}. *)

module Make (H : Hashtbl.HashedType) : S with type elt = H.t
(** Functor building an implementation of the hash set structure.
    The functor [Hashset.Make] returns a structure containing
    a type [elt] of elements and a type [t] of hash sets.
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing.  Since the hash function is not seeded,
    the [create] operation of the result structure always returns
    non-randomized hash sets. *)

module type SeededS =
  sig
    type elt
    (** The type of elements of the hash set. *)

    type t
    (** The type of hash sets. *)

    val create : ?random (* thwart tools/sync_stdlib_docs *) :bool ->
                 int -> t
    val clear : t -> unit
    val reset : t -> unit
    val copy : t -> t
    val add : t -> elt -> unit
    val remove : t -> elt -> unit
    val mem : t -> elt -> bool
    val iter : (elt -> unit) -> t -> unit
    val filter_inplace : (elt -> bool) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val length : t -> int
    val stats : t -> Hashtbl.statistics
    val to_seq : t -> elt Seq.t
    val add_seq : t -> elt Seq.t -> unit
    val of_seq : elt Seq.t -> t
  end
(** The output signature of the functor {!MakeSeeded}. *)

module MakeSeeded (H : Hashtbl.SeededHashedType) : SeededS with type elt = H.t
(** Functor building an implementation of the hash set structure.
    The functor [Hashset.MakeSeeded] returns a structure containing
    a type [elt] of elements and a type [t] of hash sets.
    The operations perform similarly to those of the generic
    interface, but use the seeded hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing.  The [create] operation of the
    result structure supports the [~random] optional parameter
    and returns randomized hash sets if [~random:true] is passed
    or if randomization is globally on (see {!Hashset.randomize}). *)
