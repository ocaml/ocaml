(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Type introspection.

    @since 5.1 *)

(** {1:witness Type equality witness} *)

type (_, _) eq = Equal: ('a, 'a) eq (** *)
(** The purpose of [eq] is to represent type equalities that may not otherwise
    be known by the type checker (e.g. because they may depend on dynamic data).

    A value of type [(a, b) eq] represents the fact that types [a] and [b] are
    equal.

    If one has a value [eq : (a, b) eq] that proves types [a] and [b] are equal,
    one can use it to convert a value of type [a] to a value of type [b] by
    pattern matching on [Equal]:
    {[
      let cast (type a) (type b) (Equal : (a, b) Type.eq) (a : a) : b = a
    ]}

    At runtime, this function simply returns its second argument unchanged.
*)

(** {1:identifiers Type identifiers} *)

(** Type identifiers.

    Type identifiers are values that can be tested for
    {{!Id.equal}equality} to prove they represent the same type. Note
    that unequal identifiers does not imply unequal types: the same
    type can be represented by multiple identifiers.

    See an {{!Id.example}example} of use. *)
module Id : sig

  (** {1:ids Type identifiers} *)

  type 'a t
  (** The type for type identifiers. ['a] is the represented type. *)

  val make : unit -> 'a t
  (** [make ()] is a new type identifier. *)

  val equal : 'a t -> 'b t -> ('a, 'b) eq option
  (** [equal id0 id1] is [Some Equal] if identifier [id0] is equal to [id1]
      and [None] otherwise. *)

  (** {1:example Example}

      The following shows how type identifiers can be used to implement
      an heterogeneous key-value dictionary.
{[
(** Heterogeneous dictionaries. *)
module Dict : sig
  type t
  (** The type for dictionaries. *)

  type 'a key
  (** The type for keys binding values of type ['a]. *)

  val key : unit -> 'a key
  (** [key ()] is a new dictionary key. *)

  val empty : t
  (** [empty] is the empty dictionary. *)

  val add : 'a key -> 'a -> t -> t
  (** [add k v d] is [d] with [k] bound to [v]. *)

  val remove : 'a key -> t -> t
  (** [remove k d] is [d] with the last binding of [k] removed. *)

  val find : 'a key -> t -> 'a option
  (** [find k d] is the binding of [k] in [d], if any. *)
end = struct
  type 'a key = { tid : 'a Type.Id.t; exist : exist_key }
  and exist_key = K : 'a key -> exist_key

  let key () = let rec k = { tid = Type.Id.make (); exist = K k } in k

  type binding = B : 'a key * 'a -> binding
  type t = (exist_key * binding) list

  let empty = []
  let add k v d = (k.exist, B (k, v)) :: d
  let remove k d = List.remove_assq k.exist d
  let find : type a. a key -> t -> a option = fun k d ->
    match List.assq_opt k.exist d with
    | None -> None
    | Some (B (k', v)) ->
        match Type.Id.equal k.tid k'.tid with
        | None -> None
        | Some Type.Equal -> Some v
end
]}
*)
end
