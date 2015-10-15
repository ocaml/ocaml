(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Extended version of Set, Map and Hashtbl functors *)

module type PrintableHashOrdered = sig
  type t
  val compare : t -> t -> int
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val hash : t -> int
  val equal : t -> t -> bool
end

module type ExtMap = sig
  module M : PrintableHashOrdered
  include Map.S with type key = M.t
                 and type 'a t = 'a Map.Make(M).t
  val map_option : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val of_list : (key * 'a) list -> 'a t

  (** [disjoint_union m1 m2] contains all bindings from [m1] and
      [m2]. If some binding is present in both and the associated
      value is not equal, a Fatal_error is raised *)
  val disjoint_union : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> 'a t

  (** [union_right m1 m2] contains all bindings from [m1] and [m2]. If
      some binding is present in both, the one from [m2] is taken *)
  val union_right : 'a t -> 'a t -> 'a t

  (** [union_left m1 m2 = union_right m2 m1] *)
  val union_left : 'a t -> 'a t -> 'a t

  val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val rename : key t -> key -> key
  val map_keys : (key -> key) -> 'a t -> 'a t
  val keys : 'a t -> Set.Make(M).t
  val of_set : (key -> 'a) -> Set.Make(M).t -> 'a t
  (* CR mshinwell: rename revert -> transpose_keys_and_data *)
  val revert : key t -> key t
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

module type ExtSet = sig
  module M : PrintableHashOrdered
  include Set.S with type elt = M.t
                 and type t = Set.Make(M).t
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
end

module type ExtHashtbl = sig
  module M : PrintableHashOrdered
  include Hashtbl.S with type key = M.t
                     and type 'a t = 'a Hashtbl.Make(M).t
  val to_map : 'a t -> 'a Map.Make(M).t
  val of_map : 'a Map.Make(M).t -> 'a t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module ExtMap :
  functor (M : PrintableHashOrdered) -> ExtMap with module M := M

module ExtSet :
  functor (M : PrintableHashOrdered) -> ExtSet with module M := M

module ExtHashtbl :
  functor (M : PrintableHashOrdered) -> ExtHashtbl with module M := M

(** Generic identifier type *)
module type BaseId =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val name : t -> string option
  val to_string : t -> string
  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Id =
sig
  include BaseId
  val create : ?name:string -> unit -> t
end

(** Fully qualified identifiers *)
module type UnitId =
sig
  module Compilation_unit : PrintableHashOrdered
  include BaseId
  val create : ?name:string -> Compilation_unit.t -> t
  val unit : t -> Compilation_unit.t
end

(** If applied generatively, i.e. [Id(struct end)], creates a new type
    of identifiers. *)
module Id : functor (E : sig end) -> Id

module UnitId :
  functor (Id : Id) ->
  functor (Compilation_unit : PrintableHashOrdered) ->
    UnitId with module Compilation_unit := Compilation_unit

module String_M : PrintableHashOrdered with type t = string
(** The module is named Stirng_M to avoid name clash with stdlib
    String if Ext_types is openend *)

module StringSet : ExtSet with module M := String_M
module StringMap : ExtMap with module M := String_M
module StringTbl : ExtHashtbl with module M := String_M

(* CR mshinwell: We should use [Identifiable] above so everything is
   uniform. *)
module type Identifiable = sig
  type t
  module M : PrintableHashOrdered with type t = t
  include PrintableHashOrdered with type t := M.t
  module Set : ExtSet with module M := M
  module Map : ExtMap with module M := M
  module Tbl : ExtHashtbl with module M := M
end

module Identifiable : sig
  module Make (P : PrintableHashOrdered) : Identifiable
    with type t := P.t
end

module Int : Identifiable with type t = int

module IntSet : sig
  (* [zero_to_n n] is the set of numbers {0, ..., n} (inclusive). *)
  val zero_to_n : int -> Int.Set.t
end

module Pair :
  functor (A : PrintableHashOrdered) ->
  functor(B : PrintableHashOrdered) ->
    PrintableHashOrdered with type t = A.t * B.t
