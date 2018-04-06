(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Jacques Garrigue, Kyoto University RIMS                 *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Extra labeled libraries.

   This meta-module provides labelized version of the {!Hashtbl},
   {!Map} and {!Set} modules.

   They only differ by their labels. They are provided to help
   porting from previous versions of OCaml.
   The contents of this module are subject to change.
*)

module Hashtbl : sig
  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  val create : ?random:bool -> int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val reset : ('a, 'b) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b
  val find_opt : ('a, 'b) t -> 'a -> 'b option
  val find_all : ('a, 'b) t -> 'a -> 'b list
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
  val filter_map_inplace:
    f:(key:'a -> data:'b -> 'b option) -> ('a, 'b) t -> unit
  val fold :
      f:(key:'a -> data:'b -> 'c -> 'c) ->
        ('a, 'b) t -> init:'c -> 'c
  val length : ('a, 'b) t -> int
  val randomize : unit -> unit
  val is_randomized : unit -> bool
  type statistics = Hashtbl.statistics
  val stats : ('a, 'b) t -> statistics
  val to_seq : ('a,'b) t -> ('a * 'b) Seq.t
  val to_seq_keys : ('a,_) t -> 'a Seq.t
  val to_seq_values : (_,'b) t -> 'b Seq.t
  val add_seq : ('a,'b) t -> ('a * 'b) Seq.t -> unit
  val replace_seq : ('a,'b) t -> ('a * 'b) Seq.t -> unit
  val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t
  module type HashedType = Hashtbl.HashedType
  module type SeededHashedType = Hashtbl.SeededHashedType
  module type S =
    sig
      type key
      and 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key:key -> data:'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt: 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key:key -> data:'a -> unit
      val mem : 'a t -> key -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val filter_map_inplace:
        f:(key:key -> data:'a -> 'a option) -> 'a t -> unit
      val fold :
          f:(key:key -> data:'a -> 'b -> 'b) ->
          'a t -> init:'b -> 'b
      val length : 'a t -> int
      val stats: 'a t -> statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : _ t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end
  module type SeededS =
    sig
      type key
      and 'a t
      val create : ?random:bool -> int -> 'a t
      val clear : 'a t -> unit
      val reset : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key:key -> data:'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_opt : 'a t -> key -> 'a option
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key:key -> data:'a -> unit
      val mem : 'a t -> key -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val filter_map_inplace:
        f:(key:key -> data:'a -> 'a option) -> 'a t -> unit
      val fold :
          f:(key:key -> data:'a -> 'b -> 'b) ->
          'a t -> init:'b -> 'b
      val length : 'a t -> int
      val stats: 'a t -> statistics
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_keys : _ t -> key Seq.t
      val to_seq_values : 'a t -> 'a Seq.t
      val add_seq : 'a t -> (key * 'a) Seq.t -> unit
      val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
      val of_seq : (key * 'a) Seq.t -> 'a t
    end
  module Make : functor (H : HashedType) -> S
    with type key = H.t
     and type 'a t = 'a Hashtbl.Make(H).t
  module MakeSeeded (H : SeededHashedType) : SeededS
    with type key = H.t
     and type 'a t = 'a Hashtbl.MakeSeeded(H).t
  val hash : 'a -> int
  val seeded_hash : int -> 'a -> int
  val hash_param : int -> int -> 'a -> int
  val seeded_hash_param : int -> int -> int -> 'a -> int
end

module Map : sig
  module type OrderedType = Map.OrderedType
  module type S =
    sig
      type key
      and (+'a) t
      val empty : 'a t
      val is_empty: 'a t -> bool
      val mem : key -> 'a t -> bool
      val add : key:key -> data:'a -> 'a t -> 'a t
      val update: key:key -> f:('a option -> 'a option) -> 'a t -> 'a t
      val singleton: key -> 'a -> 'a t
      val remove : key -> 'a t -> 'a t
      val merge:
          f:(key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
      val union: f:(key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
      val compare: cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal: cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val fold :
          f:(key:key -> data:'a -> 'b -> 'b) ->
          'a t -> init:'b -> 'b
      val for_all: f:(key -> 'a -> bool) -> 'a t -> bool
      val exists: f:(key -> 'a -> bool) -> 'a t -> bool
      val filter: f:(key -> 'a -> bool) -> 'a t -> 'a t
      val partition: f:(key -> 'a -> bool) -> 'a t -> 'a t * 'a t
      val cardinal: 'a t -> int
      val bindings: 'a t -> (key * 'a) list
      val min_binding: 'a t -> (key * 'a)
      val min_binding_opt: 'a t -> (key * 'a) option
      val max_binding: 'a t -> (key * 'a)
      val max_binding_opt: 'a t -> (key * 'a) option
      val choose: 'a t -> (key * 'a)
      val choose_opt: 'a t -> (key * 'a) option
      val split: key -> 'a t -> 'a t * 'a option * 'a t
      val find : key -> 'a t -> 'a
      val find_opt: key -> 'a t -> 'a option
      val find_first : f:(key -> bool) -> 'a t -> key * 'a
      val find_first_opt : f:(key -> bool) -> 'a t -> (key * 'a) option
      val find_last : f:(key -> bool) -> 'a t -> key * 'a
      val find_last_opt : f:(key -> bool) -> 'a t -> (key * 'a) option
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
      val to_seq : 'a t -> (key * 'a) Seq.t
      val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
      val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
      val of_seq : (key * 'a) Seq.t -> 'a t
  end
  module Make : functor (Ord : OrderedType) -> S
    with type key = Ord.t
     and type 'a t = 'a Map.Make(Ord).t
end

module Set : sig
  module type OrderedType = Set.OrderedType
  module type S =
    sig
      type elt
      and t
      val empty : t
      val is_empty : t -> bool
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val singleton : elt -> t
      val remove : elt -> t -> t
      val union : t -> t -> t
      val inter : t -> t -> t
      val diff : t -> t -> t
      val compare : t -> t -> int
      val equal : t -> t -> bool
      val subset : t -> t -> bool
      val iter : f:(elt -> unit) -> t -> unit
      val map : f:(elt -> elt) -> t -> t
      val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
      val for_all : f:(elt -> bool) -> t -> bool
      val exists : f:(elt -> bool) -> t -> bool
      val filter : f:(elt -> bool) -> t -> t
      val partition : f:(elt -> bool) -> t -> t * t
      val cardinal : t -> int
      val elements : t -> elt list
      val min_elt : t -> elt
      val min_elt_opt: t -> elt option
      val max_elt : t -> elt
      val max_elt_opt: t -> elt option
      val choose : t -> elt
      val choose_opt: t -> elt option
      val split: elt -> t -> t * bool * t
      val find: elt -> t -> elt
      val find_opt: elt -> t -> elt option
      val find_first: f:(elt -> bool) -> t -> elt
      val find_first_opt: f:(elt -> bool) -> t -> elt option
      val find_last: f:(elt -> bool) -> t -> elt
      val find_last_opt: f:(elt -> bool) -> t -> elt option
      val of_list: elt list -> t
      val to_seq_from : elt -> t -> elt Seq.t
      val to_seq : t -> elt Seq.t
      val add_seq : elt Seq.t -> t -> t
      val of_seq : elt Seq.t -> t
    end
  module Make : functor (Ord : OrderedType) -> S
    with type elt = Ord.t
     and type t = Set.Make(Ord).t
end
