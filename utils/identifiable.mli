(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Uniform interface for common data structures over various things. *)

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module Pair : functor (A : Thing) (B : Thing) -> Thing with type t = A.t * B.t

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : sig
    include Set.S
      with type elt = T.t
      and type t = Set.Make (T).t

    val output : out_channel -> t -> unit
    val print : Format.formatter -> t -> unit
    val to_string : t -> string
    val of_list : elt list -> t
    val map : (elt -> elt) -> t -> t
  end

  module Map : sig
    include Map.S
      with type key = T.t
      and type 'a t = 'a Map.Make (T).t

    val filter_map : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
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
    val keys : 'a t -> Set.t
    val data : 'a t -> 'a list
    val of_set : (key -> 'a) -> Set.t -> 'a t
    val transpose_keys_and_data : key t -> key t
    val print :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  end

  module Tbl : sig
    include Hashtbl.S
      with type key = T.t
      and type 'a t = 'a Hashtbl.Make (T).t

    val to_list : 'a t -> (T.t * 'a) list
    val of_list : (T.t * 'a) list -> 'a t

    val to_map : 'a t -> 'a Map.t
    val of_map : 'a Map.t -> 'a t
    val memoize : 'a t -> (key -> 'a) -> key -> 'a
    val map : 'a t -> ('a -> 'b) -> 'b t
  end
end

module Make (T : Thing) : S with type t := T.t
