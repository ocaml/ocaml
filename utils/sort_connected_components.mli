(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(***********************************************************************)

(** The sorting of connected components of a directed graph. *)

module type S = sig
  module Id : Identifiable.S

  type directed_graph = Id.Set.t Id.Map.t
  (** If (a -> set) belongs to the map, it means that there are edges
      from [a] to every element of [set].  It is assumed that no edge
      points to a vertex not represented in the map. *)

  type component =
    | Has_loop of Id.t list
    | No_loop of Id.t

  val connected_components_sorted_from_roots_to_leaf
     : directed_graph
    -> component array

  val component_graph : directed_graph -> (component * int list) array
end

module Make (Id : Identifiable.S) : S with module Id := Id
