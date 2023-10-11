(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Module dependencies.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

module String = Misc.Stdlib.String

type map_tree = Node of String.Set.t * bound_map
and  bound_map = map_tree String.Map.t
val make_leaf : string -> map_tree
val make_node : bound_map -> map_tree
val weaken_map : String.Set.t -> map_tree -> map_tree

(** Collect free module identifiers in the a.s.t. *)
val free_structure_names : String.Set.t ref

(** Dependencies found by preprocessing tools. *)
val pp_deps : string list ref

val open_module : bound_map -> Longident.t -> bound_map

val add_use_file : bound_map -> Parsetree.toplevel_phrase list -> unit

val add_interface : bound_map -> Parsetree.interface -> unit

val add_implementation : bound_map -> Parsetree.implementation -> unit

val add_implementation_binding : bound_map -> Parsetree.implementation -> bound_map
val add_interface_binding : bound_map -> Parsetree.interface -> bound_map
