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

val free_structure_names : String.Set.t ref

(** dependencies found by preprocessing tools *)
val pp_deps : string list ref

val open_module : bound_map -> Longident.t -> bound_map

val add_use_file : bound_map -> Parsetree.toplevel_phrase list -> unit

val add_signature : bound_map -> Parsetree.signature -> unit

val add_implementation : bound_map -> Parsetree.structure -> unit

val add_implementation_binding : bound_map -> Parsetree.structure -> bound_map
val add_signature_binding : bound_map -> Parsetree.signature -> bound_map
