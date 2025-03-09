(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Cambium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Top modules dependencies. *)

val impl_dependencies : Parsetree.structure -> Misc.Stdlib.String.Set.elt list

val intf_dependencies : Parsetree.signature -> Misc.Stdlib.String.Set.elt list

val kernel_deps_of_modules : Odoc_module.t_module list -> unit

val deps_of_types :
  ?kernel:bool ->
  Odoc_type.t_type list ->
  (Odoc_type.t_type * Misc.Stdlib.String.Set.elt list) list
