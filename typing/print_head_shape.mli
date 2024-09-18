(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Partout, INRIA Saclay                        *)
(*   Nicolas Chataing, ENS Paris                                          *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type head_shape := Head_shape_types.shape

val doc : head_shape Format_doc.printer
val pp : head_shape Format_doc.format_printer

(** Compute and print head shapes for all type declarations
    in a structure or signature. *)
val print_in_signature :
  shape_of_type_path:(Env.t -> Path.t -> head_shape) ->
  Format.formatter -> Typedtree.signature -> unit
val print_in_structure :
  shape_of_type_path:(Env.t -> Path.t -> head_shape) ->
  Format.formatter -> Typedtree.structure -> unit
