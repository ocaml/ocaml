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

(** Representation and manipulation of method / function / class parameters. *)

(** Types *)

type simple_name = {
  sn_name : string;
  sn_type : Types.type_expr;
  mutable sn_text : Odoc_types.text option;
}
(** Representation of a simple parameter name *)

type param_info =
    Simple_name of simple_name
  | Tuple of param_info list * Types.type_expr
(** Representation of parameter names. We need it to represent parameter names in tuples.
   The value [Tuple ([], t)] stands for an anonymous parameter.*)

type parameter = param_info
(** A parameter is just a param_info.*)

(** Functions *)

val complete_name : parameter -> string
(** access to the name as a string. For tuples, parentheses and commas are added. *)

val typ : parameter -> Types.type_expr
(** access to the complete type *)

val update_parameter_text :
  (string -> Odoc_types.text option) -> parameter -> unit
(** Update the text of a parameter using a function returning
   the optional text associated to a parameter name.*)

val desc_by_name : parameter -> string -> Odoc_types.text option
(** access to the description of a specific name.
   @raise Not_found if no description is associated to the given name. *)

val names : parameter -> string list
(** access to the list of names ; only one for a simple parameter, or
   a list for tuples. *)

val type_by_name : parameter -> string -> Types.type_expr
(** access to the type of a specific name.
   @raise Not_found if no type is associated to the given name. *)

val desc_from_info_opt :
  Odoc_types.info option -> string -> Odoc_types.text option
(** access to the optional description of a parameter name from an optional info structure.*)
