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

(** Representation and manipulation of values, class attributes and class methods. *)

module Name = Odoc_name

(** Types *)

type t_value = {
  val_name : Name.t;
  mutable val_info : Odoc_types.info option;
  val_type : Types.type_expr;
  val_recursive : bool;
  mutable val_parameters : Odoc_parameter.parameter list;
  mutable val_code : string option;
  mutable val_loc : Odoc_types.location;
}
(** Representation of a value. *)

type t_attribute = {
  att_value : t_value;
  att_mutable : bool;
  att_virtual : bool;
}
(** Representation of a class attribute. *)

type t_method = {
  met_value : t_value;
  met_private : bool;
  met_virtual : bool;
}
(** Representation of a class method. *)

(** Functions *)

val value_parameter_text_by_name : t_value -> string -> Odoc_types.text option
(** Returns the text associated to the given parameter name
   in the given value, or None. *)

val update_value_parameters_text : t_value -> unit
(** Update the parameters text of a t_value, according to the val_info field. *)

val dummy_parameter_list : Types.type_expr -> Odoc_parameter.param_info list
(** Create a list of parameters with dummy names "??" from a type list.
   Used when we want to merge the parameters of a value, from the .ml
   and the .mli file. In the .mli file we don't have parameter names
   so there is nothing to merge. With this dummy list we can merge the
   parameter names from the .ml and the type from the .mli file. *)

val is_function : t_value -> bool
(** Return true if the value is a function, i.e. has a functional type.*)
