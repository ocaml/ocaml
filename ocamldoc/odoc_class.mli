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

(** Representation and manipulation of classes and class types.*)

module Name = Odoc_name

(** {1 Types} *)

type class_element =
    Class_attribute of Odoc_value.t_attribute
  | Class_method of Odoc_value.t_method
  | Class_comment of Odoc_types.text
(** To keep the order of elements in a class *)

type cct =
    Cl of t_class
  | Cltype of t_class_type * Types.type_expr list
and inherited_class = {
  ic_name : Name.t;
  mutable ic_class : cct option;
  ic_text : Odoc_types.text option;
} and class_apply = {
  capp_name : Name.t;
  mutable capp_class : t_class option;
  capp_params : Types.type_expr list;
  capp_params_code : string list;
} and class_constr = {
  cco_name : Name.t;
  mutable cco_class : cct option;
  cco_type_parameters : Types.type_expr list;
} and class_kind =
    Class_structure of inherited_class list * class_element list
  | Class_apply of class_apply
  | Class_constr of class_constr
  | Class_constraint of class_kind * class_type_kind
and t_class = {
  cl_name : Name.t;
  mutable cl_info : Odoc_types.info option;
  cl_type : Types.class_type;
  cl_type_parameters : Types.type_expr list;
  cl_virtual : bool;
  mutable cl_kind : class_kind;
  mutable cl_parameters : Odoc_parameter.parameter list;
  mutable cl_loc : Odoc_types.location;
} and class_type_alias = {
  cta_name : Name.t;
  mutable cta_class : cct option;
  cta_type_parameters : Types.type_expr list;
} and class_type_kind =
    Class_signature of inherited_class list * class_element list
  | Class_type of class_type_alias
and t_class_type = {
  clt_name : Name.t;
  mutable clt_info : Odoc_types.info option;
  clt_type : Types.class_type;
  clt_type_parameters : Types.type_expr list;
  clt_virtual : bool;
  mutable clt_kind : class_type_kind;
  mutable clt_loc : Odoc_types.location;
}

(** {1 Functions} *)

val class_parameter_text_by_name :
  t_class -> string -> Odoc_types.text option
(** Returns the text associated to the given parameter label
   in the given class, or None. *)

val class_elements : ?trans:bool -> t_class -> class_element list
(** Returns the list of elements of a t_class. *)

val class_type_elements : ?trans:bool -> t_class_type -> class_element list
(** Returns the list of elements of a t_class_type. *)

val class_attributes : ?trans:bool -> t_class -> Odoc_value.t_attribute list
(** Returns the attributes of a t_class. *)

val class_methods : ?trans:bool -> t_class -> Odoc_value.t_method list
(** Returns the methods of a t_class. *)

val class_comments : ?trans:bool -> t_class -> Odoc_types.text list
(** Returns the comments in a t_class. *)

val class_update_parameters_text : t_class -> unit
(** Update the parameters text of a t_class, according to the cl_info field. *)

val class_type_attributes :
  ?trans:bool -> t_class_type -> Odoc_value.t_attribute list
(** Returns the attributes of a t_class_type. *)

val class_type_methods :
  ?trans:bool -> t_class_type -> Odoc_value.t_method list
(** Returns the methods of a t_class_type. *)

val class_type_comments : ?trans:bool -> t_class_type -> Odoc_types.text list
(** Returns the comments in a t_class_type. *)

val class_type_parameter_text_by_name :
  t_class_type -> string -> Odoc_types.text option
(** Returns the text associated to the given parameter label
   in the given class type, or None. *)
