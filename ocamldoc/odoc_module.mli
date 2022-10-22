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

(** Representation and manipulation of modules and module types. *)

module String = Misc.Stdlib.String

module Name = Odoc_name

(** {1 Types} *)

type module_element =
    Element_module of t_module
  | Element_module_type of t_module_type
  | Element_included_module of included_module
  | Element_class of Odoc_class.t_class
  | Element_class_type of Odoc_class.t_class_type
  | Element_value of Odoc_value.t_value
  | Element_type_extension of Odoc_extension.t_type_extension
  | Element_exception of Odoc_exception.t_exception
  | Element_type of Odoc_type.t_type
  | Element_module_comment of Odoc_types.text
(** To keep the order of elements in a module. *)
and mmt = Mod of t_module | Modtype of t_module_type
and included_module = {
  im_name : Name.t;
  mutable im_module : mmt option;
  mutable im_info : Odoc_types.info option;
}
and module_alias = { ma_name : Name.t; mutable ma_module : mmt option; }
and module_parameter = {
  mp_name : string;
  mp_type : Types.module_type option;
  mp_type_code : string;
  mp_kind : module_type_kind;
}
and module_kind =
    Module_struct of module_element list
  | Module_alias of module_alias
  | Module_functor of module_parameter * module_kind
  | Module_apply of module_kind * module_kind
  | Module_with of module_type_kind * string
  | Module_constraint of module_kind * module_type_kind
  | Module_typeof of string
  | Module_unpack of string * module_type_alias
and t_module = {
  m_name : Name.t;
  mutable m_type : Types.module_type;
  mutable m_info : Odoc_types.info option;
  m_is_interface : bool;
  m_file : string;
  mutable m_kind : module_kind;
  mutable m_loc : Odoc_types.location;
  mutable m_top_deps : Name.t list;
  mutable m_code : string option;
  mutable m_code_intf : string option;
  m_text_only : bool;
}
and module_type_alias = {
  mta_name : Name.t;
  mutable mta_module : t_module_type option;
}
and module_type_kind =
    Module_type_struct of module_element list
  | Module_type_functor of module_parameter * module_type_kind
  | Module_type_alias of module_type_alias
  | Module_type_with of module_type_kind * string
  | Module_type_typeof of string
and t_module_type = {
  mt_name : Name.t;
  mutable mt_info : Odoc_types.info option;
  mutable mt_type : Types.module_type option;
  mt_is_interface : bool;
  mt_file : string;
  mutable mt_kind : module_type_kind option;
  mutable mt_loc : Odoc_types.location;
}

(** {1 Functions} *)

val values : module_element list -> Odoc_value.t_value list
(** Returns the list of values from a list of module_element. *)

val types : module_element list -> Odoc_type.t_type list
(** Returns the list of types from a list of module_element. *)

val type_extensions :
  module_element list -> Odoc_extension.t_type_extension list
(** Returns the list of type extensions from a list of module_element. *)

val exceptions : module_element list -> Odoc_exception.t_exception list
(** Returns the list of exceptions from a list of module_element. *)

val classes : module_element list -> Odoc_class.t_class list
(** Returns the list of classes from a list of module_element. *)

val class_types : module_element list -> Odoc_class.t_class_type list
(** Returns the list of class types from a list of module_element. *)

val modules : module_element list -> t_module list
(** Returns the list of modules from a list of module_element. *)

val mod_types : module_element list -> t_module_type list
(** Returns the list of module types from a list of module_element. *)

val comments : module_element list -> Odoc_types.text list
(** Returns the list of module comment from a list of module_element. *)

val included_modules : module_element list -> included_module list
(** Returns the list of included modules from a list of module_element. *)

val module_type_elements :
  ?trans:bool -> t_module_type -> module_element list
(** Returns the list of elements of a module type.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_elements : ?trans:bool -> t_module -> module_element list
(** Returns the list of elements of a module.
   @param trans indicates if, for aliased modules, we must perform a transitive search.
*)

val module_values : ?trans:bool -> t_module -> Odoc_value.t_value list
(** Returns the list of values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_functions : ?trans:bool -> t_module -> Odoc_value.t_value list
(** Returns the list of functional values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_simple_values : ?trans:bool -> t_module -> Odoc_value.t_value list
(** Returns the list of non-functional values of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_types : ?trans:bool -> t_module -> Odoc_type.t_type list
(** Returns the list of types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_extensions :
  ?trans:bool -> t_module -> Odoc_extension.t_type_extension list
(** Returns the list of type extensions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_exceptions :
  ?trans:bool -> t_module -> Odoc_exception.t_exception list
(** Returns the list of exceptions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_classes : ?trans:bool -> t_module -> Odoc_class.t_class list
(** Returns the list of classes of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_class_types :
  ?trans:bool -> t_module -> Odoc_class.t_class_type list
(** Returns the list of class types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_modules : ?trans:bool -> t_module -> t_module list
(** Returns the list of modules of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_module_types : ?trans:bool -> t_module -> t_module_type list
(** Returns the list of module types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_included_modules : ?trans:bool -> t_module -> included_module list
(** Returns the list of included module of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_comments : ?trans:bool -> t_module -> Odoc_types.text list
(** Returns the list of comments of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_parameters :
  ?trans:bool ->
  t_module_type -> (module_parameter * Odoc_types.text option) list
(** Access to the parameters, for a functor type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_parameters :
  ?trans:bool -> t_module -> (module_parameter * Odoc_types.text option) list
(** Access to the parameters, for a functor.
   @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_all_submodules : ?trans:bool -> t_module -> t_module list
(** access to all submodules and submodules of submodules ... of the given module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_is_functor : t_module_type -> bool
(** The module type is a functor if it is defined as a functor or if it is an alias for a functor. *)

val module_is_functor : t_module -> bool
(** The module is a functor if it is defined as a functor or if it is an alias for a functor. *)

val module_type_values :
  ?trans:bool -> t_module_type -> Odoc_value.t_value list
(** Returns the list of values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_types : ?trans:bool -> t_module_type -> Odoc_type.t_type list
(** Returns the list of types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_type_extensions :
  ?trans:bool -> t_module_type -> Odoc_extension.t_type_extension list
(** Returns the list of type extensions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_exceptions :
  ?trans:bool -> t_module_type -> Odoc_exception.t_exception list
(** Returns the list of exceptions of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_classes :
  ?trans:bool -> t_module_type -> Odoc_class.t_class list
(** Returns the list of classes of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_class_types :
  ?trans:bool -> t_module_type -> Odoc_class.t_class_type list
(** Returns the list of class types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_modules : ?trans:bool -> t_module_type -> t_module list
(** Returns the list of modules of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_module_types :
  ?trans:bool -> t_module_type -> t_module_type list
(** Returns the list of module types of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_included_modules :
  ?trans:bool -> t_module_type -> included_module list
(** Returns the list of included module of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_comments :
  ?trans:bool -> t_module_type -> Odoc_types.text list
(** Returns the list of comments of a module.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_functions :
  ?trans:bool -> t_module_type -> Odoc_value.t_value list
(** Returns the list of functional values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_simple_values :
  ?trans:bool -> t_module_type -> Odoc_value.t_value list
(** Returns the list of non-functional values of a module type.
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

(** {1 Functions for modules and module types} *)

val module_all_classes : ?trans:bool -> t_module -> Odoc_class.t_class list
(** The list of classes defined in this module and all its modules, functors, ....
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)

val module_type_all_classes :
  ?trans:bool -> t_module_type -> Odoc_class.t_class list
(** The list of classes defined in this module type and all its modules, functors, ....
  @param trans indicates if, for aliased modules, we must perform a transitive search.*)
