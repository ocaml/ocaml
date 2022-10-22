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

(** Scanning of modules and elements. *)

class scanner :
  object
    method scan_attribute : Odoc_value.t_attribute -> unit
    method scan_class : Odoc_class.t_class -> unit
    method scan_class_comment : Odoc_types.text -> unit
    method scan_class_elements : Odoc_class.t_class -> unit
    method scan_class_pre : Odoc_class.t_class -> bool
    method scan_class_type : Odoc_class.t_class_type -> unit
    method scan_class_type_comment : Odoc_types.text -> unit
    method scan_class_type_elements : Odoc_class.t_class_type -> unit
    method scan_class_type_pre : Odoc_class.t_class_type -> bool
    method scan_exception : Odoc_exception.t_exception -> unit
    method scan_extension_constructor :
      Odoc_extension.t_extension_constructor -> unit
    method scan_included_module : Odoc_module.included_module -> unit
    method scan_method : Odoc_value.t_method -> unit
    method scan_module : Odoc_module.t_module -> unit
    method scan_module_comment : Odoc_types.text -> unit
    method scan_module_elements : Odoc_module.t_module -> unit
    method scan_module_list : Odoc_module.t_module list -> unit
    method scan_module_pre : Odoc_module.t_module -> bool
    method scan_module_type : Odoc_module.t_module_type -> unit
    method scan_module_type_comment : Odoc_types.text -> unit
    method scan_module_type_elements : Odoc_module.t_module_type -> unit
    method scan_module_type_pre : Odoc_module.t_module_type -> bool
    method scan_type : Odoc_type.t_type -> unit
    method scan_type_const :
      Odoc_type.t_type -> Odoc_type.variant_constructor -> unit
    method scan_type_extension : Odoc_extension.t_type_extension -> unit
    method scan_type_extension_constructors :
      Odoc_extension.t_type_extension -> unit
    method scan_type_extension_pre : Odoc_extension.t_type_extension -> bool
    method scan_type_pre : Odoc_type.t_type -> bool
    method scan_type_recfield :
      Odoc_type.t_type -> Odoc_type.record_field -> unit
    method scan_value : Odoc_value.t_value -> unit
  end
