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

(** Text generation. *)

class virtual to_text :
  object
    val mutable tag_functions :
      (string * (Odoc_info.text -> Odoc_info.text)) list
    method virtual label : ?no_:bool -> string -> string
    method normal_class_params :
      Odoc_info.Name.t -> Odoc_info.Class.t_class -> string
    method normal_class_type : Odoc_info.Name.t -> Types.class_type -> string
    method normal_class_type_param_list :
      Odoc_info.Name.t -> Types.type_expr list -> string
    method normal_cstr_args :
      ?par:bool ->
      Odoc_info.Name.t -> Odoc_info.Type.constructor_args -> string
    method normal_module_type :
      ?code:string -> Odoc_info.Name.t -> Types.module_type -> string
    method normal_type : Odoc_info.Name.t -> Types.type_expr -> string
    method normal_type_list :
      ?par:bool ->
      Odoc_info.Name.t -> string -> Types.type_expr list -> string
    method relative_idents : Odoc_info.Name.t -> string -> string
    method relative_module_idents : Odoc_info.Name.t -> string -> string
    method text_of_alerts :
      Odoc_info.alert list -> Odoc_info.text_element list
    method text_of_attribute :
      Odoc_info.Value.t_attribute -> Odoc_info.text_element list
    method text_of_author_list : string list -> Odoc_info.text_element list
    method text_of_before :
      (string * Odoc_info.text) list -> Odoc_info.text_element list
    method text_of_class_kind :
      Odoc_info.Name.t ->
      Odoc_info.Class.class_kind -> Odoc_info.text_element list
    method text_of_class_params :
      Odoc_info.Name.t -> Odoc_info.Class.t_class -> Odoc_types.text
    method text_of_class_type_kind :
      Odoc_info.Name.t ->
      Odoc_info.Class.class_type_kind -> Odoc_info.text_element list
    method text_of_class_type_param_expr_list :
      Odoc_info.Name.t -> Types.type_expr list -> Odoc_info.text_element list
    method text_of_custom : (string * Odoc_info.text) list -> Odoc_info.text
    method text_of_exception :
      Odoc_info.Exception.t_exception -> Odoc_info.text_element list
    method text_of_info :
      ?block:bool -> Odoc_info.info option -> Odoc_info.text_element list
    method text_of_method :
      Odoc_info.Value.t_method -> Odoc_info.text_element list
    method text_of_module_kind :
      ?with_def_syntax:bool ->
      Odoc_info.Module.module_kind -> Odoc_info.text_element list
    method text_of_module_parameter_list :
      (Odoc_info.Module.module_parameter * Odoc_info.text_element list option)
      list -> Odoc_info.text_element list
    method text_of_module_type :
      Types.module_type -> Odoc_info.text_element list
    method text_of_module_type_kind :
      ?with_def_syntax:bool ->
      Odoc_info.Module.module_type_kind -> Odoc_info.text_element list
    method text_of_parameter_description :
      Odoc_info.Parameter.parameter -> Odoc_info.text
    method text_of_parameter_list :
      Odoc_info.Name.t ->
      Odoc_info.Parameter.parameter list -> Odoc_info.text_element list
    method text_of_raised_exceptions :
      Odoc_info.raised_exception list -> Odoc_info.text_element list
    method text_of_return_opt :
      Odoc_info.text option -> Odoc_info.text_element list
    method text_of_see : Odoc_info.see -> Odoc_info.text
    method text_of_sees : Odoc_info.see list -> Odoc_info.text_element list
    method text_of_short_type_expr :
      Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element list
    method text_of_since_opt : string option -> Odoc_info.text_element list
    method text_of_type_expr :
      Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element list
    method text_of_type_expr_list :
      Odoc_info.Name.t ->
      string -> Types.type_expr list -> Odoc_info.text_element list
    method text_of_value :
      Odoc_info.Value.t_value -> Odoc_info.text_element list
    method text_of_version_opt : string option -> Odoc_info.text_element list
  end
