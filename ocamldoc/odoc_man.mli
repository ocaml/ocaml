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

(** The man pages generator. *)

val man_suffix : string ref

val man_section : string ref

val man_mini : bool ref

module Generator :
  sig
    class man :
      object
        val mutable tag_functions :
          (string * (Odoc_info.text -> string)) list
        method create_groups :
          bool ->
          Odoc_info.Module.t_module list ->
          Odoc_info.Search.result_element list list
        method escape : string -> string
        method field_comment : Buffer.t -> Odoc_info.info option -> unit
        method file_name : Odoc_info.Name.t -> string
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_for_class : Odoc_info.Class.t_class -> unit
        method generate_for_class_type : Odoc_info.Class.t_class_type -> unit
        method generate_for_group :
          Odoc_info.Search.result_element list -> unit
        method generate_for_module : Odoc_info.Module.t_module -> unit
        method generate_for_module_type :
          Odoc_info.Module.t_module_type -> unit
        method man_of_Target :
          Buffer.t -> target:string -> code:string -> unit
        method man_of_attribute :
          Buffer.t -> Odoc_info.Value.t_attribute -> unit
        method man_of_class : Buffer.t -> Odoc_info.Class.t_class -> unit
        method man_of_class_comment : Buffer.t -> Odoc_info.text -> unit
        method man_of_class_type :
          Buffer.t -> Odoc_info.Class.t_class_type -> unit
        method man_of_class_type_expr :
          Buffer.t -> Odoc_info.Name.t -> Types.class_type -> unit
        method man_of_code : Buffer.t -> string -> unit
        method man_of_const :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Type.variant_constructor -> unit
        method man_of_cstr_args :
          ?par:bool ->
          Buffer.t ->
          Odoc_info.Name.t ->
          string -> Odoc_info.Type.constructor_args -> unit
        method man_of_custom_text :
          Buffer.t -> string -> Odoc_info.text -> unit
        method man_of_exception :
          Buffer.t -> Odoc_info.Exception.t_exception -> unit
        method man_of_included_module :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.included_module -> unit
        method man_of_info :
          ?margin:int -> Buffer.t -> Odoc_types.info option -> unit
        method man_of_method : Buffer.t -> Odoc_info.Value.t_method -> unit
        method man_of_modtype :
          Buffer.t -> Odoc_info.Module.t_module_type -> unit
        method man_of_module : Buffer.t -> Odoc_info.Module.t_module -> unit
        method man_of_module_body :
          Buffer.t -> Odoc_info.Module.t_module -> unit
        method man_of_module_comment : Buffer.t -> Odoc_info.text -> unit
        method man_of_module_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t ->
          (Odoc_info.Module.module_parameter * Odoc_info.text option) list ->
          unit
        method man_of_module_type :
          Buffer.t -> Odoc_info.Name.t -> Types.module_type -> unit
        method man_of_module_type_body :
          Buffer.t -> Odoc_info.Module.t_module_type -> unit
        method man_of_parameter_description :
          Buffer.t -> Odoc_info.Parameter.parameter -> unit
        method man_of_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Parameter.parameter list -> unit
        method man_of_recfield :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Type.record_field -> unit
        method man_of_record :
          Odoc_info.Name.t ->
          Buffer.t -> Odoc_info.Type.record_field list -> unit
        method man_of_text : Buffer.t -> Odoc_info.text -> unit
        method private man_of_text2 : Buffer.t -> Odoc_info.text -> unit
        method man_of_text_element :
          Buffer.t -> Odoc_info.text_element -> unit
        method man_of_type : Buffer.t -> Odoc_info.Type.t_type -> unit
        method man_of_type_expr :
          Buffer.t -> Odoc_info.Name.t -> Types.type_expr -> unit
        method man_of_type_expr_param_list :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Type.t_type -> unit
        method man_of_type_extension :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Extension.t_type_extension -> unit
        method man_of_value : Buffer.t -> Odoc_info.Value.t_value -> unit
        method open_out : string -> out_channel
        method relative_idents :
          Odoc_info.Name.t -> Odoc_info.Name.t -> string
        method remove_newlines : string -> string
        method str_man_of_alerts : Odoc_info.alert list -> string list
        method str_man_of_author_list : string list -> string
        method str_man_of_before : (string * Odoc_info.text) list -> string
        method str_man_of_custom :
          (string * Odoc_info.text) list -> string list
        method str_man_of_raised_exceptions :
          (string * Odoc_info.text) list -> string
        method str_man_of_return_opt : Odoc_info.text option -> string
        method str_man_of_see : Odoc_info.see_ref * Odoc_info.text -> string
        method str_man_of_sees :
          (Odoc_info.see_ref * Odoc_info.text) list -> string
        method str_man_of_since_opt : string option -> string
        method str_man_of_text : Odoc_info.text -> string
        method str_man_of_version_opt : string option -> string
      end
  end

module type Man_generator =
  sig
    class man :
      object
        val mutable tag_functions :
          (string * (Odoc_info.text -> string)) list
        method create_groups :
          bool ->
          Odoc_info.Module.t_module list ->
          Odoc_info.Search.result_element list list
        method escape : string -> string
        method field_comment : Buffer.t -> Odoc_info.info option -> unit
        method file_name : Odoc_info.Name.t -> string
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_for_class : Odoc_info.Class.t_class -> unit
        method generate_for_class_type : Odoc_info.Class.t_class_type -> unit
        method generate_for_group :
          Odoc_info.Search.result_element list -> unit
        method generate_for_module : Odoc_info.Module.t_module -> unit
        method generate_for_module_type :
          Odoc_info.Module.t_module_type -> unit
        method man_of_Target :
          Buffer.t -> target:string -> code:string -> unit
        method man_of_attribute :
          Buffer.t -> Odoc_info.Value.t_attribute -> unit
        method man_of_class : Buffer.t -> Odoc_info.Class.t_class -> unit
        method man_of_class_comment : Buffer.t -> Odoc_info.text -> unit
        method man_of_class_type :
          Buffer.t -> Odoc_info.Class.t_class_type -> unit
        method man_of_class_type_expr :
          Buffer.t -> Odoc_info.Name.t -> Types.class_type -> unit
        method man_of_code : Buffer.t -> string -> unit
        method man_of_const :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Type.variant_constructor -> unit
        method man_of_cstr_args :
          ?par:bool ->
          Buffer.t ->
          Odoc_info.Name.t ->
          string -> Odoc_info.Type.constructor_args -> unit
        method man_of_custom_text :
          Buffer.t -> string -> Odoc_info.text -> unit
        method man_of_exception :
          Buffer.t -> Odoc_info.Exception.t_exception -> unit
        method man_of_included_module :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.included_module -> unit
        method man_of_info :
          ?margin:int -> Buffer.t -> Odoc_types.info option -> unit
        method man_of_method : Buffer.t -> Odoc_info.Value.t_method -> unit
        method man_of_modtype :
          Buffer.t -> Odoc_info.Module.t_module_type -> unit
        method man_of_module : Buffer.t -> Odoc_info.Module.t_module -> unit
        method man_of_module_body :
          Buffer.t -> Odoc_info.Module.t_module -> unit
        method man_of_module_comment : Buffer.t -> Odoc_info.text -> unit
        method man_of_module_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t ->
          (Odoc_info.Module.module_parameter * Odoc_info.text option) list ->
          unit
        method man_of_module_type :
          Buffer.t -> Odoc_info.Name.t -> Types.module_type -> unit
        method man_of_module_type_body :
          Buffer.t -> Odoc_info.Module.t_module_type -> unit
        method man_of_parameter_description :
          Buffer.t -> Odoc_info.Parameter.parameter -> unit
        method man_of_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Parameter.parameter list -> unit
        method man_of_recfield :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Type.record_field -> unit
        method man_of_record :
          Odoc_info.Name.t ->
          Buffer.t -> Odoc_info.Type.record_field list -> unit
        method man_of_text : Buffer.t -> Odoc_info.text -> unit
        method private man_of_text2 : Buffer.t -> Odoc_info.text -> unit
        method man_of_text_element :
          Buffer.t -> Odoc_info.text_element -> unit
        method man_of_type : Buffer.t -> Odoc_info.Type.t_type -> unit
        method man_of_type_expr :
          Buffer.t -> Odoc_info.Name.t -> Types.type_expr -> unit
        method man_of_type_expr_param_list :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Type.t_type -> unit
        method man_of_type_extension :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Extension.t_type_extension -> unit
        method man_of_value : Buffer.t -> Odoc_info.Value.t_value -> unit
        method open_out : string -> out_channel
        method relative_idents :
          Odoc_info.Name.t -> Odoc_info.Name.t -> string
        method remove_newlines : string -> string
        method str_man_of_alerts : Odoc_info.alert list -> string list
        method str_man_of_author_list : string list -> string
        method str_man_of_before : (string * Odoc_info.text) list -> string
        method str_man_of_custom :
          (string * Odoc_info.text) list -> string list
        method str_man_of_raised_exceptions :
          (string * Odoc_info.text) list -> string
        method str_man_of_return_opt : Odoc_info.text option -> string
        method str_man_of_see : Odoc_info.see_ref * Odoc_info.text -> string
        method str_man_of_sees :
          (Odoc_info.see_ref * Odoc_info.text) list -> string
        method str_man_of_since_opt : string option -> string
        method str_man_of_text : Odoc_info.text -> string
        method str_man_of_version_opt : string option -> string
      end
  end
