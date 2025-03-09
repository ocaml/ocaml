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

(** Generation of html documentation. *)

module String = Misc.Stdlib.String

val with_parameter_list : bool ref

val css_style : string option ref

val index_only : bool ref

val colorize_code : bool ref

val html_short_functors : bool ref

val charset : string ref

val show_navbar : bool ref

module Naming :
  sig
    val mark_module : string
    val mark_module_type : string
    val mark_type : string
    val mark_type_elt : string
    val mark_function : string
    val mark_extension : string
    val mark_exception : string
    val mark_value : string
    val mark_attribute : string
    val mark_method : string
    val code_prefix : string
    val type_prefix : string
    val html_files : string -> string * string
    val target : string -> string -> string
    val complete_target : string -> Odoc_info.Name.t -> string
    val module_target : Odoc_info.Module.t_module -> string
    val module_type_target : Odoc_info.Module.t_module_type -> string
    val type_target : Odoc_info.Type.t_type -> string
    val const_target :
      Odoc_info.Type.t_type -> Odoc_info.Type.variant_constructor -> string
    val recfield_target :
      Odoc_info.Type.t_type -> Odoc_info.Type.record_field -> string
    val inline_recfield_target :
      string -> string -> Odoc_info.Type.record_field -> string
    val objfield_target :
      Odoc_info.Type.t_type -> Odoc_info.Type.object_field -> string
    val complete_type_target : Odoc_info.Type.t_type -> string
    val complete_recfield_target : Odoc_info.Name.t -> string
    val complete_const_target : Odoc_info.Name.t -> string
    val extension_target :
      Odoc_info.Extension.t_extension_constructor -> string
    val complete_extension_target :
      Odoc_info.Extension.t_extension_constructor -> string
    val exception_target : Odoc_info.Exception.t_exception -> string
    val complete_exception_target : Odoc_info.Exception.t_exception -> string
    val value_target : Odoc_info.Value.t_value -> string
    val subst_infix_symbols : string -> string
    val complete_value_target : Odoc_info.Value.t_value -> string
    val file_code_value_complete_target : Odoc_info.Value.t_value -> string
    val attribute_target : Odoc_info.Value.t_attribute -> string
    val complete_attribute_target : Odoc_info.Value.t_attribute -> string
    val file_code_attribute_complete_target :
      Odoc_info.Value.t_attribute -> string
    val method_target : Odoc_info.Value.t_method -> string
    val complete_method_target : Odoc_info.Value.t_method -> string
    val file_code_method_complete_target : Odoc_info.Value.t_method -> string
    val label_target : string -> string
    val complete_label_target : Odoc_info.Name.t -> string
    val file_type_module_complete_target : string -> string
    val file_code_module_complete_target : string -> string
    val file_type_class_complete_target : string -> string
  end

module Generator :
  sig
    class html :
      object
        val mutable default_style_options : string list
        val mutable doctype : string
        val mutable header :
          Buffer.t ->
          ?nav:(Odoc_info.Name.t option * Odoc_info.Name.t option *
                Odoc_info.Name.t)
               option ->
          ?comments:Odoc_info.text list -> string -> unit
        val mutable known_classes_names : String.Set.t
        val mutable known_modules_names : String.Set.t
        val mutable known_types_names : String.Set.t
        val mutable list_attributes : Odoc_info.Value.t_attribute list
        val mutable list_class_types : Odoc_info.Class.t_class_type list
        val mutable list_classes : Odoc_info.Class.t_class list
        val mutable list_exceptions : Odoc_info.Exception.t_exception list
        val mutable list_extensions :
          Odoc_info.Extension.t_extension_constructor list
        val mutable list_methods : Odoc_info.Value.t_method list
        val mutable list_module_types : Odoc_info.Module.t_module_type list
        val mutable list_modules : Odoc_info.Module.t_module list
        val mutable list_types : Odoc_info.Type.t_type list
        val mutable list_values : Odoc_info.Value.t_value list
        val mutable style : string
        val mutable style_file : string
        val mutable tag_functions :
          (string * (Odoc_info.text -> string)) list
        method character_encoding : Buffer.t -> unit
        method constructor : string -> string
        method create_fully_qualified_idents_links :
          Odoc_info.Name.t -> Odoc_info.Name.t -> string
        method create_fully_qualified_module_idents_links :
          Odoc_info.Name.t -> Odoc_info.Name.t -> string
        method create_title_label :
          int * string option * Odoc_info.text -> string
        method escape : string -> string
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_attributes_index :
          Odoc_info.Module.t_module list -> unit
        method generate_class_inheritance_info :
          Buffer.t -> Odoc_info.Class.t_class -> unit
        method generate_class_type_inheritance_info :
          Buffer.t -> Odoc_info.Class.t_class_type -> unit
        method generate_class_types_index :
          Odoc_info.Module.t_module list -> unit
        method generate_classes_index :
          Odoc_info.Module.t_module list -> unit
        method generate_elements :
          ('a option -> 'a option -> 'a -> unit) -> 'a list -> unit
        method generate_elements_index :
          ?strip_libname:bool ->
          'a list ->
          ('a -> Odoc_info.Name.t) ->
          ('a -> Odoc_info.info option) ->
          ('a -> string) -> string -> string -> unit
        method generate_exceptions_index :
          Odoc_info.Module.t_module list -> unit
        method generate_extensions_index :
          Odoc_info.Module.t_module list -> unit
        method generate_for_class :
          Odoc_info.Class.t_class option ->
          Odoc_info.Class.t_class option -> Odoc_info.Class.t_class -> unit
        method generate_for_class_type :
          Odoc_info.Class.t_class_type option ->
          Odoc_info.Class.t_class_type option ->
          Odoc_info.Class.t_class_type -> unit
        method generate_for_module :
          Odoc_info.Module.t_module option ->
          Odoc_info.Module.t_module option ->
          Odoc_info.Module.t_module -> unit
        method generate_for_module_type :
          Odoc_info.Module.t_module_type option ->
          Odoc_info.Module.t_module_type option ->
          Odoc_info.Module.t_module_type -> unit
        method generate_index : Odoc_info.Module.t_module list -> unit
        method generate_inheritance_info :
          Buffer.t -> Odoc_info.Class.inherited_class list -> unit
        method generate_methods_index :
          Odoc_info.Module.t_module list -> unit
        method generate_module_types_index :
          Odoc_info.Module.t_module list -> unit
        method generate_modules_index :
          Odoc_info.Module.t_module list -> unit
        method generate_types_index : Odoc_info.Module.t_module list -> unit
        method generate_values_index : Odoc_info.Module.t_module list -> unit
        method html_of_Block : Buffer.t -> Odoc_info.text -> unit
        method html_of_Bold : Buffer.t -> Odoc_info.text -> unit
        method html_of_Center : Buffer.t -> Odoc_info.text -> unit
        method html_of_Code : Buffer.t -> string -> unit
        method html_of_CodePre : Buffer.t -> string -> unit
        method html_of_Emphasize : Buffer.t -> Odoc_info.text -> unit
        method html_of_Enum : Buffer.t -> Odoc_info.text list -> unit
        method html_of_Index_list : Buffer.t -> unit
        method html_of_Italic : Buffer.t -> Odoc_info.text -> unit
        method html_of_Latex : Buffer.t -> string -> unit
        method html_of_Left : Buffer.t -> Odoc_info.text -> unit
        method html_of_Link : Buffer.t -> string -> Odoc_info.text -> unit
        method html_of_List : Buffer.t -> Odoc_info.text list -> unit
        method html_of_Module_list :
          Buffer.t -> Odoc_info.Name.t list -> unit
        method html_of_Newline : Buffer.t -> unit
        method html_of_Raw : Buffer.t -> string -> unit
        method html_of_Ref :
          Buffer.t ->
          Odoc_info.Name.t ->
          Odoc_info.ref_kind option -> Odoc_info.text option -> unit
        method html_of_Right : Buffer.t -> Odoc_info.text -> unit
        method html_of_Subscript : Buffer.t -> Odoc_info.text -> unit
        method html_of_Superscript : Buffer.t -> Odoc_info.text -> unit
        method html_of_Target :
          Buffer.t -> target:string -> code:string -> unit
        method html_of_Title :
          Buffer.t -> int -> string option -> Odoc_info.text -> unit
        method html_of_Verbatim : Buffer.t -> string -> unit
        method html_of_alerts : Buffer.t -> Odoc_info.alert list -> unit
        method html_of_attribute :
          Buffer.t -> Odoc_info.Value.t_attribute -> unit
        method html_of_author_list : Buffer.t -> string list -> unit
        method html_of_before :
          Buffer.t -> (string * Odoc_info.text) list -> unit
        method html_of_class :
          Buffer.t ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Class.t_class -> unit
        method html_of_class_comment : Buffer.t -> Odoc_info.text -> unit
        method html_of_class_element :
          Buffer.t -> Odoc_info.Class.class_element -> unit
        method html_of_class_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?cl:Odoc_info.Class.t_class -> Odoc_info.Class.class_kind -> unit
        method html_of_class_parameter_list :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Class.t_class -> unit
        method html_of_class_type :
          Buffer.t ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Class.t_class_type -> unit
        method html_of_class_type_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?ct:Odoc_info.Class.t_class_type ->
          Odoc_info.Class.class_type_kind -> unit
        method html_of_class_type_param_expr_list :
          Buffer.t -> Odoc_info.Name.t -> Types.type_expr list -> unit
        method html_of_code : Buffer.t -> ?with_pre:bool -> string -> unit
        method html_of_cstr_args :
          ?par:bool ->
          Buffer.t ->
          Odoc_info.Name.t ->
          Odoc_info.Name.t ->
          string -> Odoc_info.Type.constructor_args -> unit
        method html_of_custom :
          Buffer.t -> (string * Odoc_info.text) list -> unit
        method html_of_custom_text :
          Buffer.t -> string -> Odoc_info.text -> unit
        method html_of_dag :
          (Odoc_info.Name.t * Odoc_info.Class.cct option) Odoc_dag2html.dag ->
          string
        method html_of_described_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_parameter.parameter list -> unit
        method html_of_exception :
          Buffer.t -> Odoc_info.Exception.t_exception -> unit
        method html_of_included_module :
          Buffer.t -> Odoc_info.Module.included_module -> unit
        method html_of_info :
          ?cls:string ->
          ?indent:bool -> Buffer.t -> Odoc_types.info option -> unit
        method html_of_info_first_sentence :
          Buffer.t -> Odoc_info.info option -> unit
        method html_of_method : Buffer.t -> Odoc_info.Value.t_method -> unit
        method html_of_modtype :
          Buffer.t ->
          ?info:bool ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Module.t_module_type -> unit
        method html_of_module :
          Buffer.t ->
          ?info:bool ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Module.t_module -> unit
        method html_of_module_comment : Buffer.t -> Odoc_info.text -> unit
        method html_of_module_element :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.module_element -> unit
        method html_of_module_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?modu:Odoc_info.Module.t_module ->
          Odoc_info.Module.module_kind -> unit
        method html_of_module_parameter :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.module_parameter -> unit
        method html_of_module_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t ->
          (Odoc_info.Module.module_parameter * Odoc_info.text option) list ->
          unit
        method html_of_module_parameter_type :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.module_parameter -> unit
        method html_of_module_type :
          Buffer.t ->
          ?code:string -> Odoc_info.Name.t -> Types.module_type -> unit
        method html_of_module_type_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?modu:Odoc_info.Module.t_module ->
          ?mt:Odoc_info.Module.t_module_type ->
          Odoc_info.Module.module_type_kind -> unit
        method html_of_parameter_description :
          Buffer.t -> Odoc_info.Parameter.parameter -> unit
        method html_of_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_parameter.parameter list -> unit
        method html_of_raised_exceptions :
          Buffer.t -> (string * Odoc_info.text) list -> unit
        method html_of_record :
          father:Odoc_info.Name.t ->
          close_env:string ->
          (Odoc_info.Type.record_field -> string) ->
          Buffer.t -> Odoc_info.Type.record_field list -> unit
        method html_of_return_opt : Buffer.t -> Odoc_info.text option -> unit
        method html_of_see :
          Buffer.t -> Odoc_info.see_ref * Odoc_info.text -> unit
        method html_of_sees :
          Buffer.t -> (Odoc_info.see_ref * Odoc_info.text) list -> unit
        method html_of_since_opt : Buffer.t -> string option -> unit
        method html_of_text :
          ?with_p:bool -> Buffer.t -> Odoc_info.text -> unit
        method html_of_text_element :
          Buffer.t -> Odoc_info.text_element -> unit
        method html_of_text_with_p : Buffer.t -> Odoc_info.text -> unit
        method html_of_type : Buffer.t -> Odoc_info.Type.t_type -> unit
        method html_of_type_expr :
          Buffer.t -> Odoc_info.Name.t -> Types.type_expr -> unit
        method html_of_type_expr_param_list :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Type.t_type -> unit
        method html_of_type_extension :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Extension.t_type_extension -> unit
        method html_of_value : Buffer.t -> Odoc_info.Value.t_value -> unit
        method html_of_version_opt : Buffer.t -> string option -> unit
        method html_sections_links : Buffer.t -> Odoc_info.text list -> unit
        method index : string
        method index_attributes : string
        method index_class_types : string
        method index_classes : string
        method index_exceptions : string
        method index_extensions : string
        method index_methods : string
        method index_module_types : string
        method index_modules : string
        method index_prefix : string
        method index_types : string
        method index_values : string
        method init_style : unit
        method inner_title : Odoc_info.Name.t -> string
        method keep_alpha_num : string -> string
        method keyword : string -> string
        method label_of_text : Odoc_info.text -> string
        method list_attributes : Odoc_info.Value.t_attribute list
        method list_class_types : Odoc_info.Class.t_class_type list
        method list_classes : Odoc_info.Class.t_class list
        method list_exceptions : Odoc_info.Exception.t_exception list
        method list_extensions :
          Odoc_info.Extension.t_extension_constructor list
        method list_methods : Odoc_info.Value.t_method list
        method list_module_types : Odoc_info.Module.t_module_type list
        method list_modules : Odoc_info.Module.t_module list
        method list_types : Odoc_info.Type.t_type list
        method list_values : Odoc_info.Value.t_value list
        method meta : Buffer.t -> unit
        method output_class_type :
          Odoc_info.Name.t -> string -> Types.class_type -> unit
        method private output_code :
          ?with_pre:bool -> Odoc_info.Name.t -> string -> string -> unit
        method output_module_type :
          Odoc_info.Name.t -> string -> Types.module_type -> unit
        method prepare_header : Odoc_info.Module.t_module list -> unit
        method print_header :
          Buffer.t ->
          ?nav:(Odoc_info.Name.t option * Odoc_info.Name.t option *
                Odoc_info.Name.t)
               option ->
          ?comments:Odoc_info.text list -> string -> unit
        method print_navbar :
          Buffer.t ->
          Odoc_info.Name.t option ->
          Odoc_info.Name.t option -> Odoc_info.Name.t -> unit
        method title : string
      end
  end

module type Html_generator =
  sig
    class html :
      object
        val mutable default_style_options : string list
        val mutable doctype : string
        val mutable header :
          Buffer.t ->
          ?nav:(Odoc_info.Name.t option * Odoc_info.Name.t option *
                Odoc_info.Name.t)
               option ->
          ?comments:Odoc_info.text list -> string -> unit
        val mutable known_classes_names : String.Set.t
        val mutable known_modules_names : String.Set.t
        val mutable known_types_names : String.Set.t
        val mutable list_attributes : Odoc_info.Value.t_attribute list
        val mutable list_class_types : Odoc_info.Class.t_class_type list
        val mutable list_classes : Odoc_info.Class.t_class list
        val mutable list_exceptions : Odoc_info.Exception.t_exception list
        val mutable list_extensions :
          Odoc_info.Extension.t_extension_constructor list
        val mutable list_methods : Odoc_info.Value.t_method list
        val mutable list_module_types : Odoc_info.Module.t_module_type list
        val mutable list_modules : Odoc_info.Module.t_module list
        val mutable list_types : Odoc_info.Type.t_type list
        val mutable list_values : Odoc_info.Value.t_value list
        val mutable style : string
        val mutable style_file : string
        val mutable tag_functions :
          (string * (Odoc_info.text -> string)) list
        method character_encoding : Buffer.t -> unit
        method constructor : string -> string
        method create_fully_qualified_idents_links :
          Odoc_info.Name.t -> Odoc_info.Name.t -> string
        method create_fully_qualified_module_idents_links :
          Odoc_info.Name.t -> Odoc_info.Name.t -> string
        method create_title_label :
          int * string option * Odoc_info.text -> string
        method escape : string -> string
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_attributes_index :
          Odoc_info.Module.t_module list -> unit
        method generate_class_inheritance_info :
          Buffer.t -> Odoc_info.Class.t_class -> unit
        method generate_class_type_inheritance_info :
          Buffer.t -> Odoc_info.Class.t_class_type -> unit
        method generate_class_types_index :
          Odoc_info.Module.t_module list -> unit
        method generate_classes_index :
          Odoc_info.Module.t_module list -> unit
        method generate_elements :
          ('a option -> 'a option -> 'a -> unit) -> 'a list -> unit
        method generate_elements_index :
          ?strip_libname:bool ->
          'a list ->
          ('a -> Odoc_info.Name.t) ->
          ('a -> Odoc_info.info option) ->
          ('a -> string) -> string -> string -> unit
        method generate_exceptions_index :
          Odoc_info.Module.t_module list -> unit
        method generate_extensions_index :
          Odoc_info.Module.t_module list -> unit
        method generate_for_class :
          Odoc_info.Class.t_class option ->
          Odoc_info.Class.t_class option -> Odoc_info.Class.t_class -> unit
        method generate_for_class_type :
          Odoc_info.Class.t_class_type option ->
          Odoc_info.Class.t_class_type option ->
          Odoc_info.Class.t_class_type -> unit
        method generate_for_module :
          Odoc_info.Module.t_module option ->
          Odoc_info.Module.t_module option ->
          Odoc_info.Module.t_module -> unit
        method generate_for_module_type :
          Odoc_info.Module.t_module_type option ->
          Odoc_info.Module.t_module_type option ->
          Odoc_info.Module.t_module_type -> unit
        method generate_index : Odoc_info.Module.t_module list -> unit
        method generate_inheritance_info :
          Buffer.t -> Odoc_info.Class.inherited_class list -> unit
        method generate_methods_index :
          Odoc_info.Module.t_module list -> unit
        method generate_module_types_index :
          Odoc_info.Module.t_module list -> unit
        method generate_modules_index :
          Odoc_info.Module.t_module list -> unit
        method generate_types_index : Odoc_info.Module.t_module list -> unit
        method generate_values_index : Odoc_info.Module.t_module list -> unit
        method html_of_Block : Buffer.t -> Odoc_info.text -> unit
        method html_of_Bold : Buffer.t -> Odoc_info.text -> unit
        method html_of_Center : Buffer.t -> Odoc_info.text -> unit
        method html_of_Code : Buffer.t -> string -> unit
        method html_of_CodePre : Buffer.t -> string -> unit
        method html_of_Emphasize : Buffer.t -> Odoc_info.text -> unit
        method html_of_Enum : Buffer.t -> Odoc_info.text list -> unit
        method html_of_Index_list : Buffer.t -> unit
        method html_of_Italic : Buffer.t -> Odoc_info.text -> unit
        method html_of_Latex : Buffer.t -> string -> unit
        method html_of_Left : Buffer.t -> Odoc_info.text -> unit
        method html_of_Link : Buffer.t -> string -> Odoc_info.text -> unit
        method html_of_List : Buffer.t -> Odoc_info.text list -> unit
        method html_of_Module_list :
          Buffer.t -> Odoc_info.Name.t list -> unit
        method html_of_Newline : Buffer.t -> unit
        method html_of_Raw : Buffer.t -> string -> unit
        method html_of_Ref :
          Buffer.t ->
          Odoc_info.Name.t ->
          Odoc_info.ref_kind option -> Odoc_info.text option -> unit
        method html_of_Right : Buffer.t -> Odoc_info.text -> unit
        method html_of_Subscript : Buffer.t -> Odoc_info.text -> unit
        method html_of_Superscript : Buffer.t -> Odoc_info.text -> unit
        method html_of_Target :
          Buffer.t -> target:string -> code:string -> unit
        method html_of_Title :
          Buffer.t -> int -> string option -> Odoc_info.text -> unit
        method html_of_Verbatim : Buffer.t -> string -> unit
        method html_of_alerts : Buffer.t -> Odoc_info.alert list -> unit
        method html_of_attribute :
          Buffer.t -> Odoc_info.Value.t_attribute -> unit
        method html_of_author_list : Buffer.t -> string list -> unit
        method html_of_before :
          Buffer.t -> (string * Odoc_info.text) list -> unit
        method html_of_class :
          Buffer.t ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Class.t_class -> unit
        method html_of_class_comment : Buffer.t -> Odoc_info.text -> unit
        method html_of_class_element :
          Buffer.t -> Odoc_info.Class.class_element -> unit
        method html_of_class_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?cl:Odoc_info.Class.t_class -> Odoc_info.Class.class_kind -> unit
        method html_of_class_parameter_list :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Class.t_class -> unit
        method html_of_class_type :
          Buffer.t ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Class.t_class_type -> unit
        method html_of_class_type_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?ct:Odoc_info.Class.t_class_type ->
          Odoc_info.Class.class_type_kind -> unit
        method html_of_class_type_param_expr_list :
          Buffer.t -> Odoc_info.Name.t -> Types.type_expr list -> unit
        method html_of_code : Buffer.t -> ?with_pre:bool -> string -> unit
        method html_of_cstr_args :
          ?par:bool ->
          Buffer.t ->
          Odoc_info.Name.t ->
          Odoc_info.Name.t ->
          string -> Odoc_info.Type.constructor_args -> unit
        method html_of_custom :
          Buffer.t -> (string * Odoc_info.text) list -> unit
        method html_of_custom_text :
          Buffer.t -> string -> Odoc_info.text -> unit
        method html_of_dag :
          (Odoc_info.Name.t * Odoc_info.Class.cct option) Odoc_dag2html.dag ->
          string
        method html_of_described_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_parameter.parameter list -> unit
        method html_of_exception :
          Buffer.t -> Odoc_info.Exception.t_exception -> unit
        method html_of_included_module :
          Buffer.t -> Odoc_info.Module.included_module -> unit
        method html_of_info :
          ?cls:string ->
          ?indent:bool -> Buffer.t -> Odoc_types.info option -> unit
        method html_of_info_first_sentence :
          Buffer.t -> Odoc_info.info option -> unit
        method html_of_method : Buffer.t -> Odoc_info.Value.t_method -> unit
        method html_of_modtype :
          Buffer.t ->
          ?info:bool ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Module.t_module_type -> unit
        method html_of_module :
          Buffer.t ->
          ?info:bool ->
          ?complete:bool ->
          ?with_link:bool -> Odoc_info.Module.t_module -> unit
        method html_of_module_comment : Buffer.t -> Odoc_info.text -> unit
        method html_of_module_element :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.module_element -> unit
        method html_of_module_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?modu:Odoc_info.Module.t_module ->
          Odoc_info.Module.module_kind -> unit
        method html_of_module_parameter :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.module_parameter -> unit
        method html_of_module_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t ->
          (Odoc_info.Module.module_parameter * Odoc_info.text option) list ->
          unit
        method html_of_module_parameter_type :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Module.module_parameter -> unit
        method html_of_module_type :
          Buffer.t ->
          ?code:string -> Odoc_info.Name.t -> Types.module_type -> unit
        method html_of_module_type_kind :
          Buffer.t ->
          Odoc_info.Name.t ->
          ?modu:Odoc_info.Module.t_module ->
          ?mt:Odoc_info.Module.t_module_type ->
          Odoc_info.Module.module_type_kind -> unit
        method html_of_parameter_description :
          Buffer.t -> Odoc_info.Parameter.parameter -> unit
        method html_of_parameter_list :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_parameter.parameter list -> unit
        method html_of_raised_exceptions :
          Buffer.t -> (string * Odoc_info.text) list -> unit
        method html_of_record :
          father:Odoc_info.Name.t ->
          close_env:string ->
          (Odoc_info.Type.record_field -> string) ->
          Buffer.t -> Odoc_info.Type.record_field list -> unit
        method html_of_return_opt : Buffer.t -> Odoc_info.text option -> unit
        method html_of_see :
          Buffer.t -> Odoc_info.see_ref * Odoc_info.text -> unit
        method html_of_sees :
          Buffer.t -> (Odoc_info.see_ref * Odoc_info.text) list -> unit
        method html_of_since_opt : Buffer.t -> string option -> unit
        method html_of_text :
          ?with_p:bool -> Buffer.t -> Odoc_info.text -> unit
        method html_of_text_element :
          Buffer.t -> Odoc_info.text_element -> unit
        method html_of_text_with_p : Buffer.t -> Odoc_info.text -> unit
        method html_of_type : Buffer.t -> Odoc_info.Type.t_type -> unit
        method html_of_type_expr :
          Buffer.t -> Odoc_info.Name.t -> Types.type_expr -> unit
        method html_of_type_expr_param_list :
          Buffer.t -> Odoc_info.Name.t -> Odoc_info.Type.t_type -> unit
        method html_of_type_extension :
          Buffer.t ->
          Odoc_info.Name.t -> Odoc_info.Extension.t_type_extension -> unit
        method html_of_value : Buffer.t -> Odoc_info.Value.t_value -> unit
        method html_of_version_opt : Buffer.t -> string option -> unit
        method html_sections_links : Buffer.t -> Odoc_info.text list -> unit
        method index : string
        method index_attributes : string
        method index_class_types : string
        method index_classes : string
        method index_exceptions : string
        method index_extensions : string
        method index_methods : string
        method index_module_types : string
        method index_modules : string
        method index_prefix : string
        method index_types : string
        method index_values : string
        method init_style : unit
        method inner_title : Odoc_info.Name.t -> string
        method keep_alpha_num : string -> string
        method keyword : string -> string
        method label_of_text : Odoc_info.text -> string
        method list_attributes : Odoc_info.Value.t_attribute list
        method list_class_types : Odoc_info.Class.t_class_type list
        method list_classes : Odoc_info.Class.t_class list
        method list_exceptions : Odoc_info.Exception.t_exception list
        method list_extensions :
          Odoc_info.Extension.t_extension_constructor list
        method list_methods : Odoc_info.Value.t_method list
        method list_module_types : Odoc_info.Module.t_module_type list
        method list_modules : Odoc_info.Module.t_module list
        method list_types : Odoc_info.Type.t_type list
        method list_values : Odoc_info.Value.t_value list
        method meta : Buffer.t -> unit
        method output_class_type :
          Odoc_info.Name.t -> string -> Types.class_type -> unit
        method private output_code :
          ?with_pre:bool -> Odoc_info.Name.t -> string -> string -> unit
        method output_module_type :
          Odoc_info.Name.t -> string -> Types.module_type -> unit
        method prepare_header : Odoc_info.Module.t_module list -> unit
        method print_header :
          Buffer.t ->
          ?nav:(Odoc_info.Name.t option * Odoc_info.Name.t option *
                Odoc_info.Name.t)
               option ->
          ?comments:Odoc_info.text list -> string -> unit
        method print_navbar :
          Buffer.t ->
          Odoc_info.Name.t option ->
          Odoc_info.Name.t option -> Odoc_info.Name.t -> unit
        method title : string
      end
  end
