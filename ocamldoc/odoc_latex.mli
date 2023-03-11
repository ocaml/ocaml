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

(** Generation of LaTeX documentation. *)

val separate_files : bool ref

val latex_titles : (int * string) list ref

val latex_value_prefix : string ref

val latex_type_prefix : string ref

val latex_type_elt_prefix : string ref

val latex_extension_prefix : string ref

val latex_exception_prefix : string ref

val latex_module_prefix : string ref

val latex_module_type_prefix : string ref

val latex_class_prefix : string ref

val latex_class_type_prefix : string ref

val latex_attribute_prefix : string ref

val latex_method_prefix : string ref

module Generator :
  sig
    class latex :
      object
        val subst_strings : (Str.regexp * string) list
        val subst_strings_code : (Str.regexp * string) list
        val subst_strings_simple : (Str.regexp * string) list
        val mutable tag_functions :
          (string * (Odoc_info.text -> Odoc_info.text)) list
        method attribute_label : ?no_:bool -> Odoc_info.Name.t -> string
        method class_label : ?no_:bool -> Odoc_info.Name.t -> string
        method class_type_label : ?no_:bool -> Odoc_info.Name.t -> string
        method const_label : ?no_:bool -> Odoc_info.Name.t -> string
        method entry_comment :
          Format.formatter * (unit -> string) ->
          Odoc_info.info option -> Odoc_info.text_element list
        method escape : string -> string
        method escape_code : string -> string
        method escape_simple : string -> string
        method exception_label : ?no_:bool -> Odoc_info.Name.t -> string
        method extension_label : ?no_:bool -> Odoc_info.Name.t -> string
        method first_and_rest_of_info :
          Odoc_info.info option -> Odoc_info.text * Odoc_info.text
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_class_inheritance_info :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method generate_class_type_inheritance_info :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method generate_for_top_module :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method generate_inheritance_info :
          Format.formatter -> Odoc_info.Class.inherited_class list -> unit
        method generate_style_file : unit
        method label : ?no_:bool -> Odoc_info.Name.t -> string
        method latex_for_class_index :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method latex_for_class_label :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method latex_for_class_type_index :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method latex_for_class_type_label :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method latex_for_module_index :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method latex_for_module_label :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method latex_for_module_type_index :
          Format.formatter -> Odoc_info.Module.t_module_type -> unit
        method latex_for_module_type_label :
          Format.formatter -> Odoc_info.Module.t_module_type -> unit
        method latex_header :
          Format.formatter -> Odoc_info.Module.t_module list -> unit
        method latex_of_Block : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Bold : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Center : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Code : Format.formatter -> string -> unit
        method latex_of_CodePre : Format.formatter -> string -> unit
        method latex_of_Emphasize :
          Format.formatter -> Odoc_info.text -> unit
        method latex_of_Enum :
          Format.formatter -> Odoc_info.text list -> unit
        method latex_of_Italic : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Latex : Format.formatter -> string -> unit
        method latex_of_Left : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Link :
          Format.formatter -> string -> Odoc_info.text -> unit
        method latex_of_List :
          Format.formatter -> Odoc_info.text list -> unit
        method latex_of_Newline : Format.formatter -> unit
        method latex_of_Raw : Format.formatter -> string -> unit
        method latex_of_Ref :
          Format.formatter ->
          Odoc_info.Name.t ->
          Odoc_info.ref_kind option -> Odoc_info.text option -> unit
        method latex_of_Right : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Subscript :
          Format.formatter -> Odoc_info.text -> unit
        method latex_of_Superscript :
          Format.formatter -> Odoc_info.text -> unit
        method latex_of_Target :
          Format.formatter -> target:string -> code:string -> unit
        method latex_of_Title :
          Format.formatter ->
          int -> Odoc_info.Name.t option -> Odoc_info.text -> unit
        method latex_of_Verbatim : Format.formatter -> string -> unit
        method latex_of_attribute :
          Format.formatter -> Odoc_info.Value.t_attribute -> unit
        method latex_of_class :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method latex_of_class_element :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.class_element -> unit
        method latex_of_class_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.class_kind -> unit
        method latex_of_class_parameter_list :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.t_class -> unit
        method latex_of_class_type :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method latex_of_class_type_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.class_type_kind -> unit
        method latex_of_cstr_args :
          Format.formatter * (unit -> string) ->
          Odoc_info.Name.t ->
          Odoc_info.Type.constructor_args * Types.type_expr option ->
          Odoc_info.text_element list
        method latex_of_custom_text :
          Format.formatter -> string -> Odoc_info.text -> unit
        method latex_of_exception :
          Format.formatter -> Odoc_info.Exception.t_exception -> unit
        method latex_of_included_module :
          Format.formatter -> Odoc_info.Module.included_module -> unit
        method latex_of_info :
          Format.formatter -> ?block:bool -> Odoc_info.info option -> unit
        method latex_of_method :
          Format.formatter -> Odoc_info.Value.t_method -> unit
        method latex_of_module :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method latex_of_module_element :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_element -> unit
        method latex_of_module_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_kind -> unit
        method latex_of_module_parameter :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_parameter -> unit
        method latex_of_module_type :
          Format.formatter -> Odoc_info.Module.t_module_type -> unit
        method latex_of_module_type_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_type_kind -> unit
        method latex_of_record :
          Format.formatter * (unit -> string) ->
          Odoc_info.Name.t ->
          Odoc_info.Type.record_field list -> Odoc_info.text_element list
        method latex_of_text : Format.formatter -> Odoc_info.text -> unit
        method latex_of_text_element :
          Format.formatter -> Odoc_info.text_element -> unit
        method latex_of_type :
          Format.formatter -> Odoc_info.Type.t_type -> unit
        method latex_of_type_extension :
          Odoc_info.Name.t ->
          Format.formatter -> Odoc_info.Extension.t_type_extension -> unit
        method latex_of_type_params :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Type.t_type -> unit
        method latex_of_value :
          Format.formatter -> Odoc_info.Value.t_value -> unit
        method make_label : string -> string
        method make_ref : string -> string
        method method_label : ?no_:bool -> Odoc_info.Name.t -> string
        method module_label : ?no_:bool -> Odoc_info.Name.t -> string
        method module_type_label : ?no_:bool -> Odoc_info.Name.t -> string
        method normal_class_params :
          Odoc_info.Name.t -> Odoc_info.Class.t_class -> string
        method normal_class_type :
          Odoc_info.Name.t -> Types.class_type -> string
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
        method recfield_label : ?no_:bool -> Odoc_info.Name.t -> string
        method relative_idents : Odoc_info.Name.t -> string -> string
        method relative_module_idents : Odoc_info.Name.t -> string -> string
        method section_style : int -> string -> string
        method subst : (Str.regexp * string) list -> string -> string
        method text_of_alerts :
          Odoc_info.alert list -> Odoc_info.text_element list
        method text_of_attribute :
          Odoc_info.Value.t_attribute -> Odoc_info.text_element list
        method text_of_author_list :
          string list -> Odoc_info.text_element list
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
          Odoc_info.Name.t ->
          Types.type_expr list -> Odoc_info.text_element list
        method text_of_custom :
          (string * Odoc_info.text) list -> Odoc_info.text
        method text_of_exception :
          Odoc_info.Exception.t_exception -> Odoc_info.text_element list
        method text_of_info :
          ?block:bool -> Odoc_info.info option -> Odoc_info.text
        method text_of_method :
          Odoc_info.Value.t_method -> Odoc_info.text_element list
        method text_of_module_kind :
          ?with_def_syntax:bool ->
          Odoc_info.Module.module_kind -> Odoc_info.text_element list
        method text_of_module_parameter_list :
          (Odoc_info.Module.module_parameter *
           Odoc_info.text_element list option)
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
        method text_of_sees :
          Odoc_info.see list -> Odoc_info.text_element list
        method text_of_short_type_expr :
          Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element list
        method text_of_since_opt :
          string option -> Odoc_info.text_element list
        method text_of_type_expr :
          Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element list
        method text_of_type_expr_list :
          Odoc_info.Name.t ->
          string -> Types.type_expr list -> Odoc_info.text_element list
        method text_of_value :
          Odoc_info.Value.t_value -> Odoc_info.text_element list
        method text_of_version_opt :
          string option -> Odoc_info.text_element list
        method type_label : ?no_:bool -> Odoc_info.Name.t -> string
        method value_label : ?no_:bool -> Odoc_info.Name.t -> string
      end
  end

module type Latex_generator =
  sig
    class latex :
      object
        val subst_strings : (Str.regexp * string) list
        val subst_strings_code : (Str.regexp * string) list
        val subst_strings_simple : (Str.regexp * string) list
        val mutable tag_functions :
          (string * (Odoc_info.text -> Odoc_info.text)) list
        method attribute_label : ?no_:bool -> Odoc_info.Name.t -> string
        method class_label : ?no_:bool -> Odoc_info.Name.t -> string
        method class_type_label : ?no_:bool -> Odoc_info.Name.t -> string
        method const_label : ?no_:bool -> Odoc_info.Name.t -> string
        method entry_comment :
          Format.formatter * (unit -> string) ->
          Odoc_info.info option -> Odoc_info.text_element list
        method escape : string -> string
        method escape_code : string -> string
        method escape_simple : string -> string
        method exception_label : ?no_:bool -> Odoc_info.Name.t -> string
        method extension_label : ?no_:bool -> Odoc_info.Name.t -> string
        method first_and_rest_of_info :
          Odoc_info.info option -> Odoc_info.text * Odoc_info.text
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_class_inheritance_info :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method generate_class_type_inheritance_info :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method generate_for_top_module :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method generate_inheritance_info :
          Format.formatter -> Odoc_info.Class.inherited_class list -> unit
        method generate_style_file : unit
        method label : ?no_:bool -> Odoc_info.Name.t -> string
        method latex_for_class_index :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method latex_for_class_label :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method latex_for_class_type_index :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method latex_for_class_type_label :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method latex_for_module_index :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method latex_for_module_label :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method latex_for_module_type_index :
          Format.formatter -> Odoc_info.Module.t_module_type -> unit
        method latex_for_module_type_label :
          Format.formatter -> Odoc_info.Module.t_module_type -> unit
        method latex_header :
          Format.formatter -> Odoc_info.Module.t_module list -> unit
        method latex_of_Block : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Bold : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Center : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Code : Format.formatter -> string -> unit
        method latex_of_CodePre : Format.formatter -> string -> unit
        method latex_of_Emphasize :
          Format.formatter -> Odoc_info.text -> unit
        method latex_of_Enum :
          Format.formatter -> Odoc_info.text list -> unit
        method latex_of_Italic : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Latex : Format.formatter -> string -> unit
        method latex_of_Left : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Link :
          Format.formatter -> string -> Odoc_info.text -> unit
        method latex_of_List :
          Format.formatter -> Odoc_info.text list -> unit
        method latex_of_Newline : Format.formatter -> unit
        method latex_of_Raw : Format.formatter -> string -> unit
        method latex_of_Ref :
          Format.formatter ->
          Odoc_info.Name.t ->
          Odoc_info.ref_kind option -> Odoc_info.text option -> unit
        method latex_of_Right : Format.formatter -> Odoc_info.text -> unit
        method latex_of_Subscript :
          Format.formatter -> Odoc_info.text -> unit
        method latex_of_Superscript :
          Format.formatter -> Odoc_info.text -> unit
        method latex_of_Target :
          Format.formatter -> target:string -> code:string -> unit
        method latex_of_Title :
          Format.formatter ->
          int -> Odoc_info.Name.t option -> Odoc_info.text -> unit
        method latex_of_Verbatim : Format.formatter -> string -> unit
        method latex_of_attribute :
          Format.formatter -> Odoc_info.Value.t_attribute -> unit
        method latex_of_class :
          Format.formatter -> Odoc_info.Class.t_class -> unit
        method latex_of_class_element :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.class_element -> unit
        method latex_of_class_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.class_kind -> unit
        method latex_of_class_parameter_list :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.t_class -> unit
        method latex_of_class_type :
          Format.formatter -> Odoc_info.Class.t_class_type -> unit
        method latex_of_class_type_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Class.class_type_kind -> unit
        method latex_of_cstr_args :
          Format.formatter * (unit -> string) ->
          Odoc_info.Name.t ->
          Odoc_info.Type.constructor_args * Types.type_expr option ->
          Odoc_info.text_element list
        method latex_of_custom_text :
          Format.formatter -> string -> Odoc_info.text -> unit
        method latex_of_exception :
          Format.formatter -> Odoc_info.Exception.t_exception -> unit
        method latex_of_included_module :
          Format.formatter -> Odoc_info.Module.included_module -> unit
        method latex_of_info :
          Format.formatter -> ?block:bool -> Odoc_info.info option -> unit
        method latex_of_method :
          Format.formatter -> Odoc_info.Value.t_method -> unit
        method latex_of_module :
          Format.formatter -> Odoc_info.Module.t_module -> unit
        method latex_of_module_element :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_element -> unit
        method latex_of_module_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_kind -> unit
        method latex_of_module_parameter :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_parameter -> unit
        method latex_of_module_type :
          Format.formatter -> Odoc_info.Module.t_module_type -> unit
        method latex_of_module_type_kind :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Module.module_type_kind -> unit
        method latex_of_record :
          Format.formatter * (unit -> string) ->
          Odoc_info.Name.t ->
          Odoc_info.Type.record_field list -> Odoc_info.text_element list
        method latex_of_text : Format.formatter -> Odoc_info.text -> unit
        method latex_of_text_element :
          Format.formatter -> Odoc_info.text_element -> unit
        method latex_of_type :
          Format.formatter -> Odoc_info.Type.t_type -> unit
        method latex_of_type_extension :
          Odoc_info.Name.t ->
          Format.formatter -> Odoc_info.Extension.t_type_extension -> unit
        method latex_of_type_params :
          Format.formatter ->
          Odoc_info.Name.t -> Odoc_info.Type.t_type -> unit
        method latex_of_value :
          Format.formatter -> Odoc_info.Value.t_value -> unit
        method make_label : string -> string
        method make_ref : string -> string
        method method_label : ?no_:bool -> Odoc_info.Name.t -> string
        method module_label : ?no_:bool -> Odoc_info.Name.t -> string
        method module_type_label : ?no_:bool -> Odoc_info.Name.t -> string
        method normal_class_params :
          Odoc_info.Name.t -> Odoc_info.Class.t_class -> string
        method normal_class_type :
          Odoc_info.Name.t -> Types.class_type -> string
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
        method recfield_label : ?no_:bool -> Odoc_info.Name.t -> string
        method relative_idents : Odoc_info.Name.t -> string -> string
        method relative_module_idents : Odoc_info.Name.t -> string -> string
        method section_style : int -> string -> string
        method subst : (Str.regexp * string) list -> string -> string
        method text_of_alerts :
          Odoc_info.alert list -> Odoc_info.text_element list
        method text_of_attribute :
          Odoc_info.Value.t_attribute -> Odoc_info.text_element list
        method text_of_author_list :
          string list -> Odoc_info.text_element list
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
          Odoc_info.Name.t ->
          Types.type_expr list -> Odoc_info.text_element list
        method text_of_custom :
          (string * Odoc_info.text) list -> Odoc_info.text
        method text_of_exception :
          Odoc_info.Exception.t_exception -> Odoc_info.text_element list
        method text_of_info :
          ?block:bool -> Odoc_info.info option -> Odoc_info.text
        method text_of_method :
          Odoc_info.Value.t_method -> Odoc_info.text_element list
        method text_of_module_kind :
          ?with_def_syntax:bool ->
          Odoc_info.Module.module_kind -> Odoc_info.text_element list
        method text_of_module_parameter_list :
          (Odoc_info.Module.module_parameter *
           Odoc_info.text_element list option)
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
        method text_of_sees :
          Odoc_info.see list -> Odoc_info.text_element list
        method text_of_short_type_expr :
          Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element list
        method text_of_since_opt :
          string option -> Odoc_info.text_element list
        method text_of_type_expr :
          Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element list
        method text_of_type_expr_list :
          Odoc_info.Name.t ->
          string -> Types.type_expr list -> Odoc_info.text_element list
        method text_of_value :
          Odoc_info.Value.t_value -> Odoc_info.text_element list
        method text_of_version_opt :
          string option -> Odoc_info.text_element list
        method type_label : ?no_:bool -> Odoc_info.Name.t -> string
        method value_label : ?no_:bool -> Odoc_info.Name.t -> string
      end
  end
