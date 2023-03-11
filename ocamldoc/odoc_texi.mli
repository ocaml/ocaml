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

(** Generation of Texinfo documentation. *)

val esc_8bits : bool ref

val info_section : string ref

val info_entry : string list ref

val titles_and_headings : (int * (string * string)) list ref

type subparts =
    [ `Class of Odoc_info.Class.t_class
    | `Class_type of Odoc_info.Class.t_class_type
    | `Module of Odoc_info.Module.t_module
    | `Module_type of Odoc_info.Module.t_module_type ]

type indices =
    [ `Class
    | `Class_att
    | `Class_type
    | `Exception
    | `Extension
    | `Method
    | `Module
    | `Module_type
    | `Type
    | `Value ]

module Generator :
  sig
    class texi :
      object
        val bullet : Odoc_info.text_element
        val mutable indices_to_build :
          [ `Class
          | `Class_att
          | `Class_type
          | `Exception
          | `Extension
          | `Method
          | `Module
          | `Module_type
          | `Type
          | `Value ] list
        val linebreak : Odoc_info.text_element
        val maxdepth : int
        val minus : Odoc_info.text_element
        val node_tbl : (Odoc_info.Name.t, unit) Hashtbl.t
        val mutable tag_functions :
          (string * (Odoc_info.text -> Odoc_info.text)) list
        method do_index :
          [ `Class
          | `Class_att
          | `Class_type
          | `Exception
          | `Extension
          | `Method
          | `Module
          | `Module_type
          | `Type
          | `Value ] -> unit
        method escape : Odoc_info.Name.t -> Odoc_info.Name.t
        method private fix_linebreaks : Odoc_info.text -> Odoc_info.text
        method fixedblock :
          Odoc_info.text_element list -> Odoc_info.text_element
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_class_inheritance_info :
          out_channel -> Odoc_info.Class.t_class -> unit
        method generate_class_type_inheritance_info :
          out_channel -> Odoc_info.Class.t_class_type -> unit
        method generate_for_class :
          out_channel -> Odoc_info.Class.t_class -> unit
        method generate_for_class_type :
          out_channel -> Odoc_info.Class.t_class_type -> unit
        method generate_for_module :
          out_channel -> Odoc_info.Module.t_module -> unit
        method generate_for_module_type :
          out_channel -> Odoc_info.Module.t_module_type -> unit
        method generate_inheritance_info :
          out_channel -> Odoc_info.Class.inherited_class list -> unit
        method generate_texi_header :
          out_channel -> string -> Odoc_info.Module.t_module list -> unit
        method generate_texi_trailer : out_channel -> unit
        method heading : int -> Odoc_info.text -> string
        method index : indices -> Odoc_info.Name.t -> Odoc_info.text_element
        method label : ?no_:bool -> string -> string
        method node : int -> Odoc_info.Name.t -> Odoc_info.text_element
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
        method relative_idents : Odoc_info.Name.t -> string -> string
        method relative_module_idents : Odoc_info.Name.t -> string -> string
        method scan_for_index : subparts -> unit
        method scan_for_index_in_class :
          Odoc_info.Class.class_element -> unit
        method scan_for_index_in_mod :
          Odoc_info.Module.module_element -> unit
        method private soft_fix_linebreaks :
          int -> Odoc_info.text_element list -> Odoc_info.text_element list
        method string_of_type_args :
          Odoc_info.Type.constructor_args -> Types.type_expr option -> string
        method string_of_type_parameters : Odoc_info.Type.t_type -> string
        method texi_of_Block : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Bold : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Center : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Code : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_CodePre : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_Emphasize : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Enum : Odoc_info.text list -> Odoc_info.Name.t
        method texi_of_Italic : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Left : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Link : string -> Odoc_info.text -> Odoc_info.Name.t
        method texi_of_List : Odoc_info.text list -> Odoc_info.Name.t
        method texi_of_Newline : Odoc_info.Name.t
        method texi_of_Raw : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_Ref :
          Odoc_info.Name.t -> Odoc_info.ref_kind option -> Odoc_info.Name.t
        method texi_of_Right : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Subscript : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Superscript : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Target :
          target:string -> code:Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_Title : int -> Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Verbatim : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_attribute : Odoc_info.Value.t_attribute -> string
        method texi_of_class : Odoc_info.Class.t_class -> string
        method texi_of_class_element :
          Odoc_info.Name.t -> Odoc_info.Class.class_element -> string
        method texi_of_class_type : Odoc_info.Class.t_class_type -> string
        method texi_of_custom_text :
          string -> Odoc_info.text -> Odoc_info.Name.t
        method texi_of_exception : Odoc_info.Exception.t_exception -> string
        method texi_of_included_module :
          Odoc_info.Module.included_module -> string
        method texi_of_info : Odoc_info.info option -> string
        method texi_of_method : Odoc_info.Value.t_method -> string
        method texi_of_module : Odoc_info.Module.t_module -> string
        method texi_of_module_element :
          Odoc_info.Name.t -> Odoc_info.Module.module_element -> string
        method texi_of_module_type : Odoc_info.Module.t_module_type -> string
        method texi_of_text : Odoc_info.text -> string
        method texi_of_text_element :
          Odoc_info.text_element -> Odoc_info.Name.t
        method texi_of_type : Odoc_info.Type.t_type -> string
        method texi_of_type_extension :
          Odoc_info.Name.t -> Odoc_info.Extension.t_type_extension -> string
        method texi_of_value : Odoc_info.Value.t_value -> string
        method text_el_of_type_expr :
          Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element
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
        method text_of_desc :
          Odoc_info.text option -> Odoc_info.text_element list
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
        method text_of_params :
          Odoc_info.param list -> Odoc_info.text_element list
        method text_of_raised_exceptions :
          Odoc_info.raised_exception list -> Odoc_info.text_element list
        method text_of_return_opt :
          Odoc_info.text option -> Odoc_info.text_element list
        method text_of_see : Odoc_info.see -> Odoc_info.text
        method text_of_sees :
          Odoc_info.see list -> Odoc_info.text_element list
        method text_of_sees_opt :
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
      end
  end

module type Texi_generator =
  sig
    class texi :
      object
        val bullet : Odoc_info.text_element
        val mutable indices_to_build :
          [ `Class
          | `Class_att
          | `Class_type
          | `Exception
          | `Extension
          | `Method
          | `Module
          | `Module_type
          | `Type
          | `Value ] list
        val linebreak : Odoc_info.text_element
        val maxdepth : int
        val minus : Odoc_info.text_element
        val node_tbl : (Odoc_info.Name.t, unit) Hashtbl.t
        val mutable tag_functions :
          (string * (Odoc_info.text -> Odoc_info.text)) list
        method do_index :
          [ `Class
          | `Class_att
          | `Class_type
          | `Exception
          | `Extension
          | `Method
          | `Module
          | `Module_type
          | `Type
          | `Value ] -> unit
        method escape : Odoc_info.Name.t -> Odoc_info.Name.t
        method private fix_linebreaks : Odoc_info.text -> Odoc_info.text
        method fixedblock :
          Odoc_info.text_element list -> Odoc_info.text_element
        method generate : Odoc_info.Module.t_module list -> unit
        method generate_class_inheritance_info :
          out_channel -> Odoc_info.Class.t_class -> unit
        method generate_class_type_inheritance_info :
          out_channel -> Odoc_info.Class.t_class_type -> unit
        method generate_for_class :
          out_channel -> Odoc_info.Class.t_class -> unit
        method generate_for_class_type :
          out_channel -> Odoc_info.Class.t_class_type -> unit
        method generate_for_module :
          out_channel -> Odoc_info.Module.t_module -> unit
        method generate_for_module_type :
          out_channel -> Odoc_info.Module.t_module_type -> unit
        method generate_inheritance_info :
          out_channel -> Odoc_info.Class.inherited_class list -> unit
        method generate_texi_header :
          out_channel -> string -> Odoc_info.Module.t_module list -> unit
        method generate_texi_trailer : out_channel -> unit
        method heading : int -> Odoc_info.text -> string
        method index : indices -> Odoc_info.Name.t -> Odoc_info.text_element
        method label : ?no_:bool -> string -> string
        method node : int -> Odoc_info.Name.t -> Odoc_info.text_element
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
        method relative_idents : Odoc_info.Name.t -> string -> string
        method relative_module_idents : Odoc_info.Name.t -> string -> string
        method scan_for_index : subparts -> unit
        method scan_for_index_in_class :
          Odoc_info.Class.class_element -> unit
        method scan_for_index_in_mod :
          Odoc_info.Module.module_element -> unit
        method private soft_fix_linebreaks :
          int -> Odoc_info.text_element list -> Odoc_info.text_element list
        method string_of_type_args :
          Odoc_info.Type.constructor_args -> Types.type_expr option -> string
        method string_of_type_parameters : Odoc_info.Type.t_type -> string
        method texi_of_Block : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Bold : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Center : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Code : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_CodePre : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_Emphasize : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Enum : Odoc_info.text list -> Odoc_info.Name.t
        method texi_of_Italic : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Left : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Link : string -> Odoc_info.text -> Odoc_info.Name.t
        method texi_of_List : Odoc_info.text list -> Odoc_info.Name.t
        method texi_of_Newline : Odoc_info.Name.t
        method texi_of_Raw : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_Ref :
          Odoc_info.Name.t -> Odoc_info.ref_kind option -> Odoc_info.Name.t
        method texi_of_Right : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Subscript : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Superscript : Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Target :
          target:string -> code:Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_Title : int -> Odoc_info.text -> Odoc_info.Name.t
        method texi_of_Verbatim : Odoc_info.Name.t -> Odoc_info.Name.t
        method texi_of_attribute : Odoc_info.Value.t_attribute -> string
        method texi_of_class : Odoc_info.Class.t_class -> string
        method texi_of_class_element :
          Odoc_info.Name.t -> Odoc_info.Class.class_element -> string
        method texi_of_class_type : Odoc_info.Class.t_class_type -> string
        method texi_of_custom_text :
          string -> Odoc_info.text -> Odoc_info.Name.t
        method texi_of_exception : Odoc_info.Exception.t_exception -> string
        method texi_of_included_module :
          Odoc_info.Module.included_module -> string
        method texi_of_info : Odoc_info.info option -> string
        method texi_of_method : Odoc_info.Value.t_method -> string
        method texi_of_module : Odoc_info.Module.t_module -> string
        method texi_of_module_element :
          Odoc_info.Name.t -> Odoc_info.Module.module_element -> string
        method texi_of_module_type : Odoc_info.Module.t_module_type -> string
        method texi_of_text : Odoc_info.text -> string
        method texi_of_text_element :
          Odoc_info.text_element -> Odoc_info.Name.t
        method texi_of_type : Odoc_info.Type.t_type -> string
        method texi_of_type_extension :
          Odoc_info.Name.t -> Odoc_info.Extension.t_type_extension -> string
        method texi_of_value : Odoc_info.Value.t_value -> string
        method text_el_of_type_expr :
          Odoc_info.Name.t -> Types.type_expr -> Odoc_info.text_element
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
        method text_of_desc :
          Odoc_info.text option -> Odoc_info.text_element list
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
        method text_of_params :
          Odoc_info.param list -> Odoc_info.text_element list
        method text_of_raised_exceptions :
          Odoc_info.raised_exception list -> Odoc_info.text_element list
        method text_of_return_opt :
          Odoc_info.text option -> Odoc_info.text_element list
        method text_of_see : Odoc_info.see -> Odoc_info.text
        method text_of_sees :
          Odoc_info.see list -> Odoc_info.text_element list
        method text_of_sees_opt :
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
      end
  end
