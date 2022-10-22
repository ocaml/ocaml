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

(** The messages of the application. *)

val ok : string
val software : string
val config_version : string
val magic : string
val usage : string
val options_are : string
val latex_only : string
val texi_only : string
val latex_texi_only : string
val html_only : string
val html_latex_only : string
val html_latex_texi_only : string
val man_only : string
val option_impl : string
val option_intf : string
val option_text : string
val display_custom_generators_dir : string
val add_load_dir : string
val load_file : string
val werr : string
val show_missed_crossref : string
val hide_warnings : string
val target_dir : string
val dump : string
val load : string
val css_style : string
val index_only : string
val colorize_code : string
val html_short_functors : string
val charset : string -> string
val no_navbar : string
val generate_html : string
val generate_latex : string
val generate_texinfo : string
val generate_man : string
val generate_dot : string
val option_not_in_native_code : string -> string
val default_out_file : string
val out_file : string
val dot_include_all : string
val dot_types : string
val default_dot_colors : string list list
val dot_colors : string
val dot_reduce : string
val man_mini : string
val default_man_section : string
val man_section : string
val default_man_suffix : string
val man_suffix : string
val option_title : string
val option_intro : string
val with_parameter_list : string
val hide_modules : string
val no_header : string
val no_trailer : string
val separate_files : string
val latex_title : (int * string) list ref -> string
val default_latex_value_prefix : string
val latex_value_prefix : string
val default_latex_type_prefix : string
val latex_type_prefix : string
val default_latex_type_elt_prefix : string
val latex_type_elt_prefix : string
val default_latex_extension_prefix : string
val latex_extension_prefix : string
val default_latex_exception_prefix : string
val latex_exception_prefix : string
val default_latex_module_prefix : string
val latex_module_prefix : string
val default_latex_module_type_prefix : string
val latex_module_type_prefix : string
val default_latex_class_prefix : string
val latex_class_prefix : string
val default_latex_class_type_prefix : string
val latex_class_type_prefix : string
val default_latex_attribute_prefix : string
val latex_attribute_prefix : string
val default_latex_method_prefix : string
val latex_method_prefix : string
val no_toc : string
val sort_modules : string
val no_stop : string
val no_custom_tags : string
val remove_stars : string
val keep_code : string
val inverse_merge_ml_mli : string
val no_filter_with_module_constraints : string
val merge_description : char * string
val merge_author : char * string
val merge_version : char * string
val merge_see : char * string
val merge_since : char * string
val merge_before : char * string
val merge_deprecated : char * string
val merge_param : char * string
val merge_raised_exception : char * string
val merge_return_value : char * string
val merge_custom : char * string
val merge_all : char * string
val no_index : string
val esc_8bits : string
val texinfo_title : (int * (string * string)) list ref -> string
val info_section : string
val info_entry : string
val options_can_be : string
val string_of_options_list : (char * string) list -> string
val merge_options : string
val initially_opened_module : string
val library_namespace : string
val help : string
val warning : string
val error_location : string -> int -> int -> string
val bad_magic_number : string
val not_a_module_name : string -> string
val load_file_error : string -> string -> string
val wrong_format : string -> string
val errors_occured : int -> string
val parse_error : string
val text_parse_error : int -> int -> string -> string
val file_not_found_in_paths : string list -> string -> string
val tag_not_handled : string -> string
val should_escape_at_sign : string
val bad_tree : string
val not_a_valid_tag : string -> string
val fun_without_param : string -> string
val method_without_param : string -> string
val anonymous_parameters : string -> string
val function_colon : string -> string
val implicit_match_in_parameter : string
val unknown_extension : string -> string
val two_implementations : string -> string
val two_interfaces : string -> string
val too_many_module_objects : string -> string
val extension_not_found_in_implementation : string -> string -> string
val exception_not_found_in_implementation : string -> string -> string
val type_not_found_in_implementation : string -> string -> string
val module_not_found_in_implementation : string -> string -> string
val value_not_found_in_implementation : string -> string -> string
val class_not_found_in_implementation : string -> string -> string
val attribute_not_found_in_implementation : string -> string -> string
val method_not_found_in_implementation : string -> string -> string
val different_types : string -> string
val attribute_type_not_found : string -> string -> string
val method_type_not_found : string -> string -> string
val module_not_found : string -> string -> string
val module_type_not_found : string -> string -> string
val value_not_found : string -> string -> string
val extension_not_found : string -> string -> string
val exception_not_found : string -> string -> string
val type_not_found : string -> string -> string
val class_not_found : string -> string -> string
val class_type_not_found : string -> string -> string
val type_not_found_in_typedtree : string -> string
val extension_not_found_in_typedtree : string -> string
val exception_not_found_in_typedtree : string -> string
val module_type_not_found_in_typedtree : string -> string
val module_not_found_in_typedtree : string -> string
val class_not_found_in_typedtree : string -> string
val class_type_not_found_in_typedtree : string -> string
val inherit_classexp_not_found_in_typedtree : int -> string
val attribute_not_found_in_typedtree : string -> string
val method_not_found_in_typedtree : string -> string
val misplaced_comment : string -> int -> string
val cross_module_not_found : string -> string
val cross_module_type_not_found : string -> string
val cross_module_or_module_type_not_found : string -> string
val cross_class_not_found : string -> string
val cross_class_type_not_found : string -> string
val cross_class_or_class_type_not_found : string -> string
val cross_extension_not_found : string -> string
val cross_exception_not_found : string -> string
val cross_element_not_found : string -> string
val cross_method_not_found : string -> string
val cross_attribute_not_found : string -> string
val cross_section_not_found : string -> string
val cross_value_not_found : string -> string
val cross_type_not_found : string -> string
val cross_recfield_not_found : string -> string
val cross_const_not_found : string -> string
val code_could_be_cross_reference : string -> string -> string
val object_end : string
val struct_end : string
val sig_end : string
val current_generator_is_not : string -> string
val analysing : string -> string
val merging : string
val cross_referencing : string
val generating_doc : string
val loading : string -> string
val file_generated : string -> string
val file_exists_dont_generate : string -> string
val modul : string
val modules : string
val functors : string
val values : string
val types : string
val extensions : string
val exceptions : string
val record : string
val variant : string
val mutab : string
val functions : string
val parameters : string
val abstract : string
val functo : string
val clas : string
val classes : string
val attributes : string
val methods : string
val authors : string
val version : string
val since : string
val before : string
val deprecated : string
val alert : string
val raises : string
val returns : string
val inherits : string
val inheritance : string
val privat : string
val module_type : string
val class_type : string
val description : string
val interface : string
val type_parameters : string
val class_types : string
val module_types : string
val see_also : string
val documentation : string
val index_of : string
val top : string
val index_of_values : string
val index_of_extensions : string
val index_of_exceptions : string
val index_of_types : string
val index_of_attributes : string
val index_of_methods : string
val index_of_classes : string
val index_of_class_types : string
val index_of_modules : string
val index_of_module_types : string
val previous : string
val next : string
val up : string
