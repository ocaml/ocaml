(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Hongbo Zhang (University of Pennsylvania)                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format
open Asttypes
open Parsetree

type space_formatter = (unit, formatter, unit) format
class printer :
  unit ->
  object ('b)
    val pipe : bool
    val semi : bool
    method binding : formatter -> pattern * expression -> unit
    method bindings :
        formatter -> rec_flag * (pattern * expression) list -> unit
    method case_list : formatter -> (pattern * expression) list -> unit
    method class_expr : formatter -> class_expr -> unit
    method class_field : formatter -> class_field -> unit
    method class_params_def :
      formatter -> (string loc * (bool * bool * bool)) list -> unit
    method class_signature : formatter -> class_signature -> unit
    method class_structure : formatter -> class_structure -> unit
    method class_type : formatter -> class_type -> unit
    method class_type_declaration_list :
      formatter -> class_type_declaration list -> unit
    method constant : formatter -> constant -> unit
    method constant_string : formatter -> string -> unit
    method core_type : formatter -> core_type -> unit
    method core_type1 : formatter -> core_type -> unit
    method direction_flag : formatter -> direction_flag -> unit
    method directive_argument : formatter -> directive_argument -> unit
    method exception_declaration :
      formatter -> string * exception_declaration -> unit
    method expression : formatter -> expression -> unit
    method expression1 : formatter -> expression -> unit
    method expression2 : formatter -> expression -> unit
    method label_exp :
      formatter -> label * expression option * pattern -> unit
    method label_x_expression_param : formatter -> label * expression -> unit
    method list :
      ?sep:space_formatter ->
      ?first:space_formatter ->
      ?last:space_formatter ->
      (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
    method longident : formatter -> Longident.t -> unit
    method longident_loc : formatter -> Longident.t loc -> unit
    method module_expr : formatter -> module_expr -> unit
    method module_type : formatter -> module_type -> unit
    method mutable_flag : formatter -> mutable_flag -> unit
    method option :
      ?first:space_formatter ->
      ?last:space_formatter ->
      (formatter -> 'a -> unit) ->
      formatter -> 'a option -> unit
    method paren :
        ?first:space_formatter -> ?last:space_formatter -> bool ->
        (formatter -> 'a -> unit) -> formatter -> 'a -> unit
    method pattern : formatter -> pattern -> unit
    method pattern1 : formatter -> pattern -> unit
    method private_flag : formatter -> private_flag -> unit
    method rec_flag : formatter -> rec_flag -> unit

    method reset : 'b
    method reset_semi : 'b
    method reset_ifthenelse : 'b
    method reset_pipe : 'b

    method signature : formatter -> signature_item list -> unit
    method signature_item : formatter -> signature_item -> unit
    method simple_expr : formatter -> expression -> unit
    method simple_pattern : formatter -> pattern -> unit
    method string_quot : formatter -> label -> unit
    method structure : formatter -> structure_item list -> unit
    method structure_item : formatter -> structure_item -> unit
    method sugar_expr : formatter -> expression -> bool
    method toplevel_phrase : formatter -> toplevel_phrase -> unit
    method type_declaration : formatter -> type_declaration -> unit
    method type_def_list :
      formatter -> (string loc * type_declaration) list -> unit
    method type_param :
      formatter -> (bool * bool * bool) * string loc option -> unit
    method type_var_option : formatter -> string loc option -> unit
    method type_with_label : formatter -> label * core_type -> unit
    method tyvar : formatter -> string -> unit
    method under_pipe : 'b
    method under_semi : 'b
    method under_ifthenelse : 'b
    method value_description : formatter -> value_description -> unit
    method virtual_flag : formatter -> virtual_flag -> unit
  end
val default : printer
val toplevel_phrase : formatter -> toplevel_phrase -> unit
val expression : formatter -> expression -> unit
val string_of_expression : expression -> string
val top_phrase: formatter -> toplevel_phrase -> unit
val core_type: formatter -> core_type -> unit
val pattern: formatter -> pattern -> unit
val signature: formatter -> signature -> unit
val structure: formatter -> structure -> unit
val string_of_structure: structure -> string
