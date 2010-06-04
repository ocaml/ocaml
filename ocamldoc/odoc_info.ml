(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Interface for analysing documented OCaml source files and to the collected information. *)

type ref_kind = Odoc_types.ref_kind =
    RK_module
  | RK_module_type
  | RK_class
  | RK_class_type
  | RK_value
  | RK_type
  | RK_exception
  | RK_attribute
  | RK_method
  | RK_section of text

and text_element = Odoc_types.text_element =
  | Raw of string
  | Code of string
  | CodePre of string
  | Verbatim of string
  | Bold of text
  | Italic of text
  | Emphasize of text
  | Center of text
  | Left of text
  | Right of text
  | List of text list
  | Enum of text list
  | Newline
  | Block of text
  | Title of int * string option * text
  | Latex of string
  | Link of string * text
  | Ref of string * ref_kind option * text option
  | Superscript of text
  | Subscript of text
  | Module_list of string list
  | Index_list
  | Target of string * string

and text = text_element list

exception Text_syntax = Odoc_text.Text_syntax

type see_ref = Odoc_types.see_ref =
    See_url of string
  | See_file of string
  | See_doc of string

type see = see_ref * text

type param = (string * text)

type raised_exception = (string * text)

type info = Odoc_types.info = {
    i_desc : text option;
    i_authors : string list;
    i_version : string option;
    i_sees : see list;
    i_since : string option;
    i_before : (string * text) list ;
    i_deprecated : text option;
    i_params : param list;
    i_raised_exceptions : raised_exception list;
    i_return_value : text option ;
    i_custom : (string * text) list ;
  }

type location = Odoc_types.location = {
    loc_impl : (string * int) option ;
    loc_inter : (string * int) option ;
  }

let dummy_loc = { loc_impl = None ; loc_inter = None }

module Name = Odoc_name
module Parameter = Odoc_parameter
module Exception = Odoc_exception
module Type = Odoc_type
module Value = Odoc_value
module Class = Odoc_class
module Module = Odoc_module


let analyse_files
    ?(merge_options=([] : Odoc_types.merge_option list))
    ?(include_dirs=([] : string list))
    ?(labels=false)
    ?(sort_modules=false)
    ?(no_stop=false)
    ?(init=[])
    files =
  Odoc_args.merge_options := merge_options;
  Odoc_args.include_dirs := include_dirs;
  Odoc_args.classic := not labels;
  Odoc_args.sort_modules := sort_modules;
  Odoc_args.no_stop := no_stop;
  Odoc_analyse.analyse_files ~init: init files

let dump_modules = Odoc_analyse.dump_modules

let load_modules = Odoc_analyse.load_modules

let reset_type_names = Printtyp.reset

let string_of_variance t (co,cn) = Odoc_str.string_of_variance t (co, cn)

let string_of_type_expr t = Odoc_print.string_of_type_expr t

let string_of_class_params = Odoc_str.string_of_class_params

let string_of_type_list ?par sep type_list = Odoc_str.string_of_type_list ?par sep type_list

let string_of_type_param_list t = Odoc_str.string_of_type_param_list t

let string_of_class_type_param_list l = Odoc_str.string_of_class_type_param_list l

let string_of_module_type = Odoc_print.string_of_module_type

let string_of_class_type = Odoc_print.string_of_class_type

let string_of_text t = Odoc_misc.string_of_text t

let string_of_info i = Odoc_misc.string_of_info i

let string_of_type t = Odoc_str.string_of_type t

let string_of_exception e = Odoc_str.string_of_exception e

let string_of_value v = Odoc_str.string_of_value v

let string_of_attribute att = Odoc_str.string_of_attribute att

let string_of_method m = Odoc_str.string_of_method m

let first_sentence_of_text = Odoc_misc.first_sentence_of_text

let first_sentence_and_rest_of_text = Odoc_misc.first_sentence_and_rest_of_text

let text_no_title_no_list = Odoc_misc.text_no_title_no_list

let text_concat = Odoc_misc.text_concat

let get_titles_in_text = Odoc_misc.get_titles_in_text

let create_index_lists = Odoc_misc.create_index_lists

let remove_ending_newline = Odoc_misc.remove_ending_newline

let remove_option = Odoc_misc.remove_option

let is_optional = Odoc_misc.is_optional

let label_name = Odoc_misc.label_name

let use_hidden_modules n =
  Odoc_name.hide_given_modules !Odoc_args.hidden_modules n

let verbose s =
  if !Odoc_args.verbose then
    (print_string s ; print_newline ())
  else
    ()

let warning s = Odoc_messages.pwarning s

let errors = Odoc_global.errors

let apply_opt = Odoc_misc.apply_opt

let apply_if_equal f v1 v2 =
  if v1 = v2 then
    f v1
  else
    v2

let text_of_string = Odoc_text.Texter.text_of_string

let text_string_of_text = Odoc_text.Texter.string_of_text


let escape_arobas s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '@' -> Buffer.add_string b "\\@"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b

let info_string_of_info i =
  let b = Buffer.create 256 in
  let p = Printf.bprintf in
  (
   match i.i_desc with
     None -> ()
   | Some t -> p b "%s" (escape_arobas (text_string_of_text t))
  );
  List.iter
    (fun s -> p b "\n@author %s" (escape_arobas s))
    i.i_authors;
  (
   match i.i_version with
     None -> ()
   | Some s -> p b "\n@version %s" (escape_arobas s)
  );
  (
   (* TODO: escape characters ? *)
   let f_see_ref = function
       See_url s -> Printf.sprintf "<%s>" s
     | See_file s -> Printf.sprintf "'%s'" s
     | See_doc s -> Printf.sprintf "\"%s\"" s
   in
   List.iter
     (fun (sref, t) ->
       p b "\n@see %s %s"
	 (escape_arobas (f_see_ref sref))
	 (escape_arobas (text_string_of_text t))
     )
     i.i_sees
  );
  (
   match i.i_since with
     None -> ()
   | Some s -> p b "\n@since %s" (escape_arobas s)
  );
  (
   match i.i_deprecated with
     None -> ()
   | Some t ->
       p b "\n@deprecated %s"
	 (escape_arobas (text_string_of_text t))
  );
  List.iter
    (fun (s, t) ->
      p b "\n@param %s %s"
	(escape_arobas s)
	(escape_arobas (text_string_of_text t))
    )
    i.i_params;
  List.iter
    (fun (s, t) ->
      p b "\n@raise %s %s"
	(escape_arobas s)
	(escape_arobas (text_string_of_text t))
    )
    i.i_raised_exceptions;
  (
   match i.i_return_value with
     None -> ()
   | Some t ->
       p b "\n@return %s"
	 (escape_arobas (text_string_of_text t))
  );
  List.iter
    (fun (s, t) ->
      p b "\n@%s %s" s
	(escape_arobas (text_string_of_text t))
    )
    i.i_custom;

  Buffer.contents b

let info_of_string s =
  let dummy =
    {
      i_desc = None ;
      i_authors = [] ;
      i_version = None ;
      i_sees = [] ;
      i_since = None ;
      i_deprecated = None ;
      i_params = [] ;
      i_raised_exceptions = [] ;
      i_return_value = None ;
      i_custom = [] ;
    }
  in
  let s2 = Printf.sprintf "(** %s *)" s in
  let (_, i_opt) = Odoc_comments.Basic_info_retriever.first_special "-" s2 in
  match i_opt with
    None -> dummy
  | Some i -> i

let info_of_comment_file f =
  try
    let s = Odoc_misc.input_file_as_string f in
    info_of_string s
  with
    Sys_error s ->
      failwith s

module Search =
  struct
    type result_element = Odoc_search.result_element =
          Res_module of Module.t_module
        | Res_module_type of Module.t_module_type
        | Res_class of Class.t_class
        | Res_class_type of Class.t_class_type
        | Res_value of Value.t_value
        | Res_type of Type.t_type
        | Res_exception of Exception.t_exception
        | Res_attribute of Value.t_attribute
        | Res_method of Value.t_method
        | Res_section of string * text

    type search_result = result_element list

    let search_by_name = Odoc_search.Search_by_name.search

    let values = Odoc_search.values
    let exceptions = Odoc_search.exceptions
    let types = Odoc_search.types
    let attributes = Odoc_search.attributes
    let methods = Odoc_search.methods
    let classes = Odoc_search.classes
    let class_types = Odoc_search.class_types
    let modules = Odoc_search.modules
    let module_types = Odoc_search.module_types
  end

module Scan =
  struct
    class scanner = Odoc_scan.scanner
  end

module Dep =
  struct
    let kernel_deps_of_modules = Odoc_dep.kernel_deps_of_modules
    let deps_of_types = Odoc_dep.deps_of_types
  end

module Args = Odoc_args
