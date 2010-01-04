(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: $ *)

open Asttypes;;
open Types;;
open Path;;
open Btype;;
open Predef;;

let ident_match_failure = Ident.create_predef_exn "Match_failure"
and ident_out_of_memory = Ident.create_predef_exn "Out_of_memory"
and ident_invalid_argument = Ident.create_predef_exn "Invalid_argument"
and ident_failure = Ident.create_predef_exn "Failure"
and ident_not_found = Ident.create_predef_exn "Not_found"
and ident_sys_error = Ident.create_predef_exn "Sys_error"
and ident_end_of_file = Ident.create_predef_exn "End_of_file"
and ident_division_by_zero = Ident.create_predef_exn "Division_by_zero"
and ident_stack_overflow = Ident.create_predef_exn "Stack_overflow"
and ident_sys_blocked_io = Ident.create_predef_exn "Sys_blocked_io"
and ident_assert_failure = Ident.create_predef_exn "Assert_failure"
and ident_undefined_recursive_module =
  Ident.create_predef_exn "Undefined_recursive_module"

let path_match_failure = Pident ident_match_failure
and path_assert_failure = Pident ident_assert_failure
and path_undefined_recursive_module =
  Pident ident_undefined_recursive_module

let build_initial_env add_type add_exception empty_env =
  let decl_abstr =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = None;
     type_variance = []}
  and decl_bool =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_variant(["false", []; "true", []]);
     type_private = Public;
     type_manifest = None;
     type_variance = []}
  and decl_unit =
    {type_params = []; 
     type_arity = 0;
     type_kind = Type_variant(["()", []]);
     type_private = Public;
     type_manifest = None;
     type_variance = []}
  and decl_exn =
    {type_params = [];
     type_arity = 0;
     type_kind = Type_variant [];
     type_private = Public;
     type_manifest = None;
     type_variance = []}
  and decl_array =
    let tvar = newgenvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = None;
     type_variance = [true, true, true]}
  and decl_list =
    let tvar = newgenvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind =
       Type_variant(["[]", []; "::", [tvar; type_list tvar]]);
     type_private = Public;
     type_manifest = None;
     type_variance = [true, false, false]}
  and decl_format6 =
    {type_params = [
       newgenvar(); newgenvar(); newgenvar();
       newgenvar(); newgenvar(); newgenvar();
     ];
     type_arity = 6;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = None;
     type_variance = [
       true, true, true; true, true, true;
       true, true, true; true, true, true;
       true, true, true; true, true, true;
     ]}
  and decl_option =
    let tvar = newgenvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_variant(["None", []; "Some", [tvar]]);
     type_private = Public;
     type_manifest = None;
     type_variance = [true, false, false]}
  and decl_lazy_t =
    let tvar = newgenvar() in
    {type_params = [tvar];
     type_arity = 1;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = None;
     type_variance = [true, false, false]}
  in

  add_exception ident_match_failure
                [newgenty (Ttuple[type_string; type_int; type_int])] (
  add_exception ident_out_of_memory [] (
  add_exception ident_stack_overflow [] (
  add_exception ident_invalid_argument [type_string] (
  add_exception ident_failure [type_string] (
  add_exception ident_not_found [] (
  add_exception ident_sys_blocked_io [] (
  add_exception ident_sys_error [type_string] (
  add_exception ident_end_of_file [] (
  add_exception ident_division_by_zero [] (
  add_exception ident_assert_failure
                [newgenty (Ttuple[type_string; type_int; type_int])] (
  add_exception ident_undefined_recursive_module
                [newgenty (Ttuple[type_string; type_int; type_int])] (
  add_type ident_int64 decl_abstr (
  add_type ident_int32 decl_abstr (
  add_type ident_nativeint decl_abstr (
  add_type ident_lazy_t decl_lazy_t (
  add_type ident_option decl_option (
  add_type ident_format6 decl_format6 (
  add_type ident_list decl_list (
  add_type ident_array decl_array (
  add_type ident_exn decl_exn (
  add_type ident_unit decl_unit (
  add_type ident_bool decl_bool (
  add_type ident_float decl_abstr (
  add_type ident_string decl_abstr (
  add_type ident_char decl_abstr (
  add_type ident_int decl_abstr (
    empty_env)))))))))))))))))))))))))))

let builtin_values =
  List.map (fun id -> Ident.make_global id; (Ident.name id, id))
      [ident_match_failure; ident_out_of_memory; ident_stack_overflow;
       ident_invalid_argument;
       ident_failure; ident_not_found; ident_sys_error; ident_end_of_file;
       ident_division_by_zero; ident_sys_blocked_io;
       ident_assert_failure; ident_undefined_recursive_module ]
