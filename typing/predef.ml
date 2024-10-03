(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Predefined type constructors (with special typing rules in typecore) *)

open Path
open Types
open Btype

let builtin_idents = ref []

let wrap create s =
  let id = create s in
  builtin_idents := (s, id) :: !builtin_idents;
  id

let ident_create = wrap Ident.create_predef

type abstract_type_constr = [
  | `Int
  | `Char
  | `String
  | `Bytes
  | `Float
  | `Continuation
  | `Array
  | `Nativeint
  | `Int32
  | `Int64
  | `Lazy_t
  | `Extension_constructor
  | `Floatarray
]
type data_type_constr = [
  | `Bool
  | `Unit
  | `Exn
  | `Eff
  | `List
  | `Option
]
type type_constr = [
  | abstract_type_constr
  | data_type_constr
]

let all_type_constrs = [
  `Int;
  `Char;
  `String;
  `Bytes;
  `Float;
  `Bool;
  `Unit;
  `Exn;
  `Eff;
  `Continuation;
  `Array;
  `List;
  `Option;
  `Nativeint;
  `Int32;
  `Int64;
  `Lazy_t;
  `Extension_constructor;
  `Floatarray;
]

let ident_int = ident_create "int"
and ident_char = ident_create "char"
and ident_bytes = ident_create "bytes"
and ident_float = ident_create "float"
and ident_bool = ident_create "bool"
and ident_unit = ident_create "unit"
and ident_exn = ident_create "exn"
and ident_eff = ident_create "eff"
and ident_continuation = ident_create "continuation"
and ident_array = ident_create "array"
and ident_list = ident_create "list"
and ident_option = ident_create "option"
and ident_nativeint = ident_create "nativeint"
and ident_int32 = ident_create "int32"
and ident_int64 = ident_create "int64"
and ident_lazy_t = ident_create "lazy_t"
and ident_string = ident_create "string"
and ident_extension_constructor = ident_create "extension_constructor"
and ident_floatarray = ident_create "floatarray"

let ident_of_type_constr = function
  | `Int -> ident_int
  | `Char -> ident_char
  | `String -> ident_string
  | `Bytes -> ident_bytes
  | `Float -> ident_float
  | `Bool -> ident_bool
  | `Unit -> ident_unit
  | `Exn -> ident_exn
  | `Eff -> ident_eff
  | `Continuation -> ident_continuation
  | `Array -> ident_array
  | `List -> ident_list
  | `Option -> ident_option
  | `Nativeint -> ident_nativeint
  | `Int32 -> ident_int32
  | `Int64 -> ident_int64
  | `Lazy_t -> ident_lazy_t
  | `Extension_constructor -> ident_extension_constructor
  | `Floatarray -> ident_floatarray

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_bytes = Pident ident_bytes
and path_float = Pident ident_float
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_eff = Pident ident_eff
and path_continuation = Pident ident_continuation
and path_array = Pident ident_array
and path_list = Pident ident_list
and path_option = Pident ident_option
and path_nativeint = Pident ident_nativeint
and path_int32 = Pident ident_int32
and path_int64 = Pident ident_int64
and path_lazy_t = Pident ident_lazy_t
and path_string = Pident ident_string
and path_extension_constructor = Pident ident_extension_constructor
and path_floatarray = Pident ident_floatarray

let path_of_type_constr typ =
  Pident (ident_of_type_constr typ)

let tconstr p args = newgenty (Tconstr(p, args, ref Mnil))
let type_int = tconstr path_int []
and type_char = tconstr path_char []
and type_bytes = tconstr path_bytes []
and type_float = tconstr path_float []
and type_bool = tconstr path_bool []
and type_unit = tconstr path_unit []
and type_exn = tconstr path_exn []
and type_eff t = tconstr path_eff [t]
and type_continuation t1 t2 = tconstr path_continuation [t1; t2]
and type_array t = tconstr path_array [t]
and type_list t = tconstr path_list [t]
and type_option t = tconstr path_option [t]
and type_nativeint = tconstr path_nativeint []
and type_int32 = tconstr path_int32 []
and type_int64 = tconstr path_int64 []
and type_lazy_t t = tconstr path_lazy_t [t]
and type_string = tconstr path_string []
and type_extension_constructor = tconstr path_extension_constructor []
and type_floatarray = tconstr path_floatarray []

let find_type_constr =
  let all_predef_paths =
    all_type_constrs
    |> List.map (fun tconstr -> path_of_type_constr tconstr, tconstr)
    |> Path.Map.of_list
  in
  fun p -> Path.Map.find_opt p all_predef_paths

let ident_match_failure = ident_create "Match_failure"
and ident_out_of_memory = ident_create "Out_of_memory"
and ident_invalid_argument = ident_create "Invalid_argument"
and ident_failure = ident_create "Failure"
and ident_not_found = ident_create "Not_found"
and ident_sys_error = ident_create "Sys_error"
and ident_end_of_file = ident_create "End_of_file"
and ident_division_by_zero = ident_create "Division_by_zero"
and ident_stack_overflow = ident_create "Stack_overflow"
and ident_sys_blocked_io = ident_create "Sys_blocked_io"
and ident_assert_failure = ident_create "Assert_failure"
and ident_undefined_recursive_module =
        ident_create "Undefined_recursive_module"
and ident_continuation_already_taken = ident_create "Continuation_already_taken"

let all_predef_exns = [
  ident_match_failure;
  ident_out_of_memory;
  ident_invalid_argument;
  ident_failure;
  ident_not_found;
  ident_sys_error;
  ident_end_of_file;
  ident_division_by_zero;
  ident_stack_overflow;
  ident_sys_blocked_io;
  ident_assert_failure;
  ident_undefined_recursive_module;
  ident_continuation_already_taken;
]

let path_match_failure = Pident ident_match_failure
and path_assert_failure = Pident ident_assert_failure
and path_undefined_recursive_module = Pident ident_undefined_recursive_module

let ident_false = ident_create "false"
and ident_true = ident_create "true"
and ident_void = ident_create "()"
and ident_nil = ident_create "[]"
and ident_cons = ident_create "::"
and ident_none = ident_create "None"
and ident_some = ident_create "Some"

let decl_of_type_constr tconstr =
  let type_uid = Uid.of_predef_id (ident_of_type_constr tconstr) in
  let decl0
      ?(immediate = Type_immediacy.Unknown)
      ?(kind = Type_abstract Definition)
      ()
    =
    {type_params = [];
     type_arity = 0;
     type_kind = kind;
     type_loc = Location.none;
     type_private = Asttypes.Public;
     type_manifest = None;
     type_variance = [];
     type_separability = [];
     type_is_newtype = false;
     type_expansion_scope = lowest_level;
     type_attributes = [];
     type_immediate = immediate;
     type_unboxed_default = false;
     type_uid;
    }
  in
  let decl1
      ~variance
      ?(separability = Separability.Ind)
      ?(kind = fun _ -> Type_abstract Definition)
      ()
    =
    let param = newgenvar () in
    { (decl0 ~kind:(kind param) ()) with
      type_params = [param];
      type_arity = 1;
      type_variance = [variance];
      type_separability = [separability];
    }
  in
  let decl2
      ~variance:(var1, var2)
      ?separability:((sep1, sep2) = (Separability.Ind, Separability.Ind))
      ?(kind = fun _ _ -> Type_abstract Definition)
      ()
    =
    let param1, param2 = newgenvar (), newgenvar () in
    { (decl0 ~kind:(kind param1 param2) ()) with
      type_params = [param1; param2];
      type_arity = 2;
      type_variance = [var1; var2];
      type_separability = [sep1; sep2];
    }
  in
  let cstr id args =
    {
      cd_id = id;
      cd_args = Cstr_tuple args;
      cd_res = None;
      cd_loc = Location.none;
      cd_attributes = [];
      cd_uid = Uid.of_predef_id id;
    }
  in
  let variant constrs =
    Type_variant (constrs, Variant_regular) in
  match tconstr with
  | `Int | `Char
    -> decl0 ~immediate:Always ()
  | `String | `Bytes
  | `Float
  | `Floatarray
  | `Nativeint | `Int32 | `Int64
  | `Extension_constructor
    -> decl0 ()
  | `Bool ->
      let kind = variant [cstr ident_false [];
                          cstr ident_true []] in
      decl0 ~immediate:Always ~kind ()
  | `Unit ->
      let kind = variant [cstr ident_void []] in
      decl0 ~immediate:Always ~kind ()
  | `Exn -> decl0 ~kind:Type_open ()
  | `Eff ->
      let kind _ = Type_open in
      decl1 ~variance:Variance.full ~kind ()
  | `Continuation ->
      let variance = Variance.(contravariant, covariant) in
      decl2 ~variance ()
  | `Array ->
      decl1 ~variance:Variance.full ()
  | `List ->
      let kind tvar =
        variant [cstr ident_nil [];
                 cstr ident_cons [tvar; type_list tvar]] in
      decl1 ~variance:Variance.covariant ~kind ()
  | `Option ->
      let kind tvar =
        variant [cstr ident_none [];
                 cstr ident_some [tvar]] in
      decl1 ~variance:Variance.covariant ~kind ()
  | `Lazy_t -> decl1 ~variance:Variance.covariant ()

let build_initial_env add_type add_extension empty_env =
  let add_extension id l =
    add_extension id
      { ext_type_path = path_exn;
        ext_type_params = [];
        ext_args = Cstr_tuple l;
        ext_ret_type = None;
        ext_private = Asttypes.Public;
        ext_loc = Location.none;
        ext_attributes = [Ast_helper.Attr.mk
                            (Location.mknoloc "ocaml.warn_on_literal_pattern")
                            (Parsetree.PStr [])];
        ext_uid = Uid.of_predef_id id;
      }
  in
  List.fold_left (fun env tconstr ->
    add_type (ident_of_type_constr tconstr) (decl_of_type_constr tconstr) env
  ) empty_env all_type_constrs
  (* Predefined exceptions - alphabetical order *)
  |> add_extension ident_assert_failure
       [newgenty (Ttuple[type_string; type_int; type_int])]
  |> add_extension ident_division_by_zero []
  |> add_extension ident_end_of_file []
  |> add_extension ident_failure [type_string]
  |> add_extension ident_invalid_argument [type_string]
  |> add_extension ident_match_failure
       [newgenty (Ttuple[type_string; type_int; type_int])]
  |> add_extension ident_not_found []
  |> add_extension ident_out_of_memory []
  |> add_extension ident_stack_overflow []
  |> add_extension ident_sys_blocked_io []
  |> add_extension ident_sys_error [type_string]
  |> add_extension ident_undefined_recursive_module
       [newgenty (Ttuple[type_string; type_int; type_int])]
  |> add_extension ident_continuation_already_taken []

let builtin_values =
  List.map (fun id -> (Ident.name id, id)) all_predef_exns

let builtin_idents = List.rev !builtin_idents
