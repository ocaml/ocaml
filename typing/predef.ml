(* Predefined type constructors (with special typing rules in typecore) *)

open Misc
open Path
open Typedtree


let ident_int = Ident.new "int"
and ident_char = Ident.new "char"
and ident_string = Ident.new "string"
and ident_float = Ident.new "float"
and ident_bool = Ident.new "bool"
and ident_unit = Ident.new "unit"
and ident_exn = Ident.new "exn"
and ident_array = Ident.new "array"
and ident_list = Ident.new "list"
and ident_format = Ident.new "format"

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_string = Pident ident_string
and path_float = Pident ident_float
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_array = Pident ident_array
and path_list = Pident ident_list
and path_format = Pident ident_format

let type_int = Tconstr(Pident ident_int, [])
and type_char = Tconstr(Pident ident_char, [])
and type_string = Tconstr(Pident ident_string, [])
and type_float = Tconstr(Pident ident_float, [])
and type_bool = Tconstr(Pident ident_bool, [])
and type_unit = Tconstr(Pident ident_unit, [])
and type_exn = Tconstr(Pident ident_exn, [])
and type_array t = Tconstr(path_array, [t])
and type_list t = Tconstr(path_list, [t])

let ident_match_failure = Ident.new "Match_failure"
and ident_out_of_memory = Ident.new "Out_of_memory"
and ident_invalid_argument = Ident.new "Invalid_argument"
and ident_failure = Ident.new "Failure"
and ident_not_found = Ident.new "Not_found"
and ident_sys_error = Ident.new "Sys_error"
and ident_end_of_file = Ident.new "End_of_file"
and ident_division_by_zero = Ident.new "Division_by_zero"

let path_match_failure = Pident ident_match_failure

let build_initial_env add_type add_exception empty_env =
  let newvar() =
    (* Cannot call Ctype.newvar here because ctype imports predef via env *)
    Tvar{tvar_level = -1 (*generic_level*); tvar_link = None} in
  let decl_abstr =
    {type_params = []; type_arity = 0; type_kind = Type_abstract}
  and decl_bool =
    {type_params = []; type_arity = 0;
     type_kind = Type_variant["false",[]; "true",[]]}
  and decl_unit =
    {type_params = []; type_arity = 0; type_kind = Type_variant["()",[]]}
  and decl_exn =
    {type_params = []; type_arity = 0; type_kind = Type_variant[]}
  and decl_array =
    let tvar = newvar() in
    {type_params = [tvar]; type_arity = 1; type_kind = Type_abstract}
  and decl_list =
    let tvar = newvar() in
    {type_params = [tvar]; type_arity = 1;
     type_kind = Type_variant["[]", []; "::", [tvar; type_list tvar]]}
  and decl_format =
    {type_params = [newvar(); newvar(); newvar()]; type_arity = 3;
     type_kind = Type_abstract} in
  add_exception ident_match_failure [type_string; type_int; type_int] (
  add_exception ident_out_of_memory [] (
  add_exception ident_invalid_argument [type_string] (
  add_exception ident_failure [type_string] (
  add_exception ident_not_found [] (
  add_exception ident_sys_error [type_string] (
  add_exception ident_end_of_file [] (
  add_exception ident_division_by_zero [] (
  add_type ident_format decl_format (
  add_type ident_list decl_list (
  add_type ident_array decl_array (
  add_type ident_exn decl_exn (
  add_type ident_unit decl_unit (
  add_type ident_bool decl_bool (
  add_type ident_float decl_abstr (
  add_type ident_string decl_abstr (
  add_type ident_char decl_abstr (
  add_type ident_int decl_abstr (
    empty_env))))))))))))))))))

let builtin_values =
  List.map (fun id -> Ident.make_global id; (Ident.name id, id))
      [ident_match_failure; ident_out_of_memory; ident_invalid_argument;
       ident_failure; ident_not_found; ident_sys_error; ident_end_of_file;
       ident_division_by_zero]
