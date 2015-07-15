(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Compilation environments for compilation units *)

open Config
open Misc
open Clambda
open Cmx_format
open Ext_types

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)
let export_infos_table =
  (Hashtbl.create 10 : (string, Flambdaexport_types.exported) Hashtbl.t)

let imported_closure_table =
  (Set_of_closures_id.Tbl.create 10
   : Flambda.function_declarations Set_of_closures_id.Tbl.t)

module CstMap =
  Map.Make(struct
    type t = Clambda.ustructured_constant
    let compare = Clambda.compare_structured_constants
    (* PR#6442: it is incorrect to use Pervasives.compare on values of type t
       because it compares "0.0" and "-0.0" equal. *)
  end)

type structured_constants =
  {
    strcst_shared: string CstMap.t;
    strcst_original: string StringMap.t;
    strcst_alias: string list StringMap.t;
    strcst_all: (string * Clambda.ustructured_constant) list;
  }

let structured_constants_empty  =
  {
    strcst_shared = CstMap.empty;
    strcst_original = StringMap.empty;
    strcst_alias = StringMap.empty;
    strcst_all = [];
  }

let structured_constants = ref structured_constants_empty


let exported_constants = Hashtbl.create 17

let current_unit_id = ref (Ident.create_persistent "___UNINITIALIZED___")
let merged_environment = ref Flambdaexport.empty_export

let current_unit =
  { ui_name = "";
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_approx = Value_unknown;
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_export_info = Flambdaexport.empty_export }

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p ->
      let b = Buffer.create 64 in
      for i = 0 to String.length p - 1 do
        match p.[i] with
        | '.' -> Buffer.add_string b "__"
        |  c  -> Buffer.add_char b c
      done;
      Buffer.add_string b "__";
      Buffer.add_string b name;
      Buffer.contents b

let unit_id_from_name name = Ident.create_persistent name

let make_symbol ?(unitname = current_unit.ui_symbol) idopt =
  let prefix = "caml" ^ unitname in
  match idopt with
  | None -> prefix
  | Some id -> prefix ^ "__" ^ id

let current_unit_linkage_name () =
  Linkage_name.create (make_symbol ~unitname:current_unit.ui_symbol None)

let reset ?packname name =
  Hashtbl.clear global_infos_table;
  Set_of_closures_id.Tbl.clear imported_closure_table;
  let symbol = symbolname_for_pack packname name in
  current_unit_id := unit_id_from_name name;
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- false;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_export_info <- Flambdaexport.empty_export;
  merged_environment := Flambdaexport.empty_export;
  Hashtbl.clear export_infos_table;
  let compilation_unit =
    Compilation_unit.create
      !current_unit_id
      (current_unit_linkage_name ())
  in
  Compilation_unit.set_current compilation_unit

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

let current_unit_id () = !current_unit_id

let symbol_in_current_unit name =
  let prefix = "caml" ^ current_unit.ui_symbol in
  name = prefix ||
  (let lp = String.length prefix in
   String.length name >= 2 + lp
   && String.sub name 0 lp = prefix
   && name.[lp] = '_'
   && name.[lp + 1] = '_')

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

let get_global_info global_ident = (
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        try
          let filename =
            find_in_path_uncap !load_path (modname ^ ".cmx") in
          let (ui, crc) = read_unit_info filename in
          if ui.ui_name <> modname then
            raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
          (Some ui, Some crc)
        with Not_found ->
          (None, None) in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end
)

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let toplevel_approx = Hashtbl.create 16

let record_global_approx_toplevel id =
  Hashtbl.add toplevel_approx current_unit.ui_name current_unit.ui_approx

let global_approx id =
  if Ident.is_predef_exn id then Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Value_unknown
      | Some ui -> ui.ui_approx

let get_unit_name id =
  match get_global_info id with
  | None -> Ident.name id
  | Some ui -> ui.ui_symbol

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef_exn id then
    "caml_exn_" ^ Ident.name id
  else begin
    match get_global_info id with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.ui_symbol None
  end

let unit_for_global id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  Compilation_unit.create id sym_label

let predefined_exception_compilation_unit =
  Compilation_unit.create (Ident.create_persistent "__dummy__")
    (Linkage_name.create "__dummy__")

let is_predefined_exception sym =
  Compilation_unit.equal
    predefined_exception_compilation_unit
    (Symbol.compilation_unit sym)

let symbol_for_global' id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  if Ident.is_predef_exn id then
    Symbol.create predefined_exception_compilation_unit sym_label
  else
    Symbol.create (unit_for_global id) sym_label

(* Register the approximation of the module being compiled *)

let set_global_approx approx =
  current_unit.ui_approx <- approx

(* Exporting and importing cross module information *)

let set_export_info export_info =
  current_unit.ui_export_info <- export_info

let approx_for_global comp_unit =
  let id = Compilation_unit.get_persistent_ident comp_unit in
  if (Compilation_unit.equal
      predefined_exception_compilation_unit
      comp_unit)
     || Ident.is_predef_exn id
     || not (Ident.global id)
  then invalid_arg (Format.asprintf "approx_for_global %a" Ident.print id);
  let modname = Ident.name id in
  try Hashtbl.find export_infos_table modname with
  | Not_found ->
    let exported = match get_global_info id with
      | None -> Flambdaexport.empty_export
      | Some ui -> ui.ui_export_info in
    Hashtbl.add export_infos_table modname exported;
    merged_environment := Flambdaexport.merge !merged_environment exported;
    exported

let approx_env () = !merged_environment

(* Record that a currying function or application function is needed *)

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  assert(n > 0);
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

let need_send_fun n =
  if not (List.mem n current_unit.ui_send_fun) then
    current_unit.ui_send_fun <- n :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imports();
  write_unit_info current_unit filename

let current_unit_linkage_name () =
  Linkage_name.create (make_symbol ~unitname:current_unit.ui_symbol None)

let current_unit () =
  match Compilation_unit.get_current () with
  | Some current_unit -> current_unit
  | None -> Misc.fatal_error "Compilenv.current_unit"

let current_unit_symbol () =
  Symbol.create (current_unit ()) (current_unit_linkage_name ())

let const_label = ref 0

let new_const_label () =
  incr const_label;
  !const_label

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (string_of_int !const_label))

let snapshot () = !structured_constants
let backtrack s = structured_constants := s

let add_structured_constant lbl cst ~shared =
  let {strcst_shared; strcst_original; strcst_alias; strcst_all} = !structured_constants in
  let res, name =
    if shared then
      if CstMap.mem cst strcst_shared
      then
        let name = CstMap.find cst strcst_shared in
        let aliases =
          try StringMap.find name strcst_alias
          with Not_found -> []
        in
        {
          strcst_shared;
          strcst_all;
          strcst_original = StringMap.add lbl name strcst_original;
          strcst_alias = StringMap.add name (lbl::aliases) strcst_alias;
        }, name
      else
        {
          strcst_shared = CstMap.add cst lbl strcst_shared;
          strcst_original;
          strcst_all = (lbl, cst) :: strcst_all;
          strcst_alias;
        }, lbl
    else
      {
        strcst_shared;
        strcst_original;
        strcst_all = (lbl, cst) :: strcst_all;
        strcst_alias;
      }, lbl
  in
  structured_constants := res;
  name

let canonical_symbol lbl =
  try StringMap.find lbl (!structured_constants).strcst_original
  with Not_found -> lbl

let new_structured_constant cst ~shared =
  let {strcst_shared; strcst_original; strcst_alias; strcst_all} = !structured_constants in
  if shared then
    try
      let l = CstMap.find cst strcst_shared in
      l
    with Not_found ->
      let lbl = new_const_symbol() in
      structured_constants :=
        {
          strcst_shared = CstMap.add cst lbl strcst_shared;
          strcst_original;
          strcst_all = (lbl, cst) :: strcst_all;
          strcst_alias;
        };
      lbl
  else
    let lbl = new_const_symbol() in
    structured_constants :=
      {
        strcst_shared;
        strcst_original;
        strcst_all = (lbl, cst) :: strcst_all;
        strcst_alias;
      };
    lbl
let clear_structured_constants () =
  structured_constants := structured_constants_empty

let add_exported_constant s =
  Hashtbl.replace exported_constants s ()

let structured_constants () =
  let structured_constants = !structured_constants in
  List.map
    (fun (lbl, cst) ->
       let symbols =
         let aliases =
           lbl ::
           try StringMap.find lbl structured_constants.strcst_alias
           with Not_found -> [] in
         List.map
           (fun lbl -> lbl, Hashtbl.mem exported_constants lbl)
           aliases
       in
       (symbols, cst)
    ) structured_constants.strcst_all

let new_const_symbol' () =
  Symbol.create (current_unit ()) (Linkage_name.create (new_const_symbol ()))

let concat_symbol unitname id =
  unitname ^ "__" ^ id

let closure_symbol fv =
  let compilation_unit = Closure_id.get_compilation_unit fv in
  let unitname =
    Linkage_name.to_string (Compilation_unit.get_linkage_name compilation_unit)
  in
  let linkage_name =
    concat_symbol unitname ((Closure_id.unique_name fv) ^ "_closure")
  in
  Symbol.create compilation_unit (Linkage_name.create linkage_name)

let function_label fv =
  let compilation_unit = Closure_id.get_compilation_unit fv in
  let unitname =
    Linkage_name.to_string
      (Compilation_unit.get_linkage_name compilation_unit)
  in
  (concat_symbol unitname (Closure_id.unique_name fv))

let imported_closure =
  let open Flambda in
  let import_closure clos =

    let orig_var_map clos =
      Variable.Map.fold
        (fun id _ acc ->
           let fun_id = Closure_id.wrap id in
           let sym = closure_symbol fun_id in
           Symbol.Map.add sym id acc)
        clos.funs Symbol.Map.empty in

    let sym_map = orig_var_map clos in

    let f expr = expr in
    let f_named (named : Flambda.named) =
      match named with
      | Symbol sym ->
          (try Expr(Var(Symbol.Map.find sym sym_map)) with
           | Not_found -> named)
      | named -> named
    in

    { clos with
      funs =
        Variable.Map.map
          (fun (ff : Flambda.function_declaration) ->
             let body = Flambda_iterators.map_toplevel f f_named ff.body in
             let free_variables = Free_variables.calculate body in
             { ff with body; free_variables })
          clos.funs } in
  let aux fun_id =
    let ex_info = approx_env () in
    let closure = Set_of_closures_id.Map.find fun_id ex_info.Flambdaexport_types.ex_functions in
    let cl = import_closure closure in
    cl
  in
  Set_of_closures_id.Tbl.memoize imported_closure_table aux

(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %s when %s was expected"
        Location.print_filename filename name modname

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
