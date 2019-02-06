(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016, 2019 Jane Street Group LLC                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation environments for compilation units *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Config
open Misc
open Cmx_format

type error =
  | Not_a_unit_info of { filename : string; }
  | Corrupted_unit_info of { filename : string; }
  | Illegal_renaming of { name : string; modname : string; filename : string; }

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)
let export_infos_table =
  (Hashtbl.create 10 : (string, Export_info.t) Hashtbl.t)

let imported_sets_of_closures_table =
  (Set_of_closures_id.Tbl.create 10
   : Simple_value_approx.function_declarations option
       Set_of_closures_id.Tbl.t)

module CstMap =
  Map.Make(struct
    type t = Closure.ustructured_constant
    let compare = Closure.compare_structured_constants
    (* PR#6442: it is incorrect to use Stdlib.compare on values of type t
       because it compares "0.0" and "-0.0" equal. *)
  end)

type structured_constants =
  { strcst_shared: Symbol.t CstMap.t;
    strcst_all: (Symbol.t * Closure.ustructured_constant) list;
  }

let structured_constants_empty  =
  { strcst_shared = CstMap.empty;
    strcst_all = [];
  }

let structured_constants = ref structured_constants_empty

let default_ui_export_info =
  if Config.flambda then
    Cmx_format.Flambda Export_info.empty
  else
    Cmx_format.Closure Value_unknown

let current_unit =
  { ui_name = Compilation_unit.startup;
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_export_info = default_ui_export_info;
  }

let unit_id_from_name name = Ident.create_persistent name

let reset comp_unit =
  Hashtbl.clear global_infos_table;
  Set_of_closures_id.Tbl.clear imported_sets_of_closures_table;
  current_unit.ui_name <- comp_unit;
  current_unit.ui_defines <- [comp_unit];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- !Clflags.link_everything;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_export_info <- default_ui_export_info;
  merged_environment := Export_info.empty;
  Hashtbl.clear export_infos_table;
  Compilation_unit.set_current compilation_unit

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

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

let get_global_info global_ident =
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque modname then (None, None)
        else begin
          try
            let filename =
              find_in_path_uncap !load_path (modname ^ ".cmx") in
            let (ui, crc) = read_unit_info filename in
            if ui.ui_name <> modname then
              raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
            (Some ui, Some crc)
          with Not_found ->
            let warn = Warnings.No_cmx_file modname in
              Location.prerr_warning Location.none warn;
              (None, None)
          end
      in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

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

module Closure_only = struct
  let exported_constants = Hashtbl.create 17

  let add_exported_constant s =
    assert (not Config.flambda);
    Hashtbl.replace exported_constants s ()

  let toplevel_approx :
    (string, Closure.value_approximation) Hashtbl.t = Hashtbl.create 16

  let record_global_approx_toplevel () =
    assert (not Config.flambda);
    Hashtbl.add toplevel_approx current_unit.ui_name
      (get_clambda_approx current_unit)

  let global_approx id =
    assert (not Config.flambda);
    if Ident.is_predef id then Closure.Value_unknown
    else try Hashtbl.find toplevel_approx (Ident.name id)
    with Not_found ->
      match get_global_info id with
        | None -> Closure.Value_unknown
        | Some ui -> get_clambda_approx ui

  (* Register the approximation of the module being compiled *)

  let set_global_approx approx =
    assert (not Config.flambda);
    current_unit.ui_export_info <- Closure approx

  (* Return the approximation of a global identifier *)

  let get_clambda_approx ui =
    assert (not Config.flambda);
    match ui.ui_export_info with
    | Flambda _ ->
      Misc.fatal_error "Expected Closure export information but found Flambda"
    | Closure approx -> approx

  let snapshot () =
    assert (not Config.flambda);
    !structured_constants

  let backtrack s =
    assert (not Config.flambda);
    structured_constants := s
end

module Flambda_only = struct
  let merged_environment = ref Export_info.empty

  let get_flambda_export_info ui =
    assert Config.flambda;
    match ui.ui_export_info with
    | Closure _ -> assert false
    | Flambda ei -> ei

  let set_export_info export_info =
    assert Config.flambda;
    current_unit.ui_export_info <- Flambda export_info

  let approx_for_global comp_unit =
    assert Config.flambda;
    let modname = Compilation_unit.name comp_unit in
    match Hashtbl.find export_infos_table modname with
    | otherwise -> Some otherwise
    | exception Not_found ->
      match get_global_info id with
      | None -> None
      | Some ui ->
        let exported = get_flambda_export_info ui in
        Hashtbl.add export_infos_table modname exported;
        merged_environment := Export_info.merge !merged_environment exported;
        Some exported

  let approx_env () =
    assert Config.flambda;
    !merged_environment
end

let new_structured_constant cst ~shared =
  let {strcst_shared; strcst_all} = !structured_constants in
  if shared then
    try
      CstMap.find cst strcst_shared
    with Not_found ->
      let sym = Symbol.create_constant_data () in
      structured_constants :=
        {
          strcst_shared = CstMap.add cst sym strcst_shared;
          strcst_all = (sym, cst) :: strcst_all;
        };
      lbl
  else
    let sym = new_const_symbol() in
    structured_constants :=
      {
        strcst_shared;
        strcst_all = (sym, cst) :: strcst_all;
      };
    sym

let clear_structured_constants () =
  structured_constants := structured_constants_empty

let structured_constants () =
  List.map
    (fun (symbol, definition) ->
       { Closure.
         symbol;
         exported = Hashtbl.mem exported_constants symbol;
         definition;
       })
    (!structured_constants).strcst_all

let require_global global_ident =
  if not (Ident.is_predef global_ident) then
    ignore (get_global_info global_ident : Cmx_format.unit_infos option)

(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info { filename; } ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info { filename; } ->
      fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming { name; modname; filename; } ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %s when %s was expected"
        Location.print_filename filename name modname

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
