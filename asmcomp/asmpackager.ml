(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

open Misc
open Cmx_format

type error =
    Illegal_renaming of string * string * string
  | Forward_reference of string * string
  | Wrong_for_pack of string * string
  | Linking_error
  | Assembler_error of string
  | File_not_found of string


exception Error of error

(* Read the unit information from a .cmx file. *)

type pack_member_kind = PM_intf | PM_impl of unit_infos

type pack_member =
  { pm_file: string;
    pm_name: string;
    pm_kind: pack_member_kind }

let read_member_info pack_path file = (
  let unit_info = Unit_info.Artifact.from_filename file in
  let name = Unit_info.Artifact.modname unit_info in
  let kind =
    if Unit_info.is_cmi unit_info  then
      PM_intf
    else begin
      let (info, crc) = Compilenv.read_unit_info file in
      if info.ui_name <> name
      then raise(Error(Illegal_renaming(name, file, info.ui_name)));
      let expected_symbol =
        Printf.sprintf "%s%c%s"
          (Compilenv.current_unit_infos()).ui_symbol Compilenv.symbol_separator
          info.ui_name
      in
      if info.ui_symbol <> expected_symbol
      then raise(Error(Wrong_for_pack(file, pack_path)));
      Asmlink.check_consistency file info crc;
      Compilenv.cache_unit_info info;
      PM_impl info
    end in
  { pm_file = file; pm_name = name; pm_kind = kind }
)

(* Check absence of forward references *)

let check_units members =
  let rec check forbidden = function
    [] -> ()
  | mb :: tl ->
      begin match mb.pm_kind with
      | PM_intf -> ()
      | PM_impl infos ->
          List.iter
            (fun (unit, _) ->
              if List.mem unit forbidden
              then raise(Error(Forward_reference(mb.pm_file, unit))))
            infos.ui_imports_cmx
      end;
      check (list_remove mb.pm_name forbidden) tl in
  check (List.map (fun mb -> mb.pm_name) members) members

(* Make the .o file for the package *)

let make_package_object ~ppf_dump members target coercion ~backend =
  let pack_name =
    Printf.sprintf "pack(%s)" (Unit_info.Artifact.modname target) in
  Profile.record_call pack_name (fun () ->
    let objtemp =
      if !Clflags.keep_asm_file
      then Unit_info.Artifact.prefix target ^ ".pack" ^ Config.ext_obj
      else
        (* Put the full name of the module in the temporary file name
           to avoid collisions with MSVC's link /lib in case of successive
           packs *)
        Filename.temp_file (Compilenv.make_symbol (Some "")) Config.ext_obj in
    let components =
      List.map
        (fun m ->
          match m.pm_kind with
          | PM_intf -> None
          | PM_impl _ -> Some(Ident.create_persistent m.pm_name))
        members in
    let module_ident =
      Ident.create_persistent @@ Unit_info.Artifact.modname target in
    let prefixname = Filename.remove_extension objtemp in
    let required_globals = Ident.Set.empty in
    let program, middle_end =
      if Config.flambda then
        let main_module_block_size, code =
          Translmod.transl_package_flambda components coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Flambda_middle_end.lambda_to_clambda
      else
        let main_module_block_size, code =
          Translmod.transl_store_package components module_ident coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Closure_middle_end.lambda_to_clambda
    in
    Asmgen.compile_implementation ~backend
      ~prefixname
      ~middle_end
      ~ppf_dump
      program;
    let objfiles =
      List.map
        (fun m -> Filename.remove_extension m.pm_file ^ Config.ext_obj)
        (List.filter (fun m -> m.pm_kind <> PM_intf) members) in
    let exitcode =
      Ccomp.call_linker Ccomp.Partial (Unit_info.Artifact.filename target)
        (objtemp :: objfiles) ""
    in
    remove_file objtemp;
    if not (exitcode = 0) then raise(Error Linking_error)
  )

(* Make the .cmx file for the package *)

let get_export_info ui =
  assert(Config.flambda);
  match ui.ui_export_info with
  | Clambda _ -> assert false
  | Flambda info -> info

let get_approx ui =
  assert(not Config.flambda);
  match ui.ui_export_info with
  | Flambda _ -> assert false
  | Clambda info -> info

let build_package_cmx members cmxfile =
  let unit_names =
    List.map (fun m -> m.pm_name) members in
  let filter lst =
    List.filter (fun (name, _crc) -> not (List.mem name unit_names)) lst in
  let union lst =
    List.fold_left
      (List.fold_left
          (fun accu n -> if List.mem n accu then accu else n :: accu))
      [] lst in
  let units =
    List.fold_right
      (fun m accu ->
        match m.pm_kind with PM_intf -> accu | PM_impl info -> info :: accu)
      members [] in
  let pack_units =
    List.fold_left
      (fun set info ->
         let unit_id = Compilenv.unit_id_from_name info.ui_name in
         Compilation_unit.Set.add
           (Compilenv.unit_for_global unit_id) set)
      Compilation_unit.Set.empty units in
  let units =
    if Config.flambda then
      List.map (fun info ->
          { info with
            ui_export_info =
              Flambda
                (Export_info_for_pack.import_for_pack ~pack_units
                   ~pack:(Compilenv.current_unit ())
                   (get_export_info info)) })
        units
    else
      units
  in
  let ui = Compilenv.current_unit_infos() in
  let ui_export_info =
    if Config.flambda then
      let ui_export_info =
        List.fold_left (fun acc info ->
            Export_info.merge acc (get_export_info info))
          (Export_info_for_pack.import_for_pack ~pack_units
             ~pack:(Compilenv.current_unit ())
             (get_export_info ui))
          units
      in
      Flambda ui_export_info
    else
      Clambda (get_approx ui)
  in
  Export_info_for_pack.clear_import_state ();
  let pkg_infos =
    { ui_name = ui.ui_name;
      ui_symbol = ui.ui_symbol;
      ui_defines =
          List.flatten (List.map (fun info -> info.ui_defines) units) @
          [ui.ui_symbol];
      ui_imports_cmi =
          (ui.ui_name, Some (Env.crc_of_unit ui.ui_name)) ::
          filter(Asmlink.extract_crc_interfaces());
      ui_imports_cmx =
          filter(Asmlink.extract_crc_implementations());
      ui_curry_fun =
          union(List.map (fun info -> info.ui_curry_fun) units);
      ui_apply_fun =
          union(List.map (fun info -> info.ui_apply_fun) units);
      ui_send_fun =
          union(List.map (fun info -> info.ui_send_fun) units);
      ui_force_link =
          List.exists (fun info -> info.ui_force_link) units;
      ui_export_info;
      ui_for_pack = None;
    } in
  Compilenv.write_unit_info pkg_infos cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files ~ppf_dump files target targetcmx coercion ~backend =
  let pack_path =
    match !Clflags.for_package with
    | None -> Unit_info.Artifact.modname target
    | Some p -> p ^ "." ^ Unit_info.Artifact.modname target in
  let members = map_left_right (read_member_info pack_path) files in
  check_units members;
  make_package_object ~ppf_dump members target coercion ~backend;
  build_package_cmx members targetcmx

(* The entry point *)

let package_files ~ppf_dump initial_env files targetcmx ~backend =
  let files =
    List.map
      (fun f ->
        try Load_path.find f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let cmx = Unit_info.Artifact.from_filename targetcmx in
  let cmi = Unit_info.companion_cmi cmx in
  let obj = Unit_info.companion_obj cmx in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
  Compilenv.reset ?packname:!Clflags.for_package
    (Unit_info.Artifact.modname cmi);
  Misc.try_finally (fun () ->
      let coercion = Typemod.package_units initial_env files cmi in
      package_object_files ~ppf_dump files obj targetcmx coercion ~backend
    )
    ~exceptionally:(fun () ->
        remove_file targetcmx; remove_file (Unit_info.Artifact.filename obj)
      )

(* Error report *)

open Format_doc
module Style = Misc.Style

let report_error_doc ppf = function
    Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        Location.Doc.quoted_filename file
        Style.inline_code name Style.inline_code id
  | Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %a in file %a" Style.inline_code ident
        Location.Doc.quoted_filename file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the %a option"
        Location.Doc.quoted_filename file
        Style.inline_code ("-for-pack " ^ path)
  | File_not_found file ->
      fprintf ppf "File %a not found" Style.inline_code file
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a" Style.inline_code file
  | Linking_error ->
      fprintf ppf "Error during partial linking"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
