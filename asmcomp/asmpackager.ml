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
  | Module_compiled_without_lto of string


exception Error of error

(* Read the unit information from a .cmx file. *)

type pack_member_kind = PM_intf | PM_impl of unit_infos

type pack_member =
  { pm_file: string;
    pm_name: string;
    pm_kind: pack_member_kind }

let read_member_info pack_path file = (
  let name =
    String.capitalize_ascii(Filename.basename(chop_extensions file)) in
  let kind =
    if Filename.check_suffix file ".cmx" then begin
      let (info, crc) = Compilenv.read_unit_info file in
      if info.ui_name <> name
      then raise(Error(Illegal_renaming(name, file, info.ui_name)));
      if info.ui_symbol <>
         (Compilenv.current_unit_infos()).ui_symbol ^ "__" ^ info.ui_name
      then raise(Error(Wrong_for_pack(file, pack_path)));
      Asmlink.check_consistency file info crc;
      Compilenv.cache_unit_info info;
      PM_impl info
    end else
      PM_intf in
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

let make_package_object ppf members targetobj targetname coercion
      ~backend =
  let objtemp =
    if !Clflags.keep_asm_file
    then Filename.remove_extension targetobj ^ ".pack" ^ Config.ext_obj
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
  let module_ident = Ident.create_persistent targetname in
  let source_provenance = Timings.Pack targetname in
  let prefixname = Filename.remove_extension objtemp in
  if Config.flambda then begin
    let size, lam = Translmod.transl_package_flambda components coercion in
    let flam =
      Middle_end.middle_end ppf
        ~source_provenance
        ~prefixname
        ~backend
        ~size
        ~filename:targetname
        ~module_ident
        ~module_initializer:lam
    in
    Asmgen.compile_implementation_flambda ~source_provenance
      prefixname ~backend ~required_globals:Ident.Set.empty ppf flam;
  end else begin
    let main_module_block_size, code =
      Translmod.transl_store_package
        components (Ident.create_persistent targetname) coercion in
    Asmgen.compile_implementation_clambda ~source_provenance
      prefixname ppf { Lambda.code; main_module_block_size;
                       module_ident; required_globals = Ident.Set.empty }
  end;
  let objfiles =
    List.map
      (fun m -> Filename.remove_extension m.pm_file ^ Config.ext_obj)
      (List.filter (fun m -> m.pm_kind <> PM_intf) members) in
  let ok =
    Ccomp.call_linker Ccomp.Partial targetobj (objtemp :: objfiles) ""
  in
  remove_file objtemp;
  if not ok then raise(Error Linking_error)

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

let get_flambda_codes pack_units =
  assert(Config.flambda);
  List.map (fun info ->
    match info.Cmx_format.ui_export_info with
    | Clambda _ -> assert false
    | Flambda { Export_info.code } ->
      match code with
      | None ->
        raise (Error (Module_compiled_without_lto info.Cmx_format.ui_name))
      | Some code ->
        code)
    pack_units

let build_package_cmx ppf members cmxfile =
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
      let current_ui_export_info =
        if !Clflags.cmx_contains_all_code then begin
          let codes = get_flambda_codes units in
          let current_unit_code =
            match (get_export_info ui).Export_info.code with
            | None -> assert false (* We compiled with -lto *)
            | Some code -> code
          in
          let program =
            (* Only the pack symbol is directly accessible from outside *)
            Flambda_utils.clear_all_exported_symbols
              (Flambda_utils.concatenate codes)
          in
          let program =
            Flambda_utils.replace_compilation_unit_of_symbols
              (Compilenv.current_unit ())
              (Flambda_utils.concatenate [program; current_unit_code])
          in
          if !Clflags.dump_rawflambda then
            Format.fprintf ppf "After concatenation:@ %a@."
              Flambda.print_program program;
          let cleaned_program =
            Remove_unused_program_constructs.remove_unused_program_constructs
              program
          in
          if !Clflags.dump_flambda then
            Format.fprintf ppf "After cleaning:@ %a@."
              Flambda.print_program cleaned_program;
          Export_info.merge
            (Export_info.empty_with_code ~code:(Some cleaned_program))
            (get_export_info ui)
        end
        else
          get_export_info ui
      in
      let ui_export_info =
        List.fold_left (fun acc info ->
            Export_info.merge acc (get_export_info info))
          (Export_info_for_pack.import_for_pack ~pack_units
             ~pack:(Compilenv.current_unit ())
             current_ui_export_info)
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
    } in
  Compilenv.write_unit_info pkg_infos cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files ppf files targetcmx
                         targetobj targetname coercion ~backend =
  let pack_path =
    match !Clflags.for_package with
    | None -> targetname
    | Some p -> p ^ "." ^ targetname in
  let members = map_left_right (read_member_info pack_path) files in
  check_units members;
  make_package_object ppf members targetobj targetname coercion ~backend;
  build_package_cmx ppf members targetcmx

(* The entry point *)

let package_files ppf initial_env files targetcmx ~backend =
  let files =
    List.map
      (fun f ->
        try find_in_path !Config.load_path f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extensions targetcmx in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = Filename.remove_extension targetcmx ^ Config.ext_obj in
  let targetname = String.capitalize_ascii(Filename.basename prefix) in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
  Compilenv.reset ~source_provenance:(Timings.Pack targetname)
    ?packname:!Clflags.for_package targetname;
  try
    let coercion =
      Typemod.package_units initial_env files targetcmi targetname in
    package_object_files ppf files targetcmx targetobj targetname coercion
      ~backend
  with x ->
    remove_file targetcmx; remove_file targetobj;
    raise x

(* Error report *)

open Format

let report_error ppf = function
    Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %s when %s was expected"
        Location.print_filename file name id
  | Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %s in file %a" ident
        Location.print_filename file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the `-for-pack %s' option"
              Location.print_filename file path
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Assembler_error file ->
      fprintf ppf "Error while assembling %s" file
  | Linking_error ->
      fprintf ppf "Error during partial linking"
  | Module_compiled_without_lto name ->
      fprintf ppf
        "@[<hov>Modules %s@ was compiled without the `-lto` option.@ \
         It is needed for building a pack with the `-lto` option.@]"
        name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
