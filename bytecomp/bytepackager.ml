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

(* "Package" a set of .cmo files into one .cmo file having the
   original compilation units as sub-modules. *)

open Misc
open Instruct
open Cmo_format
module String = Misc.Stdlib.String

type error =
    Forward_reference of string * Ident.t
  | Multiple_definition of string * Ident.t
  | Not_an_object_file of string
  | Illegal_renaming of string * string * string
  | File_not_found of string

exception Error of error

(* References accumulating information on the .cmo files *)

let relocs = ref ([] : (reloc_info * int) list)
let events = ref ([] : debug_event list)
let debug_dirs = ref String.Set.empty
let primitives = ref ([] : string list)
let force_link = ref false

(* Update a relocation.  adjust its offset, and rename GETGLOBAL and
   SETGLOBAL relocations that correspond to one of the units being
   consolidated. *)

let rename_relocation packagename objfile mapping defined base (rel, ofs) =
  let rel' =
    match rel with
      Reloc_getglobal id ->
        begin try
          let id' = List.assoc id mapping in
          if List.mem id defined
          then Reloc_getglobal id'
          else raise(Error(Forward_reference(objfile, id)))
        with Not_found ->
          (* PR#5276: unique-ize dotted global names, which appear
             if one of the units being consolidated is itself a packed
             module. *)
          let name = Ident.name id in
          if String.contains name '.' then
            Reloc_getglobal (Ident.create_persistent (packagename ^ "." ^ name))
          else
            rel
        end
    | Reloc_setglobal id ->
        begin try
          let id' = List.assoc id mapping in
          if List.mem id defined
          then raise(Error(Multiple_definition(objfile, id)))
          else Reloc_setglobal id'
        with Not_found ->
          (* PR#5276, as above *)
          let name = Ident.name id in
          if String.contains name '.' then
            Reloc_setglobal (Ident.create_persistent (packagename ^ "." ^ name))
          else
            rel
        end
    | _ ->
        rel in
  (rel', base + ofs)

(* relocate a debugging event *)

let relocate_debug base prefix subst ev =
  { ev with ev_pos = base + ev.ev_pos;
            ev_module = prefix ^ "." ^ ev.ev_module;
            ev_typsubst = Subst.compose ev.ev_typsubst subst }

(* Read the unit information from a .cmo file. *)

type pack_member_kind = PM_intf | PM_impl of compilation_unit

type pack_member =
  { pm_file: string;
    pm_name: string;
    pm_ident: Ident.t;
    pm_packed_ident: Ident.t;
    pm_kind: pack_member_kind }

let read_member_info targetname file = (
  let name =
    String.capitalize_ascii(Filename.basename(chop_extensions file)) in
  let kind =
    (* PR#7479: make sure it is either a .cmi or a .cmo *)
    if Filename.check_suffix file ".cmi" then
      PM_intf
    else begin
      let ic = open_in_bin file in
      try
        let buffer =
          really_input_string ic (String.length Config.cmo_magic_number)
        in
        if buffer <> Config.cmo_magic_number then
          raise(Error(Not_an_object_file file));
        let compunit_pos = input_binary_int ic in
        seek_in ic compunit_pos;
        let compunit = (input_value ic : compilation_unit) in
        if compunit.cu_name <> name
        then raise(Error(Illegal_renaming(name, file, compunit.cu_name)));
        close_in ic;
        PM_impl compunit
      with x ->
        close_in ic;
        raise x
    end in
  let pm_ident = Ident.create_persistent name in
  let pm_packed_ident = Ident.create_persistent(targetname ^ "." ^ name) in
  { pm_file = file; pm_name = name; pm_kind = kind;
    pm_ident; pm_packed_ident }
)

(* Read the bytecode from a .cmo file.
   Write bytecode to channel [oc].
   Rename globals as indicated by [mapping] in reloc info.
   Accumulate relocs, debug info, etc.
   Return size of bytecode. *)

let rename_append_bytecode packagename oc mapping defined ofs subst
                           objfile compunit =
  let ic = open_in_bin objfile in
  try
    Bytelink.check_consistency objfile compunit;
    List.iter
      (fun reloc ->
         let reloc =
           rename_relocation packagename objfile mapping defined ofs reloc in
         relocs := reloc :: !relocs)
      compunit.cu_reloc;
    primitives := compunit.cu_primitives @ !primitives;
    if compunit.cu_force_link then force_link := true;
    seek_in ic compunit.cu_pos;
    Misc.copy_file_chunk ic oc compunit.cu_codesize;
    if !Clflags.debug && compunit.cu_debug > 0 then begin
      seek_in ic compunit.cu_debug;
      List.iter (fun ev ->
          let ev = relocate_debug ofs packagename subst ev in
          events := ev :: !events) (input_value ic);
      debug_dirs := List.fold_left
        (fun s e -> String.Set.add e s)
        !debug_dirs
        (input_value ic);
    end;
    close_in ic;
    compunit.cu_codesize
  with x ->
    close_in ic;
    raise x

(* Same, for a list of .cmo and .cmi files.
   Return total size of bytecode. *)

let rec rename_append_bytecode_list packagename oc mapping defined ofs
                                    subst =
  function
    [] ->
      ofs
  | m :: rem ->
      match m.pm_kind with
      | PM_intf ->
          rename_append_bytecode_list packagename oc mapping defined ofs
                                      subst rem
      | PM_impl compunit ->
          let size =
            rename_append_bytecode packagename oc mapping defined ofs
                                   subst m.pm_file compunit in
          let id = Ident.create_persistent m.pm_name in
          let root = Path.Pident (Ident.create_persistent packagename) in
          rename_append_bytecode_list packagename oc mapping (id :: defined)
            (ofs + size)
            (Subst.add_module id (Path.Pdot (root, Ident.name id))
                              subst)
            rem

(* Generate the code that builds the tuple representing the package module *)

let build_global_target ~ppf_dump oc target_name pos components coercion =
  let lam =
    Translmod.transl_package
      components (Ident.create_persistent target_name) coercion in
  let lam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then
    Format.fprintf ppf_dump "%a@." Printlambda.lambda lam;
  let instrs =
    Bytegen.compile_implementation target_name lam in
  let rel =
    Emitcode.to_packed_file oc instrs in
  relocs := List.map (fun (r, ofs) -> (r, pos + ofs)) rel @ !relocs

(* Build the .cmo file obtained by packaging the given .cmo files. *)

let package_object_files ~ppf_dump files targetfile targetname coercion =
  let members =
    map_left_right (read_member_info targetname) files in
  let required_globals =
    List.fold_right (fun compunit required_globals -> match compunit with
        | { pm_kind = PM_intf } ->
            required_globals
        | { pm_kind = PM_impl { cu_required_globals; cu_reloc } } ->
            let remove_required (rel, _pos) required_globals =
              match rel with
                Reloc_setglobal id ->
                  Ident.Set.remove id required_globals
              | _ ->
                  required_globals
            in
            let required_globals =
              List.fold_right remove_required cu_reloc required_globals
            in
            List.fold_right Ident.Set.add cu_required_globals required_globals)
      members Ident.Set.empty
  in
  let unit_names =
    List.map (fun m -> m.pm_name) members in
  let mapping =
    List.map
      (fun m -> m.pm_ident, m.pm_packed_ident)
      members in
  let oc = open_out_bin targetfile in
  try
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let ofs = rename_append_bytecode_list targetname oc mapping [] 0
                                          Subst.identity members in
    let components =
      List.map
        (fun m ->
          match m.pm_kind with
          | PM_intf -> None
          | PM_impl _ -> Some m.pm_packed_ident)
        members in
    build_global_target ~ppf_dump oc targetname ofs components coercion;
    let pos_debug = pos_out oc in
    if !Clflags.debug && !events <> [] then begin
      output_value oc (List.rev !events);
      output_value oc (String.Set.elements !debug_dirs);
    end;
    let pos_final = pos_out oc in
    let imports =
      List.filter
        (fun (name, _crc) -> not (List.mem name unit_names))
        (Bytelink.extract_crc_interfaces()) in
    let compunit =
      { cu_name = targetname;
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev !relocs;
        cu_imports =
          (targetname, Some (Env.crc_of_unit targetname)) :: imports;
        cu_primitives = !primitives;
        cu_required_globals = Ident.Set.elements required_globals;
        cu_force_link = !force_link;
        cu_debug = if pos_final > pos_debug then pos_debug else 0;
        cu_debugsize = pos_final - pos_debug } in
    Emitcode.marshal_to_channel_with_possibly_32bit_compat
      ~filename:targetfile ~kind:"bytecode unit"
      oc compunit;
    seek_out oc pos_depl;
    output_binary_int oc pos_final;
    close_out oc
  with x ->
    close_out oc;
    raise x

(* The entry point *)

let package_files ~ppf_dump initial_env files targetfile =
    let files =
    List.map
        (fun f ->
        try Load_path.find f
        with Not_found -> raise(Error(File_not_found f)))
        files in
    let prefix = chop_extensions targetfile in
    let targetcmi = prefix ^ ".cmi" in
    let targetname = String.capitalize_ascii(Filename.basename prefix) in
    Misc.try_finally (fun () ->
        let coercion =
          Typemod.package_units initial_env files targetcmi targetname in
        package_object_files ~ppf_dump files targetfile targetname coercion
      )
      ~exceptionally:(fun () -> remove_file targetfile)

(* Error report *)

open Format

let report_error ppf = function
    Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %s in file %a" (Ident.name ident)
        Location.print_filename file
  | Multiple_definition(file, ident) ->
      fprintf ppf "File %a redefines %s"
        Location.print_filename file
        (Ident.name ident)
  | Not_an_object_file file ->
      fprintf ppf "%a is not a bytecode object file"
        Location.print_filename file
  | Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %s when %s was expected"
        Location.print_filename file name id
  | File_not_found file ->
      fprintf ppf "File %s not found" file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  relocs := [];
  events := [];
  primitives := [];
  force_link := false
