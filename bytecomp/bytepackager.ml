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

let rec rev_append_map f l rest =
  match l with
  | [] -> rest
  | x :: xs -> rev_append_map f xs (f x :: rest)

type error =
    Forward_reference of string * Ident.t
  | Multiple_definition of string * Ident.t
  | Not_an_object_file of string
  | Illegal_renaming of string * string * string
  | File_not_found of string

exception Error of error

type state = {
  relocs : (reloc_info * int) list; (** accumulated reloc info *)
  events : debug_event list;        (** accumulated debug events *)
  debug_dirs : String.Set.t;        (** accumulated debug_dirs *)
  primitives : string list;         (** accumulated primitives *)
  offset : int;                     (** offset of the current unit *)
  subst : Subst.t;                  (** Substitution for debug event *)
  mapping : (Ident.t * bool) Ident.Map.t;
  (** Mapping from module to packed-module idents.
      The boolean tells whether we've processed the compilation unit already. *)
}

let empty_state = {
  relocs = [];
  events = [];
  debug_dirs = String.Set.empty;
  primitives = [];
  offset = 0;
  mapping = Ident.Map.empty;
  subst = Subst.identity;
}

(* Update a relocation.  adjust its offset, and rename GETGLOBAL and
   SETGLOBAL relocations that correspond to one of the units being
   consolidated. *)

let rename_relocation packagename objfile mapping base (rel, ofs) =
  let rel' =
    match rel with
      Reloc_getglobal id ->
        begin try
          let id', defined = Ident.Map.find id mapping in
          if defined
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
          let id', defined = Ident.Map.find id mapping in
          if defined
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

let rename_append_bytecode packagename oc state objfile compunit =
  let ic = open_in_bin objfile in
  try
    Bytelink.check_consistency objfile compunit;
    let relocs = rev_append_map
      (rename_relocation packagename objfile state.mapping state.offset)
      compunit.cu_reloc
      state.relocs in
    let primitives = List.rev_append compunit.cu_primitives state.primitives in
    seek_in ic compunit.cu_pos;
    Misc.copy_file_chunk ic oc compunit.cu_codesize;
    let events, debug_dirs =
      if !Clflags.debug && compunit.cu_debug > 0 then begin
        seek_in ic compunit.cu_debug;
        let unit_events = (input_value ic : debug_event list) in
        let events = rev_append_map
            (relocate_debug state.offset packagename state.subst)
            unit_events
            state.events in
        let unit_debug_dirs = (input_value ic : string list) in
        let debug_dirs = List.fold_left
            (fun s e -> String.Set.add e s)
            state.debug_dirs
            unit_debug_dirs in
        events, debug_dirs
      end
      else state.events, state.debug_dirs
    in
    close_in ic;
    { state with
      relocs; primitives; events; debug_dirs;
      offset = state.offset + compunit.cu_codesize;
    }
  with x ->
    close_in ic;
    raise x

(* Same, for a list of .cmo and .cmi files.
   Return the accumulated state. *)
let rename_append_pack_member packagename oc state m =
  match m.pm_kind with
  | PM_intf -> state
  | PM_impl compunit ->
      let state =
        rename_append_bytecode packagename oc state m.pm_file compunit in
      let id = m.pm_ident in
      let root = Path.Pident (Ident.create_persistent packagename) in
      let mapping = Ident.Map.update id (function
          | Some (p,false) -> Some (p,true)
          | Some (_, true) | None -> assert false) state.mapping in
      let subst =
        Subst.add_module id (Path.Pdot (root, Ident.name id)) state.subst in
      { state with subst; mapping }

(* Generate the code that builds the tuple representing the package module *)

let build_global_target ~ppf_dump oc target_name state components coercion =
  let lam =
    Translmod.transl_package
      components (Ident.create_persistent target_name) coercion in
  let lam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then
    Format.fprintf ppf_dump "%a@." Printlambda.lambda lam;
  let instrs =
    Bytegen.compile_implementation target_name lam in
  let size, pack_relocs, pack_events, pack_debug_dirs =
    Emitcode.to_packed_file oc instrs in
  let events = List.rev_append pack_events state.events in
  let debug_dirs = String.Set.union pack_debug_dirs state.debug_dirs in
  let relocs = rev_append_map
      (fun (r, ofs) -> (r, state.offset + ofs))
      pack_relocs state.relocs in
  { state with events; debug_dirs; relocs; offset = state.offset + size}

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
  let oc = open_out_bin targetfile in
  try
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let state =
      let mapping =
        List.map
          (fun m -> m.pm_ident, (m.pm_packed_ident, false))
          members
        |> Ident.Map.of_list in
      { empty_state with mapping } in
    let state =
      List.fold_left (rename_append_pack_member targetname oc)
        state members in
    let components =
      List.map
        (fun m ->
          match m.pm_kind with
          | PM_intf -> None
          | PM_impl _ -> Some m.pm_packed_ident)
        members in
    let state =
      build_global_target ~ppf_dump oc targetname state components coercion in
    let pos_debug = pos_out oc in
    if !Clflags.debug && state.events <> [] then begin
      output_value oc (List.rev state.events);
      output_value oc (String.Set.elements state.debug_dirs);
    end;
    let force_link = List.exists (function
        | {pm_kind = PM_impl {cu_force_link}} -> cu_force_link
        | _ -> false) members in
    let pos_final = pos_out oc in
    let imports =
      let unit_names =
        List.map (fun m -> m.pm_name) members in
      List.filter
        (fun (name, _crc) -> not (List.mem name unit_names))
        (Bytelink.extract_crc_interfaces()) in
    let compunit =
      { cu_name = targetname;
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev state.relocs;
        cu_imports =
          (targetname, Some (Env.crc_of_unit targetname)) :: imports;
        cu_primitives = List.rev state.primitives;
        cu_required_globals = Ident.Set.elements required_globals;
        cu_force_link = force_link;
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
