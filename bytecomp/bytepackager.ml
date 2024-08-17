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
module Compunit = Symtable.Compunit

let rec rev_append_map f l rest =
  match l with
  | [] -> rest
  | x :: xs -> rev_append_map f xs (f x :: rest)

type error =
    Forward_reference of string * compunit
  | Multiple_definition of string * compunit
  | Not_an_object_file of string
  | Illegal_renaming of compunit * string * compunit
  | File_not_found of string

exception Error of error

type mapped_compunit = {
  packed_modname : compunit; (** qualified name of the compilation unit *)
  processed : bool
}

let record_as_processed mapping id =
  let update_processed = function
    | Some ({ processed = false; _} as r) -> Some {r with processed=true}
    | Some {processed = true;_} | None -> assert false
  in
  Compunit.Map.update id update_processed mapping

type state = {
  relocs : (reloc_info * int) list; (** accumulated reloc info *)
  events : debug_event list;        (** accumulated debug events *)
  debug_dirs : String.Set.t;        (** accumulated debug_dirs *)
  primitives : string list;         (** accumulated primitives *)
  offset : int;                     (** offset of the current unit *)
  subst : Subst.t;                  (** Substitution for debug event *)
  mapping : mapped_compunit Compunit.Map.t;
  (** Mapping from module to packed-module idents. *)
}

let empty_state = {
  relocs = [];
  events = [];
  debug_dirs = String.Set.empty;
  primitives = [];
  offset = 0;
  mapping = Compunit.Map.empty;
  subst = Subst.identity;
}

(* Update a relocation.  adjust its offset, and rename GETGLOBAL and
   SETGLOBAL relocations that correspond to one of the units being
   consolidated. *)

let rename_relocation packagename objfile mapping base (rel, ofs) =
  (* PR#5276: unique-ize dotted global names, which appear if one of
    the units being consolidated is itself a packed module. *)
  let make_compunit_name_unique cu =
    if Compunit.is_packed cu
    then Compunit (packagename ^ "." ^ (Compunit.name cu))
    else cu
  in
  let rel' =
    match rel with
      | Reloc_getcompunit cu ->
        begin try
          let mapped_modname = Compunit.Map.find cu mapping in
          if mapped_modname.processed
          then Reloc_getcompunit mapped_modname.packed_modname
          else raise(Error(Forward_reference(objfile, cu)))
        with Not_found -> Reloc_getcompunit (make_compunit_name_unique cu)
      end
    | Reloc_setcompunit cu ->
      begin try
          let mapped_modname = Compunit.Map.find cu mapping in
          if mapped_modname.processed
          then raise(Error(Multiple_definition(objfile, cu)))
          else Reloc_setcompunit mapped_modname.packed_modname
        with Not_found -> Reloc_setcompunit (make_compunit_name_unique cu)
      end
    | Reloc_literal _ | Reloc_getpredef _ | Reloc_primitive _ ->
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
    pm_ident: compunit;
    pm_packed_ident: compunit;
    pm_kind: pack_member_kind }

let read_member_info targetname file =
  let member = Unit_info.Artifact.from_filename file in
  let member_name = Unit_info.Artifact.modname member in
  let member_compunit = Compunit member_name in
  let kind =
    (* PR#7479: make sure it is either a .cmi or a .cmo *)
    if Unit_info.is_cmi member then
      PM_intf
    else begin
      let ic = open_in_bin file in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        let buffer =
          really_input_string ic (String.length Config.cmo_magic_number)
        in
        if buffer <> Config.cmo_magic_number then
          raise(Error(Not_an_object_file file));
        let compunit_pos = input_binary_int ic in
        seek_in ic compunit_pos;
        let compunit = (input_value ic : compilation_unit) in
        if compunit.cu_name <> member_compunit
        then begin
          raise(Error(Illegal_renaming
            (member_compunit, file, compunit.cu_name)))
        end;
        PM_impl compunit)
    end in
  let pm_packed_ident = Compunit (targetname ^ "." ^ member_name) in
  { pm_file = file; pm_name = member_name; pm_kind = kind;
    pm_ident = member_compunit; pm_packed_ident }

(* Read the bytecode from a .cmo file.
   Write bytecode to channel [oc].
   Rename compunits as indicated by [mapping] in reloc info.
   Accumulate relocs, debug info, etc.
   Return the accumulated state. *)

let rename_append_bytecode packagename oc state objfile compunit =
  let ic = open_in_bin objfile in
  try
    Bytelink.check_consistency objfile compunit;
    let relocs =
      rev_append_map
        (rename_relocation packagename objfile state.mapping state.offset)
        compunit.cu_reloc
        state.relocs in
    let primitives = List.rev_append compunit.cu_primitives state.primitives in
    seek_in ic compunit.cu_pos;
    Misc.copy_file_chunk ic oc compunit.cu_codesize;
    let events, debug_dirs =
      if !Clflags.debug && compunit.cu_debug > 0 then begin
        seek_in ic compunit.cu_debug;
        let unit_events = (Compression.input_value ic : debug_event list) in
        let events =
          rev_append_map
            (relocate_debug state.offset packagename state.subst)
            unit_events
            state.events in
        let unit_debug_dirs = (Compression.input_value ic : string list) in
        let debug_dirs =
          String.Set.union
            state.debug_dirs
            (String.Set.of_list unit_debug_dirs) in
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
      let mapping = record_as_processed state.mapping id in
      let subst =
        let id' = Compunit.to_ident id in
        Subst.add_module id' (Path.Pdot (root, Ident.name id')) state.subst
      in
      { state with subst; mapping }

(* Generate the code that builds the tuple representing the package module *)

let build_global_target ~ppf_dump oc target_name state components coercion =
  let components =
    List.map (Option.map Compunit.to_ident) components
  in
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
  let relocs =
    rev_append_map
      (fun (r, ofs) -> (r, state.offset + ofs))
      pack_relocs state.relocs in
  { state with events; debug_dirs; relocs; offset = state.offset + size}

(* Build the .cmo file obtained by packaging the given .cmo files. *)

let package_object_files ~ppf_dump files target coercion =
  let targetfile = Unit_info.Artifact.filename target in
  let targetname = Unit_info.Artifact.modname target in
  let members = map_left_right (read_member_info targetname) files in
  let required_compunits =
    List.fold_right (fun compunit required_compunits -> match compunit with
        | { pm_kind = PM_intf } ->
            required_compunits
        | { pm_kind = PM_impl { cu_required_compunits; cu_reloc } } ->
            let remove_required (rel, _pos) required_compunits =
              match rel with
                Reloc_setcompunit cu ->
                  Compunit.Set.remove cu required_compunits
              | Reloc_literal _ | Reloc_getcompunit _ | Reloc_getpredef _
              | Reloc_primitive _ ->
                  required_compunits
            in
            let required_compunits =
              List.fold_right remove_required cu_reloc required_compunits
            in
            List.fold_right
              Compunit.Set.add cu_required_compunits required_compunits)
      members Compunit.Set.empty
  in
  let oc = open_out_bin targetfile in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let state =
      let mapping =
        List.map
          (fun m -> m.pm_ident,
            { packed_modname = m.pm_packed_ident; processed = false }
          )
          members
        |> Compunit.Map.of_list in
      { empty_state with mapping } in
    let state =
      List.fold_left (rename_append_pack_member targetname oc) state members in
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
      Compression.output_value oc (List.rev state.events);
      Compression.output_value oc (String.Set.elements state.debug_dirs)
    end;
    let force_link =
      List.exists (function
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
      { cu_name = Compunit targetname;
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev state.relocs;
        cu_imports =
          (targetname, Some (Env.crc_of_unit targetname)) :: imports;
        cu_primitives = List.rev state.primitives;
        cu_required_compunits =
          (Compunit.Set.elements required_compunits);
        cu_force_link = force_link;
        cu_debug = if pos_final > pos_debug then pos_debug else 0;
        cu_debugsize = pos_final - pos_debug } in
    Emitcode.marshal_to_channel_with_possibly_32bit_compat
      ~filename:targetfile ~kind:"bytecode unit"
      oc compunit;
    seek_out oc pos_depl;
    output_binary_int oc pos_final)

(* The entry point *)

let package_files ~ppf_dump initial_env files targetfile =
  let files =
    List.map
      (fun f ->
         try Load_path.find f
         with Not_found -> raise(Error(File_not_found f)))
      files in
  let target = Unit_info.Artifact.from_filename targetfile in
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files (Unit_info.companion_cmi target)
      in
      package_object_files ~ppf_dump files target coercion
    )
    ~exceptionally:(fun () -> remove_file targetfile)

(* Error report *)

open Format_doc
module Style = Misc.Style

let report_error_doc ppf = function
    Forward_reference(file, compunit) ->
      fprintf ppf "Forward reference to %a in file %a"
        Style.inline_code (Compunit.name compunit)
        Location.Doc.quoted_filename file
  | Multiple_definition(file, compunit) ->
      fprintf ppf "File %a redefines %a"
        Location.Doc.quoted_filename file
        Style.inline_code (Compunit.name compunit)
  | Not_an_object_file file ->
      fprintf ppf "%a is not a bytecode object file"
        Location.Doc.quoted_filename file
  | Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        Location.Doc.quoted_filename file
        Style.inline_code (Compunit.name name)
        Style.inline_code (Compunit.name id)
  | File_not_found file ->
      fprintf ppf "File %a not found"
        Style.inline_code file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc
