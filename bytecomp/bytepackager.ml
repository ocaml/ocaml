(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* "Package" a set of .cmo files into one .cmo file having the
   original compilation units as sub-modules. *)

open Misc
open Instruct
open Emitcode

type error =
    Forward_reference of string * Ident.t
  | Multiple_definition of string * Ident.t
  | Not_an_object_file of string
  | Illegal_renaming of string * string
  | File_not_found of string

exception Error of error

(* References accumulating informations on the .cmo files *)

let relocs = ref ([] : (reloc_info * int) list)
let events = ref ([] : debug_event list)
let primitives = ref ([] : string list)
let force_link = ref false

(* Record a relocation.  Update its offset, and rename GETGLOBAL and
   SETGLOBAL relocations that correspond to one of the units being
   consolidated. *)

let rename_relocation objfile mapping defined base (rel, ofs) =
  let rel' =
    match rel with
      Reloc_getglobal id ->
        begin try
          let id' = List.assoc id mapping in
          if List.mem id defined
          then Reloc_getglobal id'
          else raise(Error(Forward_reference(objfile, id)))
        with Not_found ->
          rel
        end
    | Reloc_setglobal id ->
        begin try
          let id' = List.assoc id mapping in
          if List.mem id defined
          then raise(Error(Multiple_definition(objfile, id)))
          else Reloc_setglobal id'
        with Not_found ->
          rel
        end
    | _ ->
        rel in
  relocs := (rel', base + ofs) :: !relocs

(* Record and relocate a debugging event *)

let relocate_debug base ev =
  ev.ev_pos <- base + ev.ev_pos;
  events := ev :: !events

(* Read the unit information from a .cmo file. *)

let read_unit_info objfile =
  let ic = open_in_bin objfile in
  try
    let buffer = String.create (String.length Config.cmo_magic_number) in
    really_input ic buffer 0 (String.length Config.cmo_magic_number);
    if buffer <> Config.cmo_magic_number then
      raise(Error(Not_an_object_file objfile));
    let compunit_pos = input_binary_int ic in
    seek_in ic compunit_pos;
    let compunit = (input_value ic : compilation_unit) in
    if compunit.cu_name
    <> String.capitalize(Filename.basename(chop_extension_if_any objfile))
    then raise(Error(Illegal_renaming(objfile, compunit.cu_name)));
    close_in ic;
    compunit
  with x ->
    close_in ic;
    raise x

(* Read the bytecode from a .cmo file.
   Write bytecode to channel [oc].
   Rename globals as indicated by [mapping] in reloc info.
   Accumulate relocs, debug info, etc.
   Return size of bytecode. *)

let rename_append_bytecode oc mapping defined ofs (objfile, compunit) =
  let ic = open_in_bin objfile in
  try
    Bytelink.check_consistency objfile compunit;
    List.iter
      (rename_relocation objfile mapping defined ofs)
      compunit.cu_reloc;
    primitives := compunit.cu_primitives @ !primitives;
    if compunit.cu_force_link then force_link := true;
    seek_in ic compunit.cu_pos;
    Misc.copy_file_chunk ic oc compunit.cu_codesize;
    if !Clflags.debug && compunit.cu_debug > 0 then begin
      seek_in ic compunit.cu_debug;
      List.iter (relocate_debug ofs) (input_value ic);
    end;
    close_in ic;
    compunit.cu_codesize
  with x ->
    close_in ic;
    raise x

(* Same, for a list of .cmo files.  Return total size of bytecode. *)

let rec rename_append_bytecode_list oc mapping defined ofs = function
    [] ->
      ofs
  | ((objfile, compunit) as obj_unit) :: rem ->
      let size = rename_append_bytecode oc mapping defined ofs obj_unit in
      rename_append_bytecode_list
        oc mapping (Ident.create_persistent compunit.cu_name :: defined)
        (ofs + size) rem

(* Generate the code that builds the tuple representing the package module *)

let build_global_target oc target_name mapping pos coercion =
  let lam =
    Translmod.transl_package (List.map snd mapping)
                             (Ident.create_persistent target_name) coercion in
  let instrs =
    Bytegen.compile_implementation target_name lam in
  let rel =
    Emitcode.to_packed_file oc instrs in
  relocs := List.map (fun (r, ofs) -> (r, pos + ofs)) rel @ !relocs

(* Build the .cmo file obtained by packaging the given .cmo files. *)

let package_object_files objfiles targetfile targetname coercion =
  let units =
    List.map (fun f -> (f, read_unit_info f)) objfiles in
  let unit_names =
    List.map (fun (_, cu) -> cu.cu_name) units in
  let mapping =
    List.map
      (fun name ->
          (Ident.create_persistent name,
           Ident.create_persistent(targetname ^ "." ^ name)))
      unit_names in
  let oc = open_out_bin targetfile in
  try
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let ofs = rename_append_bytecode_list oc mapping [] 0 units in
    build_global_target oc targetname mapping ofs coercion;
    let pos_debug = pos_out oc in
    if !Clflags.debug && !events <> [] then
      output_value oc (List.rev !events);
    let pos_final = pos_out oc in
    let imports =
      List.filter
        (fun (name, crc) -> not (List.mem name unit_names))
        (Bytelink.extract_crc_interfaces()) in
    let compunit =
      { cu_name = targetname;
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev !relocs;
        cu_imports = (targetname, Env.crc_of_unit targetname) :: imports;
        cu_primitives = !primitives;
        cu_force_link = !force_link;
        cu_debug = if pos_final > pos_debug then pos_debug else 0;
        cu_debugsize = pos_final - pos_debug } in
    output_value oc compunit;
    seek_out oc pos_depl;
    output_binary_int oc pos_final;
    close_out oc
  with x ->
    close_out oc;
    raise x

(* The entry point *)

let package_files files targetfile =
  let objfiles =
    List.map
      (fun f ->
        try find_in_path !Config.load_path f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extension_if_any targetfile in
  let targetcmi = prefix ^ ".cmi" in
  let targetname = String.capitalize(Filename.basename prefix) in
  try
    let coercion = Typemod.package_units objfiles targetcmi targetname in
    package_object_files objfiles targetfile targetname coercion
  with x ->
    remove_file targetfile; raise x

(* Error report *)

open Format

let report_error ppf = function
    Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %s in file %s" (Ident.name ident) file
  | Multiple_definition(file, ident) ->
      fprintf ppf "File %s redefines %s" file (Ident.name ident)
  | Not_an_object_file file ->
      fprintf ppf "%s is not a bytecode object file" file
  | Illegal_renaming(file, id) ->
      fprintf ppf "Wrong file naming: %s@ contains the code for@ %s"
        file id
  | File_not_found file ->
      fprintf ppf "File %s not found" file
