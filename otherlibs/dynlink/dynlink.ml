(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Dynamic loading of .cmo files *)

open Emitcode

type linking_error = Symtable.error =
    Undefined_global of string
  | Unavailable_primitive of string

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string

exception Error of error

(* Initialize the linker tables and everything *)

let init () =
  Symtable.init_toplevel()  

(* Check that the object file being loaded has been compiled against
   the same interfaces as the program itself. In addition, check that
   only authorized compilation units are referenced. *)

let crc_interfaces = (Hashtbl.create 17 : (string, Digest.t) Hashtbl.t)

let check_consistency file_name cu =
  List.iter
    (fun (name, crc) ->
      if name = cu.cu_name then begin
        Hashtbl.add crc_interfaces name crc
      end else begin
        try
          let auth_crc = Hashtbl.find crc_interfaces name in
          if crc <> auth_crc then
            raise(Error(Inconsistent_import name))
        with Not_found ->
          raise(Error(Unavailable_unit name))
      end)
    cu.cu_imports

(* Reset the crc_interfaces table *)

let clear_available_units () =
  Hashtbl.clear crc_interfaces

(* Initialize the crc_interfaces table with a list of units with fixed CRCs *)

let add_available_units units =
  List.iter (fun (unit, crc) -> Hashtbl.add crc_interfaces unit crc) units

(* Read the CRC of an interface from its .cmi file *)

let digest_interface unit loadpath =
  let filename = Misc.find_in_path loadpath (String.uncapitalize unit ^ ".cmi") in
  let ic = open_in_bin filename in
  try
    let buffer = String.create (String.length Config.cmi_magic_number) in
    really_input ic buffer 0 (String.length Config.cmi_magic_number);
    if buffer <> Config.cmi_magic_number then begin
      close_in ic;
      raise(Error(Corrupted_interface filename))
    end;
    input_value ic;
    let crc = Digest.input ic in
    close_in ic;
    crc
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_interface filename))

(* Initialize the crc_interfaces table with a list of units.
   Their CRCs are read from their interfaces. *)

let add_interfaces units loadpath =
  add_available_units
    (List.map (fun unit -> (unit, digest_interface unit loadpath)) units)

(* Check whether the object file being loaded was compiled in unsafe mode *)

let unsafe_allowed = ref false

let allow_unsafe_modules b =
  unsafe_allowed := b

let check_unsafe_module cu =
  if (not !unsafe_allowed) & cu.cu_primitives <> []
  then raise(Error(Unsafe_file))

(* Load in-core and execute a bytecode object file *)

let load_compunit ic file_name compunit =
  check_consistency file_name compunit;
  check_unsafe_module compunit;
  seek_in ic compunit.cu_pos;
  let code_size = compunit.cu_codesize + 8 in
  let code = Meta.static_alloc code_size in
  unsafe_really_input ic code 0 compunit.cu_codesize;
  String.unsafe_set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
  String.unsafe_set code (compunit.cu_codesize + 1) '\000';
  String.unsafe_set code (compunit.cu_codesize + 2) '\000';
  String.unsafe_set code (compunit.cu_codesize + 3) '\000';
  String.unsafe_set code (compunit.cu_codesize + 4) '\001';
  String.unsafe_set code (compunit.cu_codesize + 5) '\000';
  String.unsafe_set code (compunit.cu_codesize + 6) '\000';
  String.unsafe_set code (compunit.cu_codesize + 7) '\000';
  let initial_symtable = Symtable.current_state() in
  begin try
    Symtable.patch_object code compunit.cu_reloc;
    Symtable.update_global_table()
  with Symtable.Error error ->
    raise(Error(Linking_error (file_name, error)))
  end;
  begin try
    (Meta.reify_bytecode code code_size) (); ()
  with exn ->
    Symtable.restore_state initial_symtable;
    raise exn
  end

let loadfile file_name =
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length Config.cmo_magic_number) in
    really_input ic buffer 0 (String.length Config.cmo_magic_number);
    if buffer = Config.cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      load_compunit ic file_name (input_value ic : compilation_unit)
    end else
    if buffer = Config.cma_magic_number then begin
      let toc_pos = input_binary_int ic in  (* Go to table of contents *)
      seek_in ic toc_pos;
      List.iter (load_compunit ic file_name)
                (input_value ic : compilation_unit list)
    end else
      raise(Error(Not_a_bytecode_file file_name));
    close_in ic
  with exc ->
    close_in ic; raise exc

let loadfile_private file_name =
  let initial_symtable = Symtable.current_state() in
  try
    loadfile file_name;
    Symtable.hide_additions initial_symtable
  with exn ->
    Symtable.hide_additions initial_symtable;
    raise exn

(* Error report *)

let error_message = function
    Not_a_bytecode_file name ->
      name ^ " is not a bytecode object file"
  | Inconsistent_import name ->
      "interface mismatch on " ^ name
  | Unavailable_unit name ->
      "no implementation available for " ^ name
  | Unsafe_file ->
      "this object file uses unsafe features"
  | Linking_error (name, Undefined_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "Reference to undefined global `" ^ s ^ "'"
  | Linking_error (name, Unavailable_primitive s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The external function `" ^ s ^ "' is not available"
  | Corrupted_interface name ->
      "corrupted interface file " ^ name

