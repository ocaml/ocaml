(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Dynamic loading of .cmo files *)

open Emitcode

type error =
    Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string
  | Corrupted_interface of string

exception Error of error

(* Initialize the linker tables and everything *)

let init () =
  Symtable.init_toplevel()  

(* Check that the object file being loaded has been compiled against
   the same interfaces as the program itself. In addition, check that
   only authorized compilation units are referenced. *)

let crc_interfaces = (Hashtbl.new 17 : (string, Digest.t) Hashtbl.t)

let check_consistency file_name cu =
  List.iter
    (fun (name, crc) ->
      try
        let auth_crc = Hashtbl.find crc_interfaces name in
        if crc <> auth_crc then
          raise(Error(Inconsistent_import name))
      with Not_found ->
        raise(Error(Unavailable_unit name)))
    cu.cu_imports;
  Hashtbl.add crc_interfaces cu.cu_name cu.cu_interface

(* Reset the crc_interfaces table *)

let clear_available_units () =
  Hashtbl.clear crc_interfaces

(* Initialize the crc_interfaces table with a list of units with fixed CRCs *)

let add_available_units units =
  List.iter (fun (unit, crc) -> Hashtbl.add crc_interfaces unit crc) units

(* Read the CRC of an interface from its .cmi file *)

let digest_interface unit loadpath =
  let filename = Misc.find_in_path loadpath (Misc.lowercase unit ^ ".cmi") in
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

let loadfile file_name =
  let ic = open_in_bin file_name in
  let buffer = String.create (String.length Config.cmo_magic_number) in
  really_input ic buffer 0 (String.length Config.cmo_magic_number);
  if buffer <> Config.cmo_magic_number then
    raise(Error(Not_a_bytecode_file file_name));
  let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
  seek_in ic compunit_pos;
  let compunit = (input_value ic : compilation_unit) in
  check_consistency file_name compunit;
  check_unsafe_module compunit;
  seek_in ic compunit.cu_pos;
  let code_size = compunit.cu_codesize + 4 in
  let code = Meta.static_alloc code_size in
  unsafe_really_input ic code 0 compunit.cu_codesize;
  close_in ic;
  String.unsafe_set code compunit.cu_codesize
			     (Char.chr Opcodes.opSTOP);
  String.unsafe_set code (compunit.cu_codesize + 1) '\000';
  String.unsafe_set code (compunit.cu_codesize + 2) '\000';
  String.unsafe_set code (compunit.cu_codesize + 3) '\000';
  begin try
    Symtable.patch_object code compunit.cu_reloc;
    Symtable.update_global_table()
  with Symtable.Error _ ->
    raise(Error(Linking_error file_name))
  end;
  Meta.execute_bytecode code code_size; ()

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
  | Linking_error name ->
      "error while linking " ^ name
  | Corrupted_interface name ->
      "corrupted interface file " ^ name

