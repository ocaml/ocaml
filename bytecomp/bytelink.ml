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

(* Link a set of .cmo files and produce a bytecode executable. *)

open Sys
open Misc
open Config
open Emitcode

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Symbol_error of string * Symtable.error
  | Inconsistent_import of string * string * string
  | Custom_runtime

exception Error of error

type link_action =
    Link_object of string * compilation_unit
      (* Name of .cmo file and descriptor of the unit *)
  | Link_archive of string * compilation_unit list
      (* Name of .cma file and descriptors of the units to be linked. *)

(* First pass: determine which units are needed *)

module IdentSet =
  Set.Make(struct
    type t = Ident.t
    let compare = compare
  end)

let missing_globals = ref IdentSet.empty

let is_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      IdentSet.mem id !missing_globals
  | _ -> false

let add_required (rel, pos) =
  match rel with
    Reloc_getglobal id ->
      missing_globals := IdentSet.add id !missing_globals
  | _ -> ()

let remove_required (rel, pos) =
  match rel with
    Reloc_setglobal id ->
      missing_globals := IdentSet.remove id !missing_globals
  | _ -> ()

let scan_file obj_name tolink =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length cmo_magic_number) in
    really_input ic buffer 0 (String.length cmo_magic_number);
    if buffer = cmo_magic_number then begin
      (* This is a .cmo file. It must be linked in any case.
         Read the relocation information to see which modules it
         requires. *)
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      close_in ic;
      List.iter add_required compunit.cu_reloc;
      Link_object(file_name, compunit) :: tolink
    end
    else if buffer = cma_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let toc = (input_value ic : compilation_unit list) in
      close_in ic;
      let required =
        List.fold_right
          (fun compunit reqd ->
            if List.exists is_required compunit.cu_reloc
            or !Clflags.link_everything
            then begin
              List.iter remove_required compunit.cu_reloc;
              List.iter add_required compunit.cu_reloc;
              compunit :: reqd
            end else
              reqd)
          toc [] in
      Link_archive(file_name, required) :: tolink
    end
    else raise(Error(Not_an_object_file file_name))
  with x ->
    close_in ic; raise x

(* Second pass: link in the required units *)

(* Consistency check between interfaces *)

let crc_interfaces = (Hashtbl.new 17 : (string, string * Digest.t) Hashtbl.t)

let check_consistency file_name cu =
  List.iter
    (fun (name, crc) ->
      try
        let (auth_name, auth_crc) = Hashtbl.find crc_interfaces name in
        if crc <> auth_crc then
          raise(Error(Inconsistent_import(name, file_name, auth_name)))
      with Not_found ->
        (* Can only happen for unit for which only a .cmi file was used,
           but no .cmo is provided *)
        Hashtbl.add crc_interfaces name (file_name, crc))
    cu.cu_imports;
  Hashtbl.add crc_interfaces cu.cu_name (file_name, cu.cu_interface)

(* Link in a compilation unit *)

let link_compunit outchan inchan file_name compunit =
  check_consistency file_name compunit;
  seek_in inchan compunit.cu_pos;
  let code_block = String.create compunit.cu_codesize in
  really_input inchan code_block 0 compunit.cu_codesize;
  Symtable.patch_object code_block compunit.cu_reloc;
  output outchan code_block 0 compunit.cu_codesize

(* Link in a .cmo file *)

let link_object outchan file_name compunit =
  let inchan = open_in_bin file_name in
  try
    link_compunit outchan inchan file_name compunit;
    close_in inchan
  with
    Symtable.Error msg ->
      close_in inchan; raise(Error(Symbol_error(file_name, msg)))
  | x ->
      close_in inchan; raise x

(* Link in a .cma file *)

let link_archive outchan file_name units_required =
  let inchan = open_in_bin file_name in
  try
    List.iter (link_compunit outchan inchan file_name) units_required;
    close_in inchan
  with
    Symtable.Error msg ->
      close_in inchan; raise(Error(Symbol_error(file_name, msg)))
  | x ->
      close_in inchan; raise x

(* Link in a .cmo or .cma file *)

let link_file outchan = function
    Link_object(file_name, unit) -> link_object outchan file_name unit
  | Link_archive(file_name, units) -> link_archive outchan file_name units

(* Create a bytecode executable file *)

let link_bytecode objfiles exec_name copy_header =
  let objfiles = "stdlib.cma" :: objfiles in
  let tolink =
    List.fold_right scan_file objfiles [] in
  let outchan =
    open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary] 0o777
                 exec_name in
  try
    (* Copy the header *)
    if copy_header then begin
      try
        let inchan = open_in_bin (find_in_path !load_path "cslheader") in
        copy_file inchan outchan;
        close_in inchan
      with Not_found | Sys_error _ -> ()
    end;
    (* The bytecode *)
    let pos1 = pos_out outchan in
    Symtable.init();
    Hashtbl.clear crc_interfaces;
    List.iter (link_file outchan) tolink;
    (* The final STOP instruction *)
    output_byte outchan Opcodes.opSTOP;
    output_byte outchan 0; output_byte outchan 0; output_byte outchan 0;
    (* The table of global data *)
    let pos2 = pos_out outchan in
    output_value outchan (Symtable.initial_global_table());
    (* The List.map of global identifiers *)
    let pos3 = pos_out outchan in
    Symtable.output_global_map outchan;
    (* The trailer *)
    let pos4 = pos_out outchan in
    output_binary_int outchan (pos2 - pos1);
    output_binary_int outchan (pos3 - pos2);
    output_binary_int outchan (pos4 - pos3);
    output_binary_int outchan 0;
    output_string outchan exec_magic_number;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file exec_name;
    raise x

(* Main entry point (build a custom runtime if needed) *)

let link objfiles =
  if not !Clflags.custom_runtime then
    link_bytecode objfiles !Clflags.exec_name true
  else begin
    let bytecode_name = temp_file "camlcode" "" in
    let prim_name = temp_file "camlprim" ".c" in
    try
      link_bytecode objfiles bytecode_name false;
      Symtable.output_primitives prim_name;
      if Sys.command
          (Printf.sprintf
           "%s -I%s -o %s %s %s -L%s %s -lcamlrun %s"
           Config.bytecomp_c_compiler
           Config.standard_library
           !Clflags.exec_name
           (String.concat " " (List.rev !Clflags.ccopts))
           prim_name
           Config.standard_library
           (String.concat " " (List.rev !Clflags.ccobjs))
           Config.c_libraries)
         <> 0
      then raise(Error Custom_runtime);
      let oc =
        open_out_gen [Open_wronly; Open_append; Open_binary] 0
                     !Clflags.exec_name in
      let ic = open_in_bin bytecode_name in
      copy_file ic oc;
      close_in ic;
      close_out oc;
      remove_file bytecode_name;
      remove_file prim_name
    with x ->
      remove_file bytecode_name;
      remove_file prim_name;
      raise x
  end

(* Error report *)

open Format

let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Not_an_object_file name ->
      print_string "The file "; print_string name;
      print_string " is not a bytecode object file"
  | Symbol_error(name, err) ->
      print_string "Error while linking "; print_string name; print_string ":";
      print_space();
      Symtable.report_error err
  | Inconsistent_import(intf, file1, file2) ->
      open_hvbox 0;
      print_string "Files "; print_string file1; print_string " and ";
      print_string file2; print_space();
      print_string "make inconsistent assumptions over interface ";
      print_string intf;
      close_box()
  | Custom_runtime ->
      print_string "Error while building custom runtime system"

