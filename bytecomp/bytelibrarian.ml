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

(* Build libraries of .cmo files *)

open Misc
open Config
open Emitcode

type error =
    File_not_found of string
  | Not_an_object_file of string

exception Error of error

let copy_object_file outchan name =
  let file_name =
    try
      find_in_path !load_path name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length cmo_magic_number) in
    really_input ic buffer 0 (String.length cmo_magic_number);
    if buffer <> cmo_magic_number then
      raise(Error(Not_an_object_file file_name));
    let compunit_pos = input_binary_int ic in
    seek_in ic compunit_pos;
    let compunit = (input_value ic : compilation_unit) in
    seek_in ic compunit.cu_pos;
    compunit.cu_pos <- pos_out outchan;
    copy_file_chunk ic outchan compunit.cu_codesize;
    close_in ic;
    compunit
  with x ->
    close_in ic;
    raise x

let create_archive file_list lib_name =
  let outchan = open_out_bin lib_name in
  try
    output_string outchan cma_magic_number;
    let ofs_pos_toc = pos_out outchan in
    output_binary_int outchan 0;
    let toc = List.map (copy_object_file outchan) file_list in
    let pos_toc = pos_out outchan in
    output_value outchan toc;
    seek_out outchan ofs_pos_toc;
    output_binary_int outchan pos_toc;
    close_out outchan
  with x ->
    close_out outchan;
    remove_file lib_name;
    raise x

open Format

let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Not_an_object_file name ->
      print_string "The file "; print_string name;
      print_string " is not a bytecode object file"

