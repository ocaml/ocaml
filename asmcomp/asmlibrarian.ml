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

(* Build libraries of .cmx files *)

open Misc
open Config
open Compilenv

type error =
    File_not_found of string
  | Archiver_error

exception Error of error

let read_info name =
  let filename =
    try
      find_in_path !load_path name
    with Not_found ->
      raise(Error(File_not_found name)) in
  (Filename.chop_suffix filename ".cmx" ^ ".o",
   Compilenv.read_unit_info filename)

let create_archive file_list lib_name =
  let archive_name = Filename.chop_suffix lib_name ".cmxa" ^ ".a" in
  let outchan = open_out_bin lib_name in
  try
    output_string outchan cmxa_magic_number;
    let (objfile_list, descr_list) =
      List.split (List.map read_info file_list) in
    output_value outchan descr_list;
    if Proc.create_archive archive_name objfile_list <> 0
    then raise(Error(Archiver_error));
    close_out outchan
  with x ->
    close_out outchan;
    remove_file lib_name;
    remove_file archive_name;
    raise x

open Format

let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Archiver_error ->
      print_string "Error while writing the .a file"

