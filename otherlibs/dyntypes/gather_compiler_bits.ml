(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(* James Leifer and Gilles Peskine, projet Moscova, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Gather the bits of compiler that are needed in the run-time library. *)

(* Usage:
   gather_compiler_bits DIRECTORY ... - COMPILATION_UNIT ...
   For every COMPILATION_UNIT, print the contents of compilation_until.ml
   surrounded by ``module Compilation_unit = struct ... end''. If there
   is a compilation_unit.mli, add its contents inside ``: sig ... end''.
   If there is only a .mli file, output it as a module.
   Each DIRECTORY is check when looking for any file.
 *)

exception File_not_found of string

let all_directories, all_compilation_units =
  let i = ref 0 in
  let rec gather_directories acc =
    if !i >= Array.length Sys.argv
    then failwith "usage: gather_compiler_bits DIRECTORY ... - COMPILATION_UNIT ...";
    let arg = Sys.argv.(!i) in
    if arg = "-" then List.rev acc else begin
      incr i;
      gather_directories (arg :: acc)
    end
  in
  let rec gather_files acc =
    if !i >= Array.length Sys.argv
    then List.rev acc else begin
      let arg = Sys.argv.(!i) in
      incr i;
      gather_files (arg :: acc)
    end
  in
  let directories = gather_directories [] in
  incr i;
  directories, gather_files []

let rec look_for_file directories filename =
  match directories with
  | [] -> raise (File_not_found filename)
  | dir :: tail ->
      let full_name = Filename.concat dir filename in
      if Sys.file_exists full_name then full_name else 
      look_for_file tail filename

let cat file_name =
  let ch = open_in file_name in
  try
    while true do print_endline (input_line ch) done
  with End_of_file -> close_in ch

let print_capitalized string =
  print_string (String.capitalize string)

let cat_compilation_unit stem =
  let dot_ml = stem ^ ".ml" in
  try
    let impl = look_for_file all_directories dot_ml in
    print_string "module ";
    print_capitalized stem;
    print_string " = (struct\n";
    cat impl;
    print_string "end";
    let intf = impl ^ "i" in
    if Sys.file_exists intf then begin
      print_string " : sig\n";
      cat intf;
      print_string "end"
    end;
    print_string ")\n\n";
    ()
  with File_not_found _ ->
    let intf = look_for_file all_directories (dot_ml ^ "i") in
    print_string "module ";
    print_capitalized stem;
    print_string " = struct\n";
    cat intf;
    print_string "end\n\n";
    ()

let main () =
  List.iter cat_compilation_unit all_compilation_units

(* let () = Printexc.catch main () (* for Ocaml <= 3.02 *) *)
let () = main ()
