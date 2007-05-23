(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Dump a compilation unit description *)

open Config
open Cmo_format

let print_digest d =
  for i = 0 to String.length d - 1 do
    Printf.printf "%02x" (Char.code d.[i])
  done

let print_info cu =
  print_string "  Unit name: "; print_string cu.cu_name; print_newline();
  print_string "  Interfaces imported:"; print_newline();
  List.iter
    (fun (name, digest) ->
      print_string "\t"; print_digest digest; print_string "\t";
      print_string name; print_newline())
    cu.cu_imports;
  print_string "  Uses unsafe features: ";
  begin match cu.cu_primitives with
    [] -> print_string "no"; print_newline()
  | l  -> print_string "YES"; print_newline();
          print_string "  Primitives declared in this module:";
          print_newline();
          List.iter
            (fun name -> print_string "\t"; print_string name; print_newline())
            l
  end

let print_spaced_string s = print_char ' '; print_string s

let print_library_info lib =
  print_string "  Force custom: ";
  print_string (if lib.lib_custom then "YES" else "no");
  print_newline();
  print_string "  Extra C object files:";
  List.iter print_spaced_string lib.lib_ccobjs; print_newline();
  print_string "  Extra C options:";
  List.iter print_spaced_string lib.lib_ccopts; print_newline();
  List.iter print_info lib.lib_units

let print_intf_info name sign comps crcs =
  print_string "  Module name: "; print_string name; print_newline();
  print_string "  Interfaces imported:"; print_newline();
  List.iter
    (fun (name, digest) ->
      print_string "\t"; print_digest digest; print_string "\t";
      print_string name; print_newline())
    crcs

let dump_obj filename =
  print_string "File "; print_string filename; print_newline();
  let ic = open_in_bin filename in
  let buffer = String.create (String.length cmo_magic_number) in
  really_input ic buffer 0 (String.length cmo_magic_number);
  if buffer = cmo_magic_number then begin
    let cu_pos = input_binary_int ic in
    seek_in ic cu_pos;
    let cu = (input_value ic : compilation_unit) in
    close_in ic;
    print_info cu
  end else
  if buffer = cma_magic_number then begin
    let toc_pos = input_binary_int ic in
    seek_in ic toc_pos;
    let toc = (input_value ic : library) in
    close_in ic;
    print_library_info toc
  end else
  if buffer = cmi_magic_number then begin
    let (name, sign, comps) = input_value ic in
    let crcs = input_value ic in
    close_in ic;
    print_intf_info name sign comps crcs
  end else begin
    prerr_endline "Not an object file"; exit 2
  end

let main() =
  for i = 1 to Array.length Sys.argv - 1 do
    dump_obj Sys.argv.(i)
  done;
  exit 0

let _ = main ()
