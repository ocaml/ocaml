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

(* Dump a compilation unit description *)

open Config
open Emitcode

let print_digest d =
  for i = 0 to String.length d - 1 do
    Printf.printf "%02x" (Char.code d.[i])
  done

let dump_obj filename =
  print_string "File "; print_string filename; print_newline();
  let ic = open_in_bin filename in
  let buffer = String.create (String.length cmo_magic_number) in
  really_input ic buffer 0 (String.length cmo_magic_number);
  if buffer <> cmo_magic_number then begin
    prerr_endline "Not an object file"; exit 2
  end;
  let cu_pos = input_binary_int ic in
  seek_in ic cu_pos;
  let cu = (input_value ic : compilation_unit) in
  close_in ic;
  print_string "  Unit name: "; print_string cu.cu_name; print_newline();
  print_string "  Digest of interface implemented: ";
  print_digest cu.cu_interface; print_newline();
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

let main() =
  for i = 1 to Array.length Sys.argv - 1 do
    dump_obj Sys.argv.(i)
  done;
  exit 0

let _ = Printexc.catch main (); exit 0


