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

(* "Expunge" a toplevel by removing compiler modules from the global List.map.
   Usage: expunge <source file> <dest file> <names of modules to keep> *)

open Sys
open Misc

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

let to_keep = ref StringSet.empty

let expunge_map tbl =
  Symtable.filter_global_map
    (fun id -> StringSet.mem (Ident.name id) !to_keep)
    tbl

let openflags =
  match (Sys.get_config ()).Sys.os_type with
  | "MacOS" -> [Open_wronly; Open_trunc; Open_creat]
  | _ -> [Open_wronly; Open_trunc; Open_creat; Open_binary]
;;

let main () =
  let input_name = Sys.argv.(1) in
  let output_name = Sys.argv.(2) in
  Array.iter
    (fun exn -> to_keep := StringSet.add exn !to_keep)
    Runtimedef.builtin_exceptions;
  for i = 3 to Array.length Sys.argv - 1 do
    to_keep := StringSet.add (String.capitalize Sys.argv.(i)) !to_keep
  done;
  let ic = open_in_bin input_name in
  let pos_trailer =
    in_channel_length ic - 16 - String.length Config.exec_magic_number in
  seek_in ic pos_trailer;
  let code_size = input_binary_int ic in
  let data_size = input_binary_int ic in
  let symbol_size = input_binary_int ic in
  let debug_size = input_binary_int ic in
  let header = String.create(String.length Config.exec_magic_number) in
  really_input ic header 0 (String.length Config.exec_magic_number);
  if header <> Config.exec_magic_number then begin
    prerr_endline "Wrong magic number"; exit 2
  end;
  let oc =
    open_out_gen openflags 0o777 output_name in
  (* Copy the file up to the symbol section as is *)
  seek_in ic 0;
  copy_file_chunk ic oc (pos_trailer - symbol_size - debug_size);
  (* Read, expunge and rewrite the symbol section *)
  let global_map = (input_value ic : Symtable.global_map) in
  let pos1 = pos_out oc in
  output_value oc (expunge_map global_map);
  let pos2 = pos_out oc in
  (* Rewrite the trailer *)
  output_binary_int oc code_size;
  output_binary_int oc data_size;
  output_binary_int oc (pos2 - pos1);
  output_binary_int oc 0;
  output_string oc Config.exec_magic_number;
  (* Done *)
  close_in ic;
  close_out oc

let _ = Printexc.catch main (); exit 0

  
  
