(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Print the digests of unit interfaces *)

open! Dynlink_compilerlibs

let load_path = ref []
let first = ref true

exception Corrupted_interface

let digest_interface unit loadpath =
  let filename =
    let shortname = unit ^ ".cmi" in
    try
      Misc.find_in_path_uncap loadpath shortname
    with Not_found ->
      failwith (Printf.sprintf "Cannot find interface %s in load path"
        shortname)
  in
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmi_magic_number)
    in
    if buffer <> Config.cmi_magic_number then begin
      close_in ic;
      raise Corrupted_interface
    end;
    let cmi = Cmi_format.input_cmi ic in
    close_in ic;
    let crc =
      match cmi.Cmi_format.cmi_crcs with
        (_, Some crc) :: _ -> crc
      | _             -> raise Corrupted_interface
    in
    crc
  with End_of_file | Failure _ ->
    close_in ic;
    raise Corrupted_interface

let print_crc unit =
  try
    let crc = digest_interface unit (!load_path @ ["."]) in
    if !first then first := false else print_string ";\n";
    print_string "  \""; print_string (String.capitalize_ascii unit);
    print_string "\",\n    \"";
    for i = 0 to String.length crc - 1 do
      Printf.printf "\\%03d" (Char.code crc.[i])
    done;
    print_string "\""
  with exn ->
    prerr_string "Error while reading the interface for ";
    prerr_endline unit;
    begin match exn with
      Sys_error msg -> prerr_endline msg
    | Corrupted_interface ->
      Printf.eprintf "Ill-formed .cmi file (%s)\n" (Printexc.to_string exn)
    | _ -> raise exn
    end;
    exit 2

let usage = "Usage: extract_crc [-I <dir>] <files>"

let main () =
  print_string "let crc_unit_list = [\n";
  Arg.parse
    ["-I", Arg.String(fun dir -> load_path := !load_path @ [dir]),
           "<dir>  Add <dir> to the list of include directories"]
    print_crc usage;
  print_string "\n]\n"

let _ = main(); exit 0
