(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Handling of load path for dynamically-linked libraries *)

(* Read the [ld.conf] file and return the corresponding list of directories *)

let ld_conf_contents () =
  let path = ref [] in
  begin try
    let ic = open_in (Filename.concat Config.standard_library "ld.conf") in
    begin try
      while true do
        path := input_line ic :: !path
      done
    with End_of_file -> ()
    end;
    close_in ic
  with Sys_error _ -> ()
  end;
  List.rev !path

(* Split the CAML_LD_LIBRARY_PATH environment variable and return
   the corresponding list of directories.  *)

let split str sep =
  let rec split_rec pos =
    if pos >= String.length str then [] else begin
      try
        let newpos = String.index_from str pos sep in
        String.sub str pos (newpos - pos) ::
        split_rec (newpos + 1)
      with Not_found ->
        [String.sub str pos (String.length str - pos)]
    end in
  split_rec 0

let ld_library_path_contents () =
  let path_separator =
    match Sys.os_type with
      "Unix" | "Cygwin" -> ':' | "Win32" -> ';' | _ -> assert false in
  try
    split (Sys.getenv "CAML_LD_LIBRARY_PATH") path_separator
  with Not_found ->
    []

let split_dll_path path =
  split path '\000'
