(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Unix
open Support
open Protocol

external add_file_input : file_descr -> cbid -> unit
        =  "camltk_add_file_input"
external rem_file_input : file_descr -> cbid -> unit
        =  "camltk_rem_file_input"
external add_file_output : file_descr -> cbid -> unit
        =  "camltk_add_file_output"
external rem_file_output : file_descr -> cbid -> unit
        =  "camltk_rem_file_output"

(* File input handlers *)

let fd_table = Hashtbl.create 37 (* Avoid space leak in callback table *)

let add_fileinput ~fd ~callback:f =
  let id = new_function_id () in
  Hashtbl.add callback_naming_table id (fun _ -> f());
  Hashtbl.add fd_table (fd, 'r') id;
  if !Protocol.debug then begin
    Protocol.prerr_cbid id; prerr_endline " for fileinput"
  end;
  add_file_input fd id

let remove_fileinput ~fd =
  try
    let id = Hashtbl.find fd_table (fd, 'r') in
    clear_callback id;
    Hashtbl.remove fd_table (fd, 'r');
    if !Protocol.debug then begin
      prerr_string "clear ";
      Protocol.prerr_cbid id;
      prerr_endline " for fileinput"
    end;
    rem_file_input fd id
  with
    Not_found -> ()

let add_fileoutput ~fd ~callback:f =
  let id = new_function_id () in
  Hashtbl.add callback_naming_table id (fun _ -> f());
  Hashtbl.add fd_table (fd, 'w') id;
  if !Protocol.debug then begin
    Protocol.prerr_cbid id; prerr_endline " for fileoutput"
  end;
  add_file_output fd id

let remove_fileoutput ~fd =
  try
    let id = Hashtbl.find fd_table (fd, 'w') in
    clear_callback id;
    Hashtbl.remove fd_table (fd, 'w');
    if !Protocol.debug then begin
      prerr_string "clear ";
      Protocol.prerr_cbid id;
      prerr_endline " for fileoutput"
    end;
    rem_file_output fd id
  with
    Not_found -> ()
