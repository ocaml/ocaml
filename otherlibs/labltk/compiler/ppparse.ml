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

exception Error of string

let parse_channel ic =
  let lexbuf = Lexing.from_channel ic in
  try
    Ppyac.code_list Pplex.token lexbuf 
  with
  | Pplex.Error s ->
      let loc_start = Lexing.lexeme_start lexbuf 
      and loc_end = Lexing.lexeme_end lexbuf
      in
      raise (Error (Printf.sprintf "parse error at char %d, %d: %s" 
                 loc_start loc_end s))
  | Parsing.Parse_error ->
      let loc_start = Lexing.lexeme_start lexbuf 
      and loc_end = Lexing.lexeme_end lexbuf
      in
      raise (Error (Printf.sprintf "parse error at char %d, %d" 
                loc_start loc_end))
;;
