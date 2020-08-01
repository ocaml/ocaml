(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

{

(* Recording key=val bindings *)

let key_val_tbl : (string, string) Hashtbl.t = Hashtbl.create 17

let key_val key =
  Hashtbl.find_opt key_val_tbl key

(* Auxiliaries for parsing *)

let buf = Buffer.create 32

let contents () =
  let s = Buffer.contents buf in
  Buffer.clear buf;
  s

(* Error reporting *)

exception Error of string * int * string

let error msg lexbuf =
  Lexing.(raise (Error(lexbuf.lex_curr_p.pos_fname,
                       lexbuf.lex_curr_p.pos_lnum,
                       msg)))

let ill_formed_line lexbuf = error "ill-formed line" lexbuf

}

let whitespace = [' ' '\t' '\012' '\r']
let newline = '\r'* '\n'
let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '.']*

rule begline = parse
  | '#' [^ '\n']* ('\n' | eof)
      { Lexing.new_line lexbuf; begline lexbuf }
  | whitespace* (ident as key) whitespace* '='
      { let value = unquoted lexbuf in
        Hashtbl.add key_val_tbl key value;
        begline lexbuf }
  | eof
      { () }
  | _
      { ill_formed_line lexbuf }

and unquoted = parse
  | '\n' | eof    { Lexing.new_line lexbuf; contents () }
  | _ as c        { Buffer.add_char buf c; unquoted lexbuf }

{

(* The entry point *)

let read_config_file filename =
  let ic = open_in_bin filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.(lexbuf.lex_start_p <- {lexbuf.lex_start_p with pos_fname = filename});
  try
    Hashtbl.clear key_val_tbl;
    begline lexbuf;
    close_in ic
  with x ->
    close_in ic; raise x

let read_config_string str =
  let lexbuf = Lexing.from_string str in
  Lexing.(lexbuf.lex_start_p <- {lexbuf.lex_start_p with pos_fname = "(built-in)"});
  Hashtbl.clear key_val_tbl;
  begline lexbuf

(* Test harness *)
(*
open Printf

let _ =
  Hashtbl.clear key_val_tbl;
  begline (Lexing.from_channel stdin);
  Hashtbl.iter
    (fun key value ->
      printf "%s =" key;
      List.iter (fun v -> printf " |%s|" v) value;
      printf "\n")
    key_val_tbl
*)

}
