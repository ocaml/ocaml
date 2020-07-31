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

let key_val_tbl : (string, string list) Hashtbl.t = Hashtbl.create 17

let key_val key =
  Hashtbl.find_opt key_val_tbl key

(* Auxiliaries for parsing *)

let buf = Buffer.create 32

let stash inword words =
  if inword then begin
    let w = Buffer.contents buf in
    Buffer.clear buf;
    w :: words
  end else
    words

(* Error reporting *)

exception Error of string * int * string

let error msg lexbuf =
  Lexing.(raise (Error(lexbuf.lex_curr_p.pos_fname,
                       lexbuf.lex_curr_p.pos_lnum,
                       msg)))

let ill_formed_line lexbuf = error "ill-formed line" lexbuf
let unterminated_quote lexbuf = error "unterminated quote" lexbuf
let lone_backslash lexbuf = error "lone \\ (backslash) at end of file" lexbuf

}

let whitespace = [' ' '\t' '\012' '\r']
let newline = '\r'* '\n'
let ident = ['A'-'Z' 'a'-'z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_' '.']*

rule begline = parse
  | '#' [^ '\n']* ('\n' | eof)
      { Lexing.new_line lexbuf; begline lexbuf }
  | whitespace* (ident as key) whitespace* '='
      { let words = unquoted false [] lexbuf in
        Hashtbl.add key_val_tbl key (List.rev words);
        begline lexbuf }
  | eof
      { () }
  | _
      { ill_formed_line lexbuf }

and unquoted inword words = parse
  | '\n' | eof    { Lexing.new_line lexbuf; stash inword words }
  | whitespace+   { unquoted false (stash inword words) lexbuf }
  | '\\' newline  { Lexing.new_line lexbuf; unquoted inword words lexbuf }
  | '\\' (_ as c) { Buffer.add_char buf c; unquoted true words lexbuf }
  | '\\' eof      { lone_backslash lexbuf }
  | '\''          { singlequote lexbuf; unquoted true words lexbuf }
  | '\"'          { doublequote lexbuf; unquoted true words lexbuf }
  | _ as c        { Buffer.add_char buf c; unquoted true words lexbuf }

and singlequote = parse
  | eof           { unterminated_quote lexbuf }
  | '\''          { () }
  | newline       { Lexing.new_line lexbuf;
                    Buffer.add_char buf '\n'; singlequote lexbuf }
  | _ as c        { Buffer.add_char buf c; singlequote lexbuf }

and doublequote = parse
  | eof           { unterminated_quote lexbuf }
  | '\"'          { () }
  | '\\' newline  { Lexing.new_line lexbuf; doublequote lexbuf }
  | '\\' (['$' '`' '\"' '\\'] as c)
                  { Buffer.add_char buf c; doublequote lexbuf }
  | newline       { Lexing.new_line lexbuf;
                    Buffer.add_char buf '\n'; doublequote lexbuf }
  | _ as c        { Buffer.add_char buf c; doublequote lexbuf }

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
