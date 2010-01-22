(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* An auxiliary lexer for determining the line number corresponding to
   a file position, honoring the directives # linenum "filename" *)

{
let filename = ref ""
let linenum = ref 0
let linebeg = ref 0

let parse_sharp_line s =
  try
    (* Update the line number and file name *)
    let l1 = ref 0 in
    while let c = s.[!l1] in c < '0' || c > '9' do incr l1 done;
    let l2 = ref (!l1 + 1) in
    while let c = s.[!l2] in c >= '0' && c <= '9' do incr l2 done;
    linenum := int_of_string(String.sub s !l1 (!l2 - !l1));
    let f1 = ref (!l2 + 1) in
    while !f1 < String.length s && s.[!f1] <> '"' do incr f1 done;
    let f2 = ref (!f1 + 1) in
    while !f2 < String.length s && s.[!f2] <> '"' do incr f2 done;
    if !f1 < String.length s then
      filename := String.sub s (!f1 + 1) (!f2 - !f1 - 1)
  with Failure _ | Invalid_argument _ ->
    Misc.fatal_error "Linenum.parse_sharp_line"
}

rule skip_line = parse
    "#" [' ' '\t']* ['0'-'9']+ [' ' '\t']*
    ("\"" [^ '\n' '\r' '"' (* '"' *) ] * "\"")?
    [^ '\n' '\r'] *
    ('\n' | '\r' | "\r\n")
      { parse_sharp_line(Lexing.lexeme lexbuf);
        linebeg := Lexing.lexeme_start lexbuf;
        Lexing.lexeme_end lexbuf }
  | [^ '\n' '\r'] *
    ('\n' | '\r' | "\r\n")
      { incr linenum;
        linebeg := Lexing.lexeme_start lexbuf;
        Lexing.lexeme_end lexbuf }
  | [^ '\n' '\r'] * eof
      { incr linenum;
        linebeg := Lexing.lexeme_start lexbuf;
        raise End_of_file }

{

let for_position file loc =
  let ic = open_in_bin file in
  let lb = Lexing.from_channel ic in
  filename := file;
  linenum := 1;
  linebeg := 0;
  begin try
    while skip_line lb <= loc do () done
  with End_of_file -> ()
  end;
  close_in ic;
  (!filename, !linenum - 1, !linebeg)

}
