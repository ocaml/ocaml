(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Jacques Garrigue, Kyoto University RIMS                *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open StdLabels
open Lexer301

let input_buffer = Buffer.create 16383
let input_function ic buf len =
  let len = input ic buf 0 len in
  Buffer.add_substring input_buffer buf 0 len;
  len

let output_buffer = Buffer.create 16383

let modified = ref false

let modules =
  ref [ "Arg"; "BigArray"; "Buffer"; "Condition"; "Dbm"; "Digest"; "Dynlink";
        "Event"; "Filename"; "Format"; "Gc"; "Genlex"; "Graphics";
        "Hashtbl"; "Lexing"; "Map"; "Marshal"; "Mutex"; "Parsing";
        "Pervasives"; "Queue"; "Set"; "Sort"; "Stack"; "Str";
        "Stream"; "Sys"; "Thread"; "ThreadUnix"; "Weak" ]

exception Closing of token

let convert_impl buffer =
  let input_pos = ref 0 in
  let copy_input stop =
    Buffer.add_substring output_buffer (Buffer.contents input_buffer)
      !input_pos (stop - !input_pos);
    input_pos := stop
  in
  let next_token () =
    let token = Lexer301.token buffer
    and start = Lexing.lexeme_start buffer
    and stop = Lexing.lexeme_end buffer in
    match token with
      RPAREN | RBRACKET |BARRBRACKET | GREATERRBRACKET | END
    | RBRACE | GREATERRBRACE ->
        raise (Closing token)
    | EOF ->
        raise End_of_file
    |  _ ->
        (token, start, stop)
  in
  let rec may_start (token, s, e) =
    match token with
      LIDENT _ -> search_start (dropext (next_token ()))
    | UIDENT m when List.mem m !modules ->
        may_discard (dropext (next_token ()))
    | _ -> search_start (token, s, e)

  and dropext (token, s, e) =
    match token with
      DOT ->
        let (token, s, e) = next_token () in
        begin match token with
          LPAREN | LBRACKET | LBRACE ->
            process_paren (token, s, e);
            dropext (next_token ())
        | UIDENT _ | LIDENT _ ->
            dropext (next_token ())
        | _ ->
            prerr_endline ("bad index at position " ^ string_of_int s);
            (token, s, e)
        end
    | _ ->
        (token, s, e)

  and may_discard (token, s, e) =
    match token with
      TILDE | LABEL _ ->
        modified := true;
        copy_input s; input_pos := e;
        may_discard (next_token ())
    | LPAREN | LBRACKET | LBRACKETBAR | LBRACKETLESS | BEGIN
    | LBRACE | LBRACELESS ->
        process_paren (token, s, e);
        may_discard (next_token ())
    | PREFIXOP _ ->
        may_discard (next_token ())
    | LIDENT _ | UIDENT _ ->
        may_discard (dropext (next_token ()))
    | BACKQUOTE ->
        ignore (next_token ());
        may_discard (next_token ())
    | INT _ | CHAR _ | STRING _ | FLOAT _ | FALSE | TRUE ->
        may_discard (next_token ())
    | _ ->
        search_start (token, s, e)

  and search_start (token, s, e) =
    match token with
      LPAREN | LBRACKET | LBRACKETBAR | LBRACKETLESS | BEGIN
    | LBRACE | LBRACELESS ->
        process_paren (token, s, e);
        search_start (next_token ())
    | EQUAL | SEMI | SEMISEMI | MINUSGREATER | LESSMINUS | COMMA
    | IF | THEN | ELSE | WHILE | TO | DOWNTO | DO | IN | MATCH | TRY
    | INFIXOP0 _ | INFIXOP1 _ | INFIXOP2 _ | INFIXOP3 _ | INFIXOP4 _
    | PLUS | MINUS | MINUSDOT | STAR | LESS | GREATER
    | OR | BARBAR | AMPERSAND | AMPERAMPER | COLONEQUAL ->
        may_start (next_token ())
    | _ ->
        search_start (next_token ())

  and process_paren (token, s, e) =
    try match token with
      LPAREN | LBRACKET | LBRACKETBAR | LBRACKETLESS | BEGIN ->
        may_start (next_token ())
    | LBRACE | LBRACELESS ->
        search_start (next_token ())
    | _ ->
        assert false
    with Closing last ->
      match token, last with
        LPAREN, RPAREN 
      | (LBRACKET|LBRACKETBAR|LBRACKETLESS),
        (RBRACKET|BARRBRACKET|GREATERRBRACKET)
      | BEGIN, END
      | LBRACE, RBRACE
      | LBRACELESS, GREATERRBRACE -> ()
      | _ -> raise (Closing last)
  in
  try
    may_start (next_token ());
  with End_of_file | Closing _ ->
    copy_input (Buffer.length input_buffer)

type state = Out | Enter | In | Escape

let convert_intf buffer =
  let input_pos = ref 0 in
  let copy_input stop =
    Buffer.add_substring output_buffer (Buffer.contents input_buffer)
      !input_pos (stop - !input_pos);
    input_pos := stop
  in
  let last = ref (EOF, 0, 0) in
  let state = ref Out in
  try while true do
    let token = Lexer301.token buffer
    and start = Lexing.lexeme_start buffer
    and stop = Lexing.lexeme_end buffer
    and last_token, last_start, last_stop = !last in
    begin match token with
    | EXCEPTION | CONSTRAINT ->
        state := In
    | VAL | EXTERNAL | CLASS | METHOD | TYPE | AND ->
        state := Enter
    | EQUAL when !state = Enter ->
        state := In
    | COLON ->
        begin match !state, last_token with
        | In, LIDENT _ ->
            modified := true;
            copy_input last_start;
            input_pos := stop
        | Enter, _ ->
            state := In
        | Escape, _ ->
            state := In
        | _ ->
            state := Out
        end
    | LBRACE | SEMI | QUESTION when !state = In ->
        state := Escape
    | SEMISEMI | SIG | STRUCT | END | OBJECT | OPEN | INCLUDE | MODULE ->
        state := Out
    | EOF -> raise End_of_file
    | _ -> ()
    end;
    last := (token, start, stop)
  done with
    End_of_file ->
      copy_input (Buffer.length input_buffer)

let convert_file ~intf name =
  let ic = open_in name in
  Buffer.clear input_buffer;
  Buffer.clear output_buffer;
  modified := false;
  begin
    let convert = if intf then convert_intf else convert_impl in
    try convert (Lexing.from_function (input_function ic)); close_in ic
    with exn -> close_in ic; raise exn
  end;
  if !modified then begin
    let backup = name ^ ".bak" in
    if Sys.file_exists backup then Sys.remove name
    else Sys.rename name backup;
    let oc = open_out name in
    Buffer.output_buffer oc output_buffer;
    close_out oc
  end
  else prerr_endline ("No labels erased in " ^ file)

let _ =
  let files = ref [] and intf = ref false and keepstd = ref false in
  Arg.parse
    [ "-intf", Arg.Set intf,
      " remove all non-optional labels from an interface";
      "-keepstd", Arg.Set keepstd,
      " keep labels for Array, List, String and Unix";
      "-m", Arg.String (fun s -> modules := s :: !modules),
      "<module>  remove also labels for <module>" ]
    (fun s -> files := s :: !files)
    ("Usage: scrapelabels <options> <source files>\n" ^
     "  Remove labels from function arguments in standard library modules.\n" ^
     "  With -intf option below, can also process interfaces.\n" ^
     "  Old files are renamed to <file>.bak if there is no backup yet.\n" ^
     "Options are:");
  if not !keepstd then
    modules := ["Array"; "List"; "String"; "Unix"] @ !modules;
  List.iter (List.rev !files) ~f:
    begin fun name ->
      prerr_endline ("Processing " ^ name);
      Printexc.catch (convert_file ~intf:!intf) name
    end
