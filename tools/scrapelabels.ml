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

open Lexer301

let input_buffer = Buffer.create 16383
let input_function ic buf len =
  let len = input ic buf 0 len in
  Buffer.add_substring input_buffer buf 0 len;
  len

let output_buffer = Buffer.create 16383

let modified = ref false

type state = Out | Enter | In | Escape

let convert buffer =
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

let convert_file name =
  let ic = open_in name in
  Buffer.clear input_buffer;
  Buffer.clear output_buffer;
  modified := false;
  begin
    try convert (Lexing.from_function (input_function ic)); close_in ic
    with exn -> close_in ic; raise exn
  end;
  if !modified then begin
    let backup = name ^ ".bak" in
    if Sys.file_exists backup then Sys.remove backup;
    Sys.rename name backup;
    let oc = open_out name in
    Buffer.output_buffer oc output_buffer;
    close_out oc
  end

let _ =
  if Array.length Sys.argv < 2 || Sys.argv.(1) = "-h" || Sys.argv.(1) = "-help"
  then begin
    print_endline "Usage: scrapelabels <source file> ...";
    print_endline "Description:";
    print_endline
      "  Remove labels on non-optional arguments in function types.";
    print_endline
      "  To use only on interface (.mli) files.";
    print_endline "  Old files are renamed to <file>.bak.";
    exit 0
  end;
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    if Filename.check_suffix name ".mli" then begin
      prerr_endline ("Converting " ^ name);
      Printexc.catch convert_file name
    end
  done
