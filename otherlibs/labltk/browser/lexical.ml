(*************************************************************************)
(*                                                                       *)
(*                         OCaml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
open Tk
open Jg_tk
open Parser

let tags =
  ["control"; "define"; "structure"; "char";
   "infix"; "label"; "uident"]
and colors =
    ["blue"; "forestgreen"; "purple"; "gray40";
     "indianred4"; "saddlebrown"; "midnightblue"]

let init_tags tw =
  List.iter2 tags colors ~f:
  begin fun tag col ->
    Text.tag_configure tw ~tag ~foreground:(`Color col)
  end;
  Text.tag_configure tw ~tag:"error" ~foreground:`Red;
  Text.tag_configure tw ~tag:"error" ~relief:`Raised;
  Text.tag_raise tw ~tag:"error"

let tag ?(start=tstart) ?(stop=tend) tw =
  let tpos c = (Text.index tw ~index:start, [`Char c]) in
  let text = Text.get tw ~start ~stop in
  let buffer = Lexing.from_string text in
  Location.init buffer "";
  Location.input_name := "";
  List.iter tags
    ~f:(fun tag -> Text.tag_remove tw ~start ~stop ~tag);
  let last = ref (EOF, 0, 0) in
  try
    while true do
    let token = Lexer.token buffer
    and start = Lexing.lexeme_start buffer
    and stop = Lexing.lexeme_end buffer in
    let tag =
      match token with
        AMPERAMPER
      | AMPERSAND
      | BARBAR
      | DO | DONE
      | DOWNTO
      | ELSE
      | FOR
      | IF
      | LAZY
      | MATCH
      | OR
      | THEN
      | TO
      | TRY
      | WHEN
      | WHILE
      | WITH
          -> "control"
      | AND
      | AS
      | BAR
      | CLASS
      | CONSTRAINT
      | EXCEPTION
      | EXTERNAL
      | FUN
      | FUNCTION
      | FUNCTOR
      | IN
      | INHERIT
      | INITIALIZER
      | LET
      | METHOD
      | MODULE
      | MUTABLE
      | NEW
      | OF
      | PRIVATE
      | REC
      | TYPE
      | VAL
      | VIRTUAL
          -> "define"
      | BEGIN
      | END
      | INCLUDE
      | OBJECT
      | OPEN
      | SIG
      | STRUCT
          -> "structure"
      | CHAR _
      | STRING _
          -> "char"
      | BACKQUOTE
      | INFIXOP1 _
      | INFIXOP2 _
      | INFIXOP3 _
      | INFIXOP4 _
      | PREFIXOP _
      | SHARP
          -> "infix"
      | LABEL _
      | OPTLABEL _
      | QUESTION
      | TILDE
          -> "label"
      | UIDENT _ -> "uident"
      | LIDENT _ ->
          begin match !last with
            (QUESTION | TILDE), _, _ -> "label"
          | _ -> ""
          end
      | COLON ->
          begin match !last with
            LIDENT _, lstart, lstop ->
              if lstop = start then
                Text.tag_add tw ~tag:"label"
                  ~start:(tpos lstart) ~stop:(tpos stop);
              ""
          | _ -> ""
          end
      | EOF -> raise End_of_file
      | _ -> ""
    in
    if tag <> "" then
      Text.tag_add tw ~tag ~start:(tpos start) ~stop:(tpos stop);
    last := (token, start, stop)
    done
  with
    End_of_file -> ()
  | Lexer.Error (err, loc) -> ()
