(* $Id$ *)

open Tk
open Jg_tk
open Parser

let tags =
  ["control"; "define"; "structure"; "char";
   "infix"; "label"; "uident"]
and colors =
    ["blue"; "forestgreen"; "purple"; "gray40";
     "indianred4"; "brown"; "midnightblue"]

let init_tags tw =
  List.iter2 tags colors fun:
  begin fun tag col ->
    Text.tag_configure tw :tag foreground:(`Color col)
  end;
  Text.tag_configure tw tag:"error" foreground:`Red;
  Text.tag_configure tw tag:"error" relief:`Raised;
  Text.tag_raise tw tag:"error"

let tag ?:start{=tstart} ?end:pend{=tend} tw =
  let tpos c = (Text.index tw index:start, [`Char c]) in
  let text = Text.get tw :start end:pend in
  let buffer = Lexing.from_string text in
  List.iter tags
    fun:(fun tag -> Text.tag_remove tw :start end:pend :tag);
  try
    while true do
    let tag =
      match Lexer.token buffer with
        AMPERAMPER
      | AMPERSAND
      | BARBAR
      | DO | DONE
      | DOWNTO
      | ELSE
      | FOR
      | IF
      |	LAZY
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
      |	INITIALIZER
      | LET
      | METHOD
      | MODULE
      | MUTABLE
      | NEW
      | OF
      | PARSER
      | PRIVATE
      | REC
      | TYPE
      | VAL
      | VIRTUAL
      	  -> "define"
      | BEGIN
      | END
      | INCLUDE
      |	OBJECT
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
      |	QUESTION3
      | SHARP
      	  -> "infix"
      | LABEL _
      | QUESTION
      	  -> "label"
      | UIDENT _ -> "uident"
      | EOF -> raise End_of_file
      | _ -> ""
    in
    if tag <> "" then
    Text.tag_add tw :tag
        start:(tpos (Lexing.lexeme_start buffer))
	end:(tpos (Lexing.lexeme_end buffer))
    done
  with
    End_of_file -> ()
  | Lexer.Error (err, s, e) -> ()
