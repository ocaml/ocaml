(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Xavier Leroy, projet Cristal, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char


(* The string buffering machinery *)

let initial_buffer = String.create 32

let buffer = ref initial_buffer
let bufpos = ref 0

let reset_buffer () =
  buffer := initial_buffer;
  bufpos := 0

let store c =
  if !bufpos >= String.length !buffer then begin
    let newbuffer = String.create (2 * !bufpos) in
    String.blit !buffer 0 newbuffer 0 !bufpos;
    buffer := newbuffer
  end;
  String.set !buffer !bufpos c;
  incr bufpos

let get_string () =
  let s = String.sub !buffer 0 !bufpos in buffer := initial_buffer; s

(* The lexer *)

let make_lexer keywords =

  let kwd_table = Hashtbl.create 17 in
  List.iter (fun s -> Hashtbl.add kwd_table s (Kwd s)) keywords;

  let ident_or_keyword id =
    try Hashtbl.find kwd_table id with Not_found -> Ident id

  and keyword_or_error c =
    let s = String.make 1 c in
      try Hashtbl.find kwd_table s
      with Not_found -> raise(Stream.Parse_error("Illegal character " ^ s)) in

  let rec next_token = parser
    [< '  ' '|'\010'|'\013'|'\009'|'\026'|'\012'; s >] ->
      next_token s
  | [< '  'A'..'Z'|'a'..'z'|'\192'..'\255' as c; s>] ->
      reset_buffer(); store c; ident s
  | [< '  '!'|'%'|'&'|'$'|'#'|'+'|'/'|':'|'<'|'='|'>'|'?'|'@'|'\\'|
              '~'|'^'|'|'|'*' as c; s >] ->
      reset_buffer(); store c; ident2 s
  | [< '  '0'..'9' as c; s>] ->
      reset_buffer(); store c; number s
  | [< '  '\''; c = char; '  '\'' >] ->
      Some(Char c)
  | [< '  '"' (* '"' *); s >] ->
      reset_buffer(); Some(String(string s))
  | [< '  '-'; s >] ->
      neg_number s
  | [< '  '('; s >] ->
      maybe_comment s
  | [< ' c >] ->
      Some(keyword_or_error c)
  | [< >] ->
      None

  and ident = parser
    [< '  'A'..'Z'|'a'..'z'|'\192'..'\255'|'0'..'9'|'_'|'\'' as c; s>] ->
      store c; ident s
  | [< >] ->
      Some(ident_or_keyword(get_string()))

  and ident2 = parser
    [< '  '!'|'%'|'&'|'$'|'#'|'+'|'-'|'/'|':'|'<'|'='|'>'|'?'|'@'|'\\'|
              '~'|'^'|'|'|'*' as c; s >] ->
      store c; ident2 s
  | [< >] ->
      Some(ident_or_keyword(get_string()))

  and neg_number = parser
    [< '  '0'..'9' as c; s >] ->
      reset_buffer(); store '-'; store c; number s
  | [< s >] ->
      reset_buffer(); store '-'; ident2 s
    
  and number = parser
    [< '  '0'..'9' as c; s >] ->
      store c; number s
  | [< '  '.'; s >] ->
      store '.'; decimal_part s
  | [< '  'e'|'E'; s >] ->
      store 'E'; exponent_part s
  | [< >] ->
      Some(Int(int_of_string(get_string())))

  and decimal_part = parser
    [< '  '0'..'9' as c; s >] ->
      store c; decimal_part s
  | [< '  'e'|'E'; s >] ->
      store 'E'; exponent_part s
  | [< >] ->
      Some(Float(float_of_string(get_string())))

  and exponent_part = parser
    [< '  '+'|'-' as c; s >] ->
      store c; end_exponent_part s
  | [< s >] ->
      end_exponent_part s

  and end_exponent_part = parser
    [< '  '0'..'9' as c; s >] ->
      store c; end_exponent_part s
  | [< >] ->
      Some(Float(float_of_string(get_string())))

  and string = parser
    [< '  '"' (* '"' *)  >] -> get_string()
  | [< '  '\\'; c = escape; s >] -> store c; string s
  | [< ' c; s >] -> store c; string s

  and char = parser
    [< '  '\\'; c = escape >] -> c
  | [< ' c >] -> c

  and escape = parser
    [< '  'n' >] -> '\n'
  | [< '  'r' >] -> '\r'
  | [< '  't' >] -> '\t'
  | [< '  '0'..'9' as c1; '  '0'..'9' as c2; '  '0'..'9' as c3 >] ->
      Char.chr((Char.code c1 - 48) * 100 +
               (Char.code c2 - 48) * 10 +
               (Char.code c3 - 48))
  | [< ' c >] -> c

  and maybe_comment = parser
    [< '  '*'; s >] -> comment s; next_token s
  | [< >] -> Some(keyword_or_error '(')

  and comment = parser
    [< '  '('; s >] -> maybe_nested_comment s
  | [< '  '*'; s >] -> maybe_end_comment s
  | [< ' c; s >] -> comment s

  and maybe_nested_comment = parser
    [< '  '*'; s >] -> comment s; comment s
  | [< ' c; s >] -> comment s

  and maybe_end_comment = parser
    [< '  ')' >] -> ()
  | [< '  '*'; s >] -> maybe_end_comment s
  | [< ' c; s >] -> comment s

  in fun input -> Stream.from (fun count -> next_token input)
