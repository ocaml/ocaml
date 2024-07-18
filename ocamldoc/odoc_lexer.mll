{
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The lexer for special comments. *)

open Lexing
open Odoc_parser

let line_number = ref 0


let string_buffer = Buffer.create 32

(** Reset the buffer *)
let reset_string_buffer () = Buffer.reset string_buffer

(** Add a character to the buffer *)
let add_char_string = Buffer.add_char string_buffer

(** Add a string to the buffer. *)
let add_string = Buffer.add_string string_buffer

let read_string () = Buffer.contents string_buffer

(** The variable which will contain the description string.
   Is initialized when we encounter the start of a special comment. *)
let description = ref ""

let blank = "[ \013\009\012]"

(** The nested comments level. *)
let comments_level = ref 0

(** This function returns the given string without the leading and trailing blanks.*)
let remove_blanks s =
  let l = Str.split_delim (Str.regexp "\n") s in
  let l2 =
    let rec iter liste =
      match liste with
        h :: q ->
          let h2 = Str.global_replace (Str.regexp ("^"^blank^"+")) "" h in
          if h2 = "" then
            (
             (* we remove this line and must remove leading blanks of the next one *)
             iter q
            )
          else
            (* we don't remove leading blanks in the remaining lines *)
            h2 :: q
      | _ ->
          []
    in iter l
  in
  let l3 =
    let rec iter liste =
      match liste with
        h :: q ->
          let h2 = Str.global_replace (Str.regexp (blank^"+$")) "" h in
          if h2 = "" then
            (
             (* we remove this line and must remove trailing blanks of the next one *)
             iter q
            )
          else
            (* we don't remove trailing blanks in the remaining lines *)
            h2 :: q
      | _ ->
          []
    in
    List.rev (iter (List.rev l2))
  in
  String.concat "\n" l3

(** Remove first blank characters of each line of a string, until the first '*' *)
let remove_stars s =
  Str.global_replace (Str.regexp ("^"^blank^"*\\*")) "" s

let validate_encoding raw_name =
  match Misc.Utf8_lexeme.normalize raw_name with
  | Error s -> failwith (Format.asprintf "Invalid encoding %s" s)
  | Ok name -> name

let validate_ident raw_name =
  let name = validate_encoding raw_name in
  match Misc.Utf8_lexeme.validate_identifier name with
  | Misc.Utf8_lexeme.Valid -> name
  | Misc.Utf8_lexeme.Invalid_character u ->
    failwith (Format.asprintf "Invalid character U+%X" (Uchar.to_int u))
  | Misc.Utf8_lexeme.Invalid_beginning u  ->
    failwith (Format.asprintf "Invalid first character U+%X" (Uchar.to_int u))

 let validate_exception_uident raw_name =
    let name = validate_ident raw_name in
    if Misc.Utf8_lexeme.is_capitalized name then name else
      failwith (Format.asprintf "Invalid exception name: %s" name)
}

let blank = [ ' ' '\013' '\009' '\012']
let nl_blank = blank | '\010'
let notblank = [^ ' ' '\010' '\013' '\009' '\012']

let lowercase = ['a'-'z' '_']
let uppercase = ['A'-'Z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']
let utf8 = ['\192'-'\255'] ['\128'-'\191']*
let identchar_ext = identchar | utf8
let identstart_ext = lowercase | uppercase | utf8
let ident_ext = identstart_ext identchar_ext*

rule main = parse
    [' ' '\013' '\009' '\012'] +
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        main lexbuf
      }

  | [ '\010' ]
      {
        incr line_number;
        incr Odoc_comments_global.nb_chars;
        main lexbuf
      }
  | "(**)"
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        Description ("", None)
      }

  | "(**"("*"+)")"
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        main lexbuf
      }

  | "(***"
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level;
        main lexbuf
      }

  | "(**"
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level;
        if !comments_level = 1 then
          (
           reset_string_buffer ();
           description := "";
           special_comment lexbuf
          )
        else
          main lexbuf
      }

  | eof
      { EOF }

  |  "*)"
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        decr comments_level ;
        main lexbuf
      }

  |  "(*"
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level ;
        main lexbuf
      }

  | _
      {
        incr Odoc_comments_global.nb_chars;
        main lexbuf
      }

and special_comment = parse
  | "*)"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        if !comments_level = 1 then
          (
           (* there is just a description *)
           let s2 = read_string () in
           let s3 = remove_blanks s2 in
           let s4 =
             if !Odoc_global.remove_stars then
               remove_stars s3
             else
               s3
           in
           Description (s4, None)
          )
        else
          (
           add_string s;
           decr comments_level;
           special_comment lexbuf
          )
      }

  |  "(*"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        incr comments_level ;
        add_string s;
        special_comment lexbuf
      }

  | "\\@"
      {
        let s = Lexing.lexeme lexbuf in
        let c = (Lexing.lexeme_char lexbuf 1) in
        add_char_string c;
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        special_comment lexbuf
      }

  | "@"lowercase+
      {
        (* we keep the description before we go further *)
        let s = read_string () in
        description := remove_blanks s;
        reset_string_buffer ();
        let len = String.length (Lexing.lexeme lexbuf) in
        lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - len;
        lexbuf.Lexing.lex_curr_p <-
          { lexbuf.Lexing.lex_curr_p with
            pos_cnum = lexbuf.Lexing.lex_curr_p.pos_cnum - len
          } ;
        (* we don't increment the Odoc_comments_global.nb_chars *)
        special_comment_part2 lexbuf
      }

  | _
      {
        let c = (Lexing.lexeme_char lexbuf 0) in
        add_char_string c;
        if c = '\010' then incr line_number;
        incr Odoc_comments_global.nb_chars;
        special_comment lexbuf
      }

and special_comment_part2 = parse
  | "*)"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        if !comments_level = 1 then
          (* finally we return the description we kept *)
          let desc =
            if !Odoc_global.remove_stars then
              remove_stars !description
             else
              !description
          in
          let remain = read_string () in
          let remain2 =
            if !Odoc_global.remove_stars then
              remove_stars remain
             else
               remain
          in
          Description (desc, Some remain2)
        else
          (
           add_string s ;
           decr comments_level ;
           special_comment_part2 lexbuf
          )
      }

  |  "(*"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        add_string s;
        incr comments_level ;
        special_comment_part2 lexbuf
      }

  | _
      {
        let c = (Lexing.lexeme_char lexbuf 0) in
        add_char_string c;
        if c = '\010' then incr line_number;
        incr Odoc_comments_global.nb_chars;
        special_comment_part2 lexbuf
      }

and elements = parse
  | blank+
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        elements lexbuf
      }

  | [ '\010' ]
      { incr line_number;
        incr Odoc_comments_global.nb_chars;
        elements lexbuf }
  | "@"
      {
        raise (Failure (Odoc_messages.should_escape_at_sign))
      }

  | "@param" nl_blank+ (identchar+ as id) nl_blank+ { T_PARAM id }
  | "@param" nl_blank+ (identchar_ext+ as raw_id) nl_blank+ {
     let id = validate_ident raw_id in
     T_PARAM id
     }
  | "@param" { failwith "usage: @param id description"}
  | "@before" nl_blank+ (notblank+ as v) nl_blank+ {
     let v = validate_encoding v in
     T_BEFORE v }
  | "@before" { failwith "usage: @before version description"}
  | "@raise" nl_blank+ (ident_ext ('.' ident_ext)* as exn_path) nl_blank+
    {  let raw_path = String.split_on_char '.' exn_path in
       let path = List.map validate_exception_uident raw_path in
       let id = String.concat "." path in
       T_RAISES id }
  | "@raise" { failwith "usage: @raise Exception description"}
  | "@"lowercase+
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        let s2 = String.sub s 1 ((String.length s) - 1) in
        match s2 with
         | "author" ->
            T_AUTHOR
         | "version" ->
             T_VERSION
         | "see" ->
             T_SEE
         | "since" ->
             T_SINCE
         | "deprecated" ->
             T_DEPRECATED
         | "return" ->
             T_RETURN
         | s ->
             if !Odoc_global.no_custom_tags then
               raise (Failure (Odoc_messages.not_a_valid_tag s))
             else
               T_CUSTOM s
      }

  | ("\\@" | [^'@'])+
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        let s = Lexing.lexeme lexbuf in
        let s = Str.global_replace (Str.regexp_string "\\@") "@" s in
        let s = remove_blanks s in
        Desc s
      }
  | eof
      {
        EOF
      }
  | _ {
        let s = Lexing.lexeme lexbuf in
        failwith ("Unexpected character '"^s^"'")
      }


and simple = parse
    [' ' '\013' '\009' '\012'] +
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        simple lexbuf
      }

  | [ '\010' ]
      { incr line_number;
        incr Odoc_comments_global.nb_chars;
        simple lexbuf
      }

  | "(**"("*"+)
      {
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length (Lexing.lexeme lexbuf));
        incr comments_level;
        simple lexbuf
      }

  | "(*"("*"+)")"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        simple lexbuf
      }
  | "(**"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        incr comments_level;
        simple lexbuf
      }

  | "(*"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        incr comments_level;
        if !comments_level = 1 then
          (
           reset_string_buffer ();
           description := "";
           special_comment lexbuf
          )
        else
          (
           add_string s;
           simple lexbuf
          )
      }

  | eof
      { EOF }

  |  "*)"
      {
        let s = Lexing.lexeme lexbuf in
        Odoc_comments_global.nb_chars := !Odoc_comments_global.nb_chars + (String.length s);
        decr comments_level ;
        simple lexbuf
      }

  | _
      {
        incr Odoc_comments_global.nb_chars;
        simple lexbuf
      }
