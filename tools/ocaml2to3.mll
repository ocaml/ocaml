(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Jacques Garrigue, Kyoto University RIMS                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The lexer definition *)

{

type error =
  | Illegal_character
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
;;

exception Error of error * int * int

(* To store the position of the beginning of a string and comment *)
let string_start_pos = ref 0
and comment_start_pos = ref []
;;

(* Error report *)

let report_error = function
    Illegal_character ->
      prerr_string "Illegal character"
  | Unterminated_comment ->
      prerr_string "Comment not terminated"
  | Unterminated_string ->
      prerr_string "String literal not terminated"
  | Unterminated_string_in_comment ->
      prerr_string "This comment contains an unterminated string literal"
;;

let modified = ref false ;;

let b = Buffer.create 1024 ;;

}

let blank = [' ' '\010' '\013' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let symbolchar2 =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
    lowercase identchar * ':' [ ^ ':' '=' '>']
      { let s = Lexing.lexeme lexbuf in
      	lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 2;
        Buffer.add_string b (String.sub s 0 (String.length s - 2));
        Buffer.add_string b " ";
        modified := true;
        token lexbuf }
  | ':' lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
        Buffer.add_string b ": ";
        Buffer.add_string b (String.sub s 1 (String.length s - 1));
        modified := true;
        token lexbuf }
  | "\""
      { string_start_pos := Lexing.lexeme_start lexbuf;
        Buffer.add_string b "\"";
        string lexbuf;
        token lexbuf }
  | "(*"
      { comment_start_pos := [Lexing.lexeme_start lexbuf];
        Buffer.add_string b "(*";
        comment lexbuf;
        token lexbuf }
  | "?" 
      { Buffer.add_string b "??";
        modified := true;
        token lexbuf }
  | blank +
  | "_"
  | lowercase identchar *
  | uppercase identchar *
  | decimal_literal | hex_literal | oct_literal | bin_literal
  | float_literal
  | "'" [^ '\\' '\''] "'"
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | "#" [' ' '\t']* ['0'-'9']+ [^ '\n' '\r'] * ('\n' | '\r' | "\r\n")
  | "#"
  | "&"
  | "&&"
  | "`" 
  | "'" 
  | "(" 
  | ")" 
  | "*" 
  | "," 
  | "??"
  | "->"
  | "." 
  | ".."
  | ":" 
  | "::"
  | ":="
  | ":>"
  | ";" 
  | ";;"
  | "<" 
  | "<-"
  | "=" 
  | "[" 
  | "[|"
  | "[<"
  | "]" 
  | "{" 
  | "{="
  | "{<"
  | "|" 
  | "||"
  | "|]"
  | ">" 
  | ">]"
  | "}" 
  | ">}"
  | "!="
  | "-" 
  | "-."
  | ['!' '~'] symbolchar *
  | '?' symbolchar2 *
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
  | ['@' '^'] symbolchar *
  | ['+' '-'] symbolchar *
  | "**" symbolchar *
  | ['*' '/' '%'] symbolchar *
      { Buffer.add_string b (Lexing.lexeme lexbuf);
        token lexbuf }
  | eof { () }
  | _
      { raise (Error(Illegal_character,
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_start_pos := Lexing.lexeme_start lexbuf :: !comment_start_pos;
        Buffer.add_string b "(*";
        comment lexbuf;
      }
  | "*)"
      { Buffer.add_string b "*)";
        match !comment_start_pos with
        | [] -> assert false
        | [x] -> ()
        | _ :: l -> comment_start_pos := l;
                    comment lexbuf;
       }
  | "\""
      { string_start_pos := Lexing.lexeme_start lexbuf;
        Buffer.add_string b "\"";
        begin try string lexbuf
        with Error (Unterminated_string, _, _) ->
          let st = List.hd !comment_start_pos in
          raise (Error (Unterminated_string_in_comment, st, st + 2))
        end;
        comment lexbuf }
  | eof
      { let st = List.hd !comment_start_pos in
        raise (Error (Unterminated_comment, st, st + 2));
      }
  | "''"
  | "'" [^ '\\' '\''] "'"
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
  | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | _
      { Buffer.add_string b (Lexing.lexeme lexbuf);
        comment lexbuf }

and string = parse
    '"'
      { Buffer.add_char b '"' }
  | eof
      { raise (Error (Unterminated_string,
                      !string_start_pos, !string_start_pos+1)) }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
  | _
      { Buffer.add_string b (Lexing.lexeme lexbuf);
        string lexbuf }

{

let convert_file name =
  let ic = open_in name in
  Buffer.clear b;
  modified := false;
  begin
    try token (Lexing.from_channel ic); close_in ic
    with exn -> close_in ic; raise exn
  end;
  if !modified then begin 
    let backup = name ^ ".bak" in
    if Sys.file_exists backup then Sys.remove backup;
    Sys.rename name backup;
    let oc = open_out name in
    Buffer.output_buffer oc b;
    close_out oc
  end

let _ =
  if Array.length Sys.argv < 2 || Sys.argv.(1) = "-h" || Sys.argv.(1) = "-help"
  then begin
    print_endline "Usage: ocaml2to3 <source file> ...";
    print_endline "Description:";
    print_endline
      "Convert Objective Caml implementation or interface files to a syntax";
    print_endline
      "compatible with version 3. Old files are renamed to <file>.bak.";
    exit 0
  end;
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    prerr_endline ("Converting " ^ name);
    Printexc.catch convert_file name
  done

} 
