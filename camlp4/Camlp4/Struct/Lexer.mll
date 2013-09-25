(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright 2006-2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)



(* The lexer definition *)


{

(** A lexical analyzer. *)

(* FIXME interface module Make (Token : Token) |+ Note that this Token sig is not in Sig +| *)
(* : Sig.Lexer. S with module Loc = Token.Loc and module Token = Token; *)

(* type context =
{ loc        : Loc.t    ;
  in_comment : bool     ;
   |+* FIXME When True, all lexers built by [Plexer.make ()] do not lex the
       quotation syntax any more. Default is False (quotations are
       lexed). +|
  quotations : bool     };

value default_context : context;

value mk : Loc.t -> Stream.t char -> Stream.t (Token.t * Loc.t);

value mk' : context -> Stream.t char -> Stream.t (Token.t * Loc.t);              *)
(* FIXME Beware the context argument must be given like that:
 * mk' { (default_context) with ... = ... } strm
 *)

module TokenEval = Token.Eval
module Make (Token : Sig.Camlp4Token)
= struct
  module Loc = Token.Loc
  module Token = Token

  open Lexing
  open Sig

  (* Error report *)
  module Error = struct

    type t =
      | Illegal_character of char
      | Illegal_escape    of string
      | Unterminated_comment
      | Unterminated_string
      | Unterminated_quotation
      | Unterminated_antiquot
      | Unterminated_string_in_comment
      | Comment_start
      | Comment_not_end
      | Literal_overflow of string

    exception E of t

    open Format

    let print ppf =
      function
      | Illegal_character c ->
          fprintf ppf "Illegal character (%s)" (Char.escaped c)
      | Illegal_escape s ->
          fprintf ppf "Illegal backslash escape in string or character (%s)" s
      | Unterminated_comment ->
          fprintf ppf "Comment not terminated"
      | Unterminated_string ->
          fprintf ppf "String literal not terminated"
      | Unterminated_string_in_comment ->
          fprintf ppf "This comment contains an unterminated string literal"
      | Unterminated_quotation ->
          fprintf ppf "Quotation not terminated"
      | Unterminated_antiquot ->
          fprintf ppf "Antiquotation not terminated"
      | Literal_overflow ty ->
          fprintf ppf "Integer literal exceeds the range of representable integers of type %s" ty
      | Comment_start ->
          fprintf ppf "this is the start of a comment"
      | Comment_not_end ->
          fprintf ppf "this is not the end of a comment"

    let to_string x =
      let b = Buffer.create 50 in
      let () = bprintf b "%a" print x in Buffer.contents b
  end;;

  let module M = ErrorHandler.Register(Error) in ()

  open Error

  (* To store some context information:
  *   loc       : position of the beginning of a string, quotation and comment
  *   in_comment: are we in a comment?
  *   quotations: shall we lex quotation?
  *               If quotations is false it's a SYMBOL token.
  *   antiquots : shall we lex antiquotations.
  *)

  type context =
  { loc        : Loc.t    ;
    in_comment : bool     ;
    quotations : bool     ;
    antiquots  : bool     ;
    lexbuf     : lexbuf   ;
    buffer     : Buffer.t }

  let default_context lb =
  { loc        = Loc.ghost ;
    in_comment = false     ;
    quotations = true      ;
    antiquots  = false     ;
    lexbuf     = lb        ;
    buffer     = Buffer.create 256 }

  (* To buffer string literals, quotations and antiquotations *)

  let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
  let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
  let buff_contents c =
    let contents = Buffer.contents c.buffer in
    Buffer.reset c.buffer; contents

  let loc c = Loc.merge c.loc (Loc.of_lexbuf c.lexbuf)
  let quotations c = c.quotations
  let antiquots c = c.antiquots
  let is_in_comment c = c.in_comment
  let in_comment c = { (c) with in_comment = true }
  let set_start_p c = c.lexbuf.lex_start_p <- Loc.start_pos c.loc
  let move_start_p shift c = (* FIXME Please see PR#5820*)
    let p = c.lexbuf.lex_start_p in
    c.lexbuf.lex_start_p <- { (p) with pos_cnum = p.pos_cnum + shift }

  let update_loc c = { (c) with loc = Loc.of_lexbuf c.lexbuf }
  let with_curr_loc f c = f (update_loc c) c.lexbuf
  let parse_nested f c =
    with_curr_loc f c;
    set_start_p c;
    buff_contents c
  let shift n c = { (c) with loc = Loc.move `both n c.loc }
  let store_parse f c = store c ; f c c.lexbuf
  let parse f c = f c c.lexbuf
  let mk_quotation quotation c name loc shift =
    let s = parse_nested quotation (update_loc c) in
    let contents = String.sub s 0 (String.length s - 2) in
    QUOTATION { q_name     = name     ;
                q_loc      = loc      ;
                q_shift    = shift    ;
                q_contents = contents }


  (* Update the current location with file name and line number. *)

  let update_loc c file line absolute chars =
    let lexbuf = c.lexbuf in
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
                  | None -> pos.pos_fname
                  | Some s -> s
    in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }

    (* To convert integer literals, copied from "../parsing/lexer.mll" *)

    let cvt_int_literal s =
      - int_of_string ("-" ^ s)
    let cvt_int32_literal s =
      Int32.neg (Int32.of_string ("-" ^ s))
    let cvt_int64_literal s =
      Int64.neg (Int64.of_string ("-" ^ s))
    let cvt_nativeint_literal s =
      Nativeint.neg (Nativeint.of_string ("-" ^ s))


  let err error loc =
    raise(Loc.Exc_located(loc, Error.E error))

  let warn error loc =
    Format.eprintf "Warning: %a: %a@." Loc.print loc Error.print error

  }

  let newline = ('\010' | '\013' | "\013\010")
  let blank = [' ' '\009' '\012']
  let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
  let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
  let identchar =
    ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
  let ident = (lowercase|uppercase) identchar*
  let locname = ident
  let not_star_symbolchar =
    ['$' '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\']
  let symbolchar = '*' | not_star_symbolchar
  let quotchar =
    ['!' '%' '&' '+' '-' '.' '/' ':' '=' '?' '@' '^' '|' '~' '\\' '*']
  let hexa_char = ['0'-'9' 'A'-'F' 'a'-'f']
  let decimal_literal =
    ['0'-'9'] ['0'-'9' '_']*
  let hex_literal =
    '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  let oct_literal =
    '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
  let bin_literal =
    '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
  let int_literal =
    decimal_literal | hex_literal | oct_literal | bin_literal
  let float_literal =
    ['0'-'9'] ['0'-'9' '_']*
    ('.' ['0'-'9' '_']* )?
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

  (* Delimitors are extended (from 3.09) in a conservative way *)

  (* These chars that can't start an expression or a pattern: *)
  let safe_delimchars = ['%' '&' '/' '@' '^']

  (* These symbols are unsafe since "[<", "[|", etc. exsist. *)
  let delimchars = safe_delimchars | ['|' '<' '>' ':' '=' '.']

  let left_delims  = ['(' '[' '{']
  let right_delims = [')' ']' '}']

  let left_delimitor =
    (* At least a safe_delimchars *)
    left_delims delimchars* safe_delimchars (delimchars|left_delims)*

  (* A '(' or a new super '(' without "(<" *)
  | '(' (['|' ':'] delimchars*)?
  (* Old brackets, no new brackets starting with "[|" or "[:" *)
  | '[' ['|' ':']?
  (* Old "[<","{<" and new ones *)
  | ['[' '{'] delimchars* '<'
  (* Old brace and new ones *)
  | '{' (['|' ':'] delimchars*)?

  let right_delimitor =
    (* At least a safe_delimchars *)
    (delimchars|right_delims)* safe_delimchars (delimchars|right_delims)* right_delims
  (* A ')' or a new super ')' without ">)" *)
  | (delimchars* ['|' ':'])? ')'
  (* Old brackets, no new brackets ending with "|]" or ":]" *)
  | ['|' ':']? ']'
  (* Old ">]",">}" and new ones *)
  | '>' delimchars* [']' '}']
  (* Old brace and new ones *)
  | (delimchars* ['|' ':'])? '}'


  rule token c = parse
    | newline                            { update_loc c None 1 false 0; NEWLINE }
    | blank + as x                                                   { BLANKS x }
    | "~" (lowercase identchar * as x) ':'                            { LABEL x }
    | "?" (lowercase identchar * as x) ':'                         { OPTLABEL x }
    | lowercase identchar * as x                                     { LIDENT x }
    | uppercase identchar * as x                                     { UIDENT x }
    | int_literal as i
        { try  INT(cvt_int_literal i, i)
          with Failure _ -> err (Literal_overflow "int") (Loc.of_lexbuf lexbuf) }
    | float_literal as f
        { try  FLOAT(float_of_string f, f)
          with Failure _ -> err (Literal_overflow "float") (Loc.of_lexbuf lexbuf) }
    | (int_literal as i) "l"
        { try INT32(cvt_int32_literal i, i)
          with Failure _ -> err (Literal_overflow "int32") (Loc.of_lexbuf lexbuf) }
    | (int_literal as i) "L"
        { try  INT64(cvt_int64_literal i, i)
          with Failure _ -> err (Literal_overflow "int64") (Loc.of_lexbuf lexbuf) }
    | (int_literal as i) "n"
        { try NATIVEINT(cvt_nativeint_literal i, i)
          with Failure _ -> err (Literal_overflow "nativeint") (Loc.of_lexbuf lexbuf) }
    | '"'
        { with_curr_loc string c;
          let s = buff_contents c in STRING (TokenEval.string s, s)             }
    | "'" (newline as x) "'"
        { update_loc c None 1 false 1; CHAR (TokenEval.char x, x)               }
    | "'" ( [^ '\\' '\010' '\013']
          | '\\' (['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
                |['0'-'9'] ['0'-'9'] ['0'-'9']
                |'x' hexa_char hexa_char)
          as x) "'"                                { CHAR (TokenEval.char x, x) }
    | "'\\" (_ as c)
        { err (Illegal_escape (String.make 1 c)) (Loc.of_lexbuf lexbuf)         }
    | "(*"
        { store c; COMMENT(parse_nested comment (in_comment c))                 }
    | "(*)"
        { warn Comment_start (Loc.of_lexbuf lexbuf)                             ;
          parse comment (in_comment c); COMMENT (buff_contents c)               }
    | "*)"
        { warn Comment_not_end (Loc.of_lexbuf lexbuf)                           ;
          c.lexbuf.lex_curr_pos <- c.lexbuf.lex_curr_pos - 1;
          SYMBOL "*"                                       }
    | "<<" (quotchar* as beginning)
      { if quotations c
        then (move_start_p (-String.length beginning);
              mk_quotation quotation c "" "" 2)
        else parse (symbolchar_star ("<<" ^ beginning)) c                       }
    | "<<>>"
      { if quotations c
        then QUOTATION { q_name = ""; q_loc = ""; q_shift = 2; q_contents = "" }
        else parse (symbolchar_star "<<>>") c                                   }
    | "<@"
      { if quotations c then with_curr_loc maybe_quotation_at c
        else parse (symbolchar_star "<@") c                                     }
    | "<:"
      { if quotations c then with_curr_loc maybe_quotation_colon c
        else parse (symbolchar_star "<:") c                                     }
    | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
          ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
          [^ '\010' '\013'] * newline
      { let inum = int_of_string num
        in update_loc c name inum true 0; LINE_DIRECTIVE(inum, name)            }
    | '(' (not_star_symbolchar as op) ')'
                                             { ESCAPED_IDENT (String.make 1 op) }
    | '(' (not_star_symbolchar symbolchar* not_star_symbolchar as op) ')'
                                                             { ESCAPED_IDENT op }
    | '(' (not_star_symbolchar symbolchar* as op) blank+ ')'
                                                             { ESCAPED_IDENT op }
    | '(' blank+ (symbolchar* not_star_symbolchar as op) ')'
                                                             { ESCAPED_IDENT op }
    | '(' blank+ (symbolchar+ as op) blank+ ')'
                                                             { ESCAPED_IDENT op }
    | ( "#"  | "`"  | "'"  | ","  | "."  | ".." | ":"  | "::"
      | ":=" | ":>" | ";"  | ";;" | "_"
      | left_delimitor | right_delimitor ) as x  { SYMBOL x }
    | '$' { if antiquots c
            then with_curr_loc dollar (shift 1 c)
            else parse (symbolchar_star "$") c }
    | ['~' '?' '!' '=' '<' '>' '|' '&' '@' '^' '+' '-' '*' '/' '%' '\\'] symbolchar *
                                                                as x { SYMBOL x }
    | eof
      { let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with pos_bol  = pos.pos_bol  + 1 ;
                                        pos_cnum = pos.pos_cnum + 1 }; EOI      }
    | _ as c                 { err (Illegal_character c) (Loc.of_lexbuf lexbuf) }

  and comment c = parse
      "(*"
        { store c; with_curr_loc comment c; parse comment c                     }
    | "*)"                                                            { store c }
    | '<' (':' ident)? ('@' locname)? '<'
        { store c;
          if quotations c then with_curr_loc quotation c; parse comment c       }
    | ident                                             { store_parse comment c }
    | "\""
        { store c;
          begin try with_curr_loc string c
          with Loc.Exc_located(_, Error.E Unterminated_string) ->
            err Unterminated_string_in_comment (loc c)
          end;
          Buffer.add_char c.buffer '"';
          parse comment c }
    | "''"                                              { store_parse comment c }
    | "'''"                                             { store_parse comment c }
    | "'" newline "'"
      { update_loc c None 1 false 1; store_parse comment c                      }
    | "'" [^ '\\' '\'' '\010' '\013' ] "'"              { store_parse comment c }
    | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"     { store_parse comment c }
    | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"           { store_parse comment c }
    | "'\\" 'x' hexa_char hexa_char "'"                 { store_parse comment c }
    | eof
      { err Unterminated_comment (loc c)                                        }
    | newline
      { update_loc c None 1 false 0; store_parse comment c                      }
    | _                                                 { store_parse comment c }

  and string c = parse
      '"'                                                       { set_start_p c }
    | '\\' newline ([' ' '\t'] * as space)
        { update_loc c None 1 false (String.length space);
          store_parse string c                                                  }
    | '\\' ['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']           { store_parse string c }
    | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']                 { store_parse string c }
    | '\\' 'x' hexa_char hexa_char                       { store_parse string c }
    | '\\' (_ as x)
        { if is_in_comment c
          then store_parse string c
          else begin
            warn (Illegal_escape (String.make 1 x)) (Loc.of_lexbuf lexbuf);
            store_parse string c
          end }
    | newline
      { update_loc c None 1 false 0; store_parse string c                       }
    | eof                                     { err Unterminated_string (loc c) }
    | _                                                  { store_parse string c }

  and symbolchar_star beginning c = parse
    | symbolchar* as tok            { move_start_p (-String.length beginning) c ;
                                                        SYMBOL(beginning ^ tok) }

  and maybe_quotation_at c = parse
    | (ident as loc) '<'
      { mk_quotation quotation c "" loc (1 + String.length loc)                 }
    | symbolchar* as tok                                   { SYMBOL("<@" ^ tok) }

  and maybe_quotation_colon c = parse
    | (ident as name) '<'
      { mk_quotation quotation c name "" (1 + String.length name)               }
    | (ident as name) '@' (locname as loc) '<'
      { mk_quotation quotation c name loc
                     (2 + String.length loc + String.length name)               }
    | symbolchar* as tok                                   { SYMBOL("<:" ^ tok) }

  and quotation c = parse
    | '<' (':' ident)? ('@' locname)? '<'    {                          store c ;
                                                      with_curr_loc quotation c ;
                                                              parse quotation c }
    | ">>"                                                            { store c }
    | eof                                  { err Unterminated_quotation (loc c) }
    | newline                                     { update_loc c None 1 false 0 ;
                                                        store_parse quotation c }
    | _                                               { store_parse quotation c }

  and dollar c = parse
    | '$'                                     { set_start_p c; ANTIQUOT("", "") }
    | ('`'? (identchar*|['.' '!']+) as name) ':'
      { with_curr_loc (antiquot name) (shift (1 + String.length name) c)        }
    | _                                           { store_parse (antiquot "") c }

  and antiquot name c = parse
    | '$'                      { set_start_p c; ANTIQUOT(name, buff_contents c) }
    | eof                                   { err Unterminated_antiquot (loc c) }
    | newline
      { update_loc c None 1 false 0; store_parse (antiquot name) c              }
    | '<' (':' ident)? ('@' locname)? '<'
      { store c; with_curr_loc quotation c; parse (antiquot name) c             }
    | _                                         { store_parse (antiquot name) c }

  {

  let lexing_store s buff max =
    let rec self n s =
      if n >= max then n
      else
        match Stream.peek s with
        | Some x ->
            Stream.junk s;
            buff.[n] <- x;
            succ n
        | _ -> n
    in
    self 0 s

  let from_context c =
    let next _ =
      let tok = with_curr_loc token c in
      let loc = Loc.of_lexbuf c.lexbuf in
      Some ((tok, loc))
    in Stream.from next

  let from_lexbuf ?(quotations = true) lb =
    let c = { (default_context lb) with
              loc        = Loc.of_lexbuf lb;
              antiquots  = !Camlp4_config.antiquotations;
              quotations = quotations      }
    in from_context c

  let setup_loc lb loc =
    let start_pos = Loc.start_pos loc in
    lb.lex_abs_pos <- start_pos.pos_cnum;
    lb.lex_curr_p  <- start_pos

  let from_string ?quotations loc str =
    let lb = Lexing.from_string str in
    setup_loc lb loc;
    from_lexbuf ?quotations lb

  let from_stream ?quotations loc strm =
    let lb = Lexing.from_function (lexing_store strm) in
    setup_loc lb loc;
    from_lexbuf ?quotations lb

  let mk () loc strm =
    from_stream ~quotations:!Camlp4_config.quotations loc strm
end
}
