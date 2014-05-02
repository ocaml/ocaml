{
(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                             Leo White                               *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The lexer for string to build text structures. *)

open Documentation
open Docparser
open Docerr
open Lexing

(* Update the current location with file name and line number. *)

let incr_line lexbuf =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with
        pos_lnum = pos.pos_lnum + 1;
        pos_bol = pos.pos_cnum; }

(* To buffer verbatim strings and code sections *)

let string_buffer = Buffer.create 32

let buffer_empty = ref true

let reset_string_buffer () =
  Buffer.reset string_buffer;
  buffer_empty := true

let buffer_char c =
  Buffer.add_char string_buffer c;
  buffer_empty := false

let buffer_lexeme lexbuf =
  let s = Lexing.lexeme lexbuf in
    Buffer.add_string string_buffer s;
    buffer_empty := false

let get_raw_buffered_string () = Buffer.contents string_buffer

let remove_opening_blanks s =
  let length = String.length s in
  let rec loop i =
    if i >= length then "" else
    match s.[i] with
    | ' ' | '\009' | '\012' -> loop (i + 1)
    | '\010' ->
        String.sub s (i + 1) (length - (i + 1))
    | '\013' ->
        let j = i + 1 in
          if j >= length && s.[j] = '\010' then
            String.sub s (j + 1) (length - (j + 1))
          else
            String.sub s j (length - j)
    | _ -> String.sub s i (length - i)
  in
    loop 0

let remove_closing_blanks s =
  let length = String.length s in
  let rec loop i =
    if i < 0 then "" else
    match s.[i] with
    | ' ' | '\009' | '\012' -> loop (i - 1)
    | '\010' ->
        let j = i - 1 in
          if j >= 0 && s.[j] = '\013' then
            String.sub s 0 j
          else
            String.sub s 0 i
    | _ -> String.sub s 0 (i + 1)
  in
    loop (length - 1)

let get_buffered_string () =
  get_raw_buffered_string ()
  |> remove_opening_blanks
  |> remove_closing_blanks

let buffer_not_empty () = not !buffer_empty

(* To store the position of the beginning of a
   verbatim string or code section *)
let string_start_loc = ref Location.none;;
let code_start_locs = ref [];;

(* To store the format of a target *)
let target_format = ref None;;

(* To store the kind of a reference *)
let ref_kind = ref RK_element;;

(* To store the modules of a module list *)
let module_list_modules = ref [];;

let reset_module_list () =
  module_list_modules := [];;

let add_module md =
  module_list_modules := md :: !module_list_modules

let get_module_list () =
  List.rev !module_list_modules

(* Hash table of styles (initialized below) *)
let style_table = Hashtbl.create 19

(* Hash table of reference kinds (initialized below) *)
let ref_kind_table = Hashtbl.create 19

(* Hash table of tags (initialized below) *)
let tag_table = Hashtbl.create 19

}

let newline = ('\010' | "\013\010" )
let blank = [' ' '\009' '\012']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let alpha = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' 'A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let versionchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
   '0'-'9' '+' '-' '.' '/' ':' '<' '=' '>' '?' '^' '~' ]
let namechar =
  ['a'-'z' '\223'-'\246' '\248'-'\255' 'A'-'Z' '\192'-'\214'
   '\216'-'\222' '.' '-' ' ' ]

let ident = alpha identchar*
let version = versionchar+
let uident = uppercase identchar*
let exception = uident ('.' uident)*
let name = namechar+

(* The characters which are not the start of any tokens other than Char *)
let safe = [^ ' ' '\009' '\012' '\010' '\013' '\\' '{' '}' '[' ']' '<' 'v' '%' '@' '-' '+']

let escape = '\\' (['{' '}' '[' ']' '@'] as chr)

let tag = '@'

let begin = '{'
let end = '}'

let item = '-'
let superscript = '^'
let subscript = '_'

let verb = 'v'

let target = '%'
let target_format = (identchar+ as fmt) ':'

let begin_code = "["
let end_code = "]"

let begin_pre_code = "{["
let end_pre_code = "]}"

let link = ':'

let ref = '!'
let ref_kind = (ident as kind) ':'

let title = decimal_literal as num
let title_label =  ":" (ident as lbl)

(* Shortcut format for lists *)

let minus = '-'
let plus = '+'

(* html marks, to use as alternative markup *)

let html_bold = '<' (['b' 'B'] as tag) blank* '>'
let html_end_bold = "</" ['b' 'B'] blank* '>'
let html_italic = '<' (['i' 'I'] as tag) blank* '>'
let html_end_italic = "</" ['i' 'I'] blank* '>'
let html_title = '<' (['h' 'H'](['0'-'9']+ as num) as tag) blank* '>'
let html_end_title = "</" ['h' 'H'](['0'-'9']+ as num) blank* '>'
let html_list = '<' (['u' 'U']['l' 'L'] as tag) blank* '>'
let html_end_list = "</" ['u' 'U']['l' 'L'] blank* '>'
let html_enum = '<' (['o' 'O']['l' 'L'] as tag) blank* '>'
let html_end_enum = "</" ['o' 'O']['l' 'L'] blank* '>'
let html_item = '<' (['l' 'L']['i' 'I'] as tag) blank* '>'
let html_end_item = "</" ['l' 'L']['i' 'I'] blank* '>'
let html_code = '<' ['c' 'C']['o' 'O']['d' 'D']['e' 'E'] blank* '>'
let html_end_code = "</" ['c' 'C']['o' 'O']['d' 'D']['e' 'E'] blank* '>'
let html_center = '<' (['c' 'C']['e' 'E']['n' 'N']['t' 'T']['e' 'E']['r' 'R'] as tag) blank* '>'
let html_end_center = "</" ['c' 'C']['e' 'E']['n' 'N']['t' 'T']['e' 'E']['r' 'R'] blank* '>'
let html_left = '<' (['l' 'L']['e' 'E']['f' 'F']['t' 'T'] as tag) blank* '>'
let html_end_left = "</" ['l' 'L']['e' 'E']['f' 'F']['t' 'T'] blank* '>'
let html_right = '<' (['r' 'R']['i' 'I']['g' 'G']['h' 'H']['t' 'T'] as tag) blank* '>'
let html_end_right = "</" ['r' 'R']['i' 'I']['g' 'G']['h' 'H']['t' 'T'] blank* '>'

rule main = parse
| escape
    { Char (String.make 1 chr) }
| tag (ident as tag)
    { try
        let f = Hashtbl.find tag_table tag in
          f lexbuf
      with Not_found -> Custom tag }
| begin
    { BEGIN }
| end
    { END }
| begin verb
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      verb lexbuf }
| verb end
    { raise (Error(Location.curr lexbuf, Lexer Unmatched_verbatim)) }
| begin target target_format
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      target_format := Some fmt;
      target lexbuf }
| begin target
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      target_format := None;
      target lexbuf }
| target end
    { raise (Error(Location.curr lexbuf, Lexer Unmatched_target)) }
| begin_code
    { reset_string_buffer ();
      code_start_locs := [Location.curr lexbuf];
      code lexbuf }
| end_code
    { raise (Error(Location.curr lexbuf, Lexer Unmatched_code)) }
| begin_pre_code
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      pre_code lexbuf }
| end_pre_code
    { raise (Error(Location.curr lexbuf, Lexer Unmatched_pre_code)) }
| begin ref
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      ref_kind := RK_element;
      reference lexbuf }
| begin ref (ident as lbl) end
    { if lbl = "indexlist" then Special_Ref SRK_index_list
      else Ref(RK_element, lbl) }
| begin ref ref_kind
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      if kind = "modules" then begin
        reset_module_list ();
        module_list lexbuf
      end else begin
        let kind =
          try
            Hashtbl.find ref_kind_table kind
          with Not_found -> RK_custom kind
        in
          ref_kind := kind;
          reference lexbuf
      end }
| begin link
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      ref_kind := RK_link;
      reference lexbuf }
| begin title title_label?
    { Title (int_of_string num, lbl) }
| begin (ident as style)
    { try
        Hashtbl.find style_table style
      with Not_found -> Style (SK_custom style) }
| begin item
    { Item true }
| begin superscript
    { Style SK_superscript }
| begin subscript
    { Style SK_subscript }
| html_code
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      html_code lexbuf }
| html_end_code
    { raise (Error(Location.curr lexbuf, Lexer Unmatched_html_code)) }
| html_title
    { HTML_Title(tag, int_of_string num) }
| html_end_title
    { HTML_END_Title (int_of_string num) }
| html_bold
    { HTML_Bold (String.make 1 tag)}
| html_end_bold
    { HTML_END_BOLD }
| html_italic
    { HTML_Italic (String.make 1 tag)}
| html_end_italic
    { HTML_END_ITALIC }
| html_center
    { HTML_Center tag}
| html_end_center
    { HTML_END_CENTER }
| html_left
    { HTML_Left tag}
| html_end_left
    { HTML_END_LEFT }
| html_right
    { HTML_Right tag}
| html_end_right
    { HTML_END_RIGHT }
| html_list
    { HTML_List tag}
| html_end_list
    { HTML_END_LIST }
| html_enum
    { HTML_Enum tag}
| html_end_enum
    { HTML_END_ENUM }
| html_item
    { HTML_Item tag}
| html_end_item
    { HTML_END_ITEM }
| minus
    { MINUS }
| plus
    { PLUS }
| newline
    { incr_line lexbuf;
      NEWLINE }
| blank+
    { BLANK }
| safe+ | _
    { Char (Lexing.lexeme lexbuf) }
| eof                    { EOF }

and author_tag = parse
| blank+
    { author_tag lexbuf }
| newline
    { incr_line lexbuf;
      author_tag lexbuf }
| name as name
    { let name =
        name
        |> remove_opening_blanks
        |> remove_closing_blanks
      in
        Author name }
| _
    { raise (Error(Location.curr lexbuf, Lexer Expected_name)) }

and param_tag = parse
| blank+
    { param_tag lexbuf }
| newline
    { incr_line lexbuf;
      param_tag lexbuf }
| ident as id
    { Param id }
| _
    { raise (Error(Location.curr lexbuf, Lexer Expected_ident)) }

and raise_tag = parse
| blank+
    { raise_tag lexbuf }
| newline
    { incr_line lexbuf;
      raise_tag lexbuf }
| exception as exn
    { Raise exn }
| _
    { raise (Error(Location.curr lexbuf, Lexer Expected_exception)) }

and see_tag = parse
| blank+
    { see_tag lexbuf }
| newline
    { incr_line lexbuf;
      see_tag lexbuf }
| '<'
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      see_tag_url lexbuf }
| '\''
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      see_tag_file lexbuf }
| '"'
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      see_tag_doc lexbuf }
| _
    { raise (Error(Location.curr lexbuf, Lexer Expected_see)) }

and see_tag_url = parse
| '>'
    { See (See_url (get_raw_buffered_string ())) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_see_url)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_tag_url lexbuf }
| [^ '>' '\010' '\013' ]+ | _
    { buffer_lexeme lexbuf;
      see_tag_url lexbuf }

and see_tag_file = parse
| '\\' '\''
    { buffer_char '\''; verb lexbuf }
| '\''
    { See (See_file (get_raw_buffered_string ())) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_see_file)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_tag_file lexbuf }
| [^ '\'' '\\' '\010' '\013' ]+ | _
    { buffer_lexeme lexbuf;
      see_tag_file lexbuf }

and see_tag_doc = parse
| '\\' '\"'
    { buffer_char '\"'; verb lexbuf }
| '\"'
    { See (See_doc (get_raw_buffered_string ())) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_see_doc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_tag_doc lexbuf }
| [^ '\"' '\\' '\010' '\013' ]+ | _
    { buffer_lexeme lexbuf;
      see_tag_doc lexbuf }

and since_tag = parse
| blank+
    { since_tag lexbuf }
| newline
    { incr_line lexbuf;
      since_tag lexbuf }
| version as ver
    { Since ver }
| _
    { raise (Error(Location.curr lexbuf, Lexer Expected_version)) }

and before_tag = parse
| blank+
    { before_tag lexbuf }
| newline
    { incr_line lexbuf;
      before_tag lexbuf }
| version as ver
    { Before ver }
| _
    { raise (Error(Location.curr lexbuf, Lexer Expected_version)) }

and version_tag = parse
| blank+
    { version_tag lexbuf }
| newline
    { incr_line lexbuf;
      version_tag lexbuf }
| version as ver
    { Version ver }
| _
    { raise (Error(Location.curr lexbuf, Lexer Expected_version)) }

and verb = parse
| escape
    { buffer_char chr; verb lexbuf }
| begin verb
    { raise (Error(Location.curr lexbuf, Lexer Nested_verbatim)) }
| verb end
    { Verb (get_buffered_string ()) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_verbatim)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      verb lexbuf }
| safe+ | blank+ | _
    { buffer_lexeme lexbuf; verb lexbuf }

and target = parse
| escape
    { buffer_char chr; target lexbuf }
| begin target
    { raise (Error(Location.curr lexbuf, Lexer Nested_target)) }
| target end
    { Target(!target_format, get_buffered_string ()) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_target)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      target lexbuf }
| safe+ | blank+ | _
    { buffer_lexeme lexbuf; target lexbuf }

and code = parse
| escape
    { buffer_char chr; code lexbuf }
| begin_code
    { code_start_locs := (Location.curr lexbuf) :: !code_start_locs;
      buffer_lexeme lexbuf;
      code lexbuf }
| end_code
    { match !code_start_locs with
        [] -> assert false
      | [l] ->
        code_start_locs := [];
        Code(get_buffered_string ())
      | _ :: rest ->
        code_start_locs := rest;
        buffer_lexeme lexbuf;
        code lexbuf }
| eof
    { match !code_start_locs with
        [] -> assert false
      | l:: _ -> raise (Error(l, Lexer Unterminated_code)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      code lexbuf }
| safe+ | blank+ | _
    { buffer_lexeme lexbuf; code lexbuf }

and pre_code = parse
| escape
    { buffer_char chr; pre_code lexbuf }
| begin_pre_code
    { raise (Error(Location.curr lexbuf, Lexer Nested_pre_code)) }
| end_pre_code
    { Pre_Code (get_buffered_string ()) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_pre_code)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      pre_code lexbuf }
| safe+ | blank+ | _
    { buffer_lexeme lexbuf; pre_code lexbuf }

and html_code = parse
| escape
    { buffer_char chr; html_code lexbuf }
| html_code
    { raise (Error(Location.curr lexbuf, Lexer Nested_html_code))  }
| html_end_code
    { Code(get_buffered_string ()) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_html_code)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      html_code lexbuf }
| safe+ | blank+ | _
    { buffer_lexeme lexbuf; html_code lexbuf }

and reference = parse
| escape
    { buffer_char chr; reference lexbuf }
| end
    { Ref(!ref_kind, get_buffered_string ()) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_ref)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      reference lexbuf }
| safe+ | blank+ | _
    { buffer_lexeme lexbuf; reference lexbuf }

and module_list = parse
| escape
    { buffer_char chr; reference lexbuf }
| end
    { if buffer_not_empty () then add_module (get_buffered_string ());
      Special_Ref(SRK_module_list (get_module_list ())) }
| eof
    { raise (Error(!string_start_loc, Lexer Unterminated_ref)) }
| blank+
    { if buffer_not_empty () then begin
        add_module (get_buffered_string ());
        reset_string_buffer ()
      end;
      reference lexbuf }
| newline
    { incr_line lexbuf;
      if buffer_not_empty () then begin
        add_module (get_buffered_string ());
        reset_string_buffer ()
      end;
      reference lexbuf }
| safe+ | _
    { buffer_lexeme lexbuf; reference lexbuf }

{

(* Initialize style hash table *)
let _ =
  List.iter
    (fun (kwd, tok) -> Hashtbl.add style_table kwd tok)
    [ ("b", Style SK_bold);
      ("e", Style SK_emphasize);
      ("C", Style SK_center);
      ("L", Style SK_left);
      ("R", Style SK_right);
      ("i", Style SK_italic);
      ("ul", LIST);
      ("ol", ENUM);
      ("li", Item false); ]

(* Initialize reference kind hash table *)
let _ =
  List.iter
    (fun (kind, tok) -> Hashtbl.add ref_kind_table kind tok)
    [ ("val", RK_value);
      ("type", RK_type);
      ("exception", RK_exception);
      ("module", RK_module);
      ("modtype", RK_module_type);
      ("class", RK_class);
      ("classtype", RK_class_type);
      ("attribute", RK_attribute);
      ("method", RK_method);
      ("section", RK_section);
      ("recfield", RK_recfield);
      ("const", RK_const); ]

(* Initialize tag hash table *)
let _ =
  List.iter
    (fun (tag, tok) -> Hashtbl.add tag_table tag tok)
    [ ("author", author_tag);
      ("deprecated", fun _ -> DEPRECATED);
      ("param", param_tag);
      ("raise", raise_tag);
      ("return", fun _ -> RETURN);
      ("see", see_tag);
      ("since", since_tag);
      ("before", before_tag);
      ("version", version_tag); ]

}
