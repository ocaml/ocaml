(* pa_r.cmo pa_rp.cmo pa_extend.cmo q_MLast.cmo pr_dump.cmo *)
(***********************************************************************)
(*                                                                     *)
(*                               Camlp4                                *)
(*                                                                     *)
(*    Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file                 *)
(*   ../../../LICENSE.                                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

module Plexer =
  struct
    open Stdpp;
    open Token;
    value buff = ref (String.create 80);
    value store len x =
      do {
        if len >= String.length buff.val then
          buff.val := buff.val ^ String.create (String.length buff.val)
        else ();
        buff.val.[len] := x;
        succ len
      }
    ;
    value mstore len s =
      add_rec len 0 where rec add_rec len i =
        if i == String.length s then len
        else add_rec (store len s.[i]) (succ i)
    ;
    value get_buff len = String.sub buff.val 0 len;
    value rec ident len =
      parser
      [ [: `('A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
             '\248'..'\255' | '0'..'9' | '_' | ''' as
             c)
            ;
           s :] ->
          ident (store len c) s
      | [: :] -> len ]
    and ident2 len =
      parser
      [ [: `('!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
             '%' | '.' | ':' | '<' | '>' | '|' as
             c)
            ;
           s :] ->
          ident2 (store len c) s
      | [: :] -> len ]
    and ident3 len =
      parser
      [ [: `('0'..'9' | 'A'..'Z' | 'a'..'z' | '\192'..'\214' |
             '\216'..'\246' | '\248'..'\255' | '_' | '!' | '%' | '&' | '*' |
             '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' |
             '|' | '~' | ''' | '$' as
             c)
            ;
           s :] ->
          ident3 (store len c) s
      | [: :] -> len ]
    and ident4 len =
      parser
      [ [: `('!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
             '%' | '.' | '<' | '>' | '|' as
             c)
            ;
           s :] ->
          ident4 (store len c) s
      | [: :] -> len ]
    and base_number len =
      parser
      [ [: `'o' | 'O'; s :] -> octal_digits (store len 'o') s
      | [: `'x' | 'X'; s :] -> hexa_digits (store len 'x') s
      | [: `'b' | 'B'; s :] -> binary_digits (store len 'b') s
      | [: a = number len :] -> a ]
    and octal_digits len =
      parser
      [ [: `('0'..'7' as d); s :] -> octal_digits (store len d) s
      | [: :] -> ("INT", get_buff len) ]
    and hexa_digits len =
      parser
      [ [: `('0'..'9' | 'a'..'f' | 'A'..'F' as d); s :] ->
          hexa_digits (store len d) s
      | [: :] -> ("INT", get_buff len) ]
    and binary_digits len =
      parser
      [ [: `('0'..'1' as d); s :] -> binary_digits (store len d) s
      | [: :] -> ("INT", get_buff len) ]
    and number len =
      parser
      [ [: `('0'..'9' as c); s :] -> number (store len c) s
      | [: `'.'; s :] -> decimal_part (store len '.') s
      | [: `'e' | 'E'; s :] -> exponent_part (store len 'E') s
      | [: :] -> ("INT", get_buff len) ]
    and decimal_part len =
      parser
      [ [: `('0'..'9' as c); s :] -> decimal_part (store len c) s
      | [: `'e' | 'E'; s :] -> exponent_part (store len 'E') s
      | [: :] -> ("FLOAT", get_buff len) ]
    and exponent_part len =
      parser
      [ [: `('+' | '-' as c); s :] -> end_exponent_part (store len c) s
      | [: a = end_exponent_part len :] -> a ]
    and end_exponent_part len =
      parser
      [ [: `('0'..'9' as c); s :] -> end_exponent_part (store len c) s
      | [: :] -> ("FLOAT", get_buff len) ]
    ;
    value valch x = Char.code x - Char.code '0';
    value rec backslash s i =
      if i = String.length s then raise Not_found
      else
        match s.[i] with
        [ 'n' -> ('\n', i + 1)
        | 'r' -> ('\r', i + 1)
        | 't' -> ('\t', i + 1)
        | 'b' -> ('\b', i + 1)
        | '\\' -> ('\\', i + 1)
        | '0'..'9' as c -> backslash1 (valch c) s (i + 1)
        | _ -> raise Not_found ]
    and backslash1 cod s i =
      if i = String.length s then (Char.chr cod, i)
      else
        match s.[i] with
        [ '0'..'9' as c -> backslash2 (10 * cod + valch c) s (i + 1)
        | _ -> (Char.chr cod, i) ]
    and backslash2 cod s i =
      if i = String.length s then (Char.chr cod, i)
      else
        match s.[i] with
        [ '0'..'9' as c -> (Char.chr (10 * cod + valch c), i + 1)
        | _ -> (Char.chr cod, i) ]
    ;
    value rec skip_indent s i =
      if i = String.length s then i
      else
        match s.[i] with
        [ ' ' | '\t' -> skip_indent s (i + 1)
        | _ -> i ]
    ;
    value skip_opt_linefeed s i =
      if i = String.length s then i else if s.[i] = '\010' then i + 1 else i
    ;
    value char_of_char_token s =
      if String.length s = 1 then s.[0]
      else if String.length s = 0 then failwith "invalid char token"
      else if s.[0] = '\\' then
        if String.length s = 2 && s.[1] = ''' then '''
        else
          try
            let (c, i) = backslash s 1 in
            if i = String.length s then c else raise Not_found
          with
          [ Not_found -> failwith "invalid char token" ]
      else failwith "invalid char token"
    ;
    value string_of_string_token s =
      loop 0 0 where rec loop len i =
        if i = String.length s then get_buff len
        else
          let (len, i) =
            if s.[i] = '\\' then
              let i = i + 1 in
              if i = String.length s then failwith "invalid string token"
              else if s.[i] = '"' then (store len '"', i + 1)
              else
                match s.[i] with
                [ '\010' -> (len, skip_indent s (i + 1))
                | '\013' -> (len, skip_indent s (skip_opt_linefeed s (i + 1)))
                | c ->
                    try
                      let (c, i) = backslash s i in
                      (store len c, i)
                    with
                    [ Not_found -> (store (store len '\\') c, i + 1) ] ]
            else (store len s.[i], i + 1)
          in
          loop len i
    ;
    value rec skip_spaces =
      parser
      [ [: `' ' | '\n' | '\r' | '\t' | '\026' | '\012'; s :] -> skip_spaces s
      | [: :] -> () ]
    ;
    value error_on_unknown_keywords = ref False;
    value next_token_fun find_id_kwd find_spe_kwd fname lnum bolpos =
      let make_pos p =
        {Lexing.pos_fname = fname.val; Lexing.pos_lnum = lnum.val;
         Lexing.pos_bol = bolpos.val; Lexing.pos_cnum = p} in
      let mkloc (bp, ep) = (make_pos bp, make_pos ep) in

      let err loc msg = raise_with_loc loc (Token.Error msg) in
      let keyword_or_error (bp,ep) s =
        try ("", find_spe_kwd s) with
        [ Not_found ->
            if error_on_unknown_keywords.val then
              err (mkloc (bp, ep)) ("illegal token: " ^ s)
            else ("", s) ]
      in
      let rec next_token =
        parser bp
        [ [: `('A'..'Z' | 'À'..'Ö' | 'Ø'..'Þ' as c); s :] ->
            let id = get_buff (ident (store 0 c) s) in
            try ("", find_id_kwd id) with [ Not_found -> ("UIDENT", id) ]
        | [: `('a'..'z' | 'ß'..'ö' | 'ø'..'ÿ' | '_' as c); s :] ->
            let id = get_buff (ident (store 0 c) s) in
            let is_label =
              match Stream.peek s with
              [ Some ':' ->
                  match Stream.npeek 2 s with
                  [ [_; ':' | '=' | '>'] -> False
                  | _ -> True ]
              | _ -> False ]
            in
            if is_label then do { Stream.junk s; ("LABEL", id) }
            else try ("", find_id_kwd id) with [ Not_found -> ("LIDENT", id) ]
        | [: `('1'..'9' as c); s :] -> number (store 0 c) s
        | [: `'0'; s :] -> base_number (store 0 '0') s
        | [: `'''; s :] ep ->
            match Stream.npeek 2 s with
            [ [_; '''] | ['\\'; _] -> ("CHAR", char bp 0 s)
            | _ -> keyword_or_error (bp, ep) "'" ]
        | [: `'"'; s :] -> ("STRING", string bp 0 s)
        | [: `'$'; s :] -> locate_or_antiquot bp 0 s
        | [: `('!' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
               '%' as
               c)
              ;
             s :] ->
            let id = get_buff (ident2 (store 0 c) s) in
            keyword_or_error (bp, Stream.count s) id
        | [: `('?' as c); s :] ->
            let id = get_buff (ident4 (store 0 c) s) in
            keyword_or_error (bp, Stream.count s) id
        | [: `'<'; s :] -> less bp s
        | [: `(':' as c1);
             (is_label, len) =
               parser
               [ [: `(']' | ':' | '=' | '>' as c2) :] ->
                   (False, store (store 0 c1) c2)
               | [: `('a'..'z' | 'ß'..'ö' | 'ø'..'ÿ' | '_' as c); s :] ->
                   (True, ident (store 0 c) s)
               | [: :] -> (False, store 0 c1) ] :] ep ->
            let id = get_buff len in
            if is_label then ("ELABEL", id) else keyword_or_error (bp, ep) id
        | [: `('>' | '|' as c1);
             len =
               parser
               [ [: `(']' | '}' as c2) :] -> store (store 0 c1) c2
               | [: a = ident2 (store 0 c1) :] -> a ] :] ep ->
            let id = get_buff len in
            keyword_or_error (bp, ep) id
        | [: `('[' | '{' as c1); s :] ->
            let len =
              match Stream.npeek 2 s with
              [ ['<'; '<' | ':'] -> store 0 c1
              | _ ->
                  match s with parser
                  [ [: `('|' | '<' | ':' as c2) :] -> store (store 0 c1) c2
                  | [: :] -> store 0 c1 ] ]
            in
            let ep = Stream.count s in
            let id = get_buff len in
            keyword_or_error (bp, ep) id
        | [: `'.'; id = parser [ [: `'.' :] -> ".." | [: :] -> "." ] :] ep ->
            keyword_or_error (bp, ep) id
        | [: `';'; id = parser [ [: `';' :] -> ";;" | [: :] -> ";" ] :] ep ->
            keyword_or_error (bp, ep) id
        | [: `'\\'; s :] -> ("LIDENT", get_buff (ident3 0 s))
        | [: `c :] ep -> keyword_or_error (bp, ep) (String.make 1 c) ]
      and less bp =
        parser
        [ [: `'<'; s :] -> ("QUOTATION", ":" ^ get_buff (quotation bp 0 s))
        | [: `':'; i = parser [: len = ident 0 :] -> get_buff len;
             `'<' ? "character '<' expected"; s :] ->
            ("QUOTATION", i ^ ":" ^ get_buff (quotation bp 0 s))
        | [: s :] ep ->
            let id = get_buff (ident2 (store 0 '<') s) in
            keyword_or_error (bp, ep) id ]
      and string bp len =
        parser
        [ [: `'"' :] -> get_buff len
        | [: `'\\'; `c; s :] -> string bp (store (store len '\\') c) s
        | [: `c; s :] -> string bp (store len c) s
        | [: :] ep -> err (mkloc (bp, ep)) "string not terminated" ]
      and char bp len =
        parser
        [ [: `'''; s :] ->
            if len = 0 then char bp (store len ''') s else get_buff len
        | [: `'\\'; `c; s :] -> char bp (store (store len '\\') c) s
        | [: `c; s :] -> char bp (store len c) s
        | [: :] ep -> err (mkloc(bp,ep)) "char not terminated" ]
      and locate_or_antiquot bp len =
        parser
        [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
        | [: `('a'..'z' | 'A'..'Z' as c); s :] -> antiquot bp (store len c) s
        | [: `('0'..'9' as c); s :] -> maybe_locate bp (store len c) s
        | [: `':'; s :] ->
            let k = get_buff len in
            ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 s)
        | [: `'\\'; `c; s :] ->
            ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
        | [: `c; s :] ->
            ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
        | [: :] ep -> err (mkloc(bp,ep)) "antiquotation not terminated" ]
      and maybe_locate bp len =
        parser
        [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
        | [: `('0'..'9' as c); s :] -> maybe_locate bp (store len c) s
        | [: `':'; s :] ->
            ("LOCATE", get_buff len ^ ":" ^ locate_or_antiquot_rest bp 0 s)
        | [: `'\\'; `c; s :] ->
            ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
        | [: `c; s :] ->
            ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
        | [: :] ep -> err (mkloc(bp,ep)) "antiquotation not terminated" ]
      and antiquot bp len =
        parser
        [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
        | [: `('a'..'z' | 'A'..'Z' | '0'..'9' as c); s :] ->
            antiquot bp (store len c) s
        | [: `':'; s :] ->
            let k = get_buff len in
            ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 s)
        | [: `'\\'; `c; s :] ->
            ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
        | [: `c; s :] ->
            ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
        | [: :] ep -> err (mkloc(bp,ep)) "antiquotation not terminated" ]
      and locate_or_antiquot_rest bp len =
        parser
        [ [: `'$' :] -> get_buff len
        | [: `'\\'; `c; s :] -> locate_or_antiquot_rest bp (store len c) s
        | [: `c; s :] -> locate_or_antiquot_rest bp (store len c) s
        | [: :] ep -> err (mkloc(bp,ep)) "antiquotation not terminated" ]
      and quotation bp len =
        parser
        [ [: `'>'; s :] -> maybe_end_quotation bp len s
        | [: `'<'; s :] ->
            quotation bp (maybe_nested_quotation bp (store len '<') __strm) s
        | [: `'\\';
             len =
               parser
               [ [: `('>' | '<' | '\\' as c) :] -> store len c
               | [: :] -> store len '\\' ];
             s :] ->
            quotation bp len s
        | [: `c; s :] -> quotation bp (store len c) s
        | [: :] ep -> err (mkloc(bp,ep)) "quotation not terminated" ]
      and maybe_nested_quotation bp len =
        parser
        [ [: `'<'; s :] -> mstore (quotation bp (store len '<') s) ">>"
        | [: `':'; len = ident (store len ':');
             a =
               parser
               [ [: `'<'; s :] -> mstore (quotation bp (store len '<') s) ">>"
               | [: :] -> len ] :] ->
            a
        | [: :] -> len ]
      and maybe_end_quotation bp len =
        parser
        [ [: `'>' :] -> len
        | [: a = quotation bp (store len '>') :] -> a ]
      in
      let rec next_token_loc =
        parser bp
        [ [: `' ' | '\n' | '\r' | '\t' | '\026' | '\012'; s :] ->
            next_token_loc s
        | [: `'('; s :] -> maybe_comment bp s
        | [: `'#'; _ = spaces_tabs; a = linenum bp :] -> a
        | [: tok = next_token :] ep -> (tok, mkloc(bp, ep))
        | [: _ = Stream.empty :] -> (("EOI", ""), mkloc(bp, succ bp)) ]
      and maybe_comment bp =
        parser
        [ [: `'*'; s :] -> do { comment bp s; next_token_loc s }
        | [: :] ep ->
            let tok = keyword_or_error (bp, ep) "(" in
            (tok, mkloc(bp, ep)) ]
      and comment bp =
        parser
        [ [: `'('; s :] -> maybe_nested_comment bp s
        | [: `'*'; s :] -> maybe_end_comment bp s
        | [: `c; s :] -> comment bp s
        | [: :] ep -> err (mkloc(bp,ep)) "comment not terminated" ]
      and maybe_nested_comment bp =
        parser
        [ [: `'*'; s :] -> do { comment bp s; comment bp s }
        | [: a = comment bp :] -> a ]
      and maybe_end_comment bp =
        parser [ [: `')' :] -> () | [: a = comment bp :] -> a ]
      and linenum bp =
        parser
        [ [: `'0'..'9'; _ = digits; _ = spaces_tabs; `'"'; _ = any_to_nl;
             s :] ->
            next_token_loc s
        | [: :] -> (keyword_or_error (bp, bp + 1) "#", mkloc(bp, bp + 1)) ]
      and spaces_tabs =
        parser [ [: `' ' | '\t'; s :] -> spaces_tabs s | [: :] -> () ]
      and digits = parser [ [: `'0'..'9'; s :] -> digits s | [: :] -> () ]
      and any_to_nl =
        parser
        [ [: `'\r' | '\n' :] -> ()
        | [: `_; s :] -> any_to_nl s
        | [: :] -> () ]
      in
      fun cstrm ->
        try next_token_loc cstrm with
        [ Stream.Error str ->
            err (mkloc(Stream.count cstrm, Stream.count cstrm + 1)) str ]
    ;
    value locerr () = invalid_arg "Lexer: location function";
    value loct_create () = ref (Array.create 1024 None);
    value loct_func loct i =
      match
        if i < 0 || i >= Array.length loct.val then None
        else Array.unsafe_get loct.val i
      with
      [ Some loc -> loc
      | _ -> locerr () ]
    ;
    value loct_add loct i loc =
      do {
        if i >= Array.length loct.val then do {
          let new_tmax = Array.length loct.val * 2 in
          let new_loct = Array.create new_tmax None in
          Array.blit loct.val 0 new_loct 0 (Array.length loct.val);
          loct.val := new_loct
        }
        else ();
        loct.val.(i) := Some loc
      }
    ;
    value func kwd_table =
      let bolpos = ref 0 in
      let lnum = ref 0 in
      let fname = ref "" in
      let find = Hashtbl.find kwd_table in
      let lex cstrm =
        let next_token_loc = next_token_fun find find fname lnum bolpos in
        let loct = loct_create () in
        let ts =
          Stream.from
            (fun i ->
               let (tok, loc) = next_token_loc cstrm in
               do { loct_add loct i loc; Some tok })
        in
        let locf = loct_func loct in
        (ts, locf)
      in
      lex
    ;
    value rec check_keyword_stream =
      parser [: _ = check; _ = Stream.empty :] -> True
    and check =
      parser
      [ [: `'A'..'Z' | 'a'..'z' | 'À'..'Ö' | 'Ø'..'ö' | 'ø'..'ÿ'; s :] ->
          check_ident s
      | [: `'!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
            '%' | '.'
            ;
           s :] ->
          check_ident2 s
      | [: `'<'; s :] ->
          match Stream.npeek 1 s with
          [ [':' | '<'] -> ()
          | _ -> check_ident2 s ]
      | [: `':';
           _ =
             parser
             [ [: `']' | ':' | '=' | '>' :] -> ()
             | [: :] -> () ] :] ep ->
          ()
      | [: `'>' | '|';
           _ =
             parser
             [ [: `']' | '}' :] -> ()
             | [: a = check_ident2 :] -> a ] :] ->
          ()
      | [: `'[' | '{'; s :] ->
          match Stream.npeek 2 s with
          [ ['<'; '<' | ':'] -> ()
          | _ ->
              match s with parser
              [ [: :] ->
                  match Stream.peek __strm with
                  [ Some ('|' | '<' | ':') -> Stream.junk __strm
                  | _ -> () ] ] ]
      | [: `';'; _ = parser [ [: `';' :] -> () | [: :] -> () ] :] -> ()
      | [: `_ :] -> () ]
    and check_ident =
      parser
      [ [: `'A'..'Z' | 'a'..'z' | 'À'..'Ö' | 'Ø'..'ö' | 'ø'..'ÿ' | '0'..'9' |
            '_' | '''
            ;
           s :] ->
          check_ident s
      | [: :] -> () ]
    and check_ident2 =
      parser
      [ [: `'!' | '?' | '~' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' |
            '%' | '.' | ':' | '<' | '>' | '|'
            ;
           s :] ->
          check_ident2 s
      | [: :] -> () ]
    ;
    value check_keyword s =
      try check_keyword_stream (Stream.of_string s) with _ -> False
    ;
    value using_token kwd_table (p_con, p_prm) =
      match p_con with
      [ "" ->
          try
            let _ = Hashtbl.find kwd_table p_prm in
            ()
          with
          [ Not_found ->
              if check_keyword p_prm then Hashtbl.add kwd_table p_prm p_prm
              else
                raise
                  (Token.Error
                     ("the token \"" ^ p_prm ^
                        "\" does not respect Plexer rules")) ]
      | "LIDENT" | "UIDENT" | "INT" | "FLOAT" | "CHAR" | "STRING" |
        "QUOTATION" | "ANTIQUOT" | "LOCATE" | "LABEL" | "ELABEL" | "EOI" ->
          ()
      | _ ->
          raise
            (Token.Error
               ("the constructor \"" ^ p_con ^
                  "\" is not recognized by Llexer")) ]
    ;
    value removing_token kwd_table (p_con, p_prm) =
      if p_con = "" then Hashtbl.remove kwd_table p_prm else ()
    ;
    value text =
      fun
      [ ("", t) -> "'" ^ t ^ "'"
      | ("LIDENT", "") -> "lowercase identifier"
      | ("LIDENT", t) -> "'" ^ t ^ "'"
      | ("UIDENT", "") -> "uppercase identifier"
      | ("UIDENT", t) -> "'" ^ t ^ "'"
      | ("INT", "") -> "integer"
      | ("INT", s) -> "'" ^ s ^ "'"
      | ("FLOAT", "") -> "float"
      | ("STRING", "") -> "string"
      | ("CHAR", "") -> "char"
      | ("QUOTATION", "") -> "quotation"
      | ("ANTIQUOT", k) -> "antiquot \"" ^ k ^ "\""
      | ("LOCATE", "") -> "locate"
      | ("LABEL", "") -> "label"
      | ("ELABEL", "") -> "elabel"
      | ("EOI", "") -> "end of input"
      | (con, "") -> con
      | (con, prm) -> con ^ " \"" ^ prm ^ "\"" ]
    ;
    value eq_before_colon p e =
      loop 0 where rec loop i =
        if i == String.length e then
          failwith "Internal error in Plexer: incorrect ANTIQUOT"
        else if i == String.length p then e.[i] == ':'
        else if p.[i] == e.[i] then loop (i + 1)
        else False
    ;
    value after_colon e =
      try
        let i = String.index e ':' in
        String.sub e (i + 1) (String.length e - i - 1)
      with
      [ Not_found -> "" ]
    ;
    value gmake () =
      let kwd_table = Hashtbl.create 301 in
      {tok_func = func kwd_table; tok_using = using_token kwd_table;
       tok_removing = removing_token kwd_table;
       tok_match = Token.default_match; tok_text = text; tok_comm = None}
    ;
  end
;

open Stdpp;
open Pcaml;

Pcaml.no_constructors_arity.val := True;

do {
  Grammar.Unsafe.gram_reinit gram (Plexer.gmake ());
  Grammar.Unsafe.clear_entry interf;
  Grammar.Unsafe.clear_entry implem;
  Grammar.Unsafe.clear_entry top_phrase;
  Grammar.Unsafe.clear_entry use_file;
  Grammar.Unsafe.clear_entry module_type;
  Grammar.Unsafe.clear_entry module_expr;
  Grammar.Unsafe.clear_entry sig_item;
  Grammar.Unsafe.clear_entry str_item;
  Grammar.Unsafe.clear_entry expr;
  Grammar.Unsafe.clear_entry patt;
  Grammar.Unsafe.clear_entry ctyp;
  Grammar.Unsafe.clear_entry let_binding;
  Grammar.Unsafe.clear_entry class_type;
  Grammar.Unsafe.clear_entry class_expr;
  Grammar.Unsafe.clear_entry class_sig_item;
  Grammar.Unsafe.clear_entry class_str_item
};

Pcaml.parse_interf.val := Grammar.Entry.parse interf;
Pcaml.parse_implem.val := Grammar.Entry.parse implem;

value o2b =
  fun
  [ Some _ -> True
  | None -> False ]
;

value mkumin loc f arg =
  match arg with
  [ <:expr< $int:n$ >> when int_of_string n > 0 ->
      let n = "-" ^ n in
      <:expr< $int:n$ >>
  | <:expr< $flo:n$ >> when float_of_string n > 0.0 ->
      let n = "-" ^ n in
      <:expr< $flo:n$ >>
  | _ ->
      let f = "~" ^ f in
      <:expr< $lid:f$ $arg$ >> ]
;

external loc_of_node : 'a -> Loc.t = "%field0";

value mklistexp loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some e -> e
        | None -> <:expr< [] >> ]
    | [e1 :: el] ->
        let loc = if top then loc else (fst (loc_of_node e1), snd loc) in
        <:expr< [$e1$ :: $loop False el$] >> ]
;

value mklistpat loc last =
  loop True where rec loop top =
    fun
    [ [] ->
        match last with
        [ Some p -> p
        | None -> <:patt< [] >> ]
    | [p1 :: pl] ->
        let loc = if top then loc else (fst (loc_of_node p1), snd loc) in
        <:patt< [$p1$ :: $loop False pl$] >> ]
;

value neg s = string_of_int (- int_of_string s);

value is_operator =
  let ht = Hashtbl.create 73 in
  let ct = Hashtbl.create 73 in
  do {
    List.iter (fun x -> Hashtbl.add ht x True)
      ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"];
    List.iter (fun x -> Hashtbl.add ct x True)
      ['!'; '&'; '*'; '+'; '-'; '/'; ':'; '<'; '='; '>'; '@'; '^'; '|'; '~';
       '?'; '%'; '.'];
    fun x ->
      try Hashtbl.find ht x with
      [ Not_found -> try Hashtbl.find ct x.[0] with _ -> False ]
  }
;

(*
value p_operator strm =
  match Stream.peek strm with
  [ Some (Token.Tterm "(") ->
      match Stream.npeek 3 strm with
      [ [_; Token.Tterm x; Token.Tterm ")"] when is_operator x ->
          do { Stream.junk strm; Stream.junk strm; Stream.junk strm; x }
      | _ -> raise Stream.Failure ]
  | _ -> raise Stream.Failure ]
;

value operator = Grammar.Entry.of_parser gram "operator" p_operator;
*)

value operator =
  Grammar.Entry.of_parser gram "operator"
    (parser [: `("", x) when is_operator x :] -> x)
;

value symbolchar =
  let list =
    ['!'; '$'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
     '@'; '^'; '|'; '~']
  in
  let rec loop s i =
    if i == String.length s then True
    else if List.mem s.[i] list then loop s (i + 1)
    else False
  in
  loop
;

value prefixop =
  let list = ['!'; '?'; '~'] in
  let excl = ["!="] in
  Grammar.Entry.of_parser gram "prefixop"
    (parser
       [: `("", x)
           when
             not (List.mem x excl) && String.length x >= 2 &&
             List.mem x.[0] list && symbolchar x 1 :] ->
         x)
;

value infixop0 =
  let list = ['='; '<'; '>'; '|'; '&'; '$'] in
  let excl = ["<-"; "||"; "&&"] in
  Grammar.Entry.of_parser gram "infixop0"
    (parser
       [: `("", x)
           when
             not (List.mem x excl) && String.length x >= 2 &&
             List.mem x.[0] list && symbolchar x 1 :] ->
         x)
;

value infixop1 =
  let list = ['@'; '^'] in
  Grammar.Entry.of_parser gram "infixop1"
    (parser
       [: `("", x)
           when
             String.length x >= 2 && List.mem x.[0] list &&
             symbolchar x 1 :] ->
         x)
;

value infixop2 =
  let list = ['+'; '-'] in
  Grammar.Entry.of_parser gram "infixop2"
    (parser
       [: `("", x)
           when
             x <> "->" && String.length x >= 2 && List.mem x.[0] list &&
             symbolchar x 1 :] ->
         x)
;

value infixop3 =
  let list = ['*'; '/'; '%'] in
  Grammar.Entry.of_parser gram "infixop3"
    (parser
       [: `("", x)
           when
             String.length x >= 2 && List.mem x.[0] list &&
             symbolchar x 1 :] ->
         x)
;

value infixop4 =
  Grammar.Entry.of_parser gram "infixop4"
    (parser
       [: `("", x)
           when
             String.length x >= 3 && x.[0] == '*' && x.[1] == '*' &&
             symbolchar x 2 :] ->
         x)
;

value test_constr_decl =
  Grammar.Entry.of_parser gram "test_constr_decl"
    (fun strm ->
       match Stream.npeek 1 strm with
       [ [("UIDENT", _)] ->
           match Stream.npeek 2 strm with
           [ [_; ("", ".")] -> raise Stream.Failure
           | [_; ("", "(")] -> raise Stream.Failure
           | [_ :: _] -> ()
           | _ -> raise Stream.Failure ]
       | [("", "|")] -> ()
       | _ -> raise Stream.Failure ])
;

value stream_peek_nth n strm =
  loop n (Stream.npeek n strm) where rec loop n =
    fun
    [ [] -> None
    | [x] -> if n == 1 then Some x else None
    | [_ :: l] -> loop (n - 1) l ]
;

value test_label_eq =
  let rec test lev strm =
    match stream_peek_nth lev strm with
    [ Some (("UIDENT", _) | ("LIDENT", _) | ("", ".")) -> test (lev + 1) strm
    | Some ("", "=") -> ()
    | _ -> raise Stream.Failure ]
  in
  Grammar.Entry.of_parser gram "test_label_eq" (test 1)
;

value constr_arity = ref [("Some", 1); ("Match_Failure", 1)];

value rec constr_expr_arity =
  fun
  [ <:expr< $uid:c$ >> ->
      try List.assoc c constr_arity.val with [ Not_found -> 0 ]
  | <:expr< $uid:_$.$e$ >> -> constr_expr_arity e
  | _ -> 1 ]
;

value rec constr_patt_arity =
  fun
  [ <:patt< $uid:c$ >> ->
      try List.assoc c constr_arity.val with [ Not_found -> 0 ]
  | <:patt< $uid:_$.$p$ >> -> constr_patt_arity p
  | _ -> 1 ]
;

value rec get_seq =
  fun
  [ <:expr< do { $list:el$ } >> -> el
  | e -> [e] ]
;

value choose_tvar tpl =
  let rec find_alpha v =
    let s = String.make 1 v in
    if List.mem_assoc s tpl then
      if v = 'z' then None else find_alpha (Char.chr (Char.code v + 1))
    else Some (String.make 1 v)
  in
  let rec make_n n =
    let v = "a" ^ string_of_int n in
    if List.mem_assoc v tpl then make_n (succ n) else v
  in
  match find_alpha 'a' with
  [ Some x -> x
  | None -> make_n 1 ]
;

value rec patt_lid =
  fun
  [ <:patt< $lid:i$ $p$ >> -> Some (i, [p])
  | <:patt< $p1$ $p2$ >> ->
      match patt_lid p1 with
      [ Some (i, pl) -> Some (i, [p2 :: pl])
      | None -> None ]
  | _ -> None ]
;

value type_parameter = Grammar.Entry.create gram "type_parameter";
value fun_def = Grammar.Entry.create gram "fun_def";
value fun_binding = Grammar.Entry.create gram "fun_binding";

EXTEND
  GLOBAL: interf implem top_phrase use_file sig_item str_item ctyp patt expr
    module_type module_expr let_binding type_parameter fun_def fun_binding;
  (* Main entry points *)
  interf:
    [ [ st = LIST0 [ s = sig_item; OPT ";;" -> (s, loc) ]; EOI ->
          (st, False) ] ]
  ;
  implem:
    [ [ st = LIST0 [ s = str_item; OPT ";;" -> (s, loc) ]; EOI ->
          (st, False) ] ]
  ;
  top_phrase:
    [ [ ph = phrase; ";;" -> Some ph
      | EOI -> None ] ]
  ;
  use_file:
    [ [ l = LIST0 [ ph = phrase; OPT ";;" -> ph ]; EOI -> (l, False) ] ]
  ;
  phrase:
    [ [ sti = str_item -> sti
      | "#"; n = LIDENT; dp = dir_param -> MLast.StDir loc n dp ] ]
  ;
  dir_param:
    [ [ -> None
      | e = expr -> Some e ] ]
  ;
  (* Module expressions *)
  module_expr:
    [ [ "functor"; "("; i = UIDENT; ":"; t = module_type; ")"; "->";
        me = SELF ->
          <:module_expr< functor ( $i$ : $t$ ) -> $me$ >>
      | "struct"; st = LIST0 [ s = str_item; OPT ";;" -> s ]; "end" ->
          <:module_expr< struct $list:st$ end >> ]
    | [ me1 = SELF; me2 = SELF -> <:module_expr< $me1$ $me2$ >> ]
    | [ i = mod_expr_ident -> i
      | "("; me = SELF; ":"; mt = module_type; ")" ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "("; me = SELF; ")" -> <:module_expr< $me$ >> ] ]
  ;
  mod_expr_ident:
    [ LEFTA
      [ m1 = SELF; "."; m2 = SELF -> <:module_expr< $m1$ . $m2$ >> ]
    | [ m = UIDENT -> <:module_expr< $uid:m$ >> ] ]
  ;
  str_item:
    [ "top"
      [ "exception"; (_, c, tl) = constructor_declaration ->
          <:str_item< exception $c$ of $list:tl$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "external"; i = LABEL; t = ctyp; "="; pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "external"; "("; i = operator; ")"; ":"; t = ctyp; "=";
        pd = LIST1 STRING ->
          <:str_item< external $i$ : $t$ = $list:pd$ >>
      | "module"; i = UIDENT; mb = module_binding ->
          <:str_item< module $i$ = $mb$ >>
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:str_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:str_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:str_item< type $list:tdl$ >>
      | "let"; r = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = expr ->
          let e = <:expr< let $opt:o2b r$ $list:l$ in $x$ >> in
          <:str_item< $exp:e$ >>
      | "let"; r = OPT "rec"; l = LIST1 let_binding SEP "and" ->
          match l with
          [ [(<:patt< _ >>, e)] -> <:str_item< $exp:e$ >>
          | _ -> <:str_item< value $opt:o2b r$ $list:l$ >> ]
      | "let"; "module"; m = UIDENT; mb = module_binding; "in"; e = expr ->
          <:str_item< let module $m$ = $mb$ in $e$ >>
      | e = expr -> <:str_item< $exp:e$ >> ] ]
  ;
  module_binding:
    [ RIGHTA
      [ "("; m = UIDENT; ":"; mt = module_type; ")"; mb = SELF ->
          <:module_expr< functor ( $m$ : $mt$ ) -> $mb$ >>
      | ":"; mt = module_type; "="; me = module_expr ->
          <:module_expr< ( $me$ : $mt$ ) >>
      | "="; me = module_expr -> <:module_expr< $me$ >> ] ]
  ;
  (* Module types *)
  module_type:
    [ [ "functor"; "("; i = UIDENT; ":"; t = SELF; ")"; "->"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ]
    | [ mt = SELF; "with"; wcl = LIST1 with_constr SEP "and" ->
          <:module_type< $mt$ with $list:wcl$ >> ]
    | [ "sig"; sg = LIST0 [ s = sig_item; OPT ";;" -> s ]; "end" ->
          <:module_type< sig $list:sg$ end >>
      | i = mod_type_ident -> i
      | "("; mt = SELF; ")" -> <:module_type< $mt$ >> ] ]
  ;
  mod_type_ident:
    [ LEFTA
      [ m1 = SELF; "."; m2 = SELF -> <:module_type< $m1$ . $m2$ >>
      | m1 = SELF; "("; m2 = SELF; ")" -> <:module_type< $m1$ $m2$ >> ]
    | [ m = UIDENT -> <:module_type< $uid:m$ >>
      | m = LIDENT -> <:module_type< $lid:m$ >> ] ]
  ;
  sig_item:
    [ "top"
      [ "exception"; (_, c, tl) = constructor_declaration ->
          <:sig_item< exception $c$ of $list:tl$ >>
      | "external"; i = LIDENT; ":"; t = ctyp; "="; pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "external"; i = LABEL; t = ctyp; "="; pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "external"; "("; i = operator; ")"; ":"; t = ctyp; "=";
        pd = LIST1 STRING ->
          <:sig_item< external $i$ : $t$ = $list:pd$ >>
      | "include"; mt = module_type -> <:sig_item< include $mt$ >>
      | "module"; i = UIDENT; mt = module_declaration ->
          <:sig_item< module $i$ : $mt$ >>
      | "module"; "type"; i = UIDENT; "="; mt = module_type ->
          <:sig_item< module type $i$ = $mt$ >>
      | "open"; i = mod_ident -> <:sig_item< open $i$ >>
      | "type"; tdl = LIST1 type_declaration SEP "and" ->
          <:sig_item< type $list:tdl$ >>
      | "val"; i = LIDENT; ":"; t = ctyp -> <:sig_item< value $i$ : $t$ >>
      | "val"; i = LABEL; t = ctyp -> <:sig_item< value $i$ : $t$ >>
      | "val"; "("; i = operator; ")"; ":"; t = ctyp ->
          <:sig_item< value $i$ : $t$ >> ] ]
  ;
  module_declaration:
    [ RIGHTA
      [ ":"; mt = module_type -> <:module_type< $mt$ >>
      | "("; i = UIDENT; ":"; t = module_type; ")"; mt = SELF ->
          <:module_type< functor ( $i$ : $t$ ) -> $mt$ >> ] ]
  ;
  (* "with" constraints (additional type equations over signature
     components) *)
  with_constr:
    [ [ "type"; tp = type_parameters; i = mod_ident; "="; t = ctyp ->
          MLast.WcTyp loc i tp t
      | "module"; i = mod_ident; "="; me = module_expr ->
          MLast.WcMod loc i me ] ]
  ;
  (* Core expressions *)
  expr:
    [ "top" LEFTA
      [ e1 = SELF; ";"; e2 = SELF ->
          <:expr< do { $list:[e1 :: get_seq e2]$ } >>
      | e1 = SELF; ";" -> e1 ]
    | "expr1"
      [ "let"; o = OPT "rec"; l = LIST1 let_binding SEP "and"; "in";
        x = expr LEVEL "top" ->
          <:expr< let $opt:o2b o$ $list:l$ in $x$ >>
      | "let"; "module"; m = UIDENT; mb = module_binding; "in";
        e = expr LEVEL "top" ->
          <:expr< let module $m$ = $mb$ in $e$ >>
      | "function"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< fun [ $list:l$ ] >>
      | "fun"; p = patt LEVEL "simple"; e = fun_def ->
          <:expr< fun [$p$ -> $e$] >>
      | "match"; x = SELF; "with"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< match $x$ with [ $list:l$ ] >>
      | "try"; x = SELF; "with"; OPT "|"; l = LIST1 match_case SEP "|" ->
          <:expr< try $x$ with [ $list:l$ ] >>
      | "if"; e1 = SELF; "then"; e2 = expr LEVEL "expr1";
        e3 = [ "else"; e = expr LEVEL "expr1" -> e | -> <:expr< () >> ] ->
          <:expr< if $e1$ then $e2$ else $e3$ >>
      | "for"; i = LIDENT; "="; e1 = SELF; df = direction_flag; e2 = SELF;
        "do"; e = SELF; "done" ->
          <:expr< for $i$ = $e1$ $to:df$ $e2$ do { $list:get_seq e$ } >>
      | "while"; e1 = SELF; "do"; e2 = SELF; "done" ->
          <:expr< while $e1$ do { $list:get_seq e2$ } >> ]
    | [ e = SELF; ","; el = LIST1 NEXT SEP "," ->
          <:expr< ( $list:[e :: el]$ ) >> ]
    | ":=" NONA
      [ e1 = SELF; ":="; e2 = expr LEVEL "expr1" ->
          <:expr< $e1$.val := $e2$ >>
      | e1 = SELF; "<-"; e2 = expr LEVEL "expr1" -> <:expr< $e1$ := $e2$ >> ]
    | "||" RIGHTA
      [ e1 = SELF; f = [ op = "or" -> op | op = "||" -> op ]; e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "&&" RIGHTA
      [ e1 = SELF; f = [ op = "&" -> op | op = "&&" -> op ]; e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "<" LEFTA
      [ e1 = SELF;
        f =
          [ op = "<" -> op
          | op = ">" -> op
          | op = "<=" -> op
          | op = ">=" -> op
          | op = "=" -> op
          | op = "<>" -> op
          | op = "==" -> op
          | op = "!=" -> op
          | op = infixop0 -> op ];
        e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "^" RIGHTA
      [ e1 = SELF;
        f = [ op = "^" -> op | op = "@" -> op | op = infixop1 -> op ];
        e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | RIGHTA
      [ e1 = SELF; "::"; e2 = SELF -> <:expr< [$e1$ :: $e2$] >> ]
    | "+" LEFTA
      [ e1 = SELF;
        f =
          [ op = "+" -> op
          | op = "-" -> op
          | op = "+." -> op
          | op = "-." -> op
          | op = infixop2 -> op ];
        e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "*" LEFTA
      [ e1 = SELF;
        f =
          [ op = "*" -> op
          | op = "/" -> op
          | op = "*." -> op
          | op = "/." -> op
          | op = "land" -> op
          | op = "lor" -> op
          | op = "lxor" -> op
          | op = "mod" -> op
          | op = infixop3 -> op ];
        e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "**" RIGHTA
      [ e1 = SELF;
        f =
          [ op = "**" -> op
          | op = "asr" -> op
          | op = "lsl" -> op
          | op = "lsr" -> op
          | op = infixop4 -> op ];
        e2 = SELF ->
          <:expr< $lid:f$ $e1$ $e2$ >> ]
    | "unary minus" NONA
      [ f = [ op = "-" -> op | op = "-." -> op ]; e = SELF ->
          <:expr< $mkumin loc f e$ >> ]
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF ->
          match constr_expr_arity e1 with
          [ 1 -> <:expr< $e1$ $e2$ >>
          | _ ->
              match e2 with
              [ <:expr< ( $list:el$ ) >> ->
                  List.fold_left (fun e1 e2 -> <:expr< $e1$ $e2$ >>) e1 el
              | _ -> <:expr< $e1$ $e2$ >> ] ]
      | "assert"; e = expr LEVEL "simple" ->
          match e with
          [ <:expr< False >> -> MLast.ExAsf loc
          | _ -> MLast.ExAsr loc e ]
      | "lazy"; e = SELF ->
          <:expr< lazy ($e$) >> ]
    | "simple" LEFTA
      [ e1 = SELF; "."; "("; e2 = SELF; ")" -> <:expr< $e1$ .( $e2$ ) >>
      | e1 = SELF; "."; "["; e2 = SELF; "]" -> <:expr< $e1$ .[ $e2$ ] >>
      | e1 = SELF; "."; e2 = SELF -> <:expr< $e1$ . $e2$ >>
      | "!"; e = SELF -> <:expr< $e$ . val>>
      | f =
          [ op = "~-" -> op
          | op = "~-." -> op
          | op = "~" -> op
          | op = prefixop -> op ];
        e = SELF ->
          <:expr< $lid:f$ $e$ >>
      | s = INT -> <:expr< $int:s$ >>
      | s = FLOAT -> <:expr< $flo:s$ >>
      | s = STRING -> <:expr< $str:s$ >>
      | c = CHAR -> <:expr< $chr:c$ >>
      | i = expr_ident -> i
      | s = "false" -> <:expr< False >>
      | s = "true" -> <:expr< True >>
      | "["; "]" -> <:expr< [] >>
      | "["; el = expr1_semi_list; "]" -> <:expr< $mklistexp loc None el$ >>
      | "[|"; "|]" -> <:expr< [| |] >>
      | "[|"; el = expr1_semi_list; "|]" -> <:expr< [| $list:el$ |] >>
      | "{"; test_label_eq; lel = lbl_expr_list; "}" ->
          <:expr< { $list:lel$ } >>
      | "{"; e = expr LEVEL "simple"; "with"; lel = lbl_expr_list; "}" ->
          <:expr< { ($e$) with $list:lel$ } >>
      | "("; ")" -> <:expr< () >>
      | "("; e = SELF; ":"; t = ctyp; ")" -> <:expr< ($e$ : $t$) >>
      | "("; e = SELF; ")" -> <:expr< $e$ >>
      | "("; "-"; ")" -> <:expr< $lid:"-"$ >>
      | "("; "-."; ")" -> <:expr< $lid:"-."$ >>
      | "("; op = operator; ")" -> <:expr< $lid:op$ >>
      | "begin"; e = SELF; "end" -> <:expr< $e$ >>
      | x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              ({Lexing.pos_fname = "";
                Lexing.pos_lnum = 0;
                Lexing.pos_bol = 0;
                Lexing.pos_cnum = int_of_string (String.sub x 0 i)},
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> (Token.nowhere, x) ]
          in
          Pcaml.handle_expr_locate loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_expr_quotation loc x ] ]
  ;
  let_binding:
    [ [ p = patt; e = fun_binding ->
          match patt_lid p with
          [ Some (i, pl) ->
              let e =
                List.fold_left (fun e p -> <:expr< fun $p$ -> $e$ >>) e pl
              in
              (<:patt< $lid:i$ >>, e)
          | None -> (p, e) ] ] ]
  ;
  fun_binding:
    [ RIGHTA
      [ p = patt LEVEL "simple"; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "="; e = expr -> <:expr< $e$ >>
      | ":"; t = ctyp; "="; e = expr -> <:expr< ($e$ : $t$) >> ] ]
  ;
  match_case:
    [ [ x1 = patt; w = OPT [ "when"; e = expr -> e ]; "->"; x2 = expr ->
          (x1, w, x2) ] ]
  ;
  lbl_expr_list:
    [ [ le = lbl_expr; ";"; lel = SELF -> [le :: lel]
      | le = lbl_expr; ";" -> [le]
      | le = lbl_expr -> [le] ] ]
  ;
  lbl_expr:
    [ [ i = patt_label_ident; "="; e = expr LEVEL "expr1" -> (i, e) ] ]
  ;
  expr1_semi_list:
    [ [ e = expr LEVEL "expr1"; ";"; el = SELF -> [e :: el]
      | e = expr LEVEL "expr1"; ";" -> [e]
      | e = expr LEVEL "expr1" -> [e] ] ]
  ;
  fun_def:
    [ RIGHTA
      [ p = patt LEVEL "simple"; e = SELF -> <:expr< fun $p$ -> $e$ >>
      | "->"; e = expr -> <:expr< $e$ >> ] ]
  ;
  expr_ident:
    [ RIGHTA
      [ i = LIDENT -> <:expr< $lid:i$ >>
      | i = UIDENT -> <:expr< $uid:i$ >>
      | m = UIDENT; "."; i = SELF ->
          let rec loop m =
            fun
            [ <:expr< $x$ . $y$ >> -> loop <:expr< $m$ . $x$ >> y
            | e -> <:expr< $m$ . $e$ >> ]
          in
          loop <:expr< $uid:m$ >> i
      | m = UIDENT; "."; "("; i = operator; ")" ->
          <:expr< $uid:m$ . $lid:i$ >> ] ]
  ;
  (* Patterns *)
  patt:
    [ LEFTA
      [ p1 = SELF; "as"; i = LIDENT -> <:patt< ($p1$ as $lid:i$) >> ]
    | LEFTA
      [ p1 = SELF; "|"; p2 = SELF -> <:patt< $p1$ | $p2$ >> ]
    | [ p = SELF; ","; pl = LIST1 NEXT SEP "," ->
          <:patt< ( $list:[p :: pl]$) >> ]
    | NONA
      [ p1 = SELF; ".."; p2 = SELF -> <:patt< $p1$ .. $p2$ >> ]
    | RIGHTA
      [ p1 = SELF; "::"; p2 = SELF -> <:patt< [$p1$ :: $p2$] >> ]
    | LEFTA
      [ p1 = SELF; p2 = SELF ->
          match constr_patt_arity p1 with
          [ 1 -> <:patt< $p1$ $p2$ >>
          | n ->
              let p2 =
                match p2 with
                [ <:patt< _ >> when n > 1 ->
                    let pl =
                      loop n where rec loop n =
                        if n = 0 then [] else [<:patt< _ >> :: loop (n - 1)]
                    in
                    <:patt< ( $list:pl$ ) >>
                | _ -> p2 ]
              in
              match p2 with
              [ <:patt< ( $list:pl$ ) >> ->
                  List.fold_left (fun p1 p2 -> <:patt< $p1$ $p2$ >>) p1 pl
              | _ -> <:patt< $p1$ $p2$ >> ] ] ]
    | LEFTA
      [ p1 = SELF; "."; p2 = SELF -> <:patt< $p1$ . $p2$ >> ]
    | "simple"
      [ s = LIDENT -> <:patt< $lid:s$ >>
      | s = UIDENT -> <:patt< $uid:s$ >>
      | s = INT -> <:patt< $int:s$ >>
      | "-"; s = INT -> <:patt< $int:neg s$ >>
      | s = STRING -> <:patt< $str:s$ >>
      | s = CHAR -> <:patt< $chr:s$ >>
      | s = "false" -> <:patt< False >>
      | s = "true" -> <:patt< True >>
      | "["; "]" -> <:patt< [] >>
      | "["; pl = patt_semi_list; "]" -> <:patt< $mklistpat loc None pl$ >>
      | "[|"; "|]" -> <:patt< [| |] >>
      | "[|"; pl = patt_semi_list; "|]" -> <:patt< [| $list:pl$ |] >>
      | "{"; lpl = lbl_patt_list; "}" -> <:patt< { $list:lpl$ } >>
      | "("; ")" -> <:patt< () >>
      | "("; p = SELF; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >>
      | "("; p = SELF; ")" -> <:patt< $p$ >>
      | "("; "-"; ")" -> <:patt< $lid:"-"$ >>
      | "("; op = operator; ")" -> <:patt< $lid:op$ >>
      | "_" -> <:patt< _ >>
      | x = LOCATE ->
          let x =
            try
              let i = String.index x ':' in
              ({Lexing.pos_fname = "";
                Lexing.pos_lnum = 0;
                Lexing.pos_bol = 0;
                Lexing.pos_cnum =  int_of_string (String.sub x 0 i)},
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found | Failure _ -> (Token.nowhere, x) ]
          in
          Pcaml.handle_patt_locate loc x
      | x = QUOTATION ->
          let x =
            try
              let i = String.index x ':' in
              (String.sub x 0 i,
               String.sub x (i + 1) (String.length x - i - 1))
            with
            [ Not_found -> ("", x) ]
          in
          Pcaml.handle_patt_quotation loc x ] ]
  ;
  patt_semi_list:
    [ [ p = patt; ";"; pl = SELF -> [p :: pl]
      | p = patt; ";" -> [p]
      | p = patt -> [p] ] ]
  ;
  lbl_patt_list:
    [ [ le = lbl_patt; ";"; lel = SELF -> [le :: lel]
      | le = lbl_patt; ";" -> [le]
      | le = lbl_patt -> [le] ] ]
  ;
  lbl_patt:
    [ [ i = patt_label_ident; "="; p = patt -> (i, p) ] ]
  ;
  patt_label_ident:
    [ RIGHTA
      [ i = UIDENT -> <:patt< $uid:i$ >>
      | i = LIDENT -> <:patt< $lid:i$ >>
      | m = UIDENT; "."; i = SELF -> <:patt< $uid:m$ . $i$ >> ] ]
  ;
  (* Type declaration *)
  type_declaration:
    [ [ tpl = type_parameters; n = type_patt; "="; tk = type_kind;
        cl = LIST0 constrain ->
          (n, tpl, tk, cl)
      | tpl = type_parameters; n = type_patt; cl = LIST0 constrain ->
          (n, tpl, <:ctyp< '$choose_tvar tpl$ >>, cl) ] ]
  ;
  type_patt:
    [ [ n = LIDENT -> (loc, n) ] ]
  ;
  constrain:
    [ [ "constraint"; t1 = ctyp; "="; t2 = ctyp -> (t1, t2) ] ]
  ;
  type_kind:
    [ [ test_constr_decl; OPT "|";
        cdl = LIST1 constructor_declaration SEP "|" ->
          <:ctyp< [ $list:cdl$ ] >>
      | t = ctyp -> <:ctyp< $t$ >>
      | t = ctyp; "="; "{"; ldl = label_declarations; "}" ->
          <:ctyp< $t$ == { $list:ldl$ } >>
      | t = ctyp; "="; OPT "|"; cdl = LIST1 constructor_declaration SEP "|" ->
          <:ctyp< $t$ == [ $list:cdl$ ] >>
      | "{"; ldl = label_declarations; "}" -> <:ctyp< { $list:ldl$ } >> ] ]
  ;
  type_parameters:
    [ [ -> (* empty *) []
      | tp = type_parameter -> [tp]
      | "("; tpl = LIST1 type_parameter SEP ","; ")" -> tpl ] ]
  ;
  type_parameter:
    [ [ "'"; i = ident -> (i, (False, False)) ] ]
  ;
  constructor_declaration:
    [ [ ci = UIDENT; "of"; cal = LIST1 ctyp LEVEL "ctyp1" SEP "*" ->
          (loc, ci, cal)
      | ci = UIDENT -> (loc, ci, []) ] ]
  ;
  label_declarations:
    [ [ ld = label_declaration; ";"; ldl = SELF -> [ld :: ldl]
      | ld = label_declaration; ";" -> [ld]
      | ld = label_declaration -> [ld] ] ]
  ;
  label_declaration:
    [ [ i = LIDENT; ":"; t = ctyp -> (loc, i, False, t)
      | i = LABEL; t = ctyp -> (loc, i, False, t)
      | "mutable"; i = LIDENT; ":"; t = ctyp -> (loc, i, True, t)
      | "mutable"; i = LABEL; t = ctyp -> (loc, i, True, t) ] ]
  ;
  (* Core types *)
  ctyp:
    [ [ t1 = SELF; "as"; "'"; i = ident -> <:ctyp< $t1$ as '$i$ >> ]
    | "arrow" RIGHTA
      [ t1 = SELF; "->"; t2 = SELF -> <:ctyp< $t1$ -> $t2$ >> ]
    | [ t = SELF; "*"; tl = LIST1 ctyp LEVEL "ctyp1" SEP "*" ->
          <:ctyp< ( $list:[t :: tl]$ ) >> ]
    | "ctyp1"
      [ t1 = SELF; t2 = SELF -> <:ctyp< $t2$ $t1$ >> ]
    | "ctyp2"
      [ t1 = SELF; "."; t2 = SELF -> <:ctyp< $t1$ . $t2$ >>
      | t1 = SELF; "("; t2 = SELF; ")" -> <:ctyp< $t1$ $t2$ >> ]
    | "simple"
      [ "'"; i = ident -> <:ctyp< '$i$ >>
      | "_" -> <:ctyp< _ >>
      | i = LIDENT -> <:ctyp< $lid:i$ >>
      | i = UIDENT -> <:ctyp< $uid:i$ >>
      | "("; t = SELF; ","; tl = LIST1 ctyp SEP ","; ")";
        i = ctyp LEVEL "ctyp2" ->
          List.fold_left (fun c a -> <:ctyp< $c$ $a$ >>) i [t :: tl]
      | "("; t = SELF; ")" -> <:ctyp< $t$ >> ] ]
  ;
  (* Identifiers *)
  ident:
    [ [ i = LIDENT -> i
      | i = UIDENT -> i ] ]
  ;
  mod_ident:
    [ RIGHTA
      [ i = UIDENT -> [i]
      | i = LIDENT -> [i]
      | m = UIDENT; "."; i = SELF -> [m :: i] ] ]
  ;
  (* Miscellaneous *)
  direction_flag:
    [ [ "to" -> True
      | "downto" -> False ] ]
  ;
END;

(* Objects and Classes *)

value rec class_type_of_ctyp loc t =
  match t with
  [ <:ctyp< $lid:i$ >> -> <:class_type< $list:[i]$ >>
  | <:ctyp< $uid:m$.$t$ >> -> <:class_type< $list:[m :: type_id_list t]$ >>
  | _ -> raise_with_loc loc (Stream.Error "lowercase identifier expected") ]
and type_id_list =
  fun
  [ <:ctyp< $uid:m$.$t$ >> -> [m :: type_id_list t]
  | <:ctyp< $lid:i$ >> -> [i]
  | t ->
      raise_with_loc (loc_of_node t)
        (Stream.Error "lowercase identifier expected") ]
;

value class_fun_binding = Grammar.Entry.create gram "class_fun_binding";

EXTEND
  GLOBAL: str_item sig_item expr ctyp class_sig_item class_str_item class_type
    class_expr class_fun_binding;
  str_item:
    [ [ "class"; cd = LIST1 class_declaration SEP "and" ->
          <:str_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:str_item< class type $list:ctd$ >> ] ]
  ;
  sig_item:
    [ [ "class"; cd = LIST1 class_description SEP "and" ->
          <:sig_item< class $list:cd$ >>
      | "class"; "type"; ctd = LIST1 class_type_declaration SEP "and" ->
          <:sig_item< class type $list:ctd$ >> ] ]
  ;
  (* Class expressions *)
  class_declaration:
    [ [ vf = OPT "virtual"; ctp = class_type_parameters; i = LIDENT;
        cfb = class_fun_binding ->
          {MLast.ciLoc = loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = i; MLast.ciExp = cfb} ] ]
  ;
  class_fun_binding:
    [ [ "="; ce = class_expr -> ce
      | ":"; ct = class_type; "="; ce = class_expr ->
          <:class_expr< ($ce$ : $ct$) >>
      | p = patt LEVEL "simple"; cfb = SELF ->
          <:class_expr< fun $p$ -> $cfb$ >> ] ]
  ;
  class_type_parameters:
    [ [ -> (loc, [])
      | "["; tpl = LIST1 type_parameter SEP ","; "]" -> (loc, tpl) ] ]
  ;
  class_fun_def:
    [ [ p = patt LEVEL "simple"; "->"; ce = class_expr ->
          <:class_expr< fun $p$ -> $ce$ >>
      | p = patt LEVEL "simple"; cfd = SELF ->
          <:class_expr< fun $p$ -> $cfd$ >> ] ]
  ;
  class_expr:
    [ "top"
      [ "fun"; cfd = class_fun_def -> cfd
      | "let"; rf = OPT "rec"; lb = LIST1 let_binding SEP "and"; "in";
        ce = SELF ->
          <:class_expr< let $opt:o2b rf$ $list:lb$ in $ce$ >> ]
    | "apply" NONA
      [ ce = SELF; e = expr LEVEL "label" ->
          <:class_expr< $ce$ $e$ >> ]
    | "simple"
      [ "["; ct = ctyp; ","; ctcl = LIST1 ctyp SEP ","; "]";
        ci = class_longident ->
          <:class_expr< $list:ci$ [ $list:[ct :: ctcl]$ ] >>
      | "["; ct = ctyp; "]"; ci = class_longident ->
          <:class_expr< $list:ci$ [ $ct$ ] >>
      | ci = class_longident -> <:class_expr< $list:ci$ >>
      | "object"; cspo = OPT class_self_patt; cf = class_structure; "end" ->
          <:class_expr< object $opt:cspo$ $list:cf$ end >>
      | "("; ce = SELF; ":"; ct = class_type; ")" ->
          <:class_expr< ($ce$ : $ct$) >>
      | "("; ce = SELF; ")" -> ce ] ]
  ;
  class_structure:
    [ [ cf = LIST0 class_str_item -> cf ] ]
  ;
  class_self_patt:
    [ [ "("; p = patt; ")" -> p
      | "("; p = patt; ":"; t = ctyp; ")" -> <:patt< ($p$ : $t$) >> ] ]
  ;
  class_str_item:
    [ [ "inherit"; ce = class_expr; pb = OPT [ "as"; i = LIDENT -> i ] ->
          <:class_str_item< inherit $ce$ $opt:pb$ >>
      | "val"; (lab, mf, e) = cvalue ->
          <:class_str_item< value $opt:mf$ $lab$ = $e$ >>
      | "method"; "private"; "virtual"; l = label; ":"; t = ctyp ->
          <:class_str_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          <:class_str_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          <:class_str_item< method virtual $l$ : $t$ >>
      | "method"; "private"; l = label; fb = fun_binding ->
          <:class_str_item< method private $l$ = $fb$ >>
      | "method"; l = label; fb = fun_binding ->
          <:class_str_item< method $l$ = $fb$ >>
      | "constraint"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_str_item< type $t1$ = $t2$ >>
      | "initializer"; se = expr -> <:class_str_item< initializer $se$ >> ] ]
  ;
  cvalue:
    [ [ mf = OPT "mutable"; l = label; "="; e = expr -> (l, o2b mf, e)
      | mf = OPT "mutable"; l = label; ":"; t = ctyp; "="; e = expr ->
          (l, o2b mf, <:expr< ($e$ : $t$) >>)
      | mf = OPT "mutable"; l = label; ":"; t1 = ctyp; ":>"; t2 = ctyp; "=";
        e = expr ->
          (l, o2b mf, <:expr< ($e$ : $t1$ :> $t2$) >>)
      | mf = OPT "mutable"; l = label; ":>"; t = ctyp; "="; e = expr ->
          (l, o2b mf, <:expr< ($e$ :> $t$) >>) ] ]
  ;
  label:
    [ [ i = LIDENT -> i ] ]
  ;
  (* Class types *)
  class_type:
    [ [ t = ctyp LEVEL "ctyp1" -> class_type_of_ctyp loc t
      | t = ctyp LEVEL "ctyp1"; "->"; ct = SELF ->
          <:class_type< [ $t$ ] -> $ct$ >>
      | t = ctyp LEVEL "ctyp1"; "*"; tl = LIST1 ctyp LEVEL "simple" SEP "*";
        "->"; ct = SELF ->
          <:class_type< [ ($t$ * $list:tl$) ] -> $ct$ >>
      | cs = class_signature -> cs ] ]
  ;
  class_signature:
    [ [ "["; tl = LIST1 ctyp SEP ","; "]"; id = clty_longident ->
          <:class_type< $list:id$ [ $list:tl$ ] >>
      | id = clty_longident -> <:class_type< $list:id$ >>
      | "object"; cst = OPT class_self_type; csf = LIST0 class_sig_item;
        "end" ->
          <:class_type< object $opt:cst$ $list:csf$ end >> ] ]
  ;
  class_self_type:
    [ [ "("; t = ctyp; ")" -> t ] ]
  ;
  class_sig_item:
    [ [ "inherit"; cs = class_signature -> <:class_sig_item< inherit $cs$ >>
      | "val"; mf = OPT "mutable"; l = label; ":"; t = ctyp ->
          <:class_sig_item< value $opt:o2b mf$ $l$ : $t$ >>
      | "method"; "private"; "virtual"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; "private"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method virtual private $l$ : $t$ >>
      | "method"; "virtual"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method virtual $l$ : $t$ >>
      | "method"; "private"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method private $l$ : $t$ >>
      | "method"; l = label; ":"; t = ctyp ->
          <:class_sig_item< method $l$ : $t$ >>
      | "constraint"; t1 = ctyp; "="; t2 = ctyp ->
          <:class_sig_item< type $t1$ = $t2$ >> ] ]
  ;
  class_description:
    [ [ vf = OPT "virtual"; ctp = class_type_parameters; n = LIDENT; ":";
        ct = class_type ->
          {MLast.ciLoc = loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = ct}
      | vf = OPT "virtual"; ctp = class_type_parameters; n = LABEL;
        ct = class_type ->
          {MLast.ciLoc = loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = ct} ] ]
  ;
  class_type_declaration:
    [ [ vf = OPT "virtual"; ctp = class_type_parameters; n = LIDENT; "=";
        cs = class_signature ->
          {MLast.ciLoc = loc; MLast.ciVir = o2b vf; MLast.ciPrm = ctp;
           MLast.ciNam = n; MLast.ciExp = cs} ] ]
  ;
  (* Expressions *)
  expr: LEVEL "apply"
    [ LEFTA
      [ "new"; i = class_longident -> <:expr< new $list:i$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ e = SELF; "#"; lab = label -> <:expr< $e$ # $lab$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "("; e = SELF; ":"; t1 = ctyp; ":>"; t2 = ctyp; ")" ->
          <:expr< ($e$ : $t1$ :> $t2$) >>
      | "("; e = SELF; ":>"; t = ctyp; ")" -> <:expr< ($e$ :> $t$) >>
      | "{<"; ">}" -> <:expr< {< >} >>
      | "{<"; fel = field_expr_list; ">}" -> <:expr< {< $list:fel$ >} >> ] ]
  ;
  field_expr_list:
    [ [ l = label; "="; e = expr LEVEL "expr1"; ";"; fel = SELF ->
          [(l, e) :: fel]
      | l = label; "="; e = expr LEVEL "expr1"; ";" -> [(l, e)]
      | l = label; "="; e = expr LEVEL "expr1" -> [(l, e)] ] ]
  ;
  (* Core types *)
  ctyp: LEVEL "simple"
    [ [ "#"; id = class_longident -> <:ctyp< # $list:id$ >>
      | "<"; (ml, v) = meth_list; ">" -> <:ctyp< < $list:ml$ $opt:v$ > >>
      | "<"; ">" -> <:ctyp< < > >> ] ]
  ;
  meth_list:
    [ [ f = field; ";"; (ml, v) = SELF -> ([f :: ml], v)
      | f = field; ";" -> ([f], False)
      | f = field -> ([f], False)
      | ".." -> ([], True) ] ]
  ;
  field:
    [ [ lab = LIDENT; ":"; t = ctyp -> (lab, t)
      | lab = LABEL; t = ctyp -> (lab, t) ] ]
  ;
  (* Identifiers *)
  clty_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
  class_longident:
    [ [ m = UIDENT; "."; l = SELF -> [m :: l]
      | i = LIDENT -> [i] ] ]
  ;
END;

(* Labels *)

EXTEND
  GLOBAL: ctyp expr patt fun_def fun_binding class_type class_fun_binding;
  ctyp: AFTER "arrow"
    [ NONA
      [ i = LABEL; t = SELF -> <:ctyp< ~ $i$ : $t$ >>
      | "?"; i = LABEL; t = SELF -> <:ctyp< ? $i$ : $t$ >> ] ]
  ;
  ctyp: LEVEL "simple"
    [ [ "["; OPT "|"; rfl = LIST0 row_field SEP "|"; "]" ->
          <:ctyp< [ = $list:rfl$ ] >>
      | "["; ">"; OPT "|"; rfl = LIST1 row_field SEP "|"; "]" ->
          <:ctyp< [ > $list:rfl$ ] >>
      | "[<"; OPT "|"; rfl = LIST1 row_field SEP "|"; "]" ->
          <:ctyp< [ < $list:rfl$ ] >>
      | "[<"; OPT "|"; rfl = LIST1 row_field SEP "|"; ">";
        ntl = LIST1 name_tag; "]" ->
          <:ctyp< [ < $list:rfl$ > $list:ntl$ ] >> ] ]
  ;
  row_field:
    [ [ "`"; i = ident -> MLast.RfTag i False []
      | "`"; i = ident; "of"; ao = OPT "&"; l = LIST1 ctyp SEP "&" ->
          MLast.RfTag i (o2b ao) l
      | "`"; i = ident; "&"; l = LIST1 ctyp SEP "&" -> MLast.RfTag i True l
      | "`"; i = ident; l = LIST1 ctyp SEP "&" -> MLast.RfTag i False l ] ]
  ;
  name_tag:
    [ [ "`"; i = ident -> i ] ]
  ;
  expr: LEVEL "expr1"
    [ [ "fun"; p = labeled_patt; e = fun_def -> <:expr< fun $p$ -> $e$ >> ] ]
  ;
  expr: AFTER "apply"
    [ "label"
      [ i = LABEL; e = SELF -> <:expr< ~ $i$ : $e$ >>
      | i = ELABEL -> <:expr< ~ $i$ >>
      | "?"; i = LABEL; e = SELF -> <:expr< ? $i$ : $e$ >>
      | "?"; i = ELABEL -> <:expr< ? $i$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "`"; s = ident -> <:expr< ` $s$ >> ] ]
  ;
  fun_def:
    [ [ p = labeled_patt; e = SELF -> <:expr< fun $p$ -> $e$ >> ] ]
  ;
  fun_binding:
    [ [ p = labeled_patt; e = SELF -> <:expr< fun $p$ -> $e$ >> ] ]
  ;
  patt: LEVEL "simple"
    [ [ "`"; s = ident -> <:patt< ` $s$ >> ] ]
  ;
  labeled_patt:
    [ [ i = LABEL; p = patt LEVEL "simple" -> <:patt< ~ $i$ : $p$ >>
      | i = ELABEL -> <:patt< ~ $i$ >>
      | "?"; i = LABEL; j = LIDENT -> <:patt< ? $i$ : ($lid:j$) >>
      | "?"; "("; i = LABEL; j = LIDENT; ")" -> <:patt< ? $i$ : ($lid:j$) >>
      | "?"; "("; i = LABEL; j = LIDENT; "="; e = expr; ")" ->
          <:patt< ? $i$ : ( $lid:j$ = $e$ ) >>
      | "?"; i = ELABEL -> <:patt< ? $i$ : ($lid:i$) >>
      | "?"; "("; i = ELABEL; "="; e = expr; ")" ->
          <:patt< ? $i$ : ( $lid:i$ = $e$ ) >> ] ]
  ;
  class_type:
    [ [ i = LABEL; t = ctyp LEVEL "ctyp1"; "->"; ct = SELF ->
          <:class_type< [ ~ $i$ : $t$ ] -> $ct$ >>
      | "?"; i = LABEL; t = ctyp LEVEL "ctyp1"; "->"; ct = SELF ->
          <:class_type< [ ? $i$ : $t$ ] -> $ct$ >> ] ]
  ;
  class_fun_binding:
    [ [ p = labeled_patt; cfb = SELF -> <:class_expr< fun $p$ -> $cfb$ >> ] ]
  ;
  ident:
    [ [ i = LIDENT -> i
      | i = UIDENT -> i ] ]
  ;
END;

type spat_comp =
  [ SpTrm of Loc.t and MLast.patt and option MLast.expr
  | SpNtr of Loc.t and MLast.patt and MLast.expr
  | SpStr of Loc.t and MLast.patt ]
;
type sexp_comp =
  [ SeTrm of Loc.t and MLast.expr | SeNtr of Loc.t and MLast.expr ]
;

value strm_n = "__strm";
value peek_fun loc = <:expr< Stream.peek >>;
value junk_fun loc = <:expr< Stream.junk >>;

(* Parsers. *)
(* In syntax generated, many cases are optimisations. *)

value rec pattern_eq_expression p e =
  match (p, e) with
  [ (<:patt< $lid:a$ >>, <:expr< $lid:b$ >>) -> a = b
  | (<:patt< $uid:a$ >>, <:expr< $uid:b$ >>) -> a = b
  | (<:patt< $p1$ $p2$ >>, <:expr< $e1$ $e2$ >>) ->
      pattern_eq_expression p1 e1 && pattern_eq_expression p2 e2
  | _ -> False ]
;

value is_raise e =
  match e with
  [ <:expr< raise $_$ >> -> True
  | _ -> False ]
;

value is_raise_failure e =
  match e with
  [ <:expr< raise Stream.Failure >> -> True
  | _ -> False ]
;

value rec handle_failure e =
  match e with
  [ <:expr< try $te$ with [ Stream.Failure -> $e$] >> -> handle_failure e
  | <:expr< match $me$ with [ $list:pel$ ] >> ->
      handle_failure me &&
      List.for_all
        (fun
         [ (_, None, e) -> handle_failure e
         | _ -> False ])
        pel
  | <:expr< let $list:pel$ in $e$ >> ->
      List.for_all (fun (p, e) -> handle_failure e) pel && handle_failure e
  | <:expr< $lid:_$ >> | <:expr< $int:_$ >> | <:expr< $str:_$ >> |
    <:expr< $chr:_$ >> | <:expr< fun [ $list:_$ ] >> | <:expr< $uid:_$ >> ->
      True
  | <:expr< raise $e$ >> ->
      match e with
      [ <:expr< Stream.Failure >> -> False
      | _ -> True ]
  | <:expr< $f$ $x$ >> ->
      is_constr_apply f && handle_failure f && handle_failure x
  | _ -> False ]
and is_constr_apply =
  fun
  [ <:expr< $uid:_$ >> -> True
  | <:expr< $lid:_$ >> -> False
  | <:expr< $x$ $_$ >> -> is_constr_apply x
  | _ -> False ]
;

value rec subst v e =
  let loc = MLast.loc_of_expr e in
  match e with
  [ <:expr< $lid:x$ >> ->
      let x = if x = v then strm_n else x in
      <:expr< $lid:x$ >>
  | <:expr< $uid:_$ >> -> e
  | <:expr< $int:_$ >> -> e
  | <:expr< $chr:_$ >> -> e
  | <:expr< $str:_$ >> -> e
  | <:expr< $_$ . $_$ >> -> e
  | <:expr< let $opt:rf$ $list:pel$ in $e$ >> ->
      <:expr< let $opt:rf$ $list:List.map (subst_pe v) pel$ in $subst v e$ >>
  | <:expr< $e1$ $e2$ >> -> <:expr< $subst v e1$ $subst v e2$ >>
  | <:expr< ( $list:el$ ) >> -> <:expr< ( $list:List.map (subst v) el$ ) >>
  | _ -> raise Not_found ]
and subst_pe v (p, e) =
  match p with
  [ <:patt< $lid:v'$ >> -> if v = v' then (p, e) else (p, subst v e)
  | _ -> raise Not_found ]
;

value stream_pattern_component skont ckont =
  fun
  [ SpTrm loc p wo ->
      <:expr< match $peek_fun loc$ $lid:strm_n$ with
              [ Some $p$ $when:wo$ ->
                  do { $junk_fun loc$ $lid:strm_n$; $skont$ }
              | _ -> $ckont$ ] >>
  | SpNtr loc p e ->
      let e =
        match e with
        [ <:expr< fun [ ($lid:v$ : Stream.t _) -> $e$ ] >> when v = strm_n ->
            e
        | _ -> <:expr< $e$ $lid:strm_n$ >> ]
      in
      if pattern_eq_expression p skont then
        if is_raise_failure ckont then e
        else if handle_failure e then e
        else <:expr< try $e$ with [ Stream.Failure -> $ckont$ ] >>
      else if is_raise_failure ckont then <:expr< let $p$ = $e$ in $skont$ >>
      else if pattern_eq_expression <:patt< Some $p$ >> skont then
        <:expr< try Some $e$ with [ Stream.Failure -> $ckont$ ] >>
      else if is_raise ckont then
        let tst =
          if handle_failure e then e
          else <:expr< try $e$ with [ Stream.Failure -> $ckont$ ] >>
        in
        <:expr< let $p$ = $tst$ in $skont$ >>
      else
        <:expr< match try Some $e$ with [ Stream.Failure -> None ] with
                [ Some $p$ -> $skont$
                | _ -> $ckont$ ] >>
  | SpStr loc p ->
      try
        match p with
        [ <:patt< $lid:v$ >> -> subst v skont
        | _ -> raise Not_found ]
      with
      [ Not_found -> <:expr< let $p$ = $lid:strm_n$ in $skont$ >> ] ]
;

value rec stream_pattern loc epo e ekont =
  fun
  [ [] ->
      match epo with
      [ Some ep -> <:expr< let $ep$ = Stream.count $lid:strm_n$ in $e$ >>
      | _ -> e ]
  | [(spc, err) :: spcl] ->
      let skont =
        let ekont err =
          let str =
            match err with
            [ Some estr -> estr
            | _ -> <:expr< "" >> ]
          in
          <:expr< raise (Stream.Error $str$) >>
        in
        stream_pattern loc epo e ekont spcl
      in
      let ckont = ekont err in
      stream_pattern_component skont ckont spc ]
;

value stream_patterns_term loc ekont tspel =
  let pel =
    List.map
      (fun (p, w, loc, spcl, epo, e) ->
         let p = <:patt< Some $p$ >> in
         let e =
           let ekont err =
             let str =
               match err with
               [ Some estr -> estr
               | _ -> <:expr< "" >> ]
             in
             <:expr< raise (Stream.Error $str$) >>
           in
           let skont = stream_pattern loc epo e ekont spcl in
           <:expr< do { $junk_fun loc$ $lid:strm_n$; $skont$ } >>
         in
         (p, w, e))
      tspel
  in
  let pel = pel @ [(<:patt< _ >>, None, ekont ())] in
  <:expr< match $peek_fun loc$ $lid:strm_n$ with [ $list:pel$ ] >>
;

value rec group_terms =
  fun
  [ [([(SpTrm loc p w, None) :: spcl], epo, e) :: spel] ->
      let (tspel, spel) = group_terms spel in
      ([(p, w, loc, spcl, epo, e) :: tspel], spel)
  | spel -> ([], spel) ]
;

value rec parser_cases loc =
  fun
  [ [] -> <:expr< raise Stream.Failure >>
  | spel ->
      match group_terms spel with
      [ ([], [(spcl, epo, e) :: spel]) ->
          stream_pattern loc epo e (fun _ -> parser_cases loc spel) spcl
      | (tspel, spel) ->
          stream_patterns_term loc (fun _ -> parser_cases loc spel) tspel ] ]
;

value cparser loc bpo pc =
  let e = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $e$ >>
    | None -> e ]
  in
  let p = <:patt< ($lid:strm_n$ : Stream.t _) >> in
  <:expr< fun $p$ -> $e$ >>
;

value cparser_match loc me bpo pc =
  let pc = parser_cases loc pc in
  let e =
    match bpo with
    [ Some bp -> <:expr< let $bp$ = Stream.count $lid:strm_n$ in $pc$ >>
    | None -> pc ]
  in
  <:expr< let $lid:strm_n$ = $me$ in $e$ >>
;

(* streams *)

value rec not_computing =
  fun
  [ <:expr< $lid:_$ >> | <:expr< $uid:_$ >> | <:expr< $int:_$ >> |
    <:expr< $flo:_$ >> | <:expr< $chr:_$ >> | <:expr< $str:_$ >> ->
      True
  | <:expr< $x$ $y$ >> -> is_cons_apply_not_computing x && not_computing y
  | _ -> False ]
and is_cons_apply_not_computing =
  fun
  [ <:expr< $uid:_$ >> -> True
  | <:expr< $lid:_$ >> -> False
  | <:expr< $x$ $y$ >> -> is_cons_apply_not_computing x && not_computing y
  | _ -> False ]
;

value slazy loc e =
  match e with
  [ <:expr< $f$ () >> ->
      match f with
      [ <:expr< $lid:_$ >> -> f
      | _ -> <:expr< fun _ -> $e$ >> ]
  | _ -> <:expr< fun _ -> $e$ >> ]
;

value rec cstream gloc =
  fun
  [ [] ->
      let loc = gloc in
      <:expr< Stream.sempty >>
  | [SeTrm loc e] ->
      if not_computing e then <:expr< Stream.ising $e$ >>
      else <:expr< Stream.lsing $slazy loc e$ >>
  | [SeTrm loc e :: secl] ->
      if not_computing e then <:expr< Stream.icons $e$ $cstream gloc secl$ >>
      else <:expr< Stream.lcons $slazy loc e$ $cstream gloc secl$ >>
  | [SeNtr loc e] ->
      if not_computing e then e else <:expr< Stream.slazy $slazy loc e$ >>
  | [SeNtr loc e :: secl] ->
      if not_computing e then <:expr< Stream.iapp $e$ $cstream gloc secl$ >>
      else <:expr< Stream.lapp $slazy loc e$ $cstream gloc secl$ >> ]
;

(* Syntax extensions in Ocaml grammar *)

EXTEND
  GLOBAL: expr;
  expr: LEVEL "expr1"
    [ [ "parser"; po = OPT ipatt; OPT "|"; pcl = LIST1 parser_case SEP "|" ->
          <:expr< $cparser loc po pcl$ >>
      | "match"; e = SELF; "with"; "parser"; po = OPT ipatt; OPT "|";
        pcl = LIST1 parser_case SEP "|" ->
          <:expr< $cparser_match loc e po pcl$ >> ] ]
  ;
  parser_case:
    [ [ "[<"; sp = stream_patt; ">]"; po = OPT ipatt; "->"; e = expr ->
          (sp, po, e) ] ]
  ;
  stream_patt:
    [ [ spc = stream_patt_comp -> [(spc, None)]
      | spc = stream_patt_comp; ";" -> [(spc, None)]
      | spc = stream_patt_comp; ";"; sp = stream_patt_comp_err_list ->
          [(spc, None) :: sp]
      | -> (* empty *) [] ] ]
  ;
  stream_patt_comp_err_list:
    [ [ spc = stream_patt_comp_err -> [spc]
      | spc = stream_patt_comp_err; ";" -> [spc]
      | spc = stream_patt_comp_err; ";"; sp = SELF -> [spc :: sp] ] ]
  ;
  stream_patt_comp:
    [ [ "'"; p = patt; eo = OPT [ "when"; e = expr LEVEL "expr1" -> e ] ->
          SpTrm loc p eo
      | p = patt; "="; e = expr LEVEL "expr1" -> SpNtr loc p e
      | p = patt -> SpStr loc p ] ]
  ;
  stream_patt_comp_err:
    [ [ spc = stream_patt_comp;
        eo = OPT [ "?"; e = expr LEVEL "expr1" -> e ] ->
          (spc, eo) ] ]
  ;
  ipatt:
    [ [ i = LIDENT -> <:patt< $lid:i$ >> ] ]
  ;
  expr: LEVEL "simple"
    [ [ "[<"; ">]" -> <:expr< $cstream loc []$ >>
      | "[<"; sel = stream_expr_comp_list; ">]" ->
          <:expr< $cstream loc sel$ >> ] ]
  ;
  stream_expr_comp_list:
    [ [ se = stream_expr_comp; ";"; sel = SELF -> [se :: sel]
      | se = stream_expr_comp; ";" -> [se]
      | se = stream_expr_comp -> [se] ] ]
  ;
  stream_expr_comp:
    [ [ "'"; e = expr LEVEL "expr1" -> SeTrm loc e
      | e = expr LEVEL "expr1" -> SeNtr loc e ] ]
  ;
END;
