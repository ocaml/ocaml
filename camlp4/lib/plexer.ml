(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Stdpp;
open Token;

value no_quotations = ref False;

(* The string buffering machinery *)

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
    if i == String.length s then len else add_rec (store len s.[i]) (succ i)
;
value get_buff len = String.sub buff.val 0 len;

(* The lexer *)

value stream_peek_nth n strm =
  loop n (Stream.npeek n strm) where rec loop n =
    fun
    [ [] -> None
    | [x] -> if n == 1 then Some x else None
    | [_ :: l] -> loop (n - 1) l ]
;

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
         '%' | '.' | ':' | '<' | '>' | '|' | '$' as
         c)
        ;
       s :] ->
      ident2 (store len c) s
  | [: :] -> len ]
and ident3 len =
  parser
  [ [: `('0'..'9' | 'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
         '\248'..'\255' | '_' | '!' | '%' | '&' | '*' | '+' | '-' | '.' |
         '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~' | ''' |
         '$' as
         c)
        ;
       s :] ->
      ident3 (store len c) s
  | [: :] -> len ]
and base_number len =
  parser
  [ [: `'o' | 'O'; s :] -> digits octal (store len 'o') s
  | [: `'x' | 'X'; s :] -> digits hexa (store len 'x') s
  | [: `'b' | 'B'; s :] -> digits binary (store len 'b') s
  | [: a = number len :] -> a ]
and digits kind len =
  parser
  [ [: d = kind; s :] -> digits_under kind (store len d) s
  | [: :] -> raise (Stream.Error "ill-formed integer constant") ]
and digits_under kind len =
  parser
  [ [: d = kind; s :] -> digits_under kind (store len d) s
  | [: `'_'; s :] -> digits_under kind len s
  | [: `'l' :] -> ("INT32", get_buff len)
  | [: `'L' :] -> ("INT64", get_buff len)
  | [: `'n' :] -> ("NATIVEINT", get_buff len)
  | [: :] -> ("INT", get_buff len) ]
and octal = parser [ [: `('0'..'7' as d) :] -> d ]
and hexa = parser [ [: `('0'..'9' | 'a'..'f' | 'A'..'F' as d) :] -> d ]
and binary = parser [ [: `('0'..'1' as d) :] -> d ]
and number len =
  parser
  [ [: `('0'..'9' as c); s :] -> number (store len c) s
  | [: `'_'; s :] -> number len s
  | [: `'.'; s :] -> decimal_part (store len '.') s
  | [: `'e' | 'E'; s :] -> exponent_part (store len 'E') s
  | [: `'l' :] -> ("INT32", get_buff len)
  | [: `'L' :] -> ("INT64", get_buff len)
  | [: `'n' :] -> ("NATIVEINT", get_buff len)
  | [: :] -> ("INT", get_buff len) ]
and decimal_part len =
  parser
  [ [: `('0'..'9' as c); s :] -> decimal_part (store len c) s
  | [: `'_'; s :] -> decimal_part len s
  | [: `'e' | 'E'; s :] -> exponent_part (store len 'E') s
  | [: :] -> ("FLOAT", get_buff len) ]
and exponent_part len =
  parser
  [ [: `('+' | '-' as c); s :] -> end_exponent_part (store len c) s
  | [: a = end_exponent_part len :] -> a ]
and end_exponent_part len =
  parser
  [ [: `('0'..'9' as c); s :] -> end_exponent_part_under (store len c) s
  | [: :] -> raise (Stream.Error "ill-formed floating-point constant") ]
and end_exponent_part_under len =
  parser
  [ [: `('0'..'9' as c); s :] -> end_exponent_part_under (store len c) s
  | [: `'_'; s :] -> end_exponent_part_under len s
  | [: :] -> ("FLOAT", get_buff len) ]
;

value error_on_unknown_keywords = ref False;
value err loc msg = raise_with_loc loc (Token.Error msg);

(*
value next_token_fun dfa find_kwd =
  let keyword_or_error loc s =
    try (("", find_kwd s), loc) with
    [ Not_found ->
        if error_on_unknown_keywords.val then err loc ("illegal token: " ^ s)
        else (("", s), loc) ]
  in
  let rec next_token =
    parser bp
    [ [: `' ' | '\010' | '\013' | '\t' | '\026' | '\012'; s :] ->
        next_token s
    | [: `'('; s :] -> left_paren bp s
    | [: `'#'; s :] -> do { spaces_tabs s; linenum bp s }
    | [: `('A'..'Z' | '\192'..'\214' | '\216'..'\222' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = (bp, Stream.count s) in
        (try ("", find_kwd id) with [ Not_found -> ("UIDENT", id) ], loc)
    | [: `('a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = (bp, Stream.count s) in
        (try ("", find_kwd id) with [ Not_found -> ("LIDENT", id) ], loc)
    | [: `('1'..'9' as c); s :] ->
        let tok = number (store 0 c) s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'0'; s :] ->
        let tok = base_number (store 0 '0') s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'''; s :] ->
        match Stream.npeek 3 s with
        [ [_; '''; _] | ['\\'; _; _] | ['\x0D'; '\x0A'; '''] ->
            let tok = ("CHAR", get_buff (char bp 0 s)) in
            let loc = (bp, Stream.count s) in
            (tok, loc)
        | _ -> keyword_or_error (bp, Stream.count s) "'" ]
    | [: `'"'; s :] ->
        let tok = ("STRING", get_buff (string bp 0 s)) in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'$'; s :] ->
        let tok = dollar bp 0 s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `('!' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' as c);
         s :] ->
        let id = get_buff (ident2 (store 0 c) s) in
        keyword_or_error (bp, Stream.count s) id
    | [: `('~' as c);
         a =
           parser
           [ [: `('a'..'z' as c); len = ident (store 0 c) :] ep ->
               (("TILDEIDENT", get_buff len), (bp, ep))
           | [: s :] ->
               let id = get_buff (ident2 (store 0 c) s) in
               keyword_or_error (bp, Stream.count s) id ] :] ->
        a
    | [: `('?' as c);
         a =
           parser
           [ [: `('a'..'z' as c); len = ident (store 0 c) :] ep ->
               (("QUESTIONIDENT", get_buff len), (bp, ep))
           | [: s :] ->
               let id = get_buff (ident2 (store 0 c) s) in
               keyword_or_error (bp, Stream.count s) id ] :] ->
        a
    | [: `'<'; s :] -> less bp s
    | [: `(':' as c1);
         len =
           parser
           [ [: `(']' | ':' | '=' | '>' as c2) :] -> store (store 0 c1) c2
           | [: :] -> store 0 c1 ] :] ep ->
        let id = get_buff len in
        keyword_or_error (bp, ep) id
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
    | [: `'.';
         id =
           parser
           [ [: `'.' :] -> ".."
           | [: :] -> if ssd && after_space then " ." else "." ] :] ep ->
        keyword_or_error (bp, ep) id
    | [: `';';
         id =
           parser
           [ [: `';' :] -> ";;"
           | [: :] -> ";" ] :] ep ->
        keyword_or_error (bp, ep) id
    | [: `'\\'; s :] ep -> (("LIDENT", get_buff (ident3 0 s)), (bp, ep))
    | [: `c :] ep -> keyword_or_error (bp, ep) (String.make 1 c)
    | [: _ = Stream.empty :] -> (("EOI", ""), (bp, succ bp)) ]
  and less bp strm =
    if no_quotations.val then
      match strm with parser
      [ [: len = ident2 (store 0 '<') :] ep ->
          let id = get_buff len in
          keyword_or_error (bp, ep) id ]
    else
      match strm with parser
      [ [: `'<'; len = quotation bp 0 :] ep ->
          (("QUOTATION", ":" ^ get_buff len), (bp, ep))
      | [: `':'; i = parser [: len = ident 0 :] -> get_buff len;
           `'<' ? "character '<' expected"; len = quotation bp 0 :] ep ->
          (("QUOTATION", i ^ ":" ^ get_buff len), (bp, ep))
      | [: len = ident2 (store 0 '<') :] ep ->
          let id = get_buff len in
          keyword_or_error (bp, ep) id ]
  and string bp len =
    parser
    [ [: `'"' :] -> len
    | [: `'\\'; `c; s :] -> string bp (store (store len '\\') c) s
    | [: `c; s :] -> string bp (store len c) s
    | [: :] ep -> err (bp, ep) "string not terminated" ]
  and char bp len =
    parser
    [ [: `'''; s :] -> if len = 0 then char bp (store len ''') s else len
    | [: `'\\'; `c; s :] -> char bp (store (store len '\\') c) s
    | [: `c; s :] -> char bp (store len c) s
    | [: :] ep -> err (bp, ep) "char not terminated" ]
  and dollar bp len =
    parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('a'..'z' | 'A'..'Z' as c); s :] -> antiquot bp (store len c) s
    | [: `('0'..'9' as c); s :] -> maybe_locate bp (store len c) s
    | [: `':'; s :] ->
        let k = get_buff len in
        ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
    | [: s :] ->
        if dfa then
          match s with parser
          [ [: `c :] ->
              ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
          | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
        else ("", get_buff (ident2 (store 0 '$') s)) ]
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
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
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
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
  and locate_or_antiquot_rest bp len =
    parser
    [ [: `'$' :] -> get_buff len
    | [: `'\\'; `c; s :] -> locate_or_antiquot_rest bp (store len c) s
    | [: `c; s :] -> locate_or_antiquot_rest bp (store len c) s
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
  and quotation bp len =
    parser
    [ [: `'>'; s :] -> maybe_end_quotation bp len s
    | [: `'<'; s :] ->
        quotation bp (maybe_nested_quotation bp (store len '<') s) s
    | [: `'\\';
         len =
           parser
           [ [: `('>' | '<' | '\\' as c) :] -> store len c
           | [: :] -> store len '\\' ];
         s :] ->
        quotation bp len s
    | [: `c; s :] -> quotation bp (store len c) s
    | [: :] ep -> err (bp, ep) "quotation not terminated" ]
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
  and left_paren bp =
    parser
    [ [: `'*'; _ = comment bp; a = next_token True :] -> a
    | [: :] ep -> keyword_or_error (bp, ep) "(" ]
  and comment bp =
    parser
    [ [: `'('; s :] -> left_paren_in_comment bp s
    | [: `'*'; s :] -> star_in_comment bp s
    | [: `'"'; _ = string bp 0; s :] -> comment bp s
    | [: `'''; s :] -> quote_in_comment bp s
    | [: `c; s :] -> comment bp s
    | [: :] ep -> err (bp, ep) "comment not terminated" ]
  and quote_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: `'\013'; s :] -> quote_cr_in_comment bp s
    | [: `'\\'; s :] -> quote_antislash_in_comment bp s
    | [: `'('; s :] -> quote_left_paren_in_comment bp s
    | [: `'*'; s :] -> quote_star_in_comment bp s
    | [: `'"'; s :] -> quote_doublequote_in_comment bp s
    | [: `_; s :] -> quote_any_in_comment bp s
    | [: s :] -> comment bp s ]
  and quote_any_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: s :] -> comment bp s ]
  and quote_cr_in_comment bp =
    parser
    [ [: `'\010'; s :] -> quote_any_in_comment bp s
    | [: s :] -> quote_any_in_comment bp s ]
  and quote_left_paren_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: s :] -> left_paren_in_comment bp s ]
  and quote_star_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: s :] -> star_in_comment bp s ]
  and quote_doublequote_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: _ = string bp 0; s :] -> comment bp s ]
  and quote_antislash_in_comment bp =
    parser
    [ [: `'''; s :] -> quote_antislash_quote_in_comment bp s
    | [: `('\\' | '"' | 'n' | 't' | 'b' | 'r'); s :] ->
        quote_any_in_comment bp s
    | [: `('0'..'9'); s :] -> quote_antislash_digit_in_comment bp s
    | [: `'x'; s :] -> quote_antislash_x_in_comment bp s
    | [: s :] -> comment bp s ]
  and quote_antislash_quote_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: s :] -> quote_in_comment bp s ]
  and quote_antislash_digit_in_comment bp =
    parser
    [ [: `('0'..'9'); s :] -> quote_antislash_digit2_in_comment bp s
    | [: s :] -> comment bp s ]
  and quote_antislash_digit2_in_comment bp =
    parser
    [ [: `('0'..'9'); s :] -> quote_any_in_comment bp s
    | [: s :] -> comment bp s ]
  and quote_antislash_x_in_comment bp =
    parser
    [ [: _ = hexa; s :] -> quote_antislash_x_digit_in_comment bp s
    | [: s :] -> comment bp s ]
  and quote_antislash_x_digit_in_comment bp =
    parser
    [ [: _ = hexa; s :] -> quote_any_in_comment bp s
    | [: s :] -> comment bp s ]
  and left_paren_in_comment bp =
    parser
    [ [: `'*'; s :] -> do { comment bp s; comment bp s }
    | [: a = comment bp :] -> a ]
  and star_in_comment bp =
    parser
    [ [: `')' :] -> ()
    | [: a = comment bp :] -> a ]
  and linedir n s =
    match stream_peek_nth n s with
    [ Some (' ' | '\t') -> linedir (n + 1) s
    | Some ('0'..'9') -> linedir_digits (n + 1) s
    | _ -> False ]
  and linedir_digits n s =
    match stream_peek_nth n s with
    [ Some ('0'..'9') -> linedir_digits (n + 1) s
    | _ -> linedir_quote n s ]
  and linedir_quote n s =
    match stream_peek_nth n s with
    [ Some (' ' | '\t') -> linedir_quote (n + 1) s
    | Some '"' -> True
    | _ -> False ]
  and any_to_nl =
    parser
    [ [: `'\013' | '\010' :] ep -> bolpos.val := ep
    | [: `_; s :] -> any_to_nl s
    | [: :] -> () ]
  in
  fun cstrm ->
    try
      let glex = glexr.val in
      let comm_bp = Stream.count cstrm in
      let r = next_token False cstrm in
      do {
        match glex.tok_comm with
        [ Some list ->
            if fst (snd r) > comm_bp then
              let comm_loc = (comm_bp, fst (snd r)) in
              glex.tok_comm := Some [comm_loc :: list]
            else ()
        | None -> () ];
        r
      }
    with
    [ Stream.Error str ->
        err (Stream.count cstrm, Stream.count cstrm + 1) str ]
;
*)

value next_token_fun dfa ssd find_kwd bolpos glexr =
  let keyword_or_error loc s =
    try (("", find_kwd s), loc) with
    [ Not_found ->
        if error_on_unknown_keywords.val then err loc ("illegal token: " ^ s)
        else (("", s), loc) ] in
  let error_if_keyword ( ((_,id), loc) as a) =
    try do {
      ignore(find_kwd id);
      err loc ("illegal use of a keyword as a label: " ^ id) }
    with [ Not_found -> a ]
  in
  let rec next_token after_space =
    parser bp
    [ [: `'\010' | '\013'; s :] ep ->
        do { bolpos.val := ep; next_token True s }
    | [: `' ' | '\t' | '\026' | '\012'; s :] -> next_token True s
    | [: `'#' when bp = bolpos.val; s :] ->
        if linedir 1 s then do { any_to_nl s; next_token True s }
        else keyword_or_error (bp, bp + 1) "#"
    | [: `'('; s :] -> left_paren bp s
    | [: `('A'..'Z' | '\192'..'\214' | '\216'..'\222' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = (bp, Stream.count s) in
        (try ("", find_kwd id) with [ Not_found -> ("UIDENT", id) ], loc)
    | [: `('a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = (bp, Stream.count s) in
        (try ("", find_kwd id) with [ Not_found -> ("LIDENT", id) ], loc)
    | [: `('1'..'9' as c); s :] ->
        let tok = number (store 0 c) s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'0'; s :] ->
        let tok = base_number (store 0 '0') s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'''; s :] ->
        match Stream.npeek 2 s with
        [ [_; '''] | ['\\'; _] ->
            let tok = ("CHAR", get_buff (char bp 0 s)) in
            let loc = (bp, Stream.count s) in
            (tok, loc)
        | _ -> keyword_or_error (bp, Stream.count s) "'" ]
    | [: `'"'; s :] ->
        let tok = ("STRING", get_buff (string bp 0 s)) in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `'$'; s :] ->
        let tok = dollar bp 0 s in
        let loc = (bp, Stream.count s) in
        (tok, loc)
    | [: `('!' | '=' | '@' | '^' | '&' | '+' | '-' | '*' | '/' | '%' as c);
         s :] ->
        let id = get_buff (ident2 (store 0 c) s) in
        keyword_or_error (bp, Stream.count s) id
    | [: `('~' as c);
         a =
           parser
           [ [: `('a'..'z' as c); len = ident (store 0 c); s :] ep ->
               let id = get_buff len in
               match s with parser
               [ [: `':' :] eb -> error_if_keyword (("LABEL", id), (bp,ep))
               | [: :] -> error_if_keyword (("TILDEIDENT", id), (bp, ep)) ]
           | [: s :] ->
               let id = get_buff (ident2 (store 0 c) s) in
               keyword_or_error (bp, Stream.count s) id ] :] ->
        a
          
    | [: `('?' as c);
         a =
           parser
           [ [: `('a'..'z' as c); len = ident (store 0 c); s :] ep ->
               let id = get_buff len in
               match s with parser
               [ [: `':' :] eb -> error_if_keyword (("OPTLABEL", id), (bp,ep))
               | [: :] -> error_if_keyword (("QUESTIONIDENT", id), (bp, ep)) ]
           | [: s :] ->
               let id = get_buff (ident2 (store 0 c) s) in
               keyword_or_error (bp, Stream.count s) id ] :] ->
        a
    | [: `'<'; s :] -> less bp s
    | [: `(':' as c1);
         len =
           parser
           [ [: `(']' | ':' | '=' | '>' as c2) :] -> store (store 0 c1) c2
           | [: :] -> store 0 c1 ] :] ep ->
        let id = get_buff len in
        keyword_or_error (bp, ep) id
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
    | [: `'.';
         id =
           parser
           [ [: `'.' :] -> ".."
           | [: :] -> if ssd && after_space then " ." else "." ] :] ep ->
        keyword_or_error (bp, ep) id
    | [: `';';
         id =
           parser
           [ [: `';' :] -> ";;"
           | [: :] -> ";" ] :] ep ->
        keyword_or_error (bp, ep) id
    | [: `'\\'; s :] ep -> (("LIDENT", get_buff (ident3 0 s)), (bp, ep))
    | [: `c :] ep -> keyword_or_error (bp, ep) (String.make 1 c)
    | [: _ = Stream.empty :] -> (("EOI", ""), (bp, succ bp)) ]
  and less bp strm =
    if no_quotations.val then
      match strm with parser
      [ [: len = ident2 (store 0 '<') :] ep ->
          let id = get_buff len in
          keyword_or_error (bp, ep) id ]
    else
      match strm with parser
      [ [: `'<'; len = quotation bp 0 :] ep ->
          (("QUOTATION", ":" ^ get_buff len), (bp, ep))
      | [: `':'; i = parser [: len = ident 0 :] -> get_buff len;
           `'<' ? "character '<' expected"; len = quotation bp 0 :] ep ->
          (("QUOTATION", i ^ ":" ^ get_buff len), (bp, ep))
      | [: len = ident2 (store 0 '<') :] ep ->
          let id = get_buff len in
          keyword_or_error (bp, ep) id ]
  and string bp len =
    parser
    [ [: `'"' :] -> len
    | [: `'\\'; `c; s :] ep  -> string bp (store (store len '\\') c) s
    | [: `c; s :] -> string bp (store len c) s
    | [: :] ep -> err (bp, ep) "string not terminated" ]
  and char bp len =
    parser
    [ [: `'''; s :] -> if len = 0 then char bp (store len ''') s else len
    | [: `'\\'; `c; s :] -> char bp (store (store len '\\') c) s
    | [: `c; s :] -> char bp (store len c) s
    | [: :] ep -> err (bp, ep) "char not terminated" ]
  and dollar bp len =
    parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('a'..'z' | 'A'..'Z' as c); s :] -> antiquot bp (store len c) s
    | [: `('0'..'9' as c); s :] -> maybe_locate bp (store len c) s
    | [: `':'; s :] ->
        let k = get_buff len in
        ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bp 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
    | [: s :] ->
        if dfa then
          match s with parser
          [ [: `c :] ->
              ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bp (store len c) s)
          | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
        else ("", get_buff (ident2 (store 0 '$') s)) ]
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
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
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
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
  and locate_or_antiquot_rest bp len =
    parser
    [ [: `'$' :] -> get_buff len
    | [: `'\\'; `c; s :] -> locate_or_antiquot_rest bp (store len c) s
    | [: `c; s :] -> locate_or_antiquot_rest bp (store len c) s
    | [: :] ep -> err (bp, ep) "antiquotation not terminated" ]
  and quotation bp len =
    parser
    [ [: `'>'; s :] -> maybe_end_quotation bp len s
    | [: `'<'; s :] ->
        quotation bp (maybe_nested_quotation bp (store len '<') s) s
    | [: `'\\';
         len =
           parser
           [ [: `('>' | '<' | '\\' as c) :] -> store len c
           | [: :] -> store len '\\' ];
         s :] ->
        quotation bp len s
    | [: `c; s :] -> quotation bp (store len c) s
    | [: :] ep -> err (bp, ep) "quotation not terminated" ]
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
  and left_paren bp =
    parser
    [ [: `'*'; _ = comment bp; a = next_token True :] -> a
    | [: :] ep -> keyword_or_error (bp, ep) "(" ]
  and comment bp =
    parser
    [ [: `'('; s :] -> left_paren_in_comment bp s
    | [: `'*'; s :] -> star_in_comment bp s
    | [: `'"'; _ = string bp 0; s :] -> comment bp s
    | [: `'''; s :] -> quote_in_comment bp s
    | [: `c; s :] -> comment bp s
    | [: :] ep -> err (bp, ep) "comment not terminated" ]
  and quote_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: `'\\'; s :] -> quote_antislash_in_comment bp 0 s
    | [: s :] ->
        do {
          match Stream.npeek 2 s with
          [ [_; '''] -> do { Stream.junk s; Stream.junk s }
          | _ -> () ];
          comment bp s
        } ]
  and quote_any_in_comment bp =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: a = comment bp :] -> a ]
  and quote_antislash_in_comment bp len =
    parser
    [ [: `'''; s :] -> comment bp s
    | [: `'\\' | '"' | 'n' | 't' | 'b' | 'r'; s :] ->
        quote_any_in_comment bp s
    | [: `'0'..'9'; s :] -> quote_antislash_digit_in_comment bp s
    | [: a = comment bp :] -> a ]
  and quote_antislash_digit_in_comment bp =
    parser
    [ [: `'0'..'9'; s :] -> quote_antislash_digit2_in_comment bp s
    | [: a = comment bp :] -> a ]
  and quote_antislash_digit2_in_comment bp =
    parser
    [ [: `'0'..'9'; s :] -> quote_any_in_comment bp s
    | [: a = comment bp :] -> a ]
  and left_paren_in_comment bp =
    parser
    [ [: `'*'; s :] -> do { comment bp s; comment bp s }
    | [: a = comment bp :] -> a ]
  and star_in_comment bp =
    parser
    [ [: `')' :] -> ()
    | [: a = comment bp :] -> a ]
  and linedir n s =
    match stream_peek_nth n s with
    [ Some (' ' | '\t') -> linedir (n + 1) s
    | Some ('0'..'9') -> linedir_digits (n + 1) s
    | _ -> False ]
  and linedir_digits n s =
    match stream_peek_nth n s with
    [ Some ('0'..'9') -> linedir_digits (n + 1) s
    | _ -> linedir_quote n s ]
  and linedir_quote n s =
    match stream_peek_nth n s with
    [ Some (' ' | '\t') -> linedir_quote (n + 1) s
    | Some '"' -> True
    | _ -> False ]
  and any_to_nl =
    parser
    [ [: `'\013' | '\010' :] ep -> bolpos.val := ep
    | [: `_; s :] -> any_to_nl s
    | [: :] -> () ]
  in
  fun cstrm ->
    try
      let glex = glexr.val in
      let comm_bp = Stream.count cstrm in
      let r = next_token False cstrm in
      do {
        match glex.tok_comm with
        [ Some list ->
            if fst (snd r) > comm_bp then
              let comm_loc = (comm_bp, fst (snd r)) in
              glex.tok_comm := Some [comm_loc :: list]
            else ()
        | None -> () ];
        r
      }
    with
    [ Stream.Error str ->
        err (Stream.count cstrm, Stream.count cstrm + 1) str ]
;


value dollar_for_antiquotation = ref True;
value specific_space_dot = ref False;

value func kwd_table glexr =
  let bolpos = ref 0 in
  let find = Hashtbl.find kwd_table in
  let dfa = dollar_for_antiquotation.val in
  let ssd = specific_space_dot.val in
  Token.lexer_func_of_parser (next_token_fun dfa ssd find bolpos glexr)
;

value rec check_keyword_stream =
  parser [: _ = check; _ = Stream.empty :] -> True
and check =
  parser
  [ [: `'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' | '\248'..'\255'
        ;
       s :] ->
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
          [ [: `'|' | '<' | ':' :] -> ()
          | [: :] -> () ] ]
  | [: `';';
       _ =
         parser
         [ [: `';' :] -> ()
         | [: :] -> () ] :] ->
      ()
  | [: `_ :] -> () ]
and check_ident =
  parser
  [ [: `'A'..'Z' | 'a'..'z' | '\192'..'\214' | '\216'..'\246' |
        '\248'..'\255' | '0'..'9' | '_' | '''
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

value error_no_respect_rules p_con p_prm =
  raise
    (Token.Error
       ("the token " ^
          (if p_con = "" then "\"" ^ p_prm ^ "\""
           else if p_prm = "" then p_con
           else p_con ^ " \"" ^ p_prm ^ "\"") ^
          " does not respect Plexer rules"))
;

value error_ident_and_keyword p_con p_prm =
  raise
    (Token.Error
       ("the token \"" ^ p_prm ^ "\" is used as " ^ p_con ^
          " and as keyword"))
;

value using_token kwd_table ident_table (p_con, p_prm) =
  match p_con with
  [ "" ->
      if not (Hashtbl.mem kwd_table p_prm) then
        if check_keyword p_prm then
          if Hashtbl.mem ident_table p_prm then
            error_ident_and_keyword (Hashtbl.find ident_table p_prm) p_prm
          else Hashtbl.add kwd_table p_prm p_prm
        else error_no_respect_rules p_con p_prm
      else ()
  | "LIDENT" ->
      if p_prm = "" then ()
      else
        match p_prm.[0] with
        [ 'A'..'Z' -> error_no_respect_rules p_con p_prm
        | _ ->
            if Hashtbl.mem kwd_table p_prm then
              error_ident_and_keyword p_con p_prm
            else Hashtbl.add ident_table p_prm p_con ]
  | "UIDENT" ->
      if p_prm = "" then ()
      else
        match p_prm.[0] with
        [ 'a'..'z' -> error_no_respect_rules p_con p_prm
        | _ ->
            if Hashtbl.mem kwd_table p_prm then
              error_ident_and_keyword p_con p_prm
            else Hashtbl.add ident_table p_prm p_con ]
  | "INT" | "INT32" | "INT64" | "NATIVEINT"
  | "FLOAT" | "CHAR" | "STRING"
  | "TILDEIDENT" | "QUESTIONIDENT" | "LABEL" | "OPTLABEL"
  | "QUOTATION" | "ANTIQUOT" | "LOCATE" | "EOI" ->
      ()
  | _ ->
      raise
        (Token.Error
           ("the constructor \"" ^ p_con ^
              "\" is not recognized by Plexer")) ]
;

value removing_token kwd_table ident_table (p_con, p_prm) =
  match p_con with
  [ "" -> Hashtbl.remove kwd_table p_prm
  | "LIDENT" | "UIDENT" ->
      if p_prm <> "" then Hashtbl.remove ident_table p_prm else ()
  | _ -> () ]
;

value text =
  fun
  [ ("", t) -> "'" ^ t ^ "'"
  | ("LIDENT", "") -> "lowercase identifier"
  | ("LIDENT", t) -> "'" ^ t ^ "'"
  | ("UIDENT", "") -> "uppercase identifier"
  | ("UIDENT", t) -> "'" ^ t ^ "'"
  | ("INT", "") -> "integer"
  | ("INT32", "") -> "32 bits integer"
  | ("INT64", "") -> "64 bits integer"
  | ("NATIVEINT", "") -> "native integer"
  | (("INT" | "INT32" | "NATIVEINT"), s) -> "'" ^ s ^ "'"
  | ("FLOAT", "") -> "float"
  | ("STRING", "") -> "string"
  | ("CHAR", "") -> "char"
  | ("QUOTATION", "") -> "quotation"
  | ("ANTIQUOT", k) -> "antiquot \"" ^ k ^ "\""
  | ("LOCATE", "") -> "locate"
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

value tok_match =
  fun
  [ ("ANTIQUOT", p_prm) ->
      fun
      [ ("ANTIQUOT", prm) when eq_before_colon p_prm prm -> after_colon prm
      | _ -> raise Stream.Failure ]
  | tok -> Token.default_match tok ]
;

value gmake () =
  let kwd_table = Hashtbl.create 301 in
  let id_table = Hashtbl.create 301 in
  let glexr =
    ref
     {tok_func = fun []; tok_using = fun []; tok_removing = fun [];
      tok_match = fun []; tok_text = fun []; tok_comm = None}
  in
  let glex =
    {tok_func = func kwd_table glexr;
     tok_using = using_token kwd_table id_table;
     tok_removing = removing_token kwd_table id_table; tok_match = tok_match;
     tok_text = text; tok_comm = None}
  in
  do { glexr.val := glex; glex }
;

value tparse =
  fun
  [ ("ANTIQUOT", p_prm) ->
      let p =
        parser
          [: `("ANTIQUOT", prm) when eq_before_colon p_prm prm :] ->
            after_colon prm
      in
      Some p
  | _ -> None ]
;

value make () =
  let kwd_table = Hashtbl.create 301 in
  let id_table = Hashtbl.create 301 in
  let glexr =
    ref
     {tok_func = fun []; tok_using = fun []; tok_removing = fun [];
      tok_match = fun []; tok_text = fun []; tok_comm = None}
  in
  {func = func kwd_table glexr; using = using_token kwd_table id_table;
   removing = removing_token kwd_table id_table; tparse = tparse; text = text}
;
