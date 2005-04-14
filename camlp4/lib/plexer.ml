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

(* Debugging positions and locations *)
value eprint_pos msg p =
   Printf.eprintf "%s: fname=%s; lnum=%d; bol=%d; cnum=%d%!"
     msg p.Lexing.pos_fname p.Lexing.pos_lnum p.Lexing.pos_bol p.Lexing.pos_cnum
;

value eprint_loc (bp, ep) =
 do { eprint_pos "P1=" bp; eprint_pos " --P2=" ep }
;
   
value check_location msg ((bp, ep) as loc) =
   let ok =
     if (bp.Lexing.pos_lnum > ep.Lexing.pos_lnum ||
         bp.Lexing.pos_bol > ep.Lexing.pos_bol ||
         bp.Lexing.pos_cnum > ep.Lexing.pos_cnum ||
         bp.Lexing.pos_lnum < 0 || ep.Lexing.pos_lnum < 0 ||
         bp.Lexing.pos_bol < 0 || ep.Lexing.pos_bol < 0 ||
         bp.Lexing.pos_cnum < 0 ||  ep.Lexing.pos_cnum < 0)
         (* Here, we don't check
            bp.Lexing.pos_cnum < bp.Lexing.pos_bol || ep.Lexing.pos_cnum < bp.Lexing.pos_bol
            since the lexer is called on antiquotations, with cnum=0, but lnum and bolpos
            have "correct" values *)
     then
       do {
         Printf.eprintf "*** Warning: (%s) strange positions ***\n" msg;
         eprint_loc loc;
         False
          }
     else
       True in
   (ok, loc)
;

value debug_token ((kind, tok), loc) = do {
   Printf.eprintf "%s(%s) at " kind tok;
   eprint_loc loc;
   Printf.eprintf "\n%!"
};

value next_token_fun dfa ssd find_kwd fname lnum bolpos glexr =
   let make_pos p =
     {Lexing.pos_fname = fname.val; Lexing.pos_lnum = lnum.val;
      Lexing.pos_bol = bolpos.val; Lexing.pos_cnum = p} in
   let mkloc (bp, ep) = (make_pos bp, make_pos ep) in
   let keyword_or_error (bp,ep) s =
    let loc = mkloc (bp, ep) in
      try (("", find_kwd s), loc) with
      [ Not_found ->
        if error_on_unknown_keywords.val then err loc ("illegal token: " ^ s)
        else (("", s), loc) ] in
  let error_if_keyword ( ((_,id) as a), bep) =
    let loc = mkloc bep in
    try do {
      ignore(find_kwd id);
      err loc ("illegal use of a keyword as a label: " ^ id) }
    with [ Not_found -> (a, loc) ]
  in
  let rec next_token after_space =
    parser bp
    [ [: `'\010'; s :] ep ->
        do { bolpos.val := ep; incr lnum; next_token True s }
    | [: `'\013'; s :] ep ->
        let ep =
          match Stream.peek s with
            [ Some '\010' -> do { Stream.junk s; ep+1 }
            | _ -> ep ] in
        do { bolpos.val := ep; incr lnum; next_token True s }
    | [: `' ' | '\t' | '\026' | '\012'; s :] -> next_token True s
    | [: `'#' when bp = bolpos.val; s :] ->
        if linedir 1 s then do { line_directive s; next_token True s }
        else keyword_or_error (bp, bp + 1) "#"
    | [: `'('; s :] -> left_paren bp s
    | [: `('A'..'Z' | '\192'..'\214' | '\216'..'\222' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = mkloc (bp, (Stream.count s)) in
        (try ("", find_kwd id) with [ Not_found -> ("UIDENT", id) ], loc)
    | [: `('a'..'z' | '\223'..'\246' | '\248'..'\255' | '_' as c); s :] ->
        let id = get_buff (ident (store 0 c) s) in
        let loc = mkloc (bp, (Stream.count s)) in
        (try ("", find_kwd id) with [ Not_found -> ("LIDENT", id) ], loc)
    | [: `('1'..'9' as c); s :] ->
        let tok = number (store 0 c) s in
        let loc = mkloc (bp, (Stream.count s)) in
        (tok, loc)
    | [: `'0'; s :] ->
        let tok = base_number (store 0 '0') s in
        let loc = mkloc (bp, (Stream.count s)) in
        (tok, loc)
    | [: `'''; s :] ->
        match Stream.npeek 2 s with
        [ [_; '''] | ['\\'; _] ->
            let tok = ("CHAR", get_buff (char bp 0 s)) in
            let loc = mkloc (bp, (Stream.count s)) in
            (tok, loc)
        | _ -> keyword_or_error (bp, Stream.count s) "'" ]
    | [: `'"'; s :] ->
        let bpos = make_pos bp in
        let tok = ("STRING", get_buff (string bpos 0 s)) in
        let loc = mkloc (bp, Stream.count s) in
        (tok, loc)
    | [: `'$'; s :] ->
        let bpos = make_pos bp in
        let tok = dollar bpos 0 s in
        let loc = (bpos, make_pos (Stream.count s)) in
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
               [ [: `':' :] eb -> error_if_keyword (("LABEL", id), (bp, ep))
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
    | [: `'\\'; s :] ep -> (("LIDENT", get_buff (ident3 0 s)), mkloc (bp, ep))
    | [: `c :] ep -> keyword_or_error (bp, ep) (String.make 1 c)
    | [: _ = Stream.empty :] -> (("EOI", ""), mkloc (bp, succ bp)) ]
  and less bp strm =
    if no_quotations.val then
      match strm with parser
      [ [: len = ident2 (store 0 '<') :] ep ->
          let id = get_buff len in
          keyword_or_error (bp, ep) id ]
    else
      let bpos = make_pos bp in
      match strm with parser
      [ [: `'<'; len = quotation bpos 0 :] ep ->
          (("QUOTATION", ":" ^ get_buff len), (bpos, make_pos ep))
      | [: `':'; i = parser [: len = ident 0 :] -> get_buff len;
           `'<' ? "character '<' expected"; len = quotation bpos 0 :] ep ->
          (("QUOTATION", i ^ ":" ^ get_buff len), (bpos, make_pos ep))
      | [: len = ident2 (store 0 '<') :] ep ->
          let id = get_buff len in
          keyword_or_error (bp, ep) id ]
  and string bpos len =
    parser
    [ [: `'"' :] -> len
    | [: `'\\'; `c; s :] ep  ->
        let len = store len '\\' in
        match c with [
          '\010' -> do { bolpos.val := ep; incr lnum; string bpos (store len c) s }
        | '\013' ->
            let (len, ep) =
              match Stream.peek s with [
                Some '\010' -> do { Stream.junk s; (store (store len '\013') '\010', ep+1) }
              | _ -> (store len '\013', ep) ] in
            do { bolpos.val := ep; incr lnum; string bpos len s }
        | c -> string bpos (store len c) s
        ]
    | [: `'\010'; s :] ep -> do { bolpos.val := ep; incr lnum; string bpos (store len '\010') s }
    | [: `'\013'; s :] ep ->
        let (len, ep) =
          match Stream.peek s with
            [ Some '\010' -> do { Stream.junk s; (store (store len '\013') '\010', ep+1) }
            | _ -> (store len '\013', ep) ] in
        do { bolpos.val := ep; incr lnum; string bpos len s }
    | [: `c; s :] -> string bpos (store len c) s
    | [: :] ep -> err (bpos, make_pos ep) "string not terminated" ]
  and char bp len =
    parser
    [ [: `'''; s :] -> if len = 0 then char bp (store len ''') s else len
    | [: `'\\'; `c; s :] -> char bp (store (store len '\\') c) s
    | [: `'\010'; s :] -> do {bolpos.val := bp+1; incr lnum; char bp (store len '\010') s}
    | [: `'\013'; s :] ->
        let bol =
          match Stream.peek s with
            [ Some '\010' -> do { Stream.junk s; bp+2 }
            | _ -> bp+1 ] in
        do { bolpos.val := bol; incr lnum; char bp (store len '\013') s}
    | [: `c; s :] -> char bp (store len c) s
    | [: :] ep -> err (mkloc (bp, ep)) "char not terminated" ]
  and dollar bpos len s =
    if no_quotations.val then
      ("", get_buff (ident2 (store 0 '$') s)) 
    else match s with parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('a'..'z' | 'A'..'Z' as c); s :] -> antiquot bpos (store len c) s
    | [: `('0'..'9' as c); s :] -> maybe_locate bpos (store len c) s
    | [: `':'; s :] ->
        let k = get_buff len in
        ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bpos 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bpos (store len c) s)
    | [: s :] ->
        if dfa then
          match s with parser
          [ [: `c :] ->
              ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bpos (store len c) s)
          | [: :] ep -> err (bpos, make_pos ep) "antiquotation not terminated" ]
        else ("", get_buff (ident2 (store 0 '$') s)) ]
  and maybe_locate bpos len =
    parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('0'..'9' as c); s :] -> maybe_locate bpos (store len c) s
    | [: `':'; s :] ->
        ("LOCATE", get_buff len ^ ":" ^ locate_or_antiquot_rest bpos 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bpos (store len c) s)
    | [: `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bpos (store len c) s)
    | [: :] ep -> err (bpos, make_pos ep) "antiquotation not terminated" ]
  and antiquot bpos len =
    parser
    [ [: `'$' :] -> ("ANTIQUOT", ":" ^ get_buff len)
    | [: `('a'..'z' | 'A'..'Z' | '0'..'9' as c); s :] ->
        antiquot bpos (store len c) s
    | [: `':'; s :] ->
        let k = get_buff len in
        ("ANTIQUOT", k ^ ":" ^ locate_or_antiquot_rest bpos 0 s)
    | [: `'\\'; `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bpos (store len c) s)
    | [: `c; s :] ->
        ("ANTIQUOT", ":" ^ locate_or_antiquot_rest bpos (store len c) s)
    | [: :] ep -> err (bpos, make_pos ep) "antiquotation not terminated" ]
  and locate_or_antiquot_rest bpos len =
    parser
    [ [: `'$' :] -> get_buff len
    | [: `'\\'; `c; s :] -> locate_or_antiquot_rest bpos (store len c) s
    | [: `c; s :] -> locate_or_antiquot_rest bpos (store len c) s
    | [: :] ep -> err (bpos, make_pos ep) "antiquotation not terminated" ]
  and quotation bpos len =
    parser
    [ [: `'>'; s :] -> maybe_end_quotation bpos len s
    | [: `'<'; s :] ->
        quotation bpos (maybe_nested_quotation bpos (store len '<') s) s
    | [: `'\\';
         len =
           parser
           [ [: `('>' | '<' | '\\' as c) :] -> store len c
           | [: :] -> store len '\\' ];
         s :] ->
        quotation bpos len s
    | [: `'\010'; s :] ep -> do {bolpos.val := ep; incr lnum; quotation bpos (store len '\010') s}
    | [: `'\013'; s :] ep ->
        let bol =
          match Stream.peek s with
            [ Some '\010' -> do { Stream.junk s; ep+1 }
            | _ -> ep ] in
        do { bolpos.val := bol; incr lnum; quotation bpos (store len '\013') s}
    | [: `c; s :] -> quotation bpos (store len c) s
    | [: :] ep -> err (bpos, make_pos ep) "quotation not terminated" ]
  and maybe_nested_quotation bpos len =
    parser
    [ [: `'<'; s :] -> mstore (quotation bpos (store len '<') s) ">>"
    | [: `':'; len = ident (store len ':');
         a =
           parser
           [ [: `'<'; s :] -> mstore (quotation bpos (store len '<') s) ">>"
           | [: :] -> len ] :] ->
        a
    | [: :] -> len ]
  and maybe_end_quotation bpos len =
    parser
    [ [: `'>' :] -> len
    | [: a = quotation bpos (store len '>') :] -> a ]
  and left_paren bp =
    parser
    [ [: `'*'; _ = comment (make_pos bp); a = next_token True :] -> a
    | [: :] ep -> keyword_or_error (bp, ep) "(" ]
  and comment bpos =
    parser
    [ [: `'('; s :] -> left_paren_in_comment bpos s
    | [: `'*'; s :] -> star_in_comment bpos s
    | [: `'"'; _ = string bpos 0; s :] -> comment bpos s
    | [: `'''; s :] -> quote_in_comment bpos s
    | [: `'\010'; s :] ep -> do { bolpos.val := ep; incr lnum; comment bpos s }
    | [: `'\013'; s :] ep ->
        let ep =
          match Stream.peek s with
            [ Some '\010' -> do { Stream.junk s; ep+1 }
            | _ -> ep ] in
        do { bolpos.val := ep; incr lnum; comment bpos s }
                    | [: `c; s :] -> comment bpos s
    | [: :] ep -> err (bpos, make_pos ep) "comment not terminated" ]
  and quote_in_comment bpos =
    parser
    [ [: `'''; s :] -> comment bpos s
    | [: `'\\'; s :] -> quote_antislash_in_comment bpos 0 s
    | [: s :] ep ->
        do {
          match Stream.npeek 2 s with
          [ [ ( '\013' | '\010' ); '''] ->
            do { bolpos.val := ep; incr lnum;
                 Stream.junk s; Stream.junk s }
          | [ '\013'; '\010' ] ->
               match Stream.npeek 3 s with
                 [ [_; _; '''] -> do { bolpos.val := ep + 1; incr lnum;
                                       Stream.junk s; Stream.junk s; Stream.junk s }
                 | _ -> () ]
          | [_; '''] -> do { Stream.junk s; Stream.junk s }
          | _ -> () ];
          comment bpos s
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
  and left_paren_in_comment bpos =
    parser
    [ [: `'*'; s :] -> do { comment bpos s; comment bpos s }
    | [: a = comment bpos :] -> a ]
  and star_in_comment bpos =
    parser
    [ [: `')' :] -> ()
    | [: a = comment bpos :] -> a ]
  and linedir n s =
    match stream_peek_nth n s with
    [ Some (' ' | '\t') -> linedir (n + 1) s
    | Some ('0'..'9') -> True
    | _ -> False ]
  and any_to_nl =
    parser
    [ [: `'\010'; s :] ep ->
        do { bolpos.val := ep; incr lnum }
    | [: `'\013'; s :] ep ->
        let ep =
          match Stream.peek s with
            [ Some '\010' -> do { Stream.junk s; ep+1 }
            | _ -> ep ] in
        do { bolpos.val := ep; incr lnum }
    | [: `_; s :] -> any_to_nl s
    | [: :] -> () ]
  and line_directive = parser (* we are sure that there is a line directive here *)
    [ [: _ = skip_spaces; n = line_directive_number 0;
         _ = skip_spaces; _ = line_directive_string;
         _ = any_to_nl :] ep
       -> do { bolpos.val := ep; lnum.val := n }
    ]
  and skip_spaces = parser
    [ [: `' ' | '\t'; s :] -> skip_spaces s
    | [: :] -> () ]
  and line_directive_number n = parser
    [ [: `('0'..'9' as c) ; s :]
      -> line_directive_number (10*n + (Char.code c - Char.code '0')) s
    | [: :] -> n ]
  and line_directive_string = parser
    [ [: ` '"' ; _ = line_directive_string_contents 0 :] -> ()
    | [: :] -> ()
   ]
  and line_directive_string_contents len = parser
    [ [: ` '\010' | '\013' :]  -> ()
    | [: ` '"' :] -> fname.val := get_buff len
    | [: `c; s :] -> line_directive_string_contents (store len c) s
    ]
  in
  fun cstrm ->
    try
      let glex = glexr.val in
      let comm_bp = Stream.count cstrm in
      let r = next_token False cstrm in
      do {
        match glex.tok_comm with
        [ Some list ->
            let next_bp = (fst (snd r)).Lexing.pos_cnum in
            if next_bp > comm_bp then
              let comm_loc = mkloc (comm_bp, next_bp) in
              glex.tok_comm := Some [comm_loc :: list]
            else ()
        | None -> () ];
        (* debug_token r; *)
        r
      }
    with
    [ Stream.Error str ->
        err (mkloc (Stream.count cstrm, Stream.count cstrm + 1)) str ]
;


value dollar_for_antiquotation = ref True;
value specific_space_dot = ref False;

value func kwd_table glexr =
  let bolpos = ref 0 in
  let lnum = ref 1 in
  let fname = ref "" in
  let find = Hashtbl.find kwd_table in
  let dfa = dollar_for_antiquotation.val in
  let ssd = specific_space_dot.val in
  (Token.lexer_func_of_parser (next_token_fun dfa ssd find fname lnum bolpos glexr),
   (bolpos, lnum, fname))
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

value make_lexer () =
  let kwd_table = Hashtbl.create 301 in
  let id_table = Hashtbl.create 301 in
  let glexr =
    ref
     {tok_func = fun []; tok_using = fun []; tok_removing = fun [];
      tok_match = fun []; tok_text = fun []; tok_comm = None}
  in
  let (f,pos) = func kwd_table glexr in
  let glex =
    {tok_func = f;
     tok_using = using_token kwd_table id_table;
     tok_removing = removing_token kwd_table id_table; tok_match = tok_match;
     tok_text = text; tok_comm = None}
  in
  do { glexr.val := glex; (glex, pos) }
;

value gmake () =
  let (p,_) = make_lexer () in p
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
  {func = fst(func kwd_table glexr); using = using_token kwd_table id_table;
   removing = removing_token kwd_table id_table; tparse = tparse; text = text}
;
