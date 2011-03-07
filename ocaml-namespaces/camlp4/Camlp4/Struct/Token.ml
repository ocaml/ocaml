(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

open Format;

module Make (Loc : Sig.Loc)
: Sig.Camlp4Token with module Loc = Loc
= struct
  module Loc  = Loc;
  open Sig;
  type t = camlp4_token;
  type token = t;

  value to_string =
    fun
    [ KEYWORD s    -> sprintf "KEYWORD %S" s
    | SYMBOL s     -> sprintf "SYMBOL %S" s
    | LIDENT s     -> sprintf "LIDENT %S" s
    | UIDENT s     -> sprintf "UIDENT %S" s
    | INT _ s      -> sprintf "INT %s" s
    | INT32 _ s    -> sprintf "INT32 %sd" s
    | INT64 _ s    -> sprintf "INT64 %sd" s
    | NATIVEINT _ s-> sprintf "NATIVEINT %sd" s
    | FLOAT _ s    -> sprintf "FLOAT %s" s
    | CHAR _ s     -> sprintf "CHAR '%s'" s
    | STRING _ s   -> sprintf "STRING \"%s\"" s
                      (* here it's not %S since the string is already escaped *)
    | LABEL s      -> sprintf "LABEL %S" s
    | OPTLABEL s   -> sprintf "OPTLABEL %S" s
    | ANTIQUOT n s -> sprintf "ANTIQUOT %s: %S" n s
    | QUOTATION x  -> sprintf "QUOTATION { q_name=%S; q_loc=%S; q_shift=%d; q_contents=%S }"
                        x.q_name x.q_loc x.q_shift x.q_contents
    | COMMENT s    -> sprintf "COMMENT %S" s
    | BLANKS s     -> sprintf "BLANKS %S" s
    | NEWLINE      -> sprintf "NEWLINE"
    | EOI          -> sprintf "EOI"
    | ESCAPED_IDENT s -> sprintf "ESCAPED_IDENT %S" s
    | LINE_DIRECTIVE i None -> sprintf "LINE_DIRECTIVE %d" i
    | LINE_DIRECTIVE i (Some s) -> sprintf "LINE_DIRECTIVE %d %S" i s ];

  value print ppf x = pp_print_string ppf (to_string x);

  value match_keyword kwd =
    fun
    [ KEYWORD kwd' when kwd = kwd' -> True
    | _ -> False ];

  value extract_string =
    fun
    [ KEYWORD s | SYMBOL s | LIDENT s | UIDENT s | INT _ s | INT32 _ s |
      INT64 _ s | NATIVEINT _ s | FLOAT _ s | CHAR _ s | STRING _ s |
      LABEL s | OPTLABEL s | COMMENT s | BLANKS s | ESCAPED_IDENT s -> s
    | tok ->
        invalid_arg ("Cannot extract a string from this token: "^
                     to_string tok) ];

  module Error = struct
    type t =
      [ Illegal_token of string
      | Keyword_as_label of string
      | Illegal_token_pattern of string and string
      | Illegal_constructor of string ];

    exception E of t;

    value print ppf =
      fun
      [ Illegal_token s ->
          fprintf ppf "Illegal token (%s)" s
      | Keyword_as_label kwd ->
          fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
      | Illegal_token_pattern p_con p_prm ->
          fprintf ppf "Illegal token pattern: %s %S" p_con p_prm
      | Illegal_constructor con ->
          fprintf ppf "Illegal constructor %S" con ];

    value to_string x =
      let b = Buffer.create 50 in
      let () = bprintf b "%a" print x in Buffer.contents b;
  end;
  let module M = ErrorHandler.Register Error in ();

  module Filter = struct
    type token_filter = stream_filter t Loc.t;

    type t =
      { is_kwd : string -> bool;
        filter : mutable token_filter };

    value err error loc =
      raise (Loc.Exc_located loc (Error.E error));

    value keyword_conversion tok is_kwd =
      match tok with
      [ SYMBOL s | LIDENT s | UIDENT s when is_kwd s -> KEYWORD s
      | ESCAPED_IDENT s -> LIDENT s
      | _ -> tok ];

    value check_keyword_as_label tok loc is_kwd =
      let s =
        match tok with
        [ LABEL s         -> s
        | OPTLABEL s      -> s
        | _               -> "" ]
      in if s <> "" && is_kwd s then err (Error.Keyword_as_label s) loc else ();

    value check_unknown_keywords tok loc =
      match tok with
      [ SYMBOL s -> err (Error.Illegal_token s) loc
      | _        -> () ];

    value error_no_respect_rules p_con p_prm =
      raise (Error.E (Error.Illegal_token_pattern p_con p_prm));

    value check_keyword _ = True;
      (* FIXME let lb = Lexing.from_string s in
      let next () = token default_context lb in
      try
        match next () with
        [ SYMBOL _ | UIDENT _ | LIDENT _ -> (next () = EOI)
        | _ -> False ]
      with [ Stream.Error _ -> False ];                        *)

    value error_on_unknown_keywords = ref False;

    value rec ignore_layout =
      parser
      [ [: `(COMMENT _ | BLANKS _ | NEWLINE | LINE_DIRECTIVE _ _, _); s :] ->
          ignore_layout s
      | [: ` x; s :] -> [: ` x; ignore_layout s :]
      | [: :] -> [: :] ];

    value mk is_kwd =
      { is_kwd = is_kwd;
        filter = ignore_layout };

    value filter x =
      let f tok loc = do {
        let tok = keyword_conversion tok x.is_kwd;
        check_keyword_as_label tok loc x.is_kwd;
        if error_on_unknown_keywords.val
        then check_unknown_keywords tok loc else ();
        debug token "@[<hov 2>Lexer before filter:@ %a@ at@ %a@]@."
                    print tok Loc.dump loc in
        (tok, loc)
      } in
      let rec filter =
        parser
        [ [: `(tok, loc); s :] -> [: ` f tok loc; filter s :]
        | [: :] -> [: :] ]
      in
      let rec tracer = (* FIXME add a debug block construct *)
        parser
        [ [: `((_tok, _loc) as x); xs :] ->
            debug token "@[<hov 2>Lexer after filter:@ %a@ at@ %a@]@."
                        print _tok Loc.dump _loc in
            [: ` x; tracer xs :]
        | [: :] -> [: :] ]
      in fun strm -> tracer (x.filter (filter strm));

    value define_filter x f = x.filter := f x.filter;

    value keyword_added _ _ _ = ();
    value keyword_removed _ _ = ();
  end;

end;

(* Char and string tokens to real chars and string *)
module Eval = struct

  value valch x = Char.code x - Char.code '0';
  value valch_hex x =
    let d = Char.code x in
    if d >= 97 then d - 87
    else if d >= 65 then d - 55
    else d - 48;

  value rec skip_indent = parser
    [ [: `' ' | '\t'; s :] -> skip_indent s
    | [: :] -> () ];

  value skip_opt_linefeed = parser
    [ [: `'\010' :] -> ()
    | [: :] -> () ];

  value chr c =
    if c < 0 || c > 255 then failwith "invalid char token" else Char.chr c;

  value rec backslash = parser
    [ [: `'\010' :] -> '\010'
    | [: `'\013' :] -> '\013'
    | [: `'n' :]  -> '\n'
    | [: `'r' :]  -> '\r'
    | [: `'t' :]  -> '\t'
    | [: `'b' :]  -> '\b'
    | [: `'\\' :] -> '\\'
    | [: `'"' :]  -> '"'
    | [: `''' :]  -> '''
    | [: `' ' :]  -> ' '
    | [: `('0'..'9' as c1); `('0'..'9' as c2); `('0'..'9' as c3) :] ->
        chr (100 * (valch c1) + 10 * (valch c2) + (valch c3))
    | [: `'x'; `('0'..'9' | 'a'..'f' | 'A'..'F' as c1) ;
              `('0'..'9' | 'a'..'f' | 'A'..'F' as c2) :] ->
        chr (16 * (valch_hex c1) + (valch_hex c2)) ];

  value rec backslash_in_string strict store = parser
    [ [: `'\010'; s :] -> skip_indent s
    | [: `'\013'; s :] -> do { skip_opt_linefeed s; skip_indent s }
    | [: x = backslash :] -> store x
    | [: `c when not strict :] -> do { store '\\'; store c }
    | [: :] -> failwith "invalid string token" ];

  value char s =
    if String.length s = 1 then s.[0]
    else if String.length s = 0 then failwith "invalid char token"
    else match Stream.of_string s with parser
        [ [: `'\\'; x = backslash :] -> x
        | [: :] -> failwith "invalid char token" ];

  value string ?strict s =
    let buf = Buffer.create 23 in
    let store = Buffer.add_char buf in
    let rec parse = parser
      [ [: `'\\'; _ = backslash_in_string (strict <> None) store; s :] -> parse s
      | [: `c; s :] -> do { store c; parse s }
      | [: :] -> Buffer.contents buf ]
    in parse (Stream.of_string s);
end;
