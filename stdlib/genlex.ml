(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char

(* The string buffering machinery *)

let initial_buffer = Bytes.create 32

let buffer = ref initial_buffer
let bufpos = ref 0

let reset_buffer () = buffer := initial_buffer; bufpos := 0

let store c =
  if !bufpos >= Bytes.length !buffer then begin
    let newbuffer = Bytes.create (2 * !bufpos) in
    Bytes.blit !buffer 0 newbuffer 0 !bufpos;
    buffer := newbuffer
  end;
  Bytes.set !buffer !bufpos c;
  incr bufpos

let get_string () =
  let s = Bytes.sub_string !buffer 0 !bufpos in buffer := initial_buffer; s

(* The lexer *)

exception Error of string
exception End_of_input

let from_seq keywords =
  let open Seq in
  let kwd_table = Hashtbl.create 17 in
  List.iter (fun s -> Hashtbl.add kwd_table s (Kwd s)) keywords;
  let ident_or_keyword id =
    try Hashtbl.find kwd_table id with
      Not_found -> Ident id
  and keyword_or_error c =
    let s = String.make 1 c in
    try Hashtbl.find kwd_table s with
      Not_found -> raise (Error ("Illegal character " ^ s))
  in
  let rec (+::) e seq = Cons (e, fun () -> next_token seq)
  and next_token (seq : _ Seq.t) : _ Seq.node =
    match seq () with
    | Nil -> Nil
    | Cons (head, rest) ->
        begin match head with
        | ' ' | '\010' | '\013' | '\009' | '\026' | '\012' ->
            next_token rest
        | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' as c ->
            reset_buffer (); store c; ident rest
        | '!' | '%' | '&' | '$' | '#' | '+' | '/' | ':' | '<' | '=' | '>' |
          '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c ->
            reset_buffer (); store c; ident2 rest
        | '0'..'9' as c ->
            reset_buffer (); store c; number rest
        | '\'' ->
            let c, rest =
              try char rest with
                End_of_input -> raise (Error "")
            in
            begin match rest () with
            | Cons ('\'', rest) -> Char c +:: rest
            | _ -> raise (Error "")
            end
        | '\"' ->
            reset_buffer ();
            let s, rest = string rest in
            String s +:: rest
        | '-' -> neg_number rest
        | '(' -> maybe_comment rest
        | c -> keyword_or_error c +:: rest
        end
  and ident (seq : _ Seq.t) =
    match seq () with
    | Cons (
        ('A'..'Z' | 'a'..'z' | '\192'..'\255' | '0'..'9' | '_' | '\'' as c),
        rest) ->
        store c; ident rest
    | _ ->
        ident_or_keyword (get_string ()) +:: seq
  and ident2 (seq : _ Seq.t) =
    match seq () with
    | Cons
        ('!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' |
         '>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c
        , rest) ->
        store c; ident2 rest
    | _ ->
        ident_or_keyword (get_string ()) +:: seq
  and neg_number (seq : _ Seq.t) =
    match seq () with
    | Cons ('0'..'9' as c, rest) ->
        reset_buffer (); store '-'; store c; number rest
    | _ ->
        reset_buffer (); store '-'; ident2 seq
  and number (seq : _ Seq.t) =
    match seq () with
    | Cons ('0'..'9' as c, rest) ->
        store c; number rest
    | Cons ('.', rest) ->
        store '.'; decimal_part rest
    | Cons (('e' | 'E'), rest) ->
        store 'E'; exponent_part rest
    | _ ->
        Int (int_of_string (get_string ())) +:: seq
  and decimal_part (seq : _ Seq.t) =
    match seq () with
    | Cons ('0'..'9' as c, rest) ->
        store c; decimal_part rest
    | Cons (('e' | 'E'), rest) ->
        store 'E'; exponent_part rest
    | _ ->
        Float (float_of_string (get_string ())) +:: seq
  and exponent_part (seq : _ Seq.t) =
    match seq () with
    | Cons ('+' | '-' as c, rest) ->
        store c; end_exponent_part rest
    | _ ->
        end_exponent_part seq
  and end_exponent_part (seq : _ Seq.t) =
    match seq () with
    | Cons ('0'..'9' as c, rest) ->
        store c; end_exponent_part rest
    | _ ->
        Float (float_of_string (get_string ())) +:: seq
  and string (seq : _ Seq.t) =
    match seq () with
    | Cons ('\"', rest) -> get_string (), rest
    | Cons ('\\', rest) ->
        let c, rest =
          try escape rest with
            End_of_input -> raise (Error "")
        in
        store c; string rest
    | Cons (c, rest) -> store c; string rest
    | Nil -> raise End_of_input
  and char (seq : _ Seq.t) =
    match seq () with
    | Cons ('\\', rest) ->
        begin try escape rest with
          End_of_input -> raise (Error "")
        end
    | Cons (c, rest) -> c, rest
    | Nil -> raise End_of_input
  and escape (seq : _ Seq.t) =
    match seq () with
    | Cons ('n', rest) -> '\n', rest
    | Cons ('r', rest) -> '\r', rest
    | Cons ('t', rest) -> '\t', rest
    | Cons ('0'..'9' as c1, rest) ->
        begin match rest () with
        | Cons ('0'..'9' as c2, rest) ->
            begin match rest () with
            | Cons ('0'..'9' as c3, rest) ->
                let c = Char.chr
                    ((Char.code c1 - 48) * 100 + (Char.code c2 - 48) * 10 +
                     (Char.code c3 - 48))
                in
                c, rest
            | _ -> raise (Error "")
            end
        | _ -> raise (Error "")
        end
    | Cons (c, rest) -> c, rest
    | Nil -> raise End_of_input
  and maybe_comment (seq : _ Seq.t) =
    match seq () with
    | Cons ('*', rest) ->
        let rest = comment rest in
        next_token rest
    | _ ->
        keyword_or_error '(' +:: seq
  and comment (seq : _ Seq.t) =
    match seq () with
    | Cons ('(', rest) -> maybe_nested_comment rest
    | Cons ('*', rest) -> maybe_end_comment rest
    | Cons (_, rest) -> comment rest
    | Nil -> raise End_of_input
  and maybe_nested_comment (seq : _ Seq.t) =
    match seq () with
    | Cons ('*', rest) ->
        let rest = comment rest in
        comment rest
    | Cons (_, rest) -> comment rest
    | Nil -> raise End_of_input
  and maybe_end_comment (seq : _ Seq.t) =
    match seq () with
    | Cons (')', rest) -> rest
    | Cons ('*', rest) -> maybe_end_comment rest
    | Cons (_, rest) -> comment rest
    | Nil -> raise End_of_input
  in
  fun input () -> next_token input


let make_lexer keywords =
  let kwd_table = Hashtbl.create 17 in
  List.iter (fun s -> Hashtbl.add kwd_table s (Kwd s)) keywords;
  let ident_or_keyword id =
    try Hashtbl.find kwd_table id with
      Not_found -> Ident id
  and keyword_or_error c =
    let s = String.make 1 c in
    try Hashtbl.find kwd_table s with
      Not_found -> raise (Stream.Error ("Illegal character " ^ s))
  in
  let rec next_token (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some (' ' | '\010' | '\013' | '\009' | '\026' | '\012') ->
        Stream.junk strm__; next_token strm__
    | Some ('A'..'Z' | 'a'..'z' | '_' | '\192'..'\255' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store c; ident s
    | Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '/' | ':' | '<' | '=' | '>' |
         '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store c; ident2 s
    | Some ('0'..'9' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store c; number s
    | Some '\'' ->
        Stream.junk strm__;
        let c =
          try char strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        begin match Stream.peek strm__ with
          Some '\'' -> Stream.junk strm__; Some (Char c)
        | _ -> raise (Stream.Error "")
        end
    | Some '\"' ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); Some (String (string s))
    | Some '-' -> Stream.junk strm__; neg_number strm__
    | Some '(' -> Stream.junk strm__; maybe_comment strm__
    | Some c -> Stream.junk strm__; Some (keyword_or_error c)
    | _ -> None
  and ident (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some
        ('A'..'Z' | 'a'..'z' | '\192'..'\255' | '0'..'9' | '_' | '\'' as c) ->
        Stream.junk strm__; let s = strm__ in store c; ident s
    | _ -> Some (ident_or_keyword (get_string ()))
  and ident2 (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some
        ('!' | '%' | '&' | '$' | '#' | '+' | '-' | '/' | ':' | '<' | '=' |
         '>' | '?' | '@' | '\\' | '~' | '^' | '|' | '*' as c) ->
        Stream.junk strm__; let s = strm__ in store c; ident2 s
    | _ -> Some (ident_or_keyword (get_string ()))
  and neg_number (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__;
        let s = strm__ in reset_buffer (); store '-'; store c; number s
    | _ -> let s = strm__ in reset_buffer (); store '-'; ident2 s
  and number (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__; let s = strm__ in store c; number s
    | Some '.' ->
        Stream.junk strm__; let s = strm__ in store '.'; decimal_part s
    | Some ('e' | 'E') ->
        Stream.junk strm__; let s = strm__ in store 'E'; exponent_part s
    | _ -> Some (Int (int_of_string (get_string ())))
  and decimal_part (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__; let s = strm__ in store c; decimal_part s
    | Some ('e' | 'E') ->
        Stream.junk strm__; let s = strm__ in store 'E'; exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and exponent_part (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('+' | '-' as c) ->
        Stream.junk strm__; let s = strm__ in store c; end_exponent_part s
    | _ -> end_exponent_part strm__
  and end_exponent_part (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ('0'..'9' as c) ->
        Stream.junk strm__; let s = strm__ in store c; end_exponent_part s
    | _ -> Some (Float (float_of_string (get_string ())))
  and string (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\"' -> Stream.junk strm__; get_string ()
    | Some '\\' ->
        Stream.junk strm__;
        let c =
          try escape strm__ with
            Stream.Failure -> raise (Stream.Error "")
        in
        let s = strm__ in store c; string s
    | Some c -> Stream.junk strm__; let s = strm__ in store c; string s
    | _ -> raise Stream.Failure
  and char (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\\' ->
        Stream.junk strm__;
        begin try escape strm__ with
          Stream.Failure -> raise (Stream.Error "")
        end
    | Some c -> Stream.junk strm__; c
    | _ -> raise Stream.Failure
  and escape (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some 'n' -> Stream.junk strm__; '\n'
    | Some 'r' -> Stream.junk strm__; '\r'
    | Some 't' -> Stream.junk strm__; '\t'
    | Some ('0'..'9' as c1) ->
        Stream.junk strm__;
        begin match Stream.peek strm__ with
          Some ('0'..'9' as c2) ->
            Stream.junk strm__;
            begin match Stream.peek strm__ with
              Some ('0'..'9' as c3) ->
                Stream.junk strm__;
                Char.chr
                  ((Char.code c1 - 48) * 100 + (Char.code c2 - 48) * 10 +
                     (Char.code c3 - 48))
            | _ -> raise (Stream.Error "")
            end
        | _ -> raise (Stream.Error "")
        end
    | Some c -> Stream.junk strm__; c
    | _ -> raise Stream.Failure
  and maybe_comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '*' ->
        Stream.junk strm__; let s = strm__ in comment s; next_token s
    | _ -> Some (keyword_or_error '(')
  and comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '(' -> Stream.junk strm__; maybe_nested_comment strm__
    | Some '*' -> Stream.junk strm__; maybe_end_comment strm__
    | Some _ -> Stream.junk strm__; comment strm__
    | _ -> raise Stream.Failure
  and maybe_nested_comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '*' -> Stream.junk strm__; let s = strm__ in comment s; comment s
    | Some _ -> Stream.junk strm__; comment strm__
    | _ -> raise Stream.Failure
  and maybe_end_comment (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some ')' -> Stream.junk strm__; ()
    | Some '*' -> Stream.junk strm__; maybe_end_comment strm__
    | Some _ -> Stream.junk strm__; comment strm__
    | _ -> raise Stream.Failure
  in
  fun input -> Stream.from (fun _count -> next_token input)
