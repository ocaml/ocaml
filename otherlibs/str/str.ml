(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type regexp

external compile_regexp: string -> bool -> regexp = "str_compile_regexp"
external string_match: regexp -> string -> int -> bool = "str_string_match"
external search_forward: regexp -> string -> int -> int = "str_search_forward"
external search_backward: regexp -> string -> int -> int = "str_search_backward"
external beginning_group: int -> int = "str_beginning_group"
external end_group: int -> int = "str_end_group"
external replacement_text: string -> string -> string = "str_replacement_text"

let string_before s n =
  if n = 0 then s else String.sub s 0 n

let string_after s n = String.sub s n (String.length s - n)

let first_chars s n = String.sub s 0 n

let last_chars s n = String.sub s (String.length s - n) n

let regexp e = compile_regexp e false

let regexp_case_fold e = compile_regexp e true

let group_beginning n =
  if n < 0 or n >= 10 then invalid_arg "Str.group_beginning" else
  let pos = beginning_group n in
  if pos = -1 then raise Not_found else pos

let group_end n =
  if n < 0 or n >= 10 then invalid_arg "Str.group_end" else
  let pos = end_group n in
  if pos = -1 then raise Not_found else pos

let matched_group n txt =
  let b = group_beginning n and e = group_end n in String.sub txt b (e-b)

let match_beginning () = group_beginning 0
and match_end () = group_end 0
and matched_string txt = matched_group 0 txt

let substitute_first expr repl_fun text =
  try
    let pos = search_forward expr text 0 in
    String.concat "" [string_before text pos; 
                      repl_fun text;
                      string_after text (match_end())]
  with Not_found ->
    text

let global_substitute expr repl_fun text =
  let rec replace start =
    try
      let pos = search_forward expr text start in
      let repl_text = repl_fun text in
      String.sub text start (pos-start) ::
      repl_text ::
      replace (match_end())
    with Not_found ->
      [string_after text start] in
  String.concat "" (replace 0)

let global_replace expr repl text =
  global_substitute expr (replacement_text repl) text
and replace_first expr repl text =
  substitute_first expr (replacement_text repl) text

let bounded_split expr text num =
  let start =
    if string_match expr text 0 then match_end() else 0 in
  let rec split start n =
    if start >= String.length text then [] else
    if n = 1 then [string_after text start] else
      try
        let pos = search_forward expr text start in
        String.sub text start (pos-start) :: split (match_end()) (n-1)
      with Not_found ->
        [string_after text start] in
  split start num

let split expr text = bounded_split expr text 0

