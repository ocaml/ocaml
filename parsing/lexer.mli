(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The lexical analyzer *)

val init : unit -> unit
val token: Lexing.lexbuf -> Parser.token
val skip_sharp_bang: Lexing.lexbuf -> unit

type directive_type = 
  | Dir_type_bool 
  | Dir_type_float 
  | Dir_type_int 
  | Dir_type_string 

type directive_value =
  | Dir_bool of bool 
  | Dir_float of float
  | Dir_int of int
  | Dir_string of string

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Literal_overflow of string
  | Unterminated_paren_in_conditional
  | Unterminated_if
  | Unterminated_else 
  | Unexpected_token_in_conditional 
  | Expect_hash_then_in_conditional
  | Illegal_semver of string
  | Unexpected_directive
  | Conditional_expr_expected_type of directive_type * directive_type
;;

exception Error of error * Location.t

open Format

val report_error: formatter -> error -> unit
 (* Deprecated.  Use Location.{error_of_exn, report_error}. *)

val in_comment : unit -> bool;;
val in_string : unit -> bool;;


val print_warnings : bool ref
val comments : unit -> (string * Location.t) list
val token_with_comments : Lexing.lexbuf -> Parser.token

(*
  [set_preprocessor init preprocessor] registers [init] as the function
to call to initialize the preprocessor when the lexer is initialized,
and [preprocessor] a function that is called when a new token is needed
by the parser, as [preprocessor lexer lexbuf] where [lexer] is the
lexing function.

When a preprocessor is configured by calling [set_preprocessor], the lexer
changes its behavior to accept backslash-newline as a token-separating blank.
*)

val set_preprocessor :
  (unit -> unit) ->
  ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parser.token) ->
  unit


val replace_directive_built_in_value : 
  string ->  directive_value -> unit

(** Raises Not_found *)
val find_directive_built_in_value :
  string -> directive_value

val iter_directive_built_in_value : 
  (string -> directive_value -> unit) -> unit


(** semantic version predicate *)
val semver : Location.t ->   string -> string -> bool

val filter_directive_from_lexbuf : Lexing.lexbuf -> (int * int) list
