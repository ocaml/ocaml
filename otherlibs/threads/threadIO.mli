(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [ThreadIO]: thread-compatible input-output operations *)

(* This module reimplements some of the functions from [Pervasives]
   and [Lexing] so that they only block the calling thread, not all threads
   in the program, if input or output is not immediately possible.
   See the documentation of the [Pervasives] and [Lexing] modules for
   precise descriptions of the functions below. *)

(** Output functions on standard output *)

val print_char : char -> unit
val print_string : string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_endline : string -> unit
val print_newline : unit -> unit

(** Output functions on standard error *)

val prerr_char : char -> unit
val prerr_string : string -> unit
val prerr_int : int -> unit
val prerr_float : float -> unit
val prerr_endline : string -> unit
val prerr_newline : unit -> unit

(** Input functions on standard input *)

val read_line : unit -> string
val read_int : unit -> int
val read_float : unit -> float

(** General output functions *)

val flush : out_channel -> unit
val output_char : out_channel -> char -> unit
val output_string : out_channel -> string -> unit
val output : out_channel -> string -> int -> int -> unit
val output_byte : out_channel -> int -> unit
val output_binary_int : out_channel -> int -> unit
val output_value : out_channel -> 'a -> unit
val seek_out : out_channel -> int -> unit
val close_out : out_channel -> unit

(** General input functions *)

val input_char : in_channel -> char
val input_line : in_channel -> string
val input : in_channel -> string -> int -> int -> int
val really_input : in_channel -> string -> int -> int -> unit
val input_byte : in_channel -> int
val input_binary_int : in_channel -> int
val input_value : in_channel -> 'a

(** Lexer buffers *)

val lexing_from_channel: in_channel -> Lexing.lexbuf

