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

(* Module [ThreadIO]: thread-compatible input-output operations *)

(* This module reimplements some of the input functions from [Pervasives]
   and [Lexing] so that they only block the calling thread, not all threads
   in the program, if data is not immediately available on the input. *)

(** General input functions *)

val input_char : in_channel -> char
val input : in_channel -> string -> int -> int -> int
val really_input : in_channel -> string -> int -> int -> unit
val input_byte : in_channel -> int
        (* See the corresponding functions in module [Pervasives]. *)
val input_line : in_channel -> string
val input_binary_int : in_channel -> int
val input_value : in_channel -> 'a
        (* See the corresponding functions in module [Pervasives].
           For the three functions above, the other program threads continue
           to run until the first character of the input is available;
           all threads are blocked while the remaining characters of the
           input are being read. *)

(** Input functions on standard input *)

val read_line : unit -> string
val read_int : unit -> int
val read_float : unit -> float
        (* See the corresponding functions in module [Pervasives]. *)

(** Lexer buffers *)

val lexing_from_channel: in_channel -> Lexing.lexbuf
        (* See [Lexing.from_channel]. *)
