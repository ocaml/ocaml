(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** The run-time library for scanners. *)

(** {6 Scanning buffers} *)

type scanbuf;;
(** The type of scanning buffers. A scanning buffer is the argument passed
   to the scanning functions used by the [scanf] family functions.
   The scanning buffer holds the current state of the scan, plus
   a function to find the next char from the input, and a token buffer
   to store the string matched so far. *)

val next_char : scanbuf -> unit;;
(** [Scanning.next_char scanbuf] advance the scanning buffer for
    one character. *)

val peek_char : scanbuf -> char;;
(** [Scanning.peek_char scanbuf] returns the current char available in
    the input. *)

val store_char : scanbuf -> char -> int -> int;;
(** [Scanning.store_char scanbuf c lim] adds [c] to the token buffer
    of the scanning buffer. It returns [lim - 1], indicating that there
    is one less character to read. *)

val char_count : scanbuf -> int;;
(** [Scanning.char_count scanbuf] returns the number of characters read
    from the given buffer. *)

val token : scanbuf -> string;;
(** [Scanning.token scanbuf] returns the string stored into the token
    buffer of the scanning buffer: it returns the token matched by the
    format. *)

val reset_token : scanbuf -> unit;;
(** [Scanning.reset_token scanbuf] rests to zero the token buffer of
    the given scanning buffer. *)

val token_count : scanbuf -> int;;
(** [Scanning.token_count scanbuf] returns the number of tokens read
    so far from [scanbuf]. *)

val end_of_input : scanbuf -> bool;;
(** [Scanning.end_of_input scanbuf] tests the end of input condition
    of the given buffer. *)

val from_string : string -> scanbuf;;
(** Create a scanning buffer which reads from the given string.
    Reading starts from the first character in the string.
    The end-of-input condition is set when the end of the string is reached. *)

val from_channel : in_channel -> scanbuf;;
(** Create a scanning buffer on the given input channel.
    [Scanning.from_channel inchan] returns a scanning buffer which reads
    from the input channel [inchan], at the current reading position. *)

val from_function : (unit -> char) -> scanbuf;;
(** Create a scanning buffer with the given function as its reading method.
    When the scanning needs one more character, it calls the given function.
    When the function has no more character to provide, it must set
    an end of input condition by raising the exception [End_of_file]. *)
