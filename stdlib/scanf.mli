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

(** Formatted input functions. *)

val fscanf : in_channel -> ('a, Scanning.scanbuf, 'b) format -> ('a -> 'b);;
 
(** [fscanf inchan format f] reads tokens from the channel [inchan] according
   to the format string [format], converts these tokens to values, and
   applies these values to the function [f].
   The result of this application of [f] is the result of the whole construct.
   
   The format is a character string which contains two types of
   objects:  plain  characters, which are simply matched with the
   input channel, and conversion specifications, each of which
   causes  conversion and reading of one argument for [f].
   
   Conversion specifications consist in the [%] character, followed
   by optional field width, followed by one or two conversion
   characters. The conversion characters and their meanings are:
   - [d] reads an optionally signed decimal integer.
   - [i]: reads an optionally signed integer
     (usual input formats for hexadecimal ([0x\[d\]+] and [0X\[d+\]]),
      octal ([0o\[d\]+]), and binary [ob\[d\]+] notations are understood).
   - [u]: convert an integer argument to unsigned decimal
   - [x]: convert an integer argument to unsigned hexadecimal,
     using lowercase letters.
   - [X]: convert an integer argument to unsigned hexadecimal,
     using uppercase letters.
   - [o]: reads an unsigned octal integer.
   - [s]: reads a string argument (string ends with a space).
   - [c]: reads a single character.
   - [f]: reads a floating-point number in decimal notation,
     in the style [dddd.ddd]
   - [e] or [E]: reads a floating-point argument in decimal notation,
     in the style [d.ddd e+-dd] (mantissa and exponent)
   - [g] or [G]: reads a floating-point argument in decimal notation,
     in style [f] or [e], [E].
   - [b]: reads a boolean argument ([true] or [false]).
   - [ld], [li], [lu], [lx], [lX], [lo]: reads an [int32] argument to
     the format specified by the second letter (decimal, hexadecimal, etc).
   - [nd], [ni], [nu], [nx], [nX], [no]: reads a [nativeint] argument to
     the format specified by the second letter.
   - [Ld], [Li], [Lu], [Lx], [LX], [Lo]: reads an [int64] argument to
     the format specified by the second letter.
   - [\[ range \]]: reads characters that maches one of the characters
     mentioned in the range of characters [range] (or not mentioned in
     it, if the range started by [^]).
   - [N]: applied [f] to the number of characters read so far.
   - [%]: matches one [%] character in the input.

   The field widths are composed of an optional integer literal
   indicating the maximal width of the token to read, possibly followed by
   a dot [.] and another integer literal indicating how many digits
   follow the decimal point in the [%f], [%e], and [%E] conversions.
   For instance, [%6d] reads an integer, having at most 6 decimal digits;
   and [%4f] reads a float with 4 characters.

   Each conversion starts by skiping space (except the [c]
   conversion). Space is any number of tab, white space, newline and
   return. Hence, a space in the format matches any number of white
   spaces in the input.

   Note: the [scanf] facility is not intended for heavy duty
   lexing and parsing; if you need efficient language syntactic analysis,
   use the corresponding devoted libraries. *)

val scanf : ('a, Scanning.scanbuf, 'b) format -> ('a -> 'b);;
(** Same as {!Scanf.fscanf}, but inputs from [stdin]. *)

val bscanf :
  Scanning.scanbuf -> ('a, Scanning.scanbuf, 'b) format -> ('a -> 'b);;
(** Same as {!Scanf.bscanf}, but inputs from the buffer argument. *)

val sscanf : string -> ('a, Scanning.scanbuf, 'b) format -> ('a -> 'b);;
(** Same as {!Scanf.fscanf}, but inputs from the string argument. *)
