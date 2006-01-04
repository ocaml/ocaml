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

(** Scanning buffers. *)
module Scanning : sig

type scanbuf;;
(** The type of scanning buffers. A scanning buffer is the argument passed
   to the scanning functions used by the [scanf] family of functions.
   The scanning buffer holds the current state of the scan, plus
   a function to get the next char from the input, and a token buffer
   to store the string matched so far. *)

val stdib : scanbuf;;
(** The scanning buffer reading from [stdin].
    [stdib] is equivalent to [Scanning.from_channel stdin]. *)

val from_string : string -> scanbuf;;
(** [Scanning.from_string s] returns a scanning buffer which reads
    from the given string.
    Reading starts from the first character in the string.
    The end-of-input condition is set when the end of the string is reached. *)

val from_file : string -> scanbuf;;
(** Bufferized file reading in text mode. The efficient and usual
    way to scan text mode files (in effect, [from_file] returns a
    buffer that reads characters in large chunks, rather than one
    character at a time as buffers returned by [from_channel] do).
    [Scanning.from_file fname] returns a scanning buffer which reads
    from the given file [fname] in text mode. *)

val from_file_bin : string -> scanbuf;;
(** Bufferized file reading in binary mode. *)

val from_function : (unit -> char) -> scanbuf;;
(** [Scanning.from_function f] returns a scanning buffer with
    the given function as its reading method.
    When scanning needs one more character, the given function is called.
    When the function has no more character to provide, it must signal
    an end-of-input condition by raising the exception [End_of_file]. *)

val from_channel : in_channel -> scanbuf;;
(** [Scanning.from_channel ic] returns a scanning buffer which reads
    one character at a time from the input channel [ic], starting at the
    current reading position. *)

val end_of_input : scanbuf -> bool;;
(** [Scanning.end_of_input ib] tests the end-of-input condition
    of the given buffer. *)
val beginning_of_input : scanbuf -> bool;;
(** [Scanning.beginning_of_input ib] tests the beginning of input
    condition of the given buffer. *)

val name_of_input : scanbuf -> string;;
(** [Scanning.file_name_of_input ib] returns the name of the character
    source for the input buffer [ib]. *)

end;;

exception Scan_failure of string;;
(** The exception that formatted input functions raise when the input
   cannot be read according to the given format. *)

val bscanf :
  Scanning.scanbuf -> ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b;;
(** [bscanf ib fmt f] reads tokens from the scanning buffer [ib] according
   to the format string [fmt], converts these tokens to values, and
   applies the function [f] to these values.
   The result of this application of [f] is the result of the whole construct.

   For instance, if [p] is the function [fun s i -> i + 1], then
   [Scanf.sscanf "x = 1" "%s = %i" p] returns [2].

   The format is a character string which contains three types of
   objects:
   - plain characters, which are simply matched with the
   characters of the input,
   - conversion specifications, each of which causes reading and
   conversion of one argument for [f],
   - scanning indications to specify boundaries of tokens.

   Among plain characters the space character (ASCII code 32) has a
   special meaning: it matches ``whitespace'', that is any number of tab,
   space, newline and carriage return characters. Hence, a space in the format
   matches any amount of whitespace in the input.

   Conversion specifications consist in the [%] character, followed by
   an optional flag, an optional field width, and followed by one or
   two conversion characters. The conversion characters and their
   meanings are:

   - [d]: reads an optionally signed decimal integer.
   - [i]: reads an optionally signed integer
     (usual input formats for hexadecimal ([0x[d]+] and [0X[d]+]),
      octal ([0o[d]+]), and binary [0b[d]+] notations are understood).
   - [u]: reads an unsigned decimal integer.
   - [x] or [X]: reads an unsigned hexadecimal integer.
   - [o]: reads an unsigned octal integer.
   - [s]: reads a string argument that spreads as much as possible,
     until the next white space, the next scanning indication, or the
     end-of-input is reached. Hence, this conversion always succeeds:
     it returns an empty string if the bounding condition holds
     when the scan begins.
   - [S]: reads a delimited string argument (delimiters and special
     escaped characters follow the lexical conventions of Caml).
   - [c]: reads a single character. To test the current input character
     without reading it, specify a null field width, i.e. use
     specification [%0c]. Raise [Invalid_argument], if the field width
     specification is greater than 1.
   - [C]: reads a single delimited character (delimiters and special
     escaped characters follow the lexical conventions of Caml).
   - [f], [e], [E], [g], [G]: reads an optionally signed
     floating-point number in decimal notation, in the style [dddd.ddd
     e/E+-dd].
   - [F]: reads a floating point number according to the lexical
     conventions of Caml (hence the decimal point is mandatory if the
     exponent part is not mentioned).
   - [B]: reads a boolean argument ([true] or [false]).
   - [b]: reads a boolean argument (for backward compatibility; do not use
     in new programs).
   - [ld], [li], [lu], [lx], [lX], [lo]: reads an [int32] argument to
     the format specified by the second letter (decimal, hexadecimal, etc).
   - [nd], [ni], [nu], [nx], [nX], [no]: reads a [nativeint] argument to
     the format specified by the second letter.
   - [Ld], [Li], [Lu], [Lx], [LX], [Lo]: reads an [int64] argument to
     the format specified by the second letter.
   - [\[ range \]]: reads characters that matches one of the characters
     mentioned in the range of characters [range] (or not mentioned in
     it, if the range starts with [^]). Reads a [string] that can be
     empty, if no character in the input matches the range. The set of
     characters from [c1] to [c2] (inclusively) is denoted by [c1-c2].
     Hence, [%\[0-9\]] returns a string representing a decimal number
     or an empty string if no decimal digit is found; similarly,
     [%\[\\048-\\057\\065-\\070\]] returns a string of hexadecimal digits.
     If a closing bracket appears in a range, it must occur as the
     first character of the range (or just after the [^] in case of
     range negation); hence [\[\]\]] matches a [\]] character and
     [\[^\]\]] matches any character that is not [\]].
   - [\{ fmt %\}]: reads a format string argument to the format
     specified by the internal format [fmt]. The format string to be
     read must have the same type as the internal format [fmt].
     For instance, "%\{%i%\}" reads any format string that can read a value of
     type [int]; hence [Scanf.sscanf "fmt:\\\"number is %u\\\"" "fmt:%\{%i%\}"]
     succeeds and returns the format string ["number is %u"].
   - [\( fmt %\)]: scanning format substitution.
     Reads a format string to replace [fmt]. The format string read
     must have the same type as [fmt].
   - [l]: applies [f] to the number of lines read so far.
   - [n]: applies [f] to the number of characters read so far.
   - [N] or [L]: applies [f] to the number of tokens read so far.
   - [!]: matches the end of input condition.
   - [%]: matches one [%] character in the input.

   Following the [%] character introducing a conversion, there may be
   the special flag [_]: the conversion that follows occurs as usual,
   but the resulting value is discarded.

   The field widths are composed of an optional integer literal
   indicating the maximal width of the token to read.
   For instance, [%6d] reads an integer, having at most 6 decimal digits;
   [%4f] reads a float with at most 4 characters; and [%8\[\\000-\\255\]]
   returns the next 8 characters (or all the characters still available,
   if less than 8 characters are available in the input).

   Scanning indications appear just after the string conversions [s]
   and [\[ range \]] to delimit the end of the token. A scanning
   indication is introduced by a [@] character, followed by some
   constant character [c]. It means that the string token should end
   just before the next matching [c] (which is skipped). If no [c]
   character is encountered, the string token spreads as much as
   possible. For instance, ["%s@\t"] reads a string up to the next
   tabulation character or to the end of input. If a scanning
   indication [\@c] does not follow a string conversion, it is treated
   as a plain [c] character.

   Raise [Scanf.Scan_failure] if the given input does not match the format.

   Raise [Failure] if a conversion to a number is not possible.

   Raise [End_of_file] if the end of input is encountered while some
   more characters are needed to read the current conversion
   specification (this means in particular that scanning a [%s]
   conversion never raises exception [End_of_file]: if the end of
   input is reached the conversion succeeds and simply returns [""]).

   Notes:

   - the scanning indications introduce slight differences in the
   syntax of [Scanf] format strings compared to those used by the
   [Printf] module. However, scanning indications are similar to those
   of the [Format] module; hence, when producing formatted text to be
   scanned by [!Scanf.bscanf], it is wise to use printing functions
   from [Format] (or, if you need to use functions from [Printf],
   banish or carefully double check the format strings that contain
   ['\@'] characters).

   - in addition to relevant digits, ['_'] characters may appear
   inside numbers (this is reminiscent to the usual Caml
   conventions). If stricter scanning is desired, use the range
   conversion facility instead of the number conversions.

   - the [scanf] facility is not intended for heavy duty lexical
   analysis and parsing. If it appears not expressive enough for your
   needs, several alternative exists: regular expressions (module
   [Str]), stream parsers, [ocamllex]-generated lexers,
   [ocamlyacc]-generated parsers.
*)

val fscanf : in_channel -> ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b;;
(** Same as {!Scanf.bscanf}, but inputs from the given channel.

    Warning: since all scanning functions operate from a scanning
    buffer, be aware that each [fscanf] invocation must allocate a new
    fresh scanning buffer (unless careful use of partial evaluation in
    the program). Hence, there are chances that some characters seem
    to be skipped (in fact they are pending in the previously used
    buffer). This happens in particular when calling [fscanf] again
    after a scan involving a format that necessitates some look ahead
    (such as a format that ends by skipping whitespace in the input).

    To avoid confusion, consider using [bscanf] with an explicitly
    created scanning buffer. Use for instance [Scanning.from_file f]
    to allocate the scanning buffer reading from file [f].

    This method is not only clearer it is also faster, since scanning
    buffers to files are optimized for fast bufferized reading. *)

val sscanf : string -> ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b;;
(** Same as {!Scanf.bscanf}, but inputs from the given string. *)

val scanf : ('a, Scanning.scanbuf, 'b) format -> 'a -> 'b;;
(** Same as {!Scanf.bscanf}, but reads from the predefined scanning
    buffer {!Scanf.Scanning.stdib} that is connected to [stdin]. *)

val kscanf :
  Scanning.scanbuf -> (Scanning.scanbuf -> exn -> 'a) ->
  ('b, Scanning.scanbuf, 'a) format -> 'b -> 'a;;
(** Same as {!Scanf.bscanf}, but takes an additional function argument
  [ef] that is called in case of error: if the scanning process or
  some conversion fails, the scanning function aborts and applies the
  error handling function [ef] to the scanning buffer and the
  exception that aborted the scanning process. *)

val bscanf_format :
  Scanning.scanbuf -> ('a, 'b, 'c, 'd) format4 ->
    (('a, 'b, 'c, 'd) format4 -> 'e) -> 'e;;

(** [bscanf_format ib fmt f] reads a format string token in buffer [ib],
  according to the format string [fmt], and applies the function [f] to the
  resulting format string value.
  Raises [Scan_failure] if the format string value read has not the same type
  as [fmt]. *)

val sscanf_format :
  string -> ('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4;;
(** Same as {!Scanf.bscanf_format}, but converts the given string to a format
  string. *)
