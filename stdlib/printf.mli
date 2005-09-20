(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Formatted output functions. *)

val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
(** [fprintf outchan format arg1 ... argN] formats the arguments
   [arg1] to [argN] according to the format string [format],
   and outputs the resulting string on the channel [outchan].
   
   The format is a character string which contains two types of
   objects:  plain  characters, which are simply copied to the
   output channel, and conversion specifications, each of which
   causes  conversion and printing of one argument.
   
   Conversion specifications consist in the [%] character, followed
   by optional flags and field widths, followed by one or two conversion
   character. The conversion characters and their meanings are:
   - [d], [i], [n], [l], [L], or [N]: convert an integer argument to
     signed decimal.
   - [u]: convert an integer argument to unsigned decimal.
   - [x]: convert an integer argument to unsigned hexadecimal,
     using lowercase letters.
   - [X]: convert an integer argument to unsigned hexadecimal,
     using uppercase letters.
   - [o]: convert an integer argument to unsigned octal.
   - [s]: insert a string argument.
   - [S]: insert a string argument in Caml syntax (double quotes, escapes).
   - [c]: insert a character argument.
   - [C]: insert a character argument in Caml syntax (single quotes, escapes).
   - [f]: convert a floating-point argument to decimal notation,
     in the style [dddd.ddd].
   - [F]: convert a floating-point argument to Caml syntax ([dddd.]
     or [dddd.ddd] or [d.ddd e+-dd]).
   - [e] or [E]: convert a floating-point argument to decimal notation,
     in the style [d.ddd e+-dd] (mantissa and exponent).
   - [g] or [G]: convert a floating-point argument to decimal notation,
     in style [f] or [e], [E] (whichever is more compact).
   - [B]: convert a boolean argument to the string [true] or [false]
   - [b]: convert a boolean argument (for backward compatibility; do not
     use in new programs).
   - [ld], [li], [lu], [lx], [lX], [lo]: convert an [int32] argument to
     the format specified by the second letter (decimal, hexadecimal, etc).
   - [nd], [ni], [nu], [nx], [nX], [no]: convert a [nativeint] argument to
     the format specified by the second letter.
   - [Ld], [Li], [Lu], [Lx], [LX], [Lo]: convert an [int64] argument to
     the format specified by the second letter.
   - [a]: user-defined printer. Takes two arguments and apply the first
     one to [outchan] (the current output channel) and to the second
     argument. The first argument must therefore have type
     [out_channel -> 'b -> unit] and the second ['b].
     The output produced by the function is therefore inserted
     in the output of [fprintf] at the current point.
   - [t]: same as [%a], but takes only one argument (with type
     [out_channel -> unit]) and apply it to [outchan].
   - [\{ fmt %\}]: convert a format string argument. The argument
     must have the same type as the internal format string [fmt].
   - [\( fmt %\)]: format string substitution. This convertion takes a
     format string argument and substitutes it to the specification
     [fmt] to print following arguments. The format string argument
     must have the same type as [fmt].
   - [!]: take no argument and flush the output.
   - [%]: take no argument and output one [%] character.

   The optional flags include:
   - [-]: left-justify the output (default is right justification).
   - [0]: for numerical conversions, pad with zeroes instead of spaces.
   - [+]: for numerical conversions, prefix number with a [+] sign if positive.
   - space: for numerical conversions, prefix number with a space if positive.
   - [#]: request an alternate formatting style for numbers.

   The field widths are composed of an optional integer literal
   indicating the minimal width of the result, possibly followed by
   a dot [.] and another integer literal indicating how many digits
   follow the decimal point in the [%f], [%e], and [%E] conversions.
   For instance, [%6d] prints an integer, prefixing it with spaces to
   fill at least 6 characters; and [%.4f] prints a float with 4
   fractional digits.  Each or both of the integer literals can also be
   specified as a [*], in which case an extra integer argument is taken
   to specify the corresponding width or precision. *)

val printf : ('a, out_channel, unit) format -> 'a
(** Same as {!Printf.fprintf}, but output on [stdout]. *)

val eprintf : ('a, out_channel, unit) format -> 'a
(** Same as {!Printf.fprintf}, but output on [stderr]. *)

val sprintf : ('a, unit, string) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   return a string containing the result of formatting
   the arguments. *)

val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   append the formatted arguments to the given extensible buffer
   (see module {!Buffer}). *)

val kfprintf : (out_channel -> 'a) -> out_channel ->
              ('b, out_channel, unit, 'a) format4 -> 'b;;
(** Same as [fprintf], but instead of returning immediately,
   passes the out channel to its first argument at the end of printing. *)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b;;
(** Same as [sprintf] above, but instead of returning the string,
   passes it to the first argument. *)

val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b;;
(** A deprecated synonym for [ksprintf]. *)

(**/**)

(* For system use only.  Don't call directly. *)
type sz;;

external sz_of_int : int -> sz = "%identity";;
external int_of_sz : sz -> int = "%identity";;

val scan_format : string -> 'a array -> sz -> int ->
  (sz -> string -> int -> 'b) ->
  (sz -> 'c -> 'd -> int -> 'b) ->
  (sz -> 'e -> int -> 'b) ->
  (sz -> int -> 'b) ->
  (sz -> ('h, 'i, 'j, 'k) format4 -> int -> 'b) -> 'b

val sub_format :
  (string -> int) -> (string -> int -> char -> int) ->
  char -> string -> int -> int
val summarize_format_type : string -> string
val kapr : (string -> Obj.t array -> 'a) -> string -> 'a
