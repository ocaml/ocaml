(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
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
   - [d], [i], [n], or [N]: convert an integer argument to signed decimal.
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
   - [F]: convert a floating-point argument in Caml syntax ([dddd.ddd]
     with a mandatory [.]).
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
   to specify the corresponding width or precision.
   
   Warning: if too few arguments are provided,
   for instance because the [printf] function is partially
   applied, the format is immediately printed up to
   the conversion of the first missing argument; printing
   will then resume when the missing arguments are provided.
   For example, [List.iter (printf "x=%d y=%d " 1) [2;3]]
   prints [x=1 y=2 3] instead of the expected
   [x=1 y=2 x=1 y=3].  To get the expected behavior, do
   [List.iter (fun y -> printf "x=%d y=%d " 1 y) [2;3]]. *)

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

val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** [kprintf k format arguments] is the same as [sprintf format arguments],
    except that the resulting string is passed as argument to [k]; the
    result of [k] is then returned as the result of [kprintf]. *)

(**/**)

(* For system use only.  Don't call directly. *)

val scan_format :
  string -> int -> (string -> int -> 'a) -> ('b -> 'c -> int -> 'a) ->
    ('e -> int -> 'a) -> (int -> 'a) -> 'a
