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
   [arg1] to [argN] according to the format string [format], and
   outputs the resulting string on the channel [outchan].

   The format is a character string which contains two types of
   objects: plain characters, which are simply copied to the output
   channel, and conversion specifications, each of which causes
   conversion and printing of arguments.

   Conversion specifications have the following form:

   [% \[flags\] \[width\] \[.precision\] type]

   In short, a conversion specification consists in the [%] character,
   followed by optional modifiers and a type which is made of one or
   two characters. The types and their meanings are:

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
   - [a]: user-defined printer. Takes two arguments and applies the
     first one to [outchan] (the current output channel) and to the
     second argument. The first argument must therefore have type
     [out_channel -> 'b -> unit] and the second ['b].
     The output produced by the function is inserted in the output of
     [fprintf] at the current point.
   - [t]: same as [%a], but takes only one argument (with type
     [out_channel -> unit]) and apply it to [outchan].
   - [\{ fmt %\}]: convert a format string argument. The argument must
     have the same type as the internal format string [fmt].
   - [( fmt %)]: format string substitution. Takes a format string
     argument and substitutes it to the internal format string [fmt]
     to print following arguments. The argument must have the same
     type as [fmt].
   - [!]: take no argument and flush the output.
   - [%]: take no argument and output one [%] character.
   - [,]: the no-op delimiter for conversion specifications.

   The optional [flags] are:
   - [-]: left-justify the output (default is right justification).
   - [0]: for numerical conversions, pad with zeroes instead of spaces.
   - [+]: for numerical conversions, prefix number with a [+] sign if positive.
   - space: for numerical conversions, prefix number with a space if positive.
   - [#]: request an alternate formatting style for numbers.

   The optional [width] is an integer indicating the minimal
   width of the result. For instance, [%6d] prints an integer,
   prefixing it with spaces to fill at least 6 characters.

   The optional [precision] is a dot [.] followed by an integer
   indicating how many digits follow the decimal point in the [%f],
   [%e], and [%E] conversions. For instance, [%.4f] prints a [float] with
   4 fractional digits.

   The integer in a [width] or [precision] can also be specified as
   [*], in which case an extra integer argument is taken to specify
   the corresponding [width] or [precision]. This integer argument
   precedes immediately the argument to print.
   For instance, [%.*f] prints a [float] with as many fractional
   digits as the value of the argument given before the float. *)

val printf : ('a, out_channel, unit) format -> 'a
(** Same as {!Printf.fprintf}, but output on [stdout]. *)

val eprintf : ('a, out_channel, unit) format -> 'a
(** Same as {!Printf.fprintf}, but output on [stderr]. *)

val ifprintf : 'a -> ('b, 'a, unit) format -> 'b
(** Same as {!Printf.fprintf}, but does not print anything.
    Useful to ignore some material when conditionally printing. *)

val sprintf : ('a, unit, string) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   return a string containing the result of formatting the arguments. *)

val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   append the formatted arguments to the given extensible buffer
   (see module {!Buffer}). *)

(** Formatted output functions with continuations. *)

val kfprintf : (out_channel -> 'a) -> out_channel ->
              ('b, out_channel, unit, 'a) format4 -> 'b;;
(** Same as [fprintf], but instead of returning immediately,
   passes the out channel to its first argument at the end of printing. *)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b;;
(** Same as [sprintf] above, but instead of returning the string,
   passes it to the first argument. *)

val kbprintf : (Buffer.t -> 'a) -> Buffer.t ->
              ('b, Buffer.t, unit, 'a) format4 -> 'b;;
(** Same as [bprintf], but instead of returning immediately,
   passes the buffer to its first argument at the end of printing. *)

val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b;;
(** A deprecated synonym for [ksprintf]. *)

(**/**)

(* For system use only.  Don't call directly. *)

module CamlinternalPr : sig

  module Sformat : sig
    type index;;

    val index_of_int : int -> index;;
    external int_of_index : index -> int = "%identity";;
    external unsafe_index_of_int : int -> index = "%identity";;

    val succ_index : index -> index;;

    val sub : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> index -> int -> string;;
    val to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string;;
    external length : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int
      = "%string_length";;
    external get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
      = "%string_safe_get";;
    external unsafe_to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
      = "%identity";;
    external unsafe_get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
      = "%string_unsafe_get";;

  end;;

  module Tformat : sig

    type ac = {
      mutable ac_rglr : int;
      mutable ac_skip : int;
      mutable ac_rdrs : int;
    };;

    val ac_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ac;;

    val sub_format :
        (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int) ->
        (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char -> int) ->
        char ->
        ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
        int ->
        int

    val summarize_format_type : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string

    val scan_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
        'g array ->
        Sformat.index ->
        int ->
        (Sformat.index -> string -> int -> 'h) ->
        (Sformat.index -> 'i -> 'j -> int -> 'h) ->
        (Sformat.index -> 'k -> int -> 'h) ->
        (Sformat.index -> int -> 'h) ->
        (Sformat.index -> ('l, 'm, 'n, 'o, 'p, 'q) format6 -> int -> 'h) ->
        'h

    val kapr :
        (('a, 'b, 'c, 'd, 'e, 'f) format6 -> Obj.t array -> 'g) ->
        ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
        'g

  end;;

end;;
