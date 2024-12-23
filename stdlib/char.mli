(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Character operations.

    Characters are the elements of [string] and [bytes]
    values. Characters represent bytes, that is an integer in the
    range \[[0x00];[0xFF]\].

    Some of the functions of this module interpret the characters in
    the range \[[0x00];[0x7F]\] as the characters of the ASCII character
    set. *)

(** {1:characters Characters} *)

type t = char
(** An alias for the type of characters. *)

external code : char -> int = "%identity"
(** Return the integer code of the argument. *)

val chr : int -> char
(** Return the character with the given integer code.
   @raise Invalid_argument if the argument is
   outside the range \[[0x00];[0xFF]\]. *)

val escaped : char -> string
(** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml.
    All characters outside the ASCII printable range \[[0x20];[0x7E]\] are
    escaped, as well as backslash, double-quote, and single-quote. *)

(** {1:predicates Predicates and comparisons}

    See also the {!Ascii} module. *)

val compare: t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Stdlib.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for chars.
    @since 4.03 *)

(** {1:ascii_characters ASCII characters} *)

(** ASCII character set support.

    These functions give meaning to the integers \[[0x00];[0x7F]\] of the
    {{:https://en.wikipedia.org/wiki/ASCII#Character_set}ASCII
    character set}.

    Since the UTF-8 encoding of Unicode has the same encoding and
    character semantics (U+0000 to U+001F) for these bytes, the
    functions can be safely used on elements of UTF-8 encoded [string]
    and [bytes] values. However the functions only deal with ASCII
    related matters. For example the notion of Unicode whitespace is
    much larger than the ASCII whitespace determined by
    {!Char.Ascii.is_white}.

    @since 5.4 *)
module Ascii : sig

  (** {1:characters Characters} *)

  val min : char
  (** [min] is ['\x00']. *)

  val max : char
  (** [max] is ['\x7F']. *)

  (** {1:predicates Predicates} *)

  val is_valid : char -> bool
   (** [is_valid c] is [true] if and only if [c] is an ASCII character,
       that is a byte in the range \[{!min};{!max}\]. *)

  val is_upper : char -> bool
  (** [is_upper c] is [true] if and only if [c] is an ASCII uppercase letter
      ['A'] to ['Z'], that is a byte in the range \[[0x41];[0x5A]\]. *)

  val is_lower : char -> bool
  (** [is_lower c] is [true] if and only if [c] is an ASCII lowercase letter
      ['a'] to ['z'], that is a byte in the range \[[0x61];[0x7A]\]. *)

  val is_letter : char -> bool
  (** [is_letter c] is {!is_lower}[ c || ]{!is_upper}[ c]. *)

  val is_alphanum : char -> bool
  (** [is_alphanum c] is {!is_letter}[ c || ]{!is_digit}[ c]. *)

  val is_white : char -> bool
  (** [is_white c] is [true] if and only if [c] is an ASCII white space
      character, that is one of
      tab ['\t'] ([0x09]), newline ['\n'] ([0x0A]),
      vertical tab ([0x0B]), form feed ([0x0C]),
      carriage return ['\r'] ([0x0D]) or space [' '] ([0x20]),  *)

  val is_blank : char -> bool
  (** [is_blank c] is [true] if and only if [c] is an ASCII blank character,
      that is either space [' '] ([0x20]) or tab ['\t'] ([0x09]). *)

  val is_graphic : char -> bool
  (** [is_graphic c] is [true] if and only if [c] is an ASCII graphic
      character, that is a byte in the range \[[0x21];[0x7E]\]. *)

  val is_print : char -> bool
  (** [is_print c] is {!is_graphic}[ c || c = ' ']. *)

  val is_control : char -> bool
  (** [is_control c] is [true] if and only if [c] is an ASCII control character,
      that is a byte in the range \[[0x00];[0x1F]\] or [0x7F]. *)

  (** {1:decimal_digits Decimal digits} *)

  val is_digit : char -> bool
  (** [is_digit c] is [true] if and only if [c] is an ASCII decimal digit
      ['0'] to ['9'], that is a byte in the range \[[0x30];[0x39]\]. *)

  val digit_to_int : char -> int
  (** [digit_to_int c] is the numerical value of a digit
      that satisfies {!is_digit}. Raises [Invalid_argument] if
      {!is_digit}[ c] is [false]. *)

  val digit_of_int : int -> char
  (** [digit_of_int n] is an ASCII decimal digit for the decimal
      value [abs (n mod 10)]. *)

  (** {1:hex_digits Hexadecimal digits} *)

  val is_hex_digit : char -> bool
  (** [is_hex_digit c] is [true] if and only if [c] is an ASCII hexadecimal
      digit ['0'] to ['9'], ['a'] to ['f'] or ['A'] to ['F'],
      that is a byte in one of the ranges \[[0x30];[0x39]\],
      \[[0x41];[0x46]\], \[[0x61];[0x66]\]. *)

  val hex_digit_to_int : char -> int
  (** [hex_digit_to_int c] is the numerical value of a digit that
      satisfies {!is_hex_digit}. Raises [Invalid_argument] if
      {!is_hex_digit}[ c] is [false]. *)

  val lower_hex_digit_of_int : int -> char
  (** [lower_hex_digit_of_int n] is a lowercase ASCII hexadecimal digit for
      the hexadecimal value [abs (n mod 16)]. *)

  val upper_hex_digit_of_int : int -> char
  (** [upper_hex_digit_of_int n] is an uppercase ASCII hexadecimal
      digit for the hexadecimal value [abs (n mod 16)]. *)

  (** {1:casing Casing transforms} *)

  val uppercase : char -> char
  (** [uppercase c] is [c] with ASCII characters ['a'] to ['z'] respectively
      mapped to uppercase characters ['A'] to ['Z']. Other characters are left
      untouched. *)

  val lowercase : char -> char
  (** [lowercase c] is [c] with ASCII characters ['A'] to ['Z'] respectively
      mapped to lowercase characters ['a'] to ['z']. Other characters are
      left untouched. *)
end

val lowercase_ascii : char -> char
(** Use the equivalent {!Ascii.lowercase} instead.
    @since 4.03 *)

val uppercase_ascii : char -> char
(** Use the equivalent {!Ascii.uppercase} instead.
    @since 4.03 *)

(** {1:hashing Hashing} *)

val seeded_hash : int -> t -> int
(** A seeded hash function for characters, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}.

    @since 5.1 *)

val hash : t -> int
(** An unseeded hash function for characters, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}.

    @since 5.1 *)

(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_chr : int -> char = "%identity"
