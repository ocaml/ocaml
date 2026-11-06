(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            Daniel C. Buenzli                           *)
(*               Olivier Nicole, projet Cambium, Inria Paris              *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module encodes and decodes locations that are compact. It is intended
    to be used as a backend for the standard {!Textloc.Compact} module, and by
    the compiler to generate locations in the code (typically, for implicit
    location arguments). *)

module type S = sig
  val file_none : string
  (** [file_none] is ["-"]. A file path to use when there is none. *)

  type byte_pos = int
  (** The type for zero-based, absolute, byte positions in text. If
      the text has [n] bytes, [0] is the first position and [n-1] is
      the last position. *)

  type line_num = int
  (** The type for one-based, line numbers in the text. Lines
      increment after a {e newline} which is either a line feed ['\n']
      (U+000A), a carriage return ['\r'] (U+000D) or a carriage return
      and a line feed ["\r\n"] (<U+000D,U+000A>). *)

  type line_pos = line_num * byte_pos
  (** The type for line positions. This identifies a line by its line
      number and the absolute byte position following its newline
      (or the start of text for the first line). That byte position:
      {ul
      {- Indexes the first byte of text of the line if the line is non-empty.}
      {- Indexes the first byte of the next {e newline} sequence if the line
         is empty.}
      {- Is out of bounds and equal to the text's length for a last empty
         line. This is also the case on empty text.}} *)

  type t
  (** The type for text locations. A text location identifies a text
      span in an UTF-8 encoded file by an inclusive range of absolute
      {{!type-byte_pos}byte positions} and the {{!type-line_pos}line positions}
      on which they occur. *)

  val none : t
  (** [none] is a position to use when there is none. *)

  val make :
    file:string -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> t
  (** [v ~file ~first_byte ~last_byte ~first_line ~last_line] is a text
      location with the given arguments, see corresponding accessors for
      the semantics. If you don't have a file use {!file_none}. *)

  val byte_pos_none : byte_pos
  (** [byte_pos_none] is [-1]. A position to use when there is none. *)

  val line_num_none : line_num
  (** [line_num_none] is [-1]. A line number to use when there is none. *)

  val line_pos_first : line_pos
  (** [line_pos_first] is [1, 0]. Note that this is the only line position
      of the empty text. *)

  val line_pos_none : line_pos
  (** [line_pos_none] is [(line_pos_none, pos_pos_none)]. *)

  (** {2:preds Predicates and comparisons} *)

  val is_none : t -> bool
  (** [is_none t] is [true] iff [first_byte t < 0]. *)

  val is_empty : t -> bool
  (** [is_empty t] is [true] iff [first_byte t > last_byte t]. *)

  val equal : t -> t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] are equal. This checks
      that {!file}, {!first_byte} and {!last_byte} are equal. Line information
      is ignored. *)

  val compare : t -> t -> int
  (** [compare t0 t1] orders [t0] and [t1]. The order is compatible
      with {!equal}. Comparison starts with {!file}, follows with
      {!first_byte} and ends, if needed, with {!last_byte}. Line
      information is ignored. *)

  (** {2:inspect_and_update Inspecting and updating} *)

  val file : t -> string
  (** [file l] is [l]'s file. *)

  val set_file : t -> string -> t
  (** [set_file l file] is [l] with {!file} set to [file]. *)

  val first_byte : t -> byte_pos
  (** [first_byte l] is [l]'s first byte. Irrelevant if {!is_none} is
      [true]. *)

  val last_byte : t -> byte_pos
  (** [last_byte l] is [l]'s last byte. Irrelevant if {!is_none} or
      {!is_empty} is [true]. *)

  val first_line : t -> line_pos
  (** [first_line l] is the line position on which [first_byte l] lies.
      Irrelevant if {!is_none} is [true].*)

  val last_line : t -> line_pos
  (** [last_line l] is the line position on which [last_byte l] lies.
      Irrelevant if {!is_none} or {!is_empty} is [true].*)

  type line_range =
    | Single_line of line_pos
    | Range of line_pos * line_pos
  (** Represents a line range, with a special case for single-line ranges. *)

  val line_range : t -> line_range
  (** [line_range l] is the range between [first_line l] and [last_line l].
      If [fst (first_line l) = fst (last_line l)], then [line_range l] is
      [Single_line (first_line l)]. *)

  val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
  (** [set_first l ~first_byte ~first_line] sets the the first position of
      [l] to given values. *)

  val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
  (** [set_last l ~last_byte ~last_line] sets the last position of [l]
      to given values. *)

  val to_first : t -> t
  (** [to_first l] has both first and last positions set to [l]'s first
      position. The range spans {!first_byte}. See also {!before}. *)

  val to_last : t -> t
  (** [to_last l] has both first and last positions set to [l]'s last
        position. The range spans {!last_byte}. See also {!after}. *)

  val before : t -> t
  (** [before t] is an {{!is_empty}empty} text location starting at
      {!first_byte}. *)

  val after : t -> t
  (** [after t] is an empty {{!is_empty}empty} location starting at
      [last_byte t + 1]; note that at the end of input this may be an
      invalid byte {e index}. The {!first_line} and {!last_line} of the
      result is [last_line t]. *)

  val span : t -> t -> t
  (** [span l0 l1] is the span from the smallest byte position of [l0] and
      [l1] to the largest byte position of [l0] and [l1]. The file path is
      taken from the greatest byte position. *)

  val reloc : first:t -> last:t -> t
  (** [reloc ~first ~last] uses the first position of [first], the
      last position of [last] and the file of [last]. *)
end

include S with type t = textloc

val of_lexing_range : Lexing.position -> Lexing.position -> t

module type Low_level = sig
  type repr
  val int : int -> repr
  val string : string -> repr
  val block : repr list -> repr
end

(** Translates locations into a low-level representation based on blocks
    containing integers and strings. This is intended to be used to emit
    locations in the form of [Lambda.lambda] constants. *)
module Lift_repr (Low_level : Low_level) : sig
  val make :
    file:string -> first_byte:byte_pos -> last_byte:byte_pos ->
    first_line:line_pos -> last_line:line_pos -> Low_level.repr
  val to_repr : t -> Low_level.repr
end
