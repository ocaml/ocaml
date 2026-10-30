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
  (** The type for one-based, line numbers in the text.  *)

  type line_pos = line_num * byte_pos
  (** The type for line positions. This identifies a line by its line
      number and the absolute byte position of the first byte of text
      of the line. *)

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

  val file : t -> string

  val first_byte : t -> byte_pos
  val last_byte : t -> byte_pos

  val first_line : t -> line_pos
  val last_line : t -> line_pos
end

include S with type t = textloc

type view =
  { file : string;
    first_byte : byte_pos; last_byte : byte_pos;
    first_line : line_pos; last_line : line_pos }

val view : t -> view

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
