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

type t = textloc

(** The types and functions below are documented in {!mod:Textloc}. *)

val file_none : string

type byte_pos = int
type line_num = int
type line_pos = line_num * byte_pos

val none : t
val make :
  file:string -> first_byte:byte_pos -> last_byte:byte_pos ->
  first_line:line_pos -> last_line:line_pos -> t
val byte_pos_none : byte_pos
val line_num_none : line_num
val line_pos_first : line_pos
val line_pos_none : line_pos

(** {2 Predicates and comparisons} *)

val is_none : t -> bool
val is_empty : t -> bool
val equal : t -> t -> bool
val compare : t -> t -> int

(** {2 Inspecting and updating} *)

val file : t -> string
val set_file : t -> string -> t
val first_byte : t -> byte_pos
val last_byte : t -> byte_pos
val first_line : t -> line_pos
val last_line : t -> line_pos

type line_range =
  | Single_line of line_pos
  | Range of line_pos * line_pos

val line_range : t -> line_range
val set_first : t -> first_byte:byte_pos -> first_line:line_pos -> t
val set_last : t -> last_byte:byte_pos -> last_line:line_pos -> t
val to_first : t -> t
val to_last : t -> t
val before : t -> t
val after : t -> t
val span : t -> t -> t
val reloc : first:t -> last:t -> t

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
