(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of object file sections. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The linker may share constants in [Eight_byte_literals] and
    [Sixteen_byte_literals] sections. *)
type t =
  | Text
    (** The code section. *)
  | Data
    (** The read/write data section. *)
  | Read_only_data
    (** The read-only data section. *)
  | Eight_byte_literals
    (** A section containing eight-byte constants (for example unboxed floating
        point numbers). *)
  | Sixteen_byte_literals
    (** A section containing sixteen-byte numeric constants. *)
  | Jump_tables
    (** A section containing the code of jump tables. *)

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Concise description of values of type [t] for error messages, etc. *)
val to_string : t -> string

(** All sections that this code knows about. *)
val all_sections_in_order : unit -> t list

(** [section_is_text] returns [true] if the section holds code. *)
val section_is_text : t -> bool

(** Details about a specific section, used for assembly emission. *)
type flags_for_section = private {
  names : string list;
  (** The name of the section.  Some section names have multiple constituent
      parts. *)
  flags : string option;
  (** Target-dependent flags for the section. *)
  args : string list;
  (** Target-dependent arguments for the section. *)
}

(** The necessary information for an assembler's ".section" directive. *)
val flags : t -> flags_for_section
