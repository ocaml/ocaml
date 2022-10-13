(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of sections in bytecode executable files *)


module Name : sig

  type raw_name = private string

  type t =
    | CODE (** bytecode *)
    | CRCS (** crcs for modules *)
    | DATA (** global data (constant) *)
    | DBUG (** debug info *)
    | DLLS (** dll names *)
    | DLPT (** dll paths *)
    | PRIM (** primitives names *)
    | RNTM (** The path to the bytecode interpreter (use_runtime mode) *)
    | SYMB (** global identifiers *)
    | Other of raw_name

  val of_string : string -> t
  (** @raise Invalid_argument if the input is not of size 4 *)

  val to_string : t -> string
end

(** Recording sections written to a bytecode executable file *)

type toc_writer

val init_record: out_channel -> toc_writer
(** Start recording sections from the current position in out_channel *)

val record: toc_writer -> Name.t -> unit
(** Record the current position in the out_channel as the end of
    the section with the given name. *)

val write_toc_and_trailer: toc_writer -> unit
(** Write the table of contents and the standard trailer for bytecode
    executable files *)

(** Reading sections from a bytecode executable file *)

type section_entry = {
  name : Name.t; (** name of the section. *)
  pos  : int;    (** byte offset at which the section starts. *)
  len  : int;    (** length of the section. *)
}

type section_table

exception Bad_magic_number

val read_toc: in_channel -> section_table
(** Read the table of sections from a bytecode executable.
    Raise [Bad_magic_number] if magic number doesn't match *)

val seek_section: section_table -> in_channel -> Name.t -> int
(** Position the input channel at the beginning of the section named "name",
    and return the length of that section.  Raise Not_found if no
    such section exists. *)

val read_section_string: section_table -> in_channel -> Name.t -> string
(** Return the contents of a section, as a string. *)

val read_section_struct: section_table -> in_channel -> Name.t -> 'a
(** Return the contents of a section, as marshalled data. *)

val all : section_table -> section_entry list
(** Returns all [section_entry] from a [section_table] in increasing
   position order. *)

val pos_first_section : section_table -> int
(** Return the position of the beginning of the first section *)
