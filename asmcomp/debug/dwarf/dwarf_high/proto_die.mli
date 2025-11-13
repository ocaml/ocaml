(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Prototype Debugging Information Entry (DIE).

    A proto_die represents a DIE before abbreviation codes are assigned.
    It contains all the information needed to construct a final DIE, including
    its tag, attributes, and child DIEs.

    This is the primary interface for constructing DWARF DIEs in a type-safe
    manner. *)

type t

(** Attribute specification for a DIE *)
type attribute = {
  attr : Dwarf_attributes.t;
  value : Dwarf_value.t;
  form : Dwarf_form.t;
}

(** Create an empty DIE with the given tag *)
val create : Dwarf_tag.t -> t

(** Add an attribute to a DIE *)
val add_attribute : t -> attribute -> t

(** Add a child DIE *)
val add_child : t -> t -> t

(** Add multiple children at once *)
val add_children : t -> t list -> t

(** Set whether this DIE has children *)
val set_has_children : t -> bool -> t

(** Get the tag of this DIE *)
val tag : t -> Dwarf_tag.t

(** Get all attributes *)
val attributes : t -> attribute list

(** Get all children *)
val children : t -> t list

(** Check if this DIE has children *)
val has_children : t -> bool

(** Helper: Create an attribute *)
val make_attribute :
  attr:Dwarf_attributes.t ->
  value:Dwarf_value.t ->
  form:Dwarf_form.t ->
  attribute

(** Helper: Add a name attribute *)
val with_name : t -> string -> t

(** Helper: Add a type attribute (reference to another DIE) *)
val with_type : t -> int -> t

(** Helper: Add a byte_size attribute *)
val with_byte_size : t -> int -> t

(** Helper: Add an encoding attribute *)
val with_encoding : t -> Dwarf_encoding.t -> t

(** Helper: Add a location attribute *)
val with_location : t -> bytes -> t

(** Helper: Add a low_pc and high_pc range *)
val with_pc_range : t -> start:Code_address.t -> end_:Code_address.t -> t

(** Helper: Add a const_value attribute *)
val with_const_value : t -> int -> t

(** Helper: Add a declaration flag *)
val with_declaration : t -> bool -> t

(** Helper: Add an external flag *)
val with_external : t -> bool -> t

(** Helper: Add an artificial flag (compiler-generated) *)
val with_artificial : t -> bool -> t

(** Helper: Create a variable DIE *)
val create_variable :
  name:string ->
  ?type_ref:int ->
  ?location:bytes ->
  ?is_parameter:bool ->
  ?is_artificial:bool ->
  unit ->
  t

(** Helper: Create a formal parameter DIE *)
val create_parameter :
  name:string ->
  ?type_ref:int ->
  ?location:bytes ->
  unit ->
  t

(** Pretty-printer *)
val print : Format.formatter -> t -> unit

(** Print with indentation for tree structure *)
val print_tree : Format.formatter -> t -> unit
