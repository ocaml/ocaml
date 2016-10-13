(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A "proto-DIE" contains similar information to a DWARF debugging
    information entry (DIE), but contains the actual attributes whose values
    are being specified, rather than linking to them via an abbrevation
    code.  The abbreviation codes are assigned later, when the proto-DIEs
    are turned into DIEs.

    Proto-DIEs may be formed into the required tree structures using a
    simple parenting relationship.  The complexities of flattening the
    structure to the DWARF representation (Section 2.3, DWARF-4 spec) are
    hidden. *)

type t

(** For creation of proto-DIEs in a group, with references between them. *)
(* CR mshinwell: remove type equality *)
type reference = Cmm.label
val create_reference : unit -> reference

(* It is an error for [parent] to be [None] unless the [tag] is that for
   a compilation unit (which is a top-level entity). *)
val create
   : ?reference:reference
  -> parent:t option
  -> tag:Dwarf_tag.t
  -> attribute_values:Dwarf_attribute_values.Attribute_value.t list
  -> unit
  -> t

val create_ignore
   : parent:t option
  -> tag:Dwarf_tag.t
  -> attribute_values:Dwarf_attribute_values.Attribute_value.t list
  -> unit

val set_sort_priority : t -> int -> unit

(* CR-someday mshinwell: add a [name] argument to the creation functions *)
val set_name : t -> Symbol.t -> unit

(* [reference t] returns a label that may be used when constructing other
    attribute values. *)
(* CR-someday mshinwell: ideally, attribute values could accept proto-DIE
   values directly, but there is a circularity. *)
val reference : t -> Linearize.label

(* [depth_first_fold] traverses a proto-DIE tree in a depth-first order
   convenient for DWARF information emission.  (Section 2.3, DWARF-4 spec.)
   [`End_of_siblings] indicates that a chain of siblings---that is to say,
   proto-DIEs with the same parent and at the same tree depth as each other---
   has finished.  This should correspond exactly to the points at which a
   "chain of sibling entries [must be] terminated by a null entry" specified
   in the DWARF-4 spec.
*)
val depth_first_fold
   : t
  -> init:'a
  -> f:('a
    -> [ `DIE of Dwarf_tag.t * Child_determination.t
           * (Dwarf_attribute_values.Attribute_value.t list)
           * Linearize.label * Symbol.t option (* optional name *)
       | `End_of_siblings
       ]
    -> 'a)
  -> 'a
