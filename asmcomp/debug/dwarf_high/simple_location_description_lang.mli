(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Higher-level representation of simple location descriptions than that
    provided by the DWARF standard.  This representation is compiled down
    to [Simple_location_description.t] values.

    Functions in this interface are used to build descriptions that enable
    the debugger to know what value a particular variable has at runtime.
    This is done by either giving the value itself ("rvalue" semantics) or
    explaining how to compute the address in the target's memory where the
    value may be found ("lvalue" semantics).  Which of these semantics is
    appropriate depends on the context in which the simple location
    description is being used.  (For example see the DWARF-4 standard
    section 2.6.1.1.3 bullet point 2.)

    The descriptions of the functions are written in terms of some fictional
    value V and time T.  The time is when the simple location description is
    being evaluated in the debugger to inspect V.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

type lvalue

type normal
type last
type 'rvalue_kind rvalue

(** "A piece or all of an object that is present in the source but not in
    the object code" (DWARF-4 standard 2.6.1.1.4). *)
val empty : t

module Lvalue : sig
  type t = lvalue

  (** V will be in the given register at time T. *)
  val in_register : dwarf_reg_number:int -> t

  (** V will be in the given stack slot at time T. *)
  val in_stack_slot : offset_in_words:Targetint.t -> t

  (** V will be at the address of the given symbol. *)
  val const_symbol : symbol:string -> t

  (** V will be in the given field of the given symbol at time T. *)
  val in_symbol_field : symbol:string -> field:Targetint.t -> t

  (** V is found in the location given by evaluating the location description
      (which must yield an lvalue) in the DIE at the given [die_label]. *)
  val location_from_another_die
     : die_label:Linearize.label
    -> compilation_unit_header_label:Linearize.label
    -> t
end

module Rvalue : sig
  type 'a t = 'a rvalue

  (** V is the given constant integer.  (This is a raw bit pattern, nothing
      to do with OCaml tagging.) *)
  val signed_int_const : Targetint.t -> normal t

  (** V will be in the given register at time T. *)
  val in_register : dwarf_reg_number:int -> normal t

  (** V will be in the given stack slot at time T. *)
  val in_stack_slot : offset_in_words:Targetint.t -> normal t

  (** V will be in the given field of the block whose location is given by
      the provided simple location description at time T.
      An exception will be raised if the supplied [t] was constructed using
      [implicit_pointer]. *)
  val read_field : block:normal t -> field:Targetint.t -> normal t

  (** V will be in the given field of the given symbol at time T. *)
  val read_symbol_field : symbol:string -> field:Targetint.t -> normal t

  (** V will be at the given offset added to the value of the given simple
      location description at time T. *)
  val offset_pointer : lvalue -> offset_in_words:Targetint.t -> normal t

  (** V is an optimized-out pointer to a value whose contents are given by
      evaluating the location description (which must yield an rvalue) in the
      DIE at the given [die_label].

      The resulting rvalue cannot take part in any further location
      computations.  The type parameter statically ensures this. *)
  val implicit_pointer
     : offset_in_bytes:Targetint.t
    -> die_label:Linearize.label
    -> Dwarf_version.t
    -> last t

  (** V is found in the location given by evaluating the location description
      (which must yield an rvalue) in the DIE at the given [die_label]. *)
  val location_from_another_die
     : die_label:Linearize.label
    -> compilation_unit_header_label:Linearize.label
    -> normal t
end

(** Create a high-level location description from an lvalue description. *)
val of_lvalue : Lvalue.t -> t

(** Create a high-level location description from an rvalue description. *)
val of_rvalue : _ Rvalue.t -> t

(** Transform a high-level location description into a stream of DWARF
    operators forming a DWARF simple location description. *)
val compile : t -> Simple_location_description.t
