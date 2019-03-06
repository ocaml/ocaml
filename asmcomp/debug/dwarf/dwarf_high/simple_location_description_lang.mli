(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Higher-level representation of simple location descriptions than that
    provided by the DWARF standard.  This representation is compiled down
    to [Simple_location_description.t] values.

    Functions in this interface are used to build descriptions that enable the
    debugger to know what value a particular variable has at runtime.  These
    descriptions are categorised according to their surrounding context:

    - [Lvalue] descriptions are used when the context needs to know the _place_
    where a particular variable is stored.  Typically such a description is
    some address in the target's memory or the name of some register in the
    target's CPU.

    - [Rvalue] descriptions are used when the context requires the _value_ of
    a particular variable rather than a description of the place where it lives.
    This is usually the case when the description is being used as some
    sub-expression of a larger DWARF expression.

    Sometimes the context requires that the address of a value has to be given,
    but such address cannot be computed, for example because the value has been
    entirely optimised out.  However we may well know what the value itself is
    (or was).  In these cases we use [Lvalue_without_address] semantics
    (corresponding to DWARF "implicit location descriptions").  These enable
    rvalues to be converted into descriptions that may be used in lvalue
    context; and to describe pointers that have been entirely optimised out.

    The descriptions of the functions are written in terms of some fictional
    value V and time T. The time is when the simple location description is
    being evaluated in the debugger to inspect V.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

type lvalue
type lvalue_without_address
type normal
type implicit
type 'a rvalue

(** "A piece or all of an object that is present in the source but not in
    the object code" (DWARF-4 standard 2.6.1.1.4). *)
val empty : t

module Lvalue : sig
  type t = lvalue

  (** V will be in the given register at time T. *)
  val in_register : dwarf_reg_number:int -> t

  (** The address of V will be that of the given stack slot at time T. *)
  val in_stack_slot : offset_in_words:Targetint.t -> t

  (** The address of V will be the address of the given field of the block
      whose address (expressed as an rvalue) is given by the provided simple
      location description at time T. *)
  val read_field : block:normal rvalue -> field:Targetint.t -> t

  (** The address of V will be the address of the given field of the given
      symbol at time T. *)
  val in_symbol_field : Asm_symbol.t -> field:Targetint.t -> t

  (** The address of V will be the supplied address at time T plus the
      [offset_in_words]. *)
  val offset_pointer : t -> offset_in_words:Targetint.t -> t

  (** The address (or register location) of V is found in the location given
      by evaluating the location description (which must yield an lvalue) in
      the DIE at the given [die_label].  (This is like a function call.) *)
  val location_from_another_die
     : die_label:Asm_label.t
    -> compilation_unit_header_label:Asm_label.t
    -> t
end

module Lvalue_without_address : sig
  type t = lvalue_without_address

  (** The address of V cannot be described, but the value of V is known;
      the supplied [rvalue] describes such value. *)
  val of_rvalue : normal rvalue -> t

  (** V is an optimized-out pointer to a value whose contents are given by
      evaluating the location description (which must yield an rvalue) in the
      DIE at the given [die_label]. *)
  val implicit_pointer
     : offset_in_bytes:Targetint.t
    -> die_label:Asm_label.t
    -> Dwarf_version.t
    -> t
end

module Rvalue : sig
  type 'a t = 'a rvalue

  (** V is the given constant integer.  (This is a raw bit pattern, nothing
      to do with OCaml tagging.) *)
  val signed_int_const : Targetint.t -> normal t

  (** V is the floating-point number whose bits are specified by the given
      64-bit integer. *)
  val float_const : Int64.t -> normal t

  (** V is the address of the given symbol (not the contents of memory
      pointed to by the given symbol). *)
  val const_symbol : Asm_symbol.t -> normal t

  (** V will be the contents of the given register at time T. *)
  val in_register : dwarf_reg_number:int -> normal t

  (** V will be the contents of the given stack slot at time T. *)
  val in_stack_slot : offset_in_words:Targetint.t -> normal t

  (** V will be the contents of the given field of the block whose location is
      given by the provided simple location description at time T. *)
  val read_field : block:normal t -> field:Targetint.t -> normal t

  (** V will be the contents of the given field of the given symbol at
      time T. *)
  val read_symbol_field : Asm_symbol.t -> field:Targetint.t -> normal t

  (** V will be found in the location given by evaluating the location
      description (which must yield an rvalue) in the DIE at the given
      [die_label].  (This is like a function call.) *)
  val location_from_another_die
     : die_label:Asm_label.t
    -> compilation_unit_header_label:Asm_label.t
    -> normal t

  (** V is an optimized-out pointer to a value whose contents are given by
      evaluating the location description (which must yield an rvalue) in the
      DIE at the given [die_label].

      The resulting description cannot take part in any further location
      computations.  The type parameter statically ensures this. *)
  val implicit_pointer
     : offset_in_bytes:Targetint.t
    -> die_label:Asm_label.t
    -> Dwarf_version.t
    -> implicit t
end

(** Create a high-level location description from an lvalue description. *)
val of_lvalue : Lvalue.t -> t

(** Create a high-level location description from an lvalue-without-address
    description. *)
val of_lvalue_without_address : Lvalue_without_address.t -> t

(** Create a high-level location description from an rvalue description. *)
val of_rvalue : _ Rvalue.t -> t

(** Transform a high-level location description into a stream of DWARF
    operators forming a DWARF simple location description. *)
val compile : t -> Simple_location_description.t
