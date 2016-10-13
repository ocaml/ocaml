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

type t

include Dwarf_emittable.S with type t := t

val contents_of_register : reg_number:int -> t
val contents_of_stack_slot : offset_in_bytes:int -> t list
val value_of_symbol : Symbol.t -> t
val signed_int_const : Int64.t -> t
val add_unsigned_const : Int64.t -> t
val deref : unit -> t
val deref_do_not_optimize : unit -> t
val stack_value : unit -> t
val piece : size_in_bytes:int -> t

val drop : unit -> t
val dup : unit -> t
val swap : unit -> t
val nop : unit -> t

(** [conditional] tests the value at the top of the DWARF stack for equality
    with the integer constant zero.  If the test succeeds then [if_zero] will
    be executed, otherwise [if_nonzero]. *)
val conditional : if_zero:t list -> if_nonzero:t list -> t

val call
   : die_label:Cmm.label
  -> compilation_unit_header_label:Cmm.label
  -> t

val implicit_pointer : offset_in_bytes:int
  -> die_label:Cmm.label
  -> dwarf_version:Dwarf_version.t
  -> t

val optimize_sequence : t list -> t list

val print : Format.formatter -> t -> unit
