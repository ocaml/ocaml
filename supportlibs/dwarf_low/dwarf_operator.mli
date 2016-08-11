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
val stack_value : unit -> t

val optimize_sequence : t list -> t list

val print : Format.formatter -> t -> unit
