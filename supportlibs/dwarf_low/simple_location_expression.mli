(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR-soon mshinwell: Shouldn't this module be called
   [Simple_location_description]?  (DWARF-4 spec 2.6.1) *)

type t

include Dwarf_emittable.S with type t := t

module type S = sig
  type t

  val const_symbol : Symbol.t -> t
  val const_int : Int64.t -> t
  (* CR-soon mshinwell: consider a new type to identify whose register
     numbering is in use here *)
  val in_register : reg_number:int -> t
  val in_stack_slot : offset_in_words:int -> t
  val read_symbol_field : symbol:Symbol.t -> field:int -> t
  val read_field : t -> field:int -> t
  val offset_pointer : t -> offset_in_words:int -> t
end

include S with type t := t
