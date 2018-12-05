(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Management of the .debug_addr table (DWARF-5 spec section 7.2.7,
    page 241). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val create : unit -> t

(** [add ~adjustment t addr] adds to the table the address of the label [addr]
    (which in the assembly file is referenced from the [start_of_code_symbol])
    plus the [adjustment].  If the [adjustment] is omitted then it is taken
    to be zero.

    The returned address index may be used for referencing the address e.g. in
    a location list entry.
*)
val add
   : ?adjustment:int
  -> t
  -> start_of_code_symbol:Asm_symbol.t
  -> Asm_label.t
  -> Address_index.t

(** The label to be used as the value of the [DW_AT_base] attribute
    (DWARF-5 spec page 66 line 14). *)
val base_addr : t -> Asm_label.t

include Dwarf_emittable.S with type t := t
