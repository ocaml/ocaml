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

(** Compilation unit header unit types (DWARF-5 spec section 7.5.1,
    page 199). *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Compile
  | Type
  | Partial
  | Skeleton
  | Split_compile
  | Split_type

include Dwarf_emittable.S with type t := t
