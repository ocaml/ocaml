(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Coalesce labels that point at the same source location.  This maximises
    the chance that two labels which actually point at the same location can
    be identified as doing so by using a simple equality check.  This helps
    in particular with constructing the DWARF-5 .debug_addr section.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val fundecl
   : Linearize.fundecl
  -> int Numbers.Int.Map.t * Linearize.fundecl
