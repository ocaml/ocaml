(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Coalesce labels that point at the same source location.
    This avoids generating DWARF information that appears to reference
    different ranges of code but actually points at the same place.
    In particular, generating duplicate lexical blocks at the same position
    causes some of their local variables to become hidden in gdb.
    This pass also produces tidier assembly output.
*)

val fundecl
   : Linearize.fundecl
  -> int Numbers.Int.Map.t * Linearize.fundecl
