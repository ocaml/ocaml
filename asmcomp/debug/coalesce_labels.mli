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

(** Coalesce labels that point at the same source location.  This ensures
    in particular that it is possible to detect whether the endpoints of
    available ranges coincide by just checking their label names.  This
    is not used in the compiler at present, but may be in the future if
    support for generating DWARF lexical blocks is reinstated.  (Duplicate
    lexical blocks at the same position causes variables to be hidden in
    gdb.)
    In the meantime, this pass also produces substantively tidier assembly
    output and intermediate code.
*)

val fundecl
   : Linearize.fundecl
  -> int Numbers.Int.Map.t * Linearize.fundecl
