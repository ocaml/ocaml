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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Propagate debuginfo values from one instruction to the next to avoid
    holes in debugging information.  Whilst such holes are unproblematic for
    location generation, they cause problems for lexical block generation,
    with the potential to cause a poor user experience in the debugger. *)

val fundecl : Mach.fundecl -> Mach.fundecl
