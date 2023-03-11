# 2 "asmcomp/riscv/scheduling.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Nicolas Ojeda Bar <n.oje.bar@gmail.com>                 *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction scheduling for the RISC-V *)

(* The "open!" directive below is necessary because, although
   this module does not actually depend on Schedgen in this backend, the
   dependency exists in other backends and our build system requires
   that all the backends have the same dependencies.
   We thus have to use "open!" and disable the corresponding warning
   only for this compilation unit.
*)

open! Schedgen [@@warning "-66"]

(* Scheduling is turned off. *)

let fundecl f = f
