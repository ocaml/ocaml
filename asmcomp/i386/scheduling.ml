# 2 "asmcomp/i386/scheduling.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The "open!" directive below is necessary because, although
   this module does not actually depend on Schedgen in this backend, the
   dependency exists in other backends and our build system requires
   that all the backends have the same dependencies.
   We thus have to use "open!" and disable the corresponding warning
   only for this compilation unit.
*)

open! Schedgen [@@warning "-66"]

(* Scheduling is turned off because our model does not fit the 486
   nor the Pentium very well. In particular, it messes up with the
   float reg stack. The Pentiums Pro / II / III / etc schedule
   at run-time much better than what we could do. *)

let fundecl f = f
