(**************************************************************************)
(*        ^o3                                                             *)
(* ~/\_/\_|)                       OCaml                                  *)
(* |/=_=\|                                                                *)
(* "     "                                                                *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of keyboard interrupts *)

val protect : (unit -> unit) -> unit
val unprotect : (unit -> unit) -> unit
