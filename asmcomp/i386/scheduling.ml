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

open! Schedgen (* to create a dependency *)

(* Scheduling is turned off because our model does not fit the 486
   nor the Pentium very well. In particular, it messes up with the
   float reg stack. The Pentiums Pro / II / III / etc schedule
   at run-time much better than what we could do. *)

let fundecl f = f
