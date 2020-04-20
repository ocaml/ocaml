(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                                                                        *)
(*   Copyright 2016 Stephen Dolan.                                        *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Instrumentation for afl-fuzz. *)

val instrument_function : Cmm.expression -> Debuginfo.t -> Cmm.expression
val instrument_initialiser
   : Cmm.expression
  -> (unit -> Debuginfo.t)
  -> Cmm.expression
