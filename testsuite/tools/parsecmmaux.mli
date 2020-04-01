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

(* Auxiliary functions for parsing *)

val bind_ident: string -> Backend_var.With_provenance.t
val find_ident: string -> Backend_var.t
val unbind_ident: Backend_var.With_provenance.t -> unit

val find_label: string -> int

val debuginfo: ?loc:Location.t -> unit -> Debuginfo.t

type error =
    Unbound of string

exception Error of error

val report_error: error -> unit
