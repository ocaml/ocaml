(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

(* Module viewer *)
open Widget

val search_symbol : unit -> unit
        (* search a symbol in all modules in the path *)

val f : ?dir:string -> ?on:toplevel widget -> unit -> unit
        (* open then module viewer *)

val view_defined : Longident.t -> env:Env.t -> unit
        (* displays a signature, found in environment *)

val close_all_views : unit -> unit
