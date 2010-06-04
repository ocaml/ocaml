(***********************************************************************)
(*                                                                     *)
(*              LablTk, Tcl/Tk interface of Objective Caml             *)
(*                                                                     *)
(*         Jacques Garrigue, Nagoya University Mathematics Dept.       *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Basic functions *)

(** Start the main loop in a new GUI thread. Do not use recursively. *)
val start : unit -> Thread.t
(** The actual function executed in the new thread *)
val thread_main : unit -> unit
(** The toplevel widget (an alias of [Widget.default_toplevel]) *)
val top : Widget.toplevel Widget.widget

(* Jobs are needed for Windows, as you cannot do GUI work from
   another thread.
   Even under Unix some calls need to come from the main thread.
   The basic idea is to either use async (if you don't need a result)
   or sync whenever you call a Tk related function from another thread
   (for instance with the threaded toplevel).
   With sync, beware of deadlocks!
*)

(** Add an asynchronous job (to do in the main thread) *)
val async : ('a -> unit) -> 'a -> unit
(** Add a synchronous job (to do in the main thread) *)
val sync : ('a -> 'b) -> 'a -> 'b
(** Whether it is safe to call most Tk functions directly from
    the current thread *)
val gui_safe : unit -> bool
