(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Join_types

val create_process : (unit -> unit) -> unit
val create_real_process : (unit -> unit) -> Thread.t

val incr_active : unit -> unit

val tasks_status : unit -> string

val inform_suspend : unit -> unit
val inform_unsuspend : unit -> unit

(* Important: continuation mutex must be locked before call *)
val suspend_for_reply : continuation -> 'a

val reply_to : 'a -> continuation -> unit
val reply_to_exn : exn -> continuation -> unit
