(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*****************)
(* Sender queues *) 
(*****************)

(*
   BEWARE: for wait_empty to work, there mute be exactly ONE thread
           performing get's (and clean ?)
*)

type 'a t

val create : unit -> 'a t
val put : 'a t -> 'a -> unit
val get : 'a t -> 'a
val wait_empty : 'a t -> unit
val clean : 'a t -> unit

