(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** First-in first-out queues.

   This module implements queues (FIFOs), with in-place modification.
*)

(** The type of queues containing elements of type ['a]. *)
type 'a t


(** Raised when {!Queue.take} or {!Queue.peek} is applied to an empty queue. *)
exception Empty


(** Return a new queue, initially empty. *)
val create: unit -> 'a t

(** [add x q] adds the element [x] at the end of the queue [q]. *)
val add: 'a -> 'a t -> unit

(** [take q] removes and returns the first element in queue [q],
   or raises [Empty] if the queue is empty. *)
val take: 'a t -> 'a

(** [peek q] returns the first element in queue [q], without removing
   it from the queue, or raises [Empty] if the queue is empty. *)
val peek: 'a t -> 'a

(** Discard all elements from a queue. *)
val clear: 'a t -> unit

(** Return the number of elements in a queue. *)
val length: 'a t -> int

(** [iter f q] applies [f] in turn to all elements of [q],
   from the least recently entered to the most recently entered.
   The queue itself is unchanged. *)
val iter: ('a -> unit) -> 'a t -> unit

