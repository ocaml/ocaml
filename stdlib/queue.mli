(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Queue]: first-in first-out queues *)

(* This module implements queues (FIFOs), with in-place modification. *)

type 'a t
        (* The type of queues containing elements of type ['a]. *)

exception Empty
        (* Raised when [take] is applied to an empty queue. *)

val new: unit -> 'a t
        (* Return a new queue, initially empty. *)
val add: 'a -> 'a t -> unit
        (* [add x q] adds the element [x] at the end of the queue [q]. *)
val take: 'a t -> 'a
        (* [take q] removes and returns the first element in queue [q],
           or raises [Empty] if the queue is empty. *)
val peek: 'a t -> 'a
        (* [peek q] returns the first element in queue [q], without removing
           it from the queue, or raises [Empty] if the queue is empty. *)
val clear : 'a t -> unit
        (* Discard all elements from a queue. *)
val length: 'a t -> int
        (* Return the number of elements in a queue. *)
val iter: ('a -> 'b) -> 'a t -> unit
        (* [iter f q] applies [f] in turn to all elements of [q],
           from the least recently entered to the most recently entered.
           The queue itself is unchanged. *)
