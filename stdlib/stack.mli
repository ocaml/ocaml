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

(** Last-in first-out stacks.

   This module implements stacks (LIFOs), with in-place modification. 
*)

(** The type of stacks containing elements of type ['a]. *)
type 'a t

(** Raised when {!Stack.pop} or {!Stack.top} is applied to an empty stack. *)
exception Empty


(** Return a new stack, initially empty. *)
val create: unit -> 'a t

(** [push x s] adds the element [x] at the top of stack [s]. *)
val push: 'a -> 'a t -> unit

(** [pop s] removes and returns the topmost element in stack [s],
   or raises [Empty] if the stack is empty. *)
val pop: 'a t -> 'a

(** [top s] returns the topmost element in stack [s],
   or raises [Empty] if the stack is empty. *)
val top: 'a t -> 'a

(** Discard all elements from a stack. *)
val clear: 'a t -> unit

(** Return a copy of the given stack. *)
val copy: 'a t -> 'a t

(** Return the number of elements in a stack. *)
val length: 'a t -> int

(** [iter f s] applies [f] in turn to all elements of [s],
   from the element at the top of the stack to the element at the
   bottom of the stack. The stack itself is unchanged. *)
val iter: ('a -> unit) -> 'a t -> unit

