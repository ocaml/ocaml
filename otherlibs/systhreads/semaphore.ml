(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Xavier Leroy and Damien Doligez, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Semaphore]: semaphores to synchronize between threads *)

(* Semaphores are an alternative to mutexes and conditions for
   synchronizing the execution of several threads. *)

type t
external create: int -> t = "caml_semaphore_new"
external post: t -> unit = "caml_semaphore_post"
external wait: t -> unit = "caml_semaphore_wait"
external getvalue: t -> int = "caml_semaphore_getvalue"
