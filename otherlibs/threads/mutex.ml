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

type t =
  { mutable locked: bool;
    mutable lock_waiting: t list }

(* We rely heavily on the fact that signals are detected only at
   function applications and beginning of loops, making all other operations
   atomic. *)

let new_lock () = { locked = false; lock_waiting = [] }

let rec lock l =
  if l.locked then begin
    l.lock_waiting <- Thread.self() :: l.lock_waiting;
    Thread.sleep();
    lock l
  end else begin
    l.locked <- true
  end

let try_lock l =
  if l.locked then false else begin l.locked <- true; true end

let unlock l =
  List.iter Thread.wakeup l.lock_waiting;
  l.locked <- false

