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
  { mutable cond_waiting: t list }

let new () = { cond_waiting = [] }

let wait cond lock =
  cond.cond_waiting <- Thread.self() :: cond.cond_waiting;
  Mutex.unlock lock;
  Thread.sleep()

let signal cond =
  match cond.cond_waiting with
    [] -> ()
  | pid :: rem -> Thread.wakeup pid; cond.cond_waiting <- rem

let broadcast cond =
  List.iter Thread.wakeup cond.cond_waiting;
  cond.cond_waiting <- []
