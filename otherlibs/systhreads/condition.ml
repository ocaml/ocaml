(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*         Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t = { mut: Mutex.t; mutable waiting: Thread.t list }

let create () = 
  let m =
    try
      Mutex.create()
    with Sys_error _ ->
      raise(Sys_error "Condition.create") in
  { mut = m; waiting = [] }

let wait cond mut =
  Mutex.lock cond.mut;
  cond.waiting <- Thread.self() :: cond.waiting;
  Mutex.unlock cond.mut;
  Mutex.unlock mut;
  Thread.sleep();
  Mutex.lock mut

let signal cond = 
  Mutex.lock cond.mut;
  match cond.waiting with               
    [] -> Mutex.unlock cond.mut
  | th :: rem -> cond.waiting <- rem ; Mutex.unlock cond.mut; Thread.wakeup th

let broadcast cond =
  Mutex.lock cond.mut;
  let w = cond.waiting in
  cond.waiting <- [];
  Mutex.unlock cond.mut;
  List.iter Thread.wakeup w

