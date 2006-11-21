(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*
  mutable sets with readers/writer protection
*)

open Join_misc

type 'a t = {
  mutable xs : 'a list ;
  mtx : Mutex.t ;
} 

let create () = { xs = [] ; mtx = Mutex.create () ; }
and singleton  x = { xs = [x] ; mtx = Mutex.create () ; }
and from_list xs = { xs = xs ; mtx = Mutex.create () ; }

let add t x =
  Mutex.lock t.mtx ;
  if not (List.mem x t.xs) then t.xs <- x :: t.xs;
  Mutex.unlock t.mtx

let adds t xs =
  Mutex.lock t.mtx ;
  List.iter
    (fun x ->
      if not (List.mem x t.xs) then t.xs <- x :: t.xs)
    xs ;
  Mutex.unlock t.mtx
					       
let elements {xs=xs} = xs

