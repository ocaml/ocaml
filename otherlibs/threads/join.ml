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

let create_process f = ignore (Thread.create f ())

type argument

type status = int

type automaton = {
  mutable status : status ;
  queues : argument list array ;
  matches : (status * (unit -> unit)) array ;
} 

let id () = ()

let create_automaton nchans nmatches =
  {
    status = 0 ;
    queues = Array.create nchans (Obj.magic 0) ;
    matches = Array.create nmatches (0, id) ;
  } 



