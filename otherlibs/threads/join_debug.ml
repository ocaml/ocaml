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

open Printf

type 'a t = string -> (('a, unit, string, unit) format4 -> 'a)

(*DEBUG*)let verbose =
(*DEBUG*)  try int_of_string (Sys.getenv "VERBOSE") with | _ -> 0
(*DEBUG*)
(*DEBUG*)let debug_mutex = Mutex.create ()

let do_nothing source fmt =  ksprintf (fun _ -> ()) fmt

let debug lvl source fmt = do_nothing source fmt

(*DEBUG*)let do_something source fmt =
(*DEBUG*)  ksprintf
(*DEBUG*)    (fun s ->
(*DEBUG*)      Mutex.lock debug_mutex ;
(*DEBUG*)      eprintf "%s[%i]: " source (Thread.id (Thread.self ())) ;
(*DEBUG*)      prerr_endline s ;
(*DEBUG*)      Mutex.unlock debug_mutex) fmt
(*DEBUG*)  
(*DEBUG*)let debug lvl src fmt =
(*DEBUG*)  if verbose >= lvl then begin
(*DEBUG*)    do_something src fmt
(*DEBUG*)  end else
(*DEBUG*)    do_nothing src fmt

let debug0 src fmt = debug 0 src fmt
and debug1 src fmt = debug 1 src fmt
and debug2 src fmt = debug 2 src fmt
and debug3 src fmt = debug 3 src fmt
