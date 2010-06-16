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

let verbose = -1

(*DEBUG*)let verbose =
(*DEBUG*)  try int_of_string (Sys.getenv "JOVERBOSE") with | _ -> -1

let debug_mutex = Mutex.create ()

let do_nothing source fmt =  ksprintf (fun _ -> ()) fmt

let do_something source fmt =
   ksprintf
    (fun s ->
      Mutex.lock debug_mutex ;
      eprintf "%s[%i]: " source (Thread.id (Thread.self ())) ;
      prerr_endline s ;
      Mutex.unlock debug_mutex) fmt

let do_something_no_id source fmt =
   ksprintf
    (fun s ->
      Mutex.lock debug_mutex ;
      eprintf "%s: " source ;
      prerr_endline s ;
      Mutex.unlock debug_mutex) fmt

let debug_lvl lvl source fmt = do_nothing source fmt

(*DEBUG*)let debug_lvl lvl src fmt =
(*DEBUG*)  if verbose >= lvl then begin
(*DEBUG*)    do_something src fmt
(*DEBUG*)  end else
(*DEBUG*)    do_nothing src fmt

let debug src fmt = do_something_no_id src fmt
let debug0 src fmt = debug_lvl 0 src fmt
and debug1 src fmt = debug_lvl 1 src fmt
and debug2 src fmt = debug_lvl 2 src fmt
and debug3 src fmt = debug_lvl 3 src fmt
