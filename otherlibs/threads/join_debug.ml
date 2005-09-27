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

(*DEBUG*)let verbose =
(*DEBUG*)  try int_of_string (Sys.getenv "VERBOSE") with | _ -> 0
(*DEBUG*)
(*DEBUG*)let debug_mutex = Mutex.create ()
(*DEBUG*)
let debug lvl source msg = ()

(*DEBUG*)let debug lvl source msg =
(*DEBUG*)  if verbose >= lvl then begin
(*DEBUG*)   Mutex.lock debug_mutex ;
(*DEBUG*)    eprintf "%s[%i]: %s\n" source (Thread.id (Thread.self ())) msg ;
(*DEBUG*)    flush stderr ;
(*DEBUG*)    Mutex.unlock debug_mutex
(*DEBUG*)  end
(*DEBUG*)
let debug0 = debug 0
let debug1 = debug 1
and debug2 = debug 2
and debug3 = debug 3
