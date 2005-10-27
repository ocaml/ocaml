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

(* Need different files in threads and systhreads
   since externals have different names ! *)

external thread_new : (unit -> unit) -> Thread.t = "caml_thread_new"
external thread_uncaught_exception : exn -> unit
  = "caml_thread_uncaught_exception"
