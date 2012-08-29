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

open Join_types

type 'a chan = 'a async

exception Exit = Join_misc.JoinExit

def exception Exit
def exception Not_found (* Name service *)
def exception Match_failure
def exception Assert_failure
def exception Invalid_argument
def exception Failure
def exception Out_of_memory
def exception Stack_overflow
def exception Sys_error
def exception End_of_file
def exception Division_by_zero
def exception Sys_blocked_io
def exception Undefined_recursive_module


let exit_hook = Join_scheduler.exit_hook


(* Debug from users programs *)
type 'a debug = string -> (('a, unit, string, unit) format4 -> 'a)

let debug = Join_debug.debug


module Site = Site
module Ns = Ns
