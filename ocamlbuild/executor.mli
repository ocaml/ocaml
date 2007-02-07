(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Berke Durak *)
(* Executor *)

(** UNIX-specific module for running tasks in parallel and properly multiplexing their outputs. *)

(** [execute ~ticker ~period ~display commands] will execute the commands in [commands]
    in parallel, correctly multiplexing their outputs.  A command is a pair [(cmd, action)]
    where [cmd] is a shell command string, and [action] is a thunk that is to be called just
    before [cmd] is about to be executed.  If specified, it will call [ticker] at least every [period]
    seconds.  If specified, it will call [display f] when it wishes to print something;
    [display] should then call [f] with then channel on which [f] should print.
    Note that [f] must be idempotent as it may well be called twice, once for the log file,
    once for the actual output.
    If one of the commands fails, it will exit with an appropriate error code,
    calling [cleanup] before.
*)
val execute :
  ?max_jobs:int ->
  ?ticker:(unit -> unit) ->
  ?period:float ->
  ?display:((out_channel -> unit) -> unit) ->
    ((string * (unit -> unit)) list list) ->
    (bool list * exn) option
