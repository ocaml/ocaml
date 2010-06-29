(***********************************************************************)
(*                                                                     *)
(*                           JoCaml                                    *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Convenience functions for forking Unix commands.

  All functions provided by this module
  fork commands given in the style
  of the {!Unix.execvp} function.
  That is, a command is a program name plus an array
  of command line arguments and the program name is searched
  in path.

@see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html#VALexecvp> [Unix.execvp]. *)

(**
  The functions provided by this module
  return the pid of the child process that executes
  the program, plus some I/O channels, exactly which ones depends
  on the function.

  The functions of module [JoinProc] are to be used in place of
  the homonymous ones of module [Unix], which are {i not} thread safe.
*)



val command : string -> string array -> int
(** [command prog args] executes program [prog] with arguments [args]
   in a child process.
   Standard channels stdin, stdout, and stderr are the ones
   of the parent process *)

val open_in : string -> string array -> int * in_channel
(** Same as {!command} above, except that the forked process
    standard output is redirected to a pipe, which can be read
    via the returned input channel. *)
    
val open_out : string -> string array -> int * out_channel
(** Same as {!command} above, except that the forked process
    standard input is redirected to a pipe, which can be written to
    via the returned output channel. *)

val open_in_out : string -> string array -> int * (in_channel * out_channel)
(** Redirects both standard output and input of the forked command
    to pipes.
    Returns [pid,(outch,inch)], where [outch] is for reading the forked
    command standard output, and [inch] is for writing the forked command
    standard input *)

val open_full : string -> string array -> int * (in_channel * out_channel * in_channel)
(** Redirects all three standard channels of the forked command
    to pipes.
    Returns [pid,(outch,inch,errch)], where [outch] and [errch] permit reading
    the forked command standard output and standard error respectively,
    while [inch] permits writing on the forked command standard input. *)
