(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(**************************** Time travel ******************************)

open Primitives

exception Current_checkpoint_lost

val new_checkpoint : int -> io_channel -> unit
val set_file_descriptor : int -> io_channel -> bool
val kill_all_checkpoints : unit -> unit
val forget_process : io_channel -> int -> unit
val recover : unit -> unit

val go_to : int -> unit

val run : unit -> unit
val back_run : unit -> unit
val step : int -> unit
val finish : unit -> unit
val next : int -> unit
