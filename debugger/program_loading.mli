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

(*** Debugging. ***)

val debug_loading : bool ref

(*** Load program ***)

(* Function used for launching the program. *)
val launching_func : (unit -> unit) ref

val load_program : unit -> unit

type launching_function = (unit -> unit)

val loading_modes : (string * launching_function) list
val set_launching_function : launching_function -> unit

(** Connection **)
val connection : Primitives.io_channel ref
val connection_opened : bool ref
