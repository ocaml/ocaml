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

(* Convert environment summaries to environments *)

val env_of_event: Instruct.debug_event option -> Env.t

(* Empty the environment caches. To be called when load_path changes. *)

val reset_cache: unit -> unit

(* Error report *)

type error =
    Module_not_found of Path.t

exception Error of error

val report_error: error -> unit
