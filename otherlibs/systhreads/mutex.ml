(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*          Xavier Leroy and Pascal Cuoq, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t
external create: unit -> t = "csl_mutex_new"
external lock: t -> unit = "csl_mutex_lock"
external try_lock: t -> bool = "csl_mutex_try_lock"
external unlock: t -> unit = "csl_mutex_unlock"
