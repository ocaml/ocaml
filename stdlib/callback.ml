(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Registering Caml values with the C runtime for later callbacks *)

external register_named_value: string -> Obj.t -> unit = "register_named_value"

let register name v =
  register_named_value name (Obj.repr v)

let register_exception name (exn: exn) =
  register_named_value name (Obj.field (Obj.repr exn) 0)
