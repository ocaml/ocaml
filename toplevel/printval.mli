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

(* Printing of values *)

open Types

val print_exception: Obj.t -> unit
val print_value: Env.t -> Obj.t -> type_expr -> unit

val install_printer : Path.t -> Types.type_expr -> (Obj.t -> unit) -> unit
val remove_printer : Path.t -> unit

val max_printer_depth: int ref
val max_printer_steps: int ref
