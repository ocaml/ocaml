(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Printing of values *)

open Typedtree

val print_exception: Obj.t -> unit
val print_value: Env.t -> Obj.t -> type_expr -> unit

val printers: (Path.t * type_expr * (Obj.t -> unit)) list ref
val max_printer_depth: int ref
val max_printer_steps: int ref
