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

(* The "trace" facility *)

type traced_function =
  { path: Path.t;
    closure: Obj.t;
    initial_closure: Obj.t;
    instrumented_fun: Obj.t }

val traced_functions: traced_function list ref
val copy_closure: Obj.t -> Obj.t
val overwrite_closure: Obj.t -> Obj.t -> unit
val instrument_closure: 
      Longident.t -> Typedtree.type_expr -> Obj.t -> Obj.t
val print_trace: Obj.t -> Obj.t -> Obj.t
