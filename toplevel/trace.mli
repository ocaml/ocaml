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

(* The "trace" facility *)

type codeptr

type traced_function =
  { path: Path.t;                       (* Name under which it is traced *)
    closure: Obj.t;                     (* Its function closure (patched) *)
    actual_code: codeptr;               (* Its original code pointer *)
    instrumented_fun: codeptr -> Obj.t -> Obj.t -> Obj.t }
                                        (* Printing function *)

val traced_functions: traced_function list ref
val is_traced: Obj.t -> Path.t option
val get_code_pointer: Obj.t -> codeptr
val set_code_pointer: Obj.t -> codeptr -> unit
val instrument_closure:
        Env.t -> Longident.t -> Types.type_expr -> 
        codeptr -> Obj.t -> Obj.t -> Obj.t
val print_trace: Obj.t -> Obj.t -> Obj.t
