(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* To control the runtime system and bytecode interpreter *)

external global_data : unit -> Obj.t array = "get_global_data"
external realloc_global_data : int -> unit = "realloc_global"
external static_alloc : int -> string = "static_alloc"
external static_free : string -> unit = "static_free"
external static_resize : string -> int -> string = "static_resize"
type closure = unit -> Obj.t
external reify_bytecode : string -> int -> closure = "reify_bytecode"
val run_bytecode :  string -> int -> unit
external available_primitives : unit -> string array = "available_primitives"
external invoke_traced_function : Obj.t -> Obj.t -> Obj.t -> Obj.t
                                = "invoke_traced_function"
