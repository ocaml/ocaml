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

val max_printer_depth : int ref
val max_printer_steps : int ref

val print_value :
  int -> Debugcom.Remote_value.t -> Types.type_expr -> Env.t -> unit
val print_named_value :
  int -> Parser_aux.expression ->
    Debugcom.Remote_value.t -> Types.type_expr -> Env.t ->
    unit

val reset_named_values : unit -> unit
val find_named_value : int -> Debugcom.Remote_value.t * Types.type_expr

val install_printer : Path.t -> Types.type_expr -> (Obj.t -> unit) -> unit
val remove_printer : Path.t -> unit
