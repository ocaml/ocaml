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

open Types
open Parser_aux

val path : Instruct.debug_event -> Path.t -> Debugcom.remote_value
val expression :
    Instruct.debug_event -> Env.t -> expression ->
    Debugcom.remote_value * type_expr

type error =
    Unbound_identifier of Ident.t
  | Not_initialized_yet of Path.t
  | Unbound_long_identifier of Longident.t
  | Unknown_name of int
  | Tuple_index of type_expr * int * int
  | Array_index of int * int
  | List_index of int * int
  | String_index of string * int * int
  | Wrong_item_type of type_expr * int
  | Wrong_label of type_expr * string
  | Not_a_record of type_expr
  | No_result

exception Error of error

val report_error: error -> unit
