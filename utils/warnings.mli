(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t =
  | Partial_match                    (* P *)
  | Unused_match                     (* U *)
  | Method_override of string list   (* M *)
  | Hide_instance_variable of string (* V *)
  | Partial_application              (* F *)
  | Statement_type                   (* S *)
  | Other of string                  (* X *)
;;

val parse_options : string -> unit;;

val is_active : t -> bool;;

val message : t -> string;;
