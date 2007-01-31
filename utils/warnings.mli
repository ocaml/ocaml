(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis && Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Format

type t =                             (* A is all *)
  | Comment_start                    (* C *)
  | Comment_not_end
  | Deprecated                       (* D *)
  | Fragile_match of string            (* E *)
  | Partial_application              (* F *)
  | Labels_omitted                   (* L *)
  | Method_override of string list   (* M *)
  | Partial_match of string          (* P *)
  | Statement_type                   (* S *)
  | Unused_match                     (* U *)
  | Unused_pat
  | Instance_variable_override of string (* V *)
  | Illegal_backslash                (* X *)
  | Implicit_public_methods of string list
  | Unerasable_optional_argument
  | Undeclared_virtual_method of string
  | Not_principal of string
  | Without_principality of string
  | Unused_argument
  | Nonreturning_statement
  | Camlp4 of string
  | All_clauses_guarded
  | Useless_record_with
  | Unused_var of string             (* Y *)
  | Unused_var_strict of string      (* Z *)
;;

val parse_options : bool -> string -> unit;;

val is_active : t -> bool;;
val is_error : t -> bool;;

val print : formatter -> t -> int;;
  (* returns the number of newlines in the printed string *)


exception Errors of int;;

val check_fatal : unit -> unit;;
