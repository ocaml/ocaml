(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                  Jun Furuse, University of Tokyo                    *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Types

(* konstraints *)
val type_variables : type_expr list -> type_expr list
val type_variables_of_konst_elem : konst_elem -> type_expr list

type variable_path = type_expr * int list

(* normalize konstraint set *)
val normalize_type : type_expr -> unit

val varpath_of_type : type_expr -> variable_path list
val compare_types : type_expr -> type_expr -> int
