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

val print_type_scheme_ref: (Format.formatter -> Types.type_expr -> unit) ref
val print_type_scheme: Format.formatter -> Types.type_expr -> unit

val print_konst_elem :  Format.formatter -> Types.konst_elem -> unit
val print_list : (Format.formatter -> 'a -> unit) -> (Format.formatter -> unit) -> Format.formatter -> 'a list -> unit

val print_raw_type_expr_ref: (Format.formatter -> Types.type_expr -> unit) ref
val print_raw_type_expr: Format.formatter -> Types.type_expr -> unit

val defined : string -> bool
