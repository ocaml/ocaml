(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Predefined type constructors (with special typing rules in typecore) *)

open Types

val type_int: type_expr
val type_char: type_expr
val type_string: type_expr
val type_float: type_expr
val type_bool: type_expr
val type_unit: type_expr
val type_exn: type_expr
val type_array: type_expr -> type_expr
val type_list: type_expr -> type_expr
val type_option: type_expr -> type_expr
val type_nativeint: type_expr
val type_int32: type_expr
val type_int64: type_expr
val type_lazy_t: type_expr -> type_expr

val path_int: Path.t
val path_char: Path.t
val path_string: Path.t
val path_float: Path.t
val path_bool: Path.t
val path_unit: Path.t
val path_exn: Path.t
val path_array: Path.t
val path_list: Path.t
val path_format6: Path.t
val path_option: Path.t
val path_nativeint: Path.t
val path_int32: Path.t
val path_int64: Path.t
val path_lazy_t: Path.t

val ident_int: Ident.t
val ident_char: Ident.t
val ident_string: Ident.t
val ident_float: Ident.t
val ident_bool: Ident.t
val ident_unit: Ident.t
val ident_exn: Ident.t
val ident_array: Ident.t
val ident_list: Ident.t
val ident_format6: Ident.t
val ident_option: Ident.t
val ident_nativeint: Ident.t
val ident_int32: Ident.t
val ident_int64: Ident.t
val ident_lazy_t: Ident.t
