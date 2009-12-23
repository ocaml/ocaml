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

open Asttypes
open Path
open Types
open Btype

let ident_int = Ident.create "int"
and ident_char = Ident.create "char"
and ident_string = Ident.create "string"
and ident_float = Ident.create "float"
and ident_bool = Ident.create "bool"
and ident_unit = Ident.create "unit"
and ident_exn = Ident.create "exn"
and ident_array = Ident.create "array"
and ident_list = Ident.create "list"
and ident_format6 = Ident.create "format6"
and ident_option = Ident.create "option"
and ident_nativeint = Ident.create "nativeint"
and ident_int32 = Ident.create "int32"
and ident_int64 = Ident.create "int64"
and ident_lazy_t = Ident.create "lazy_t"

let path_int = Pident ident_int
and path_char = Pident ident_char
and path_string = Pident ident_string
and path_float = Pident ident_float
and path_bool = Pident ident_bool
and path_unit = Pident ident_unit
and path_exn = Pident ident_exn
and path_array = Pident ident_array
and path_list = Pident ident_list
and path_format6 = Pident ident_format6
and path_option = Pident ident_option
and path_nativeint = Pident ident_nativeint
and path_int32 = Pident ident_int32
and path_int64 = Pident ident_int64
and path_lazy_t = Pident ident_lazy_t

let type_int = newgenty (Tconstr(path_int, [], ref Mnil))
and type_char = newgenty (Tconstr(path_char, [], ref Mnil))
and type_string = newgenty (Tconstr(path_string, [], ref Mnil))
and type_float = newgenty (Tconstr(path_float, [], ref Mnil))
and type_bool = newgenty (Tconstr(path_bool, [], ref Mnil))
and type_unit = newgenty (Tconstr(path_unit, [], ref Mnil))
and type_exn = newgenty (Tconstr(path_exn, [], ref Mnil))
and type_array t = newgenty (Tconstr(path_array, [t], ref Mnil))
and type_list t = newgenty (Tconstr(path_list, [t], ref Mnil))
and type_option t = newgenty (Tconstr(path_option, [t], ref Mnil))
and type_nativeint = newgenty (Tconstr(path_nativeint, [], ref Mnil))
and type_int32 = newgenty (Tconstr(path_int32, [], ref Mnil))
and type_int64 = newgenty (Tconstr(path_int64, [], ref Mnil))
and type_lazy_t t = newgenty (Tconstr(path_lazy_t, [t], ref Mnil))
