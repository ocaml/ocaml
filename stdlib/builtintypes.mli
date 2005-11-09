(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* This module is for the compiler use only. Do not use these 
   type declaration informations. Instead, use those of module Rtype. *)

type type_declaration

val int : type_declaration
val char : type_declaration
val string : type_declaration
val float : type_declaration
val bool : type_declaration
val unit : type_declaration
val exn : type_declaration
val array : type_declaration
val list : type_declaration
val format4 : type_declaration
val option : type_declaration
val nativeint : type_declaration
val int32 : type_declaration
val int64 : type_declaration
val lazy_t : type_declaration
