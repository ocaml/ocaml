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

type type_declaration

let rec int = typedecl int
let rec char = typedecl char
let rec string = typedecl string
let rec float = typedecl float
let rec bool = typedecl bool
let rec unit = typedecl unit
let rec exn = typedecl exn
let rec array = typedecl array
let rec list = typedecl list
let rec format4 = typedecl format4
let rec option = typedecl option
let rec nativeint = typedecl nativeint
let rec int32 = typedecl int32
let rec int64 = typedecl int64
let rec lazy_t = typedecl lazy_t
