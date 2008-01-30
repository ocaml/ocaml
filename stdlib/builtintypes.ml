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
let rec format6 = typedecl format6
let rec option = typedecl option
let rec nativeint = typedecl nativeint
let rec int32 = typedecl int32
let rec int64 = typedecl int64
let rec lazy_t = typedecl lazy_t

(*
(* dummy code required to port G'Caml to the latest O'Caml *)
(* check by Obj.magic (typedecl int) : int option *)
external magic : 'a -> 'b = "%identity"

let int = magic (Some (-1))
let char = magic (Some (-1))
let string = magic (Some (-1))
let float = magic (Some (-1))
let bool = magic (Some (-1))
let unit = magic (Some (-1))
let exn = magic (Some (-1))
let array = magic (Some (-1))
let list = magic (Some (-1))
let format6 = magic (Some (-1))
let option = magic (Some (-1))
let nativeint = magic (Some (-1))
let int32 = magic (Some (-1))
let int64 = magic (Some (-1))
let lazy_t = magic (Some (-1))
*)
