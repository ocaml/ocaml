(* type declaration information of the builtin types *)

open Rtype (* we need rtype.cmi *)

(* definitions must be recusrive, since the typedecl infos may
   create a recursive reference to the corresponding identifier,
   ex. list. *)

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
