(* DBM (NDBM) interface *)

type t

type open_flag =
   Dbm_rdonly | Dbm_wronly | Dbm_rdwr | Dbm_create

exception Dbm_error of string

external opendbm : string -> open_flag list -> int -> t 
      	      = "caml_dbm_open"
external close : t -> unit = "caml_dbm_close"
external find : t -> string -> string = "caml_dbm_fetch"
external add : t -> string -> string -> unit = "caml_dbm_insert"
external replace : t -> string -> string -> unit = "caml_dbm_replace"
external remove : t -> string -> unit = "caml_dbm_delete"
external firstkey : t -> string = "caml_dbm_firstkey"
external nextkey : t -> string = "caml_dbm_nextkey"

(* Usual iterator *)
val iter : (string -> string -> 'a) -> t -> unit
