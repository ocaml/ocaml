(* DBM (NDBM) interface *)

type t

type dbm_flag =
   DBM_INSERT
 | DBM_REPLACE

exception Dbm_error of string

external opendbm : string -> open_flag list -> int -> t 
      	      = "caml_dbm_open"
external close : t -> unit = "caml_dbm_close"
external fetch : t -> string -> string = "caml_dbm_fetch"
external store : t -> string -> string -> dbm_flag list -> unit 
              = "caml_dbm_store"
external delete : t -> string -> unit = "caml_dbm_delete"
external firstkey : t -> string = "caml_dbm_firstkey"
external nextkey : t -> string = "caml_dbm_nextkey"


(* Usual interfaces *)
val add: t -> string -> string -> unit
val find : t -> string -> string
val remove : t -> string -> unit
val iter : (string -> string -> 'a) -> t -> unit
