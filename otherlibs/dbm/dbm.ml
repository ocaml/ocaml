type t

type dbm_flag =
   DBM_INSERT
 | DBM_REPLACE

exception Dbm_error of string

external install_exn : exn -> unit
      	      = "caml_dbm_install_exn"
external opendbm : string -> open_flag list -> int -> t 
      	      = "caml_dbm_open"
external close : t -> unit = "caml_dbm_close"
external fetch : t -> string -> string = "caml_dbm_fetch"
external store : t -> string -> string -> dbm_flag list -> unit 
              = "caml_dbm_store"
external delete : t -> string -> unit = "caml_dbm_delete"
external firstkey : t -> string = "caml_dbm_firstkey"
external nextkey : t -> string = "caml_dbm_nextkey"

let _ = install_exn (Dbm_error "")

(* Usual interfaces *)
let add t x v = store t x v [DBM_INSERT]
let find = fetch
let remove = delete

let iter f t =
  let rec walk k = 
    f k (fetch t k); 
    match try Some(nextkey t) with Not_found -> None
    with
	 None -> ()
       | Some k -> walk k
  in
  walk (firstkey t)

