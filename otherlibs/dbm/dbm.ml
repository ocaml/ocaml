(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type t

type open_flag =
   Dbm_rdonly | Dbm_wronly | Dbm_rdwr | Dbm_create

type dbm_flag =
   DBM_INSERT
 | DBM_REPLACE

exception Dbm_error of string

external raw_opendbm : string -> open_flag list -> int -> t 
              = "caml_dbm_open"

let opendbm file flags mode =
  try
    raw_opendbm file flags mode
  with Dbm_error msg ->
    raise(Dbm_error("Can't open file " ^ file))

 (* By exporting opendbm as val, we are sure to link in this
    file (we must register the exception). Since t is abstract, programs
    have to call it in order to do anything *)

external close : t -> unit = "caml_dbm_close"
external find : t -> string -> string = "caml_dbm_fetch"
external add : t -> string -> string -> unit = "caml_dbm_insert"
external replace : t -> string -> string -> unit = "caml_dbm_replace"
external remove : t -> string -> unit = "caml_dbm_delete"
external firstkey : t -> string = "caml_dbm_firstkey"
external nextkey : t -> string = "caml_dbm_nextkey"

let _ = Callback.register_exception "dbmerror" (Dbm_error "")

(* Usual iterator *)
let iter f t =
  let rec walk k = 
    f k (find t k); 
    match try Some(nextkey t) with Not_found -> None
    with
         None -> ()
       | Some k -> walk k
  in
  walk (firstkey t)

