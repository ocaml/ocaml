(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Francois Rouaix, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Interface to the NDBM database. *)

type t
(** The type of file descriptors opened on NDBM databases. *)


type open_flag =
    Dbm_rdonly
  | Dbm_wronly
  | Dbm_rdwr
  | Dbm_create
(** Flags for opening a database (see {!Dbm.opendbm}). *)


exception Dbm_error of string
(** Raised by the following functions when an error is encountered. *)

val opendbm : string -> open_flag list -> int -> t
(** Open a descriptor on an NDBM database. The first argument is
   the name of the database (without the [.dir] and [.pag] suffixes).
   The second argument is a list of flags: [Dbm_rdonly] opens
   the database for reading only, [Dbm_wronly] for writing only,
   [Dbm_rdwr] for reading and writing; [Dbm_create] causes the
   database to be created if it does not already exist.
   The third argument is the permissions to give to the database
   files, if the database is created. *)

external close : t -> unit = "caml_dbm_close"
(** Close the given descriptor. *)

external find : t -> string -> string = "caml_dbm_fetch"
(** [find db key] returns the data associated with the given
   [key] in the database opened for the descriptor [db].
   Raise [Not_found] if the [key] has no associated data. *)

external add : t -> string -> string -> unit = "caml_dbm_insert"
(** [add db key data] inserts the pair ([key], [data]) in
   the database [db]. If the database already contains data
   associated with [key], raise [Dbm_error "Entry already exists"]. *)

external replace : t -> string -> string -> unit = "caml_dbm_replace"
(** [replace db key data] inserts the pair ([key], [data]) in
   the database [db]. If the database already contains data
   associated with [key], that data is discarded and silently
   replaced by the new [data]. *)

external remove : t -> string -> unit = "caml_dbm_delete"
(** [remove db key data] removes the data associated with [key]
   in [db]. If [key] has no associated data, raise
   [Dbm_error "dbm_delete"]. *)

external firstkey : t -> string = "caml_dbm_firstkey"
(** See {!Dbm.nextkey}.*)

external nextkey : t -> string = "caml_dbm_nextkey"
(** Enumerate all keys in the given database, in an unspecified order.
   [firstkey db] returns the first key, and repeated calls
   to [nextkey db] return the remaining keys. [Not_found] is raised
   when all keys have been enumerated. *)

val iter : (string -> string -> 'a) -> t -> unit
(** [iter f db] applies [f] to each ([key], [data]) pair in
   the database [db]. [f] receives [key] as first argument
   and [data] as second argument. *)
