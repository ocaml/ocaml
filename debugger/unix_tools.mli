(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(**************************** Tools for Unix ***************************)

open Unix

(* Convert a socket name into a socket address. *)
val convert_address : string -> socket_domain * sockaddr

(* Report an unix error. *)
val report_error : exn -> unit

(* Find program `name' in `PATH'. *)
(* Return the full path if found. *)
(* Raise `Not_found' otherwise. *)
val search_in_path : string -> string

(* Path expansion. *)
val expand_path : string -> string
