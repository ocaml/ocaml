(***********************************************************************)
(*                                                                     *)
(*                    ______________                                   *)
(*                                                                     *)
(*      Fabrice Le Fessant, projet SOR/PARA INRIA Rocquencourt         *)
(*      Luc Maranget, projet Moscova                                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val local_socket : Unix.file_descr

val local_id : Join_types.space_name

val new_uid : unit -> int
