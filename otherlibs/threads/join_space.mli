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

val marshal_message :
    'a ->  Marshal.extern_flags list -> string * (Join_types.t_local) array

val unmarshal_message :
    string * (Join_types.t_local) array -> 'a

