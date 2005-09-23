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

type poly_ref = { mutable f : 'a . Join_types.automaton -> int -> 'a -> unit }

val send_async_ref : poly_ref

val marshal_message :
    'a ->  Marshal.extern_flags list -> string * (Join_types.t_global) array

val unmarshal_message :
    string * (Join_types.t_global) array -> 'a

val remote_send_async :
    Join_types.remote_space ->
      int (* uid *) -> int (* channnel *) -> 'a (* message *) -> unit
