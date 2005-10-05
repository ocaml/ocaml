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

(* Forward pointers for local message sending, set by module Join *)
type async_ref =
  { mutable async : 'a . Join_types.automaton -> int -> 'a -> unit }
val send_async_ref : async_ref

type sync_ref =
    { mutable sync : 'a 'b . Join_types.automaton -> int -> 'a -> 'b}
val send_sync_ref : sync_ref


val exn_global : (string * int * int) -> Obj.t -> unit

val marshal_message :
    'a ->  Marshal.extern_flags list -> string * (Join_types.t_global) array

val unmarshal_message :
    string * (Join_types.t_global) array -> 'a

val remote_send_async :
    Join_types.remote_space ->
      int (* uid *) -> int (* channnel *) -> 'a (* message *) -> unit

val remote_send_sync :
    Join_types.remote_space ->
      int (* uid *) -> int (* channnel *) -> Join_types.continuation ->
	'a (* message *) -> 'b

val halt : unit -> unit

val flush_space : unit -> unit
