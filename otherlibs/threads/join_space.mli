(***********************************************************************)
(*                                                                     *)
(*                    Objective Caml                                   *)
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

open Join_types

(* Forward pointers for local message sending, set by module Join *)
type async_ref =
  { mutable async : 'a . automaton -> int -> 'a -> unit }
val send_async_ref : async_ref

type async_gen_ref =
  { mutable async_gen : 'a.'a async -> 'a -> unit ; }
val send_async_gen_ref :  async_gen_ref

type sync_ref =
    { mutable sync : 'a 'b . automaton -> int -> 'a -> 'b}
val send_sync_ref : sync_ref

(* Change a value to parameter and back *)

val globalize :
    'a ->  Marshal.extern_flags list -> parameter
val localize : parameter -> 'a


val here : space_id

val remote_send_async :
    bool (* may_block *) ->
    space_id ->
      int (* uid *) -> int (* channnel *) -> 'a (* message *) -> unit

val remote_send_alone :
    bool  (* may_block *) ->
    space_id ->
      int (* uid *) -> 'a (* message *) -> unit

val remote_send_sync :
    space_id ->
      int (* uid *) -> int (* channnel *) -> continuation ->
	'a (* message *) -> 'b

val remote_send_sync_alone :
    space_id ->
      int (* uid *) -> continuation ->
	'a (* message *) -> 'b

val register_service : string -> ('a -> 'b) -> unit

val call_service : space_id -> string (* key *) -> 'a (* message *) -> 'b

val rid_from_addr : Unix.sockaddr -> space_id

val halt : unit -> unit

val listen : Unix.sockaddr -> unit

val connect : Unix.file_descr -> unit

val at_fail : space_id -> unit async -> unit

val flush_space : unit -> unit

val get_sockaddrs : unit -> Unix.sockaddr list
