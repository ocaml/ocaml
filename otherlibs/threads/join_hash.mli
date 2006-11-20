(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*
  Hashtables  with readers/writer protection
*)

type ('a,'b) t

val create : unit -> ('a,'b) t

(* add a binding *)
val add :  ('a,'b) t -> 'a -> 'b -> unit

(* add a binding key, value
   if there is already a binding for key, will do nothing
   and returns the old value, otherwise will return None *)
val add_once  : ('a,'b) t -> 'a -> 'b -> 'b option

(* find a value, given a key, raise Not_found if not present *)
val find : ('a,'b) t -> 'a -> 'b

(* find and atomically remove a value *)
val find_remove  : ('a,'b) t -> 'a -> 'b

(* iterate over all bindings *)
val iter : ('a, 'b) t -> ('a -> 'b -> unit) -> unit

(* iterate over all bindings and empty table *)
val iter_empty : ('a, 'b) t -> ('a -> 'b -> unit) -> unit

(* remove a binding *)
val remove : ('a,'b) t -> 'a -> unit

(* Perform some operation on value *)
val perform : ('a,'b) t -> 'a -> 'b -> ('b -> 'b) -> unit
