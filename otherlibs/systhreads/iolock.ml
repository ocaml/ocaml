(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Since this file is linked before Pervasives and all other library
   modules, we can't use any of the standard library functions. *)

external raise : exn -> 'a = "%raise"
external (mod) : int -> int -> int = "%modint"
external (==) : 'a -> 'a -> bool = "%eq"
external array_create: int -> 'a -> 'a array = "make_vect"
external array_get: 'a array -> int -> 'a = "%array_unsafe_get"
external array_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
type obj
external obj_repr: 'a -> obj = "%identity"
external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"

type mutex
external mutex_create: unit -> mutex = "caml_mutex_new"
external mutex_lock: mutex -> unit = "caml_mutex_lock"
external mutex_unlock: mutex -> unit = "caml_mutex_unlock"

let master_lock = mutex_create()

type bucketlist =
    Empty
  | Cons of obj * mutex * bucketlist

let iolocks = array_create 27 Empty

let hash channel = (hash_param 10 10 channel) mod 27

let add channel =
  let m = mutex_create() in
  mutex_lock master_lock;
  let h = hash channel in
  array_set iolocks h (Cons(obj_repr channel, m, array_get iolocks h));
  mutex_unlock master_lock;
  channel
  
let rec remove_from_bucket ch = function
    Empty -> Empty
  | Cons(k, m, rem) ->
      if ch == k then rem else Cons(k, m, remove_from_bucket ch rem)

let remove channel =
  mutex_lock master_lock;
  let h = hash channel in
  array_set iolocks h
            (remove_from_bucket (obj_repr channel) (array_get iolocks h));
  mutex_unlock master_lock

let rec find_in_bucket ch = function
    Empty ->
      raise(Invalid_argument "Pervasives: channel closed")
  | Cons(k, m, rem) ->
      if ch == k then m else find_in_bucket ch rem

let find channel =
  find_in_bucket (obj_repr channel) (array_get iolocks (hash channel))

let lock m = mutex_lock m

let unlock m = mutex_unlock m
