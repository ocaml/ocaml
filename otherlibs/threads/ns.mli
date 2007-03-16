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

(* convenience variable (same as Join.local_addr) *)
val local_addr : Unix.inet_addr

(* abstract type for the name service *)
type t

(* the local name service *)
val local : t

(* get remote name service *)
val remote : Join.site -> t

(* get remote name service by socket address,
   shorthand for remote (Join.there addr) *)
val connect : Unix.sockaddr -> t

(* find value, raise Not_found when not present *)
val lookup : t -> string -> 'a

(* register binding, returns when done *)
val register : t -> string -> 'a -> unit


