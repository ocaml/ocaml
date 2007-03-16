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

(* abstract type for the name service *)
type t

(* the local name service *)
val here : t

(* get remote name service *)
val of_site : Join.site -> t

(* get remote name service by socket address *)
val of_sockaddr : Unix.sockaddr -> t

(* find value, raise Not_found when not present *)
val lookup : t -> string -> 'a

(* register binding, returns when done *)
val register : t -> string -> 'a -> unit


