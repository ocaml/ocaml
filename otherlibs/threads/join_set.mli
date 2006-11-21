(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2006 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*
  mutable sets with readers/writer protection
*)


type 'a t

val create : unit -> 'a t

val singleton : 'a -> 'a t

val from_list : 'a list -> 'a t

val add : 'a t -> 'a -> unit

val adds : 'a t -> 'a list -> unit


val elements : 'a t -> 'a list

