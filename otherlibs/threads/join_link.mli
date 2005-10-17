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

exception Failed

type t

(* does not raise Failed, may raise Out_of_memory, but then *)
val create : Unix.file_descr -> t

(* all those may raise Failed, and only Failed... *)

val output_string : t -> string -> unit

val really_input : t -> string -> int -> int -> unit

val flush : t -> unit

val close : t -> unit
