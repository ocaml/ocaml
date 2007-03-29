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

val magic : string

(* Function exn_global loc exn_cstr
   registers the exception constructor exn_cstr as a global one.

   It fails if a structurally equivalent constructor have
   already been globalized, loc is a position in source file for
   error message *)

val exn_global : (string * int * int) -> Obj.t -> unit

val localize_exn : exn -> exn

open Join_types

(* Specialized input/output for messages between sites *)

val input_value : Join_link.t -> 'a
val output_value : Join_link.t -> 'a -> unit

val input_parameter : Join_link.t -> parameter
val output_parameter : Join_link.t -> parameter -> unit

val input_msg : Join_link.t -> message
val output_msg : Join_link.t -> message -> unit

(* for debug *)
val string_of_msg : message -> string

