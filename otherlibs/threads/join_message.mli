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

(* Function exn_global loc exn_cstr
   registers the exception constructor exn_cstr as a global one.

   It fails if a structurally equivalent constructor have
   already been globalized, loc is a position in source file for
   error message *)

val exn_global : (string * int * int) -> Obj.t -> unit

val localize_exn : exn -> exn

open Join_types

(* Specialized input_value/output_value for messages between sites *)

val input_parameter : in_channel -> parameter
val output_parameter : out_channel -> parameter -> unit

val input_msg : in_channel -> message
val output_msg : out_channel -> message -> unit


