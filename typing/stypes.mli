(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Damien Doligez, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Recording and dumping (partial) type information *)

(* Clflags.save_types must be true *)

open Typedtree;;

type type_info =
    Ti_pat   of pattern
  | Ti_expr  of expression
  | Ti_class of class_expr
  | Ti_mod   of module_expr
;;

val record : type_info -> unit;;
val record_phrase : Location.t -> unit;;
val dump : string -> unit;;

val get_location : type_info -> Location.t;;
val get_info : unit -> type_info list;;
