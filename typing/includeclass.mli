(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Inclusion checks for the class language *)

open Types
open Typedtree
open Ctype

val class_types:
        Env.t -> class_type -> class_type -> class_match_failure list
val class_type_declarations:
        Env.t -> cltype_declaration -> cltype_declaration ->
        class_match_failure list
val class_declarations:
        Env.t -> class_declaration -> class_declaration ->
        class_match_failure list

val report_error: class_match_failure list -> unit
