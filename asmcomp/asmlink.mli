(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Link a set of .cmx/.o files and produce an executable *)

open Format

val link: formatter -> string list -> unit

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error

exception Error of error

val report_error: formatter -> error -> unit
