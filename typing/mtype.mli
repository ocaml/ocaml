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

(* Operations on module types *)

open Types

val scrape: Env.t -> module_type -> module_type
        (* Expand toplevel module type abbreviations
           till hitting a "hard" module type (signature, functor,
           or abstract module type ident. *)
val strengthen: Env.t -> module_type -> Path.t -> module_type
        (* Strengthen abstract type components relative to the
           given path. *)
val nondep_supertype: Env.t -> Ident.t -> module_type -> module_type
        (* Return the smallest supertype of the given type
           in which the given ident does not appear.
           Raise [Not_found] if no such type List.exists. *)
