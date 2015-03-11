(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Module dependencies. *)

module PathSet : Set.S with type elt = Longident.t
module StringSet : Set.S with type elt = string

val path_root : Longident.t -> string

val free_paths : PathSet.t ref

val open_module : StringSet.t -> Longident.t -> unit

val add_use_file : StringSet.t -> Parsetree.toplevel_phrase list -> unit

val add_signature : StringSet.t -> Parsetree.signature -> unit

val add_implementation : StringSet.t -> Parsetree.structure -> unit
