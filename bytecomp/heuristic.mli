(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list

(** Heuristics for matching *)

open Typedtree
type occ = int list

val opt:
    ('a * Ident.t * 'c * occ) list -> (pattern list * 'b) list ->
      ('a * Ident.t * 'c * occ) list * (pattern list * 'b) list
