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

(* Access paths *)

type t =
    Pident of Ident.t
  | Pdot of t * string * int
  | Papply of t * t

val same: t -> t -> bool
val isfree: Ident.t -> t -> bool
val binding_time: t -> int

val nopos: int

val name: t -> string
    (* Return the print name, e.g., "F(X).M.foo" *)
val unique_name : t -> string
    (* Return a name with head components appearing as unique names,
       e.g., "F_42(X_84).M.foo" *)
