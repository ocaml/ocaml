(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                   Jun Furuse, University of Tokyo                   *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Rtype

type depth_info = { max_depth : int; cur_depth : int; }
type base_printer = depth_info -> Format.formatter -> Obj.t -> unit
val base_printers : 
    (type_declaration * (base_printer list -> base_printer)) list

type printer = type_expr -> depth_info -> Format.formatter -> Obj.t -> unit

val gen_print : printer -> printer

val printer : type_expr -> ?max_depth: int -> Format.formatter -> Obj.t -> unit
val print : { 'a } => ?max_depth: int -> Format.formatter -> 'a -> unit
val eprint : { 'a } => ?max_depth: int -> 'a -> unit
