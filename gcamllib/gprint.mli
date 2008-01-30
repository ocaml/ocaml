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

type printer = Rtype.type_expr -> Format.formatter -> Obj.t -> unit

val gen_print : (int -> int -> printer) -> (int -> int -> printer)

val print : { 'a } => Format.formatter -> 'a -> unit

val eprint :
    { Format.formatter -> 'b -> unit < 
      { 'a } => Format.formatter -> 'a -> unit } => 'b -> unit
