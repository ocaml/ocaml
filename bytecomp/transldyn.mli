(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Gilles Peskine, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for dynamic typing *)

exception Unimplemented of string
    (* ((to be improved)) *)

val dynamics_prim : string -> Lambda.lambda
val make_type_repr_code : Env.t -> Types.type_expr -> Lambda.lambda
val make_sig_repr_code : Env.t -> Types.module_type -> Lambda.lambda
