(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val check_attribute
   : Typedtree.expression
  -> string Location.loc * _
  -> unit

val add_inline_attribute
   : Lambda.lambda
  -> Location.t
  -> Parsetree.attribute list
  -> Lambda.lambda

val get_inline_attribute
   : (string Location.loc * Parsetree.payload) list
  -> Lambda.inline_attribute

val get_inlined_attribute
   : Typedtree.expression
  -> Lambda.inline_attribute * Typedtree.expression

val get_tailcall_attribute
   : Typedtree.expression
  -> bool * Typedtree.expression
