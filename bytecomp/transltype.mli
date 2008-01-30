(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                  Jun Furuse, University of Tokyo                    *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val find_recursively_defined_types :
  (Ident.t * Types.type_declaration) list -> (Ident.t * Ident.t list) list
val transl_type_declarations :
  (Ident.t * Types.type_declaration) list -> (Ident.t * Lambda.lambda) list
