(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Transform [let]-bound mutable records into variables *)
val eliminate_ref
   : Abstract_identifiers.Expr_id.t Flambda.t
  -> Abstract_identifiers.Expr_id.t Flambda.t
