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

(* Converts a flambda expression to clambda.
   During the conversion it:
    * substitute variables bound by a closure by a field access
      inside the closure
    * replace symbolic closure offset by the real integer offset.
    * build the switch tables
    * add closure parameter for direct calls
    * detect constants values and transform them to Uconst
   For everything else, it is basically the identity.
*)
val convert
   : 'a Flambda.t * 'a Flambda.t Symbol.SymbolMap.t
       * Flambdaexport.exported
  -> Clambda.ulambda
