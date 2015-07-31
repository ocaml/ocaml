(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* CR pchambart:
   constant correspond to Clambda.uconstant
   Allocated_constants correspond to Clambda.structured constant

   It could be replaced by clambda types if they were extended to
   represent every case of Flambdaexport_types

   The constant type does not have a description of the contained
   constant (the option in the Uconst_ref constructor). For rebuilding
   clambda, we may need to find the description: this is usefull on
   floats (and boxed integers) for unboxing.

   mshinwell: this CR may be out of date now
*)

val lift_constants
   : Flambda.t
  -> backend:(module Backend_intf.S)
  -> Flambda.program
