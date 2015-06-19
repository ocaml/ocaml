(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell for lwhite: rename "unchanging" -> "invariant" *)

(* [unchanging_params_in_recursion] calculates the set of parameters whose
   values are known not to change during the execution of a recursive
   function.  As such, occurrences of the parameters may always be replaced
   by the corresponding values.

   For example, [x] would be in [unchanging_params] for both of the following
   functions:

     let rec f x y = (f x y) + (f x (y+1))

     let rec f x l = List.iter (f x) l
*)
val unchanging_params_in_recursion
   : 'a Flambda.function_declarations
  -> Variable.Set.t

(* CR mshinwell for lwhite: think about whether this function should
   be in this file.  Should it be called "unused_parameters"? *)
val unused_arguments
   : 'a Flambda.function_declarations
  -> Variable.Set.t
