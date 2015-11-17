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

(* [invariant_params_in_recursion] calculates the set of parameters whose
   values are known not to change during the execution of a recursive
   function.  As such, occurrences of the parameters may always be replaced
   by the corresponding values.

   For example, [x] would be in [invariant_params] for both of the following
   functions:

     let rec f x y = (f x y) + (f x (y+1))

     let rec f x l = List.iter (f x) l

   For invariant parameters it also computes the set of parameters of functions
   in the set of closures that are always aliased to it. For example in the set
   of closures:

     let rec f x y = (f x y) + (f x (y+1)) + g x
     and g z = z + 1

   The map of aliases is

     x -> { x; z }
*)
val invariant_params_in_recursion
   : Flambda.function_declarations
  -> backend:(module Backend_intf.S)
  -> Variable.Set.t Variable.Map.t

(* CR-soon mshinwell: think about whether this function should
   be in this file.  Should it be called "unused_parameters"? *)
val unused_arguments
   : Flambda.function_declarations
  -> Variable.Set.t
