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

(** Checking of invariants on Flambda expressions. *)

(** Run all tests.  Raises [Fatal_error] if a test fails.
    The [flambdasym] and [cmxfile] arguments control whether certain tests
    are run, as documented in the comments below.
*)
val check : ?flambdasym:bool -> ?cmxfile:bool -> Flambda.t -> unit

type 'a counter_example =
  | No_counter_example
  | Counter_example of 'a

val every_used_identifier_is_bound
   : Flambda.t
  -> Variable.t counter_example

val function_free_variables_are_bound_in_the_closure_and_parameters
   : Flambda.t
  -> Variable.Set.t counter_example

val no_identifier_bound_multiple_times
   : Flambda.t
  -> Variable.t counter_example

val every_bound_variable_is_from_current_compilation_unit
   : Flambda.t
  -> Variable.t counter_example

val no_assign_on_variable_of_kind_Immutable
   : Flambda.t
  -> Variable.t counter_example

val no_var_within_closure_is_bound_multiple_times
   : Flambda.t
  -> Var_within_closure.t counter_example

val no_closure_id_is_bound_multiple_times
   : Flambda.t
  -> Closure_id.t counter_example

(** This test is only run by [check] when [flambdasym] is [false]. *)
val every_declared_closure_is_from_current_compilation_unit
   : Flambda.t
  -> Compilation_unit.t counter_example

(** This test is only run by [check] when [flambdasym] is [false]. *)
val every_used_function_from_current_compilation_unit_is_declared
   : Flambda.t
  -> Closure_id.Set.t counter_example

val every_used_var_within_closure_from_current_compilation_unit_is_declared 
   : Flambda.t
  -> Var_within_closure.Set.t counter_example

val every_static_exception_is_caught
   : Flambda.t
  -> Static_exception.t counter_example

val every_static_exception_is_caught_at_a_single_position
   : Flambda.t
  -> Static_exception.t counter_example

(* This test is only run by [check] when [cmxfile] is [true].

   It is important to avoid having access to global identifiers
   (via Pgetglobal/Psetglobal/Pgetglobalfield/Psetglobalfield) in code
   exported to the .cmx file because they wouldn't be correct if the file were
   to be subsequently packed.  Such accesses must have been replaced by
   symbol accesses. *)
val no_access_to_global_module_identifiers
   : Flambda.t
  -> Lambda.primitive counter_example
