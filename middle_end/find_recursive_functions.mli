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

(** "Recursive functions" are those functions [f] that might call either:
    - themselves, or
    - another function that in turn might call [f].

    For example in the following simultaneous definition of [f] [g] and [h],
    [f] and [g] are recursive functions, but not [h]:
      [let rec f x = g x
       and g x = f x
       and h x = g x]
*)

(** Determine the recursive functions, if any, bound by the given set of
    function declarations. *)
val in_function_decls : Flambda.function_declarations -> Variable.Set.t
