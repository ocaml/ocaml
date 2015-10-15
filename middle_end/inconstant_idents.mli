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

type result = {
  id : Variable.Set.t;
  closure : Set_of_closures_id.Set.t;
}

(** [inconstants] with [for_clambda = true] finds those variables and
    set-of-closures identifiers that cannot be compiled to constants by
    [Clambdagen].

    When [for_clambda] is false, field accesses to a constant are
    considered constant.
*)
(* val inconstants *)
(*    : for_clambda:bool *)
(*   -> compilation_unit:Compilation_unit.t *)
(*   -> Flambda.t *)
(*   -> result *)

val inconstants_on_program
   : for_clambda:bool
  -> compilation_unit:Compilation_unit.t
  -> Flambda.program
  -> result
