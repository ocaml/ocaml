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

(** Introduce a stub function to avoid depending on unused arguments.

    For instance, it turns
      [let rec fact n unused =
         if n = 0 then 1
         else n * fact (n-1) unused]
    into
      [let rec fact' n =
         if n = 0 then 1
         else n * fact' (n-1)
       and fact n unused = fact' n]
*)
val separate_unused_arguments_in_closures
   : ?force:unit
  -> Flambda.program
  -> backend:(module Backend_intf.S)
  -> Flambda.program

val separate_unused_arguments_in_set_of_closures
   : Flambda.set_of_closures
  -> backend:(module Backend_intf.S)
  -> Flambda.set_of_closures
