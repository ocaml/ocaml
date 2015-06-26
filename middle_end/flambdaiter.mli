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

val apply_on_subexpressions : ('a Flambda.t -> unit) -> 'a Flambda.t -> unit

val subexpressions : 'a Flambda.t -> 'a Flambda.t list

val iter : ('a Flambda.t -> unit) -> 'a Flambda.t -> unit

(** [iter_toplevel f t] applies [f] on every toplevel subexpression of [t].
    In particular, it never applies [f] to the body of a function (which
    will always be contained within an [Fset_of_closures] expression). *)
val iter_toplevel : ('a Flambda.t -> unit) -> 'a Flambda.t -> unit

(* CR mshinwell: rename to iter_on_set_of_closures *)
val iter_on_sets_of_closures
   : ('a Flambda.set_of_closures -> 'a -> unit)
  -> 'a Flambda.t
  -> unit

val map : ('a Flambda.t -> 'a Flambda.t) -> 'a Flambda.t -> 'a Flambda.t

val map_toplevel
   : ('a Flambda.t -> 'a Flambda.t)
  -> 'a Flambda.t
  -> 'a Flambda.t

val fold_subexpressions
   : ('acc -> Variable.Set.t -> 'a Flambda.t -> 'acc * 'a Flambda.t)
  -> 'acc
  -> 'a Flambda.t
  -> 'acc * 'a Flambda.t

val free_variables : 'a Flambda.t -> Variable.Set.t

val map_data : ('a -> 'b) -> 'a Flambda.t -> 'b Flambda.t
