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

(** Apply the given functions to the immediate subexpressions of the given
    Flambda expression.  For avoidance of doubt, if a subexpression is
    [Fexpr], it is passed to the function taking [_ Flambda.named], rather
    than being followed and passed to the function taking [_ Flambda.t]. *)
val apply_on_subexpressions
   : ('a Flambda.t -> unit)
  -> ('a Flambda.named -> unit)
  -> 'a Flambda.t
  -> unit

val iter
   : ('a Flambda.t -> unit)
  -> ('a Flambda.named -> unit)
  -> 'a Flambda.t
  -> unit

(** [iter_toplevel f t] applies [f] on every toplevel subexpression of [t].
    In particular, it never applies [f] to the body of a function (which
    will always be contained within an [Fset_of_closures] expression). *)
val iter_toplevel
   : ('a Flambda.t -> unit)
  -> ('a Flambda.named -> unit)
  -> 'a Flambda.t
  -> unit

(* CR mshinwell: rename to iter_on_set_of_closures *)
val iter_on_sets_of_closures
   : ('a Flambda.set_of_closures -> 'a -> unit)
  -> 'a Flambda.t
  -> unit

val map
   : ('a Flambda.t -> 'a Flambda.t)
  -> ('a Flambda.named -> 'a Flambda.named)
  -> 'a Flambda.t
  -> 'a Flambda.t

val map_toplevel
   : ('a Flambda.t -> 'a Flambda.t)
  -> ('a Flambda.named -> 'a Flambda.named)
  -> 'a Flambda.t
  -> 'a Flambda.t

(*
val map_data : ('a -> 'b) -> 'a Flambda.t -> 'b Flambda.t
*)
