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

module Env : sig
  type t

  val empty
     : never_inline:bool
    -> backend:(module Backend_intf.S)
    -> t

  val backend : t -> (module Backend_intf.S)

  val local : t -> t

  val inlining_level_up : t -> t
  (* This environment is used to rewrite code for inlining. This is
     used by the inlining heuristics to decide wether to continue.
     Unconditionnaly inlined does not take this into account. *)

  (* Recover informations about the potential values of a variable.
     Fails if no information was present in the environment *)
  (* CR mshinwell: [t] should be first *)
  val find : Variable.t -> t -> Simple_value_approx.t

  val find_list : t -> Variable.t list -> Simple_value_approx.t list

  val find_opt : t -> Variable.t -> Simple_value_approx.t option

  val present : t -> Variable.t -> bool

  (* Causes every bound variable in code rewritten using the given environment
     to be freshened. *)
  val activate_freshening : t -> t

  val add_approx : Variable.t -> Simple_value_approx.t -> t -> t

  val enter_set_of_closures_declaration : Set_of_closures_id.t -> t -> t

  val inside_set_of_closures_declaration : Set_of_closures_id.t -> t -> bool

  (** Not inside a closure declaration.
      Toplevel code is the one evaluated when the compilation unit is
      loaded *)
  val at_toplevel : t -> bool

  val is_inside_branch : t -> bool

  val inside_branch : t -> t
  val inside_simplify : t -> t

  val set_freshening : Freshening.t -> t -> t

  val increase_closure_depth : t -> t

  val set_never_inline : t -> t

  val unrolling_allowed : t -> bool

  val inside_unrolled_function : t -> t

  val inlining_level : t -> int
  val freshening : t -> Freshening.t
  val never_inline : t -> bool

  (* If collecting inlining statistics, record that the inliner is about to
     descend into [closure_id].  This information enables us to produce a
     stack of closures that form a kind of context around an inlining
     decision point. *)
  val note_entering_closure
     : t
    -> closure_id:Closure_id.t
    -> where:Inlining_stats_types.where_entering_closure
    -> t

  (** Update a given environment to record that the inliner is about to
      descend into [closure_id] and pass the resulting environment to [f].
      If [inline_inside] is [false] then the environment passed to [f] will be
      marked as [never_inline] (see above). *)
  val enter_closure
     : t
    -> closure_id:Closure_id.t
    -> inline_inside:bool
    -> where:Inlining_stats_types.where_entering_closure
    -> f:(t -> 'a)
    -> 'a

  val inlining_stats_closure_stack
     : t
    -> Inlining_stats.Closure_stack.t

  val print : Format.formatter -> t -> unit
end

module Result : sig
  type t

  val create : unit -> t

  val approx : t -> Simple_value_approx.t
  val set_approx : t -> Simple_value_approx.t -> t

  val use_staticfail : t -> Static_exception.t -> t
  val used_staticfail : t -> Static_exception.Set.t

  val exit_scope_catch : t -> Static_exception.t -> t

  val map_benefit
    : t
    -> (Inlining_cost.Benefit.t -> Inlining_cost.Benefit.t)
    -> t

  val benefit : t -> Inlining_cost.Benefit.t
  val clear_benefit : t -> t

  val set_inlining_threshold : t -> Inlining_cost.inlining_threshold -> t
  val inlining_threshold : t -> Inlining_cost.inlining_threshold

  val add_global : t -> field_index:int -> approx:Simple_value_approx.t -> t
  val find_global : t -> field_index:int -> Simple_value_approx.t
end
