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

(** Environments and result structures used during inlining and
    simplification.  (See inline_and_simplify.ml.) *)

module Env : sig
  (** Environments follow the lexical scopes of the program. *)
  type t

  (** Create a new environment.  If [never_inline] is true then the returned
      environment will prevent [Inline_and_simplify] from inlining.  The
      [backend] parameter is used for passing information about the compiler
      backend being used.
      Newly-created environments have inactive [Freshening]s (see below) and do
      not initially hold any approximation information. *)
  val create
     : never_inline:bool
    -> backend:(module Backend_intf.S)
    -> t

  (** Obtain the first-class module that gives information about the
      compiler backend being used for compilation. *)
  val backend : t -> (module Backend_intf.S)

  (** Add the approximation of a variable---that is to say, some knowledge
      about the value(s) the variable may take on at runtime---to the
      environment. *)
  val add : t -> Variable.t -> Simple_value_approx.t -> t

  (** Find the approximation of a given variable, raising a fatal error if
      the environment does not know about the variable.  Use [find_opt]
      instead if you need to catch the failure case. *)
  val find_exn : t -> Variable.t -> Simple_value_approx.t

  (** Like [find_exn], but intended for use where the "not present in
      environment" case is to be handled by the caller. *)
  val find_opt : t -> Variable.t -> Simple_value_approx.t option

  (** Like [find_exn], but for a list of variables. *)
  val find_list_exn : t -> Variable.t list -> Simple_value_approx.t list

  (** Whether the environment has an approximation for the given variable. *)
  val mem : t -> Variable.t -> bool

  (** Return the freshening that should be applied to variables when
      rewriting code (in [Inline_and_simplify], etc.) using the given
      environment. *)
  val freshening : t -> Freshening.t

  (** Set the freshening that should be used as per [freshening], above. *)
  val set_freshening : t -> Freshening.t -> t

  (** Causes every bound variable in code rewritten during inlining and
      simplification, using the given environment, to be freshened.  This is
      used when descending into subexpressions substituted into existing
      expressions. *)
  val activate_freshening : t -> t

  (** Erase all variable approximation information and freshening information
      from the given environment.  However, the freshening activation state
      is preserved.  This function is used when rewriting inside a function
      declaration, to avoid (due to a compiler bug) accidental use of
      variables from outer scopes that are not accessible. *)
  val local : t -> t

  (** Note that the inliner is descending into a function body from the given
      set of closures.  A set of such descents is maintained. *)
  (* CR mshinwell: consider changing name to remove "declaration".  Also,
     isn't this the inlining stack?  Maybe we can use that instead. *)
  val enter_set_of_closures_declaration : Set_of_closures_id.t -> t -> t

  (** Determine whether the inliner is currently inside a function body from
      the given set of closures.  This is used to detect whether a given
      function call refers to a function which exists somewhere on the current
      inlining stack. *)
  val inside_set_of_closures_declaration : Set_of_closures_id.t -> t -> bool

  (** Not inside a closure declaration.
      Toplevel code is the one evaluated when the compilation unit is
      loaded *)
  val at_toplevel : t -> bool

  val is_inside_branch : t -> bool
  val inside_branch : t -> t
  val inside_simplify : t -> t

  val increase_closure_depth : t -> t

  (** Mark that call sites contained within code rewritten using the given
      environment should never be replaced by inlined (or unrolled) versions
      of the callee(s). *)
  val set_never_inline : t -> t

  (** Return whether [set_never_inline] is currently in effect on the given
      environment. *)
  val never_inline : t -> bool

  val inlining_level : t -> int

  (** Mark that this environment is used to rewrite code for inlining. This is
      used by the inlining heuristics to decide wether to continue.
      Unconditionally inlined does not take this into account. *)
  val inlining_level_up : t -> t

  (** Whether it is permissible to unroll a call to a recursive function
      in the given environment. *)
  val unrolling_allowed : t -> bool

  (** Whether the given environment is currently being used to rewrite the
      body of an unrolled recursive function. *)
  (* CR mshinwell: clarify comment *)
  val inside_unrolled_function : t -> t

  (** If collecting inlining statistics, record that the inliner is about to
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

  (** Return the closure stack, used for the generation of inlining statistics,
      stored inside the given environment. *)
  val inlining_stats_closure_stack
     : t
    -> Inlining_stats.Closure_stack.t

  (** Print a human-readable version of the given environment. *)
  val print : Format.formatter -> t -> unit
end

module Result : sig
  (** Result structures approximately follow the evaluation order of the
      program.  They are returned by the simplification algorithm acting on
      an Flambda subexpression. *)
  type t

  val create : unit -> t

  (** The approximation of the subexpression that has just been
      simplified. *)
  val approx : t -> Simple_value_approx.t

  (** Set the approximation of the subexpression that has just been
      simplified.  Typically used just before returning from a case of the
      simplification algorithm. *)
  val set_approx : t -> Simple_value_approx.t -> t

  (** All static exceptions for which [use_staticfail] has been called on
      the given result structure. *)
  val used_staticfail : t -> Static_exception.Set.t

  (** Mark that the given static exception has been used. *)
  (* CR mshinwell: consider rename to [use_static_exception] *)
  val use_staticfail : t -> Static_exception.t -> t

  (** Mark that we are moving up out of the scope of a static-catch block
      that catches the given static exception identifier.  This has the effect
      of removing the identifier from the [used_staticfail] set. *)
  val exit_scope_catch : t -> Static_exception.t -> t

  (** The benefit to be gained by inlining the subexpression whose
      simplification yielded the given result structure. *)
  val benefit : t -> Inlining_cost.Benefit.t

  (** Apply a transformation to the inlining benefit stored within the
      given result structure. *)
  val map_benefit
    : t
    -> (Inlining_cost.Benefit.t -> Inlining_cost.Benefit.t)
    -> t

  (* CR mshinwell: rename [clear_benefit], it might be misconstrued *)
  (** Set the benefit of inlining the subexpression corresponding to the
      given result structure to zero. *)
  val clear_benefit : t -> t

  val set_inlining_threshold : t -> Inlining_cost.inlining_threshold -> t
  val inlining_threshold : t -> Inlining_cost.inlining_threshold

  val add_global : t -> field_index:int -> approx:Simple_value_approx.t -> t
  val find_global : t -> field_index:int -> Simple_value_approx.t
end
