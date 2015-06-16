open Abstract_identifiers

type t

val empty : never_inline:bool -> t

val local : t -> t

val inlining_level_up : t -> t
(* This environment is used to rewrite code for inlining. This is
   used by the inlining heuristics to decide wether to continue.
   Unconditionnaly inlined does not take this into account. *)

val find : Variable.t -> t -> Simple_value_approx.t
(* Recover informations about the potential values of a variable.
   Fails if no information was present in the environment *)

val present : t -> Variable.t -> bool

val activate_substitution : t -> t
(* Every variables declaration in the code rewriten using this environment
   will be alpha renamed *)
val disactivate_substitution : t -> t

val add_approx : Variable.t -> Simple_value_approx.t -> t -> t

val clear_approx : Variable.t -> t -> t
(* Explicitely record the fact that this variable does not carry any
   informations. Used for mutable variables *)

val enter_set_of_closures_declaration : Set_of_closures_id.t -> t -> t

val inside_set_of_closures_declaration : Set_of_closures_id.t -> t -> bool

val at_toplevel : t -> bool
(** Not inside a closure declaration.
    Toplevel code is the one evaluated when the compilation unit is loaded *)

val is_inside_branch : t -> bool

val inside_branch : t -> t
val inside_loop : t -> t

val set_sb : Flambdasubst.t -> t -> t

val increase_closure_depth : t -> t

val set_never_inline : t -> t

val unrolling_allowed : t -> bool

val inside_unrolled_function : t -> t

val inlining_level : t -> int
val sb : t -> Flambdasubst.t
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

val inlining_stats_closure_stack
   : t
  -> Inlining_stats.Closure_stack.t
