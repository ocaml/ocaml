(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell for mshinwell: this comment needs updating to reflect
   name changes (e.g. "closure -> set_of_closures"). *)
(** Intermediate language used to perform closure conversion and inlining.

    Closure conversion starts with [Lambda] code.  The conversion transforms
    function declarations into "sets of closures" ([Set_of_closures]
    constructor) in the [Flambda] language.

    The usual case of a function declared on its own will produce a
    [Set_of_closures] containing a single closure.  The closure itself may
    be accessed using [Project_closure] and specifies which variables (by name, not
    by any numeric offset) are free in the corresponding function definition.
    Occurrences of these free variables in the body appear as the usual
    [Var] expressions.

    For the case of multiple functions defined together, possibly mutually
    recursive, a [Set_of_closures] value will be generated containing
    one closure per function.  Each closure may be accessed again using
    [Project_closure], specifying which function's closure is desired.

    As an example, the flambda representation of:

      {[let rec f x = ...
        and g x = ... ]}

    might be (for identifiers [f] and [g] corresponding to the variables of
    the same name in the source text):

      {[Let( closure, Set_of_closures { id_f -> ...; id_g -> ... },
              Let(f, Project_closure { closure = closure; closure_id = id_f },
              Let(g, Project_closure { closure = closure; closure_id = id_g },
              ...)))]}

    One can also use [Project_closure] to move between closures in the same set of
    closures.  For example for [f] and [g] as above, represented together as a
    set of closures, we might apply [Project_closure] to extract the closure for [g],
    and later decide to use [Project_closure] again on this value (not on the set of
    closures) to access the closure for [f].  This is used when inlining
    mutually-recursive functions as a means of avoiding having to keep around
    a value corresponding to the whole set of closures.  For example,
    continuing from the example above:

      {[ Project_closure { closure = Var g; closure_id = id_f;
                    relative_to = Some id_g } ]}

    After closure conversion an inlining pass is performed.  This may
    introduce [Project_var] expressions to represent accesses (from
    the body of inlined functions) to variables bound by closures.  Some of
    these [Project_var] expressions may survive in the tree after
    inlining has finished.

    Other features of this intermediate language are:

    - Access to constants across modules are performed with respect to
      named symbols in the object file (of type [Symbol.t]).

    - Direct calls are distinguished from indirect calls (as in [Clambda])
      using values of type [call_kind].

    - "Structured constants" built from the constructors in type [const]
      are not explicitly represented.  Instead, they are converted into
      expressions such as: [Prim (Pmakeblock(...), ...)].
*)

(* CR mshinwell: add comment explaining that there must be no
   shadowing (and that the invariant checks enforce this) or other
   reuse of names. *)

type call_kind =
  | Indirect
  | Direct of Closure_id.t

type const =
  | Int of int
  | Char of char
  (* [Const_pointer] is an immediate value of a type whose values may be
     boxed (typically a variant type with both constant and non-constant
     constructors). *)
  | Const_pointer of int

type apply = {
  (* CR mshinwell: rename func -> callee, and lhs_of_application -> callee *)
  func : Variable.t;
  args : Variable.t list;
  kind : call_kind;
  dbg : Debuginfo.t;
}

type assign = {
  being_assigned : Mutable_variable.t;
  new_value : Variable.t;
}

type send = {
  kind : Lambda.meth_kind;
  meth : Variable.t;
  obj : Variable.t;
  args : Variable.t list;
  dbg : Debuginfo.t;
}

type project_closure = {
  set_of_closures : Variable.t; (** must yield a set of closures *)
  closure_id : Closure_id.t;
}

type move_within_set_of_closures = {
  closure : Variable.t;  (** must yield a closure *)
  start_from : Closure_id.t;
  move_to : Closure_id.t;
}

type project_var = {
  closure : Variable.t;  (** must yield a closure *)
  closure_id : Closure_id.t;
  var : Var_within_closure.t;
}

type t =
  | Var of Variable.t
  | Let of let_expr
  | Let_mutable of Mutable_variable.t * Variable.t * t
  | Let_rec of (Variable.t * named) list * t
  | Apply of apply
  | Send of send
  | Assign of assign
  | If_then_else of Variable.t * t * t
  | Switch of Variable.t * switch
  (* Restrictions on [Lambda.Lstringswitch] also apply to [String_switch]. *)
  | String_switch of Variable.t * (string * t) list * t option
  | Static_raise of Static_exception.t * t list
  | Static_catch of Static_exception.t * Variable.t list * t * t
  | Try_with of t * Variable.t * t
  | While of t * t
  | For of for_loop
  | Proved_unreachable

(** Values of type [named] will always be [let]-bound to a [Variable.t].

    This has an important consequence: all expressions that we might deem
    constant (and thus assign to a symbol) have an associated variable.

    (The split between [t] and [named] is very similar to the split in
    the language used in Kennedy's "Compiling with Continuations, Continued".
    The main difference, apart from the fact that we do not work in CPS style
    for control flow constructs, is the presence of [Expr].  This could be
    removed in the future to provide a more rigorous ANF-like representation.)
*)

and named =
  | Symbol of Symbol.t
  | Const of const
  | Allocated_const of Allocated_const.t
  | Read_mutable of Mutable_variable.t
  | Read_symbol_field of Symbol.t * int
  (** During the lifting of [let] bindings to [program] constructions after
      closure conversion, we generate symbols and their corresponding
      definitions (which may or may not be constant), together with field
      accesses to such symbols.  We would like it to be the case that such
      field accesses are simplified to the relevant component of the
      symbol concerned.  (The rationale is to generate efficient code and
      share constants as expected: see e.g. tests/asmcomp/staticalloc.ml.)
      The components of the symbol would be identified by other symbols.
      This sort of access pattern is feasible because the top-level structure
      of symbols is statically allocated and fixed at compile time.
      It may seem that [Prim (Pfield, ...)] expressions could be used to
      perform the field accesses.  However for simplicity, to avoid having to
      keep track of properties of individual fields of blocks,
      [Inconstant_idents] never deems a [Prim (Pfield, ...)] expression to be
      constant.  This would in general prevent field accesses to symbols from
      being simplified in the way we would like, since [Lift_constants] would
      not assign new symbols (i.e. the things we would like to simplify to)
      to the various projections from the symbols in question.
      To circumvent this problem we use [Read_symbol_field] when generating
      projections from the top level of symbols.  Owing to the properties of
      symbols described above, such expressions may be eligible for declaration
      as constant by [Inconstant_idents] (and thus themselves lifted to another
      symbol), without any further complication.
      [Read_symbol_field] may only be used when the definition of the symbol
      is in scope in the [program].  For external unresolved symbols, [Pfield]
      may still be used; it will be changed to [Read_symbol_field] by
      [Inline_and_simplify] when (and if) the symbol is imported. *)
  | Set_of_closures of set_of_closures
  | Project_closure of project_closure
  | Move_within_set_of_closures of move_within_set_of_closures
  | Project_var of project_var
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t
  | Expr of t  (** ANF escape hatch. *)

(* CR-someday mshinwell: use [letcont]-style construct to remove e.g.
   [While] and [For]. *)
(* CR-someday mshinwell: try to produce a tighter definition of a "switch"
   (and translate to that earlier) so that middle- and back-end code for
   these can be reduced. *)
(* CR-someday mshinwell: remove [Expr], but to do this easily would probably
   require a continuation-binding construct. *)
(* CR-someday mshinwell: Since we lack expression identifiers on every term,
   we should probably introduce [Mutable_var] into [named] if we introduce
   more complicated analyses on these in the future.  Alternatively, maybe
   consider removing mutable variables altogether. *)

and let_expr = private {
  var : Variable.t;
  defining_expr : named;
  body : t;
  (* These free variable caches are an important optimization. *)
  free_vars_of_defining_expr : Variable.Set.t;
  free_vars_of_body : Variable.Set.t;
}

and set_of_closures = private {
  function_decls : function_declarations;
  (* CR mshinwell: consider renaming [free_vars].  Also, it's still really
     confusing which side of this map to use when.  "Vars bound by the
     closure" is the domain. *)
  free_vars : Variable.t Variable.Map.t;
  (** Parameters known to always alias some variable in the scope of the set
      of closures declaration. For instance, supposing all call sites of f
      are represented in this example,
        [let x = ... in
         let f a b c = ... in
         let y = ... in
         f x y 1;
         f x y 1]
      the specialised arguments of f can (but does not necessarily) contain
      the association [a] -> [x], but cannot contain [b] -> [y] because [f]
      is not in the scope of [y]. If f were the recursive function
      [let rec f a b c = f a 1 2 in], [a] -> [x] would still be a valid
      specialised argument because all recursive calls maintain the invariant.

      This information is used for optimisation purposes, if such a binding is
      known, it is possible to specialise the body of the function according
      to its parameter. This is usually introduced when specialising a
      recursive function, for instance.
        [let rec map f = function
           | [] -> []
           | h :: t -> f h :: map f t
         let map_succ l =
           let succ x = x + 1 in
           map succ l]
      [map] can be duplicated in [map_succ] to be specialised for the argument
      [f]. This will result in
        [let map_succ l =
           let succ x = x + 1 in
           let rec map f = function
             | [] -> []
             | h :: t -> f h :: map f t in
           map succ l]
      with map having [f] -> [succ] in its [specialised_args] field.

      Note that it is usually not correct to erase this information if the
      argument is used.
  *)
  (* CR mshinwell for pchambart: expand upon the last sentence of the previous
     comment *)
  specialised_args : Variable.t Variable.Map.t;
}

and function_declarations = {
  set_of_closures_id : Set_of_closures_id.t;
  funs : function_declaration Variable.Map.t;
  compilation_unit : Compilation_unit.t;
}

and function_declaration = private {
  params : Variable.t list;
  body : t;
  (** All variables free in the *body* of the function.  For example, a
      variable that is bound as one of the function's parameters will still
      be included in this set.  This field is present as an optimization. *)
  (* CR mshinwell: inconsistent naming free_variables/free_vars here and
     above *)
  (* CR mshinwell: make lazy *)
  free_variables : Variable.Set.t;
  (** A stub function is a generated function used to prepare arguments or
      return values to allow indirect calls to functions with a special calling
      convention.  For instance indirect calls to tuplified functions must go
      through a stub.  Stubs will be unconditionally inlined. *)
  (* CR mshinwell for pchambart: Why don't we call this
     [unconditionally_inline]? *)
  stub : bool;
  dbg : Debuginfo.t;
}

(** Equivalent to the similar type in [Lambda]. *)
and switch = {
  numconsts : Ext_types.Int.Set.t; (** Integer cases *)
  consts : (int * t) list; (** Integer cases *)
  numblocks : Ext_types.Int.Set.t; (** Number of tag block cases *)
  blocks : (int * t) list; (** Tag block cases *)
  failaction : t option; (** Action to take if none matched *)
}

and for_loop = {
  bound_var : Variable.t;
  from_value : Variable.t;
  to_value : Variable.t;
  direction : Asttypes.direction_flag;
  body : t
}

(** Like a subset of [Flambda.named], except that instead of [Variable.t]s we
    have [Symbol.t]s, and everything is a constant. *)
and constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * constant_defining_value_block_field list
  | Set_of_closures of set_of_closures  (** [free_vars] must be empty *)
  | Project_closure of Symbol.t * Closure_id.t

and constant_defining_value_block_field =
  | Symbol of Symbol.t
  | Const of const

module Constant_defining_value :
  Ext_types.Identifiable with type t = constant_defining_value

type expr = t

(** A "program" is the contents of one compilation unit. *)
type program =
  | Let_symbol of Symbol.t * constant_defining_value * program
  | Let_rec_symbol of (Symbol.t * constant_defining_value) list * program
  (* CR mshinwell: remove Import_symbol and use a record *)
  | Import_symbol of Symbol.t * program
  | Initialize_symbol of Symbol.t * Tag.t * t list * program
  | Effect of t * program
  | End of Symbol.t
  (** [End] accepts the root symbol: the only symbol that can never be
      eliminated. *)

(** Compute the free variables of a term.  (This is O(1) for [Let]s). *)
val free_variables
   : ?ignore_uses_in_apply:unit
  -> ?ignore_uses_in_project_var:unit
  -> t
  -> Variable.Set.t

(** Compute the free variables of a named expression. *)
val free_variables_named
   : ?ignore_uses_in_project_var:unit
  -> named
  -> Variable.Set.t

(** Used to avoid exceeding the stack limit when handling expressions with
    multiple consecutive nested [Let]-expressions.  This saves rewriting large
    simplification functions in CPS.

    If the expression [e] is:
        Let (v1, defining_expr1,
          ...  (* all [Let]s *)
          Let (vN, defining_exprN, body) ... )
    then [fold_lets e ~init ~for_defining_expr ~for_last_body] is:
        Let (v1, accum == for_defining_expr init defining_expr1,
          ...
          Let (vN, accum == for_defining_expr accum defining_exprN,
            for_last_body accum body) ... )
    The stack usage of [fold_lets] is independent of the depth of nesting of
    the [Let]s.

    If the expression [e] is not a [Let], then [for_last_body init e] is
    returned.
*)
(*
val fold_lets
   : t
  -> init:'a
  -> for_defining_expr:('a -> Variable.t -> named -> 'a * named)
  -> for_last_body:('a -> t -> 'b * t)
  -> 'b * t
*)

(** Like [fold_lets], but just a map. *)
val map_lets
   : t
  -> for_defining_expr:(Variable.t -> named -> named)
  -> for_last_body:(t -> t)
  -> after_rebuild:(t -> t)
  -> t

val iter_lets
   : t
  -> for_defining_expr:(Variable.t -> named -> unit)
  -> for_last_body:(t -> unit)
  -> for_each_let:(t -> unit)
  -> unit

(*
(** Like [fold_lets], but just iteration.  [for_last_body] is never called
    if the input expression is not a [Let]. *)
val iter_lets
   : t
  -> for_defining_expr:(Variable.t -> named -> unit)
  -> for_last_body:(t -> free_vars_of_body:Variable.Set.t -> unit)
  -> unit
*)

(** Creates a [Let] expression.  (This computes the free variables of the
    defining expression and the body.) *)
val create_let : Variable.t -> named -> t -> t

module With_free_variables : sig
  type 'a t

  (** O(1) time. *)
  val of_defining_expr_of_let : let_expr -> named t

  (** O(1) time. *)
  val of_body_of_let : let_expr -> expr t

  (** Takes the time required to calculate the free variables of the given
      term (proportional to the size of the term, except that the calculation
      for [Let] is O(1)). *)
  val of_expr : expr -> expr t

  (** Takes the time required to calculate the free variables of the given
      [expr]. *)
  val create_let_reusing_defining_expr
     : Variable.t
    -> named t
    -> expr
    -> expr

  (** Takes the time required to calculate the free variables of the given
      [named]. *)
  val create_let_reusing_body
     : Variable.t
    -> named
    -> expr t
    -> expr

  (** O(1) time. *)
  val create_let_reusing_both
     : Variable.t
    -> named t
    -> expr t
    -> expr

  (** The equivalent of the [Expr] constructor. *)
  val expr : expr t -> named t

  val contents : 'a t -> 'a

  (** O(1) time. *)
  val free_variables : _ t -> Variable.Set.t
end

(* CR mshinwell: try to move the non-recursive types out to a separate .mli *)
(* CR mshinwell: consider moving [Flambda_utils] functions into here, now we
   are forced to have a .ml *)

val create_function_declaration
   : params:Variable.t list
  -> body:t
  -> stub:bool
  -> dbg:Debuginfo.t
  -> function_declaration

val create_set_of_closures
   : function_decls:function_declarations
  -> free_vars:Variable.t Variable.Map.t
  -> specialised_args:Variable.t Variable.Map.t
  -> set_of_closures

val used_params : function_declaration -> Variable.Set.t

val print : Format.formatter -> t -> unit

val print_named : Format.formatter -> named -> unit

val print_program : Format.formatter -> program -> unit

val print_constant_defining_value
   : Format.formatter
  -> constant_defining_value
  -> unit

val print_function_declaration
   : Format.formatter
  -> Variable.t * function_declaration
  -> unit

val print_function_declarations
   : Format.formatter
  -> function_declarations
  -> unit

val print_project_closure
   : Format.formatter
  -> project_closure
  -> unit

val print_move_within_set_of_closures
   : Format.formatter
  -> move_within_set_of_closures
  -> unit

val print_project_var
   : Format.formatter
  -> project_var
  -> unit

val print_set_of_closures
   : Format.formatter
  -> set_of_closures
  -> unit
