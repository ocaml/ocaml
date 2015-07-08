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

    - Nodes making up an expression in the language may be annotated with
      arbitrary values (the ['a] in [type 'a Flambda.t]).

    - "Structured constants" built from the constructors in type [const]
      are not explicitly represented.  Instead, they are converted into
      expressions such as: [Prim (Pmakeblock(...), ...)].
*)

type let_kind =
  | Immutable
  | Mutable

type call_kind =
  | Indirect
  | Direct of Closure_id.t

type const =
  | Const_base of Asttypes.constant
  | Const_pointer of int
  | Const_float_array of string list
  | Const_immstring of string
  | Const_float of float

type apply = {
  func : Variable.t;
  args : Variable.t list;
  kind : call_kind;
  dbg : Debuginfo.t;
}

type project_closure = {
  (** [set_of_closures] must yield a set of closures rather than a closure. *)
  set_of_closures : Variable.t;
  closure_id : Closure_id.t;
}

type move_within_set_of_closures = {
  (** [closure] must yield a closure rather than a set of closures. *)
  closure : Variable.t;
  start_from : Closure_id.t;
  move_to : Closure_id.t;
}

type project_var = {
  (** [closure] must yield a closure rather than a set of closures. *)
  closure : Variable.t;
  closure_id : Closure_id.t;
  var : Var_within_closure.t;
}

(** The value of type ['a] may be used for annotation of an flambda expression
    by some optimization pass. *)
type 'a t =
  | Var of Variable.t * 'a
  | Apply of apply * 'a
  (* CR-someday mshinwell: consider eliminating assignment from Flambda
     onwards *)
  | Assign of Variable.t * 'a t * 'a
  | Send of Lambda.meth_kind * 'a t * 'a t * 'a t list * Debuginfo.t * 'a
  | Unreachable of 'a  (** Represents code proved unreachable. *)
  | Let of let_kind * Variable.t * 'a named * 'a t * 'a
  | Let_rec of (Variable.t * 'a named) list * 'a t * 'a
  | If_then_else of 'a t * 'a t * 'a t * 'a
  (* CR-someday mshinwell: try to produce a tighter definition of a "switch"
     (and translate to that earlier) so that middle- and back-end code for
     these can be reduced. *)
  | Switch of 'a t * 'a switch * 'a
  (* Restrictions on [Lambda.Lstringswitch] also apply here *)
  | String_switch of 'a t * (string * 'a t) list * 'a t option * 'a
  | Static_raise of Static_exception.t * 'a t list * 'a
  | Static_catch of Static_exception.t * Variable.t list * 'a t * 'a t * 'a
  | Try_with of 'a t * Variable.t * 'a t * 'a
  | While of 'a t * 'a t * 'a
  | For of Variable.t * 'a t * 'a t * Asttypes.direction_flag * 'a t * 'a

(** Values of type ['a named] will always be [let]-bound to a [Variable.t].

    This has an important consequence: all expressions that we might deem
    constant (and thus assign to a symbol) have an associated variable.

    (The split between ['a t] and ['a named] is very similar to the split in
    the language used in Kennedy's "Compiling with Continuations, Continued".
    The main difference, apart from the fact that we do not work in CPS style
    for control flow constructs, is the presence of [Expr].  This could be
    removed in the future to provide a more rigorous ANF-like representation.)
*)
and 'a named =
  | Symbol of Symbol.t * 'a
  | Const of const * 'a
  | Set_of_closures of 'a set_of_closures * 'a
  | Project_closure of project_closure * 'a
  | Move_within_set_of_closures of move_within_set_of_closures * 'a
  | Project_var of project_var * 'a
  | Prim of Lambda.primitive * Variable.t list * Debuginfo.t * 'a
  | Expr of 'a t

and 'a set_of_closures = {
  function_decls : 'a function_declarations;
  (* CR mshinwell: consider renaming [free_vars] *)
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

and 'a function_declarations = {
  set_of_closures_id : Set_of_closures_id.t;
  funs : 'a function_declaration Variable.Map.t;
  compilation_unit : Compilation_unit.t;
}

and 'a function_declaration = {
  params : Variable.t list;
  body : 'a t;
  (** All variables free in the *body* of the function.  For example, a
      variable that is bound as one of the function's parameters will still
      be included in this set.  This field is present as an optimization. *)
  (* CR mshinwell: inconsistent naming free_variables/free_vars here and
     above *)
  free_variables : Variable.Set.t;
  (** A stub function is a generated function used to prepare arguments or
      return values to allow indirect calls to functions with a special calling
      convention.  For instance indirect calls to tuplified functions must go
      through a stub.  Stubs will be unconditionally inlined. *)
  stub : bool;
  dbg : Debuginfo.t;
}

and 'a switch = {  (** Equivalent to the similar type in [Lambda]. *)
  numconsts : Ext_types.Int.Set.t; (** Integer cases *)
  consts : (int * 'a t) list; (** Integer cases *)
  numblocks : Ext_types.Int.Set.t; (** Number of tag block cases *)
  blocks : (int * 'a t) list; (** Tag block cases *)
  failaction : 'a t option; (** Action to take if none matched *)
}
