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

(** Intermediate language used to perform closure conversion and inlining.

    Closure conversion starts with [Lambda] code.  The conversion transforms
    function declarations into "sets of closures" ([Fset_of_closures]
    constructor) in the [Flambda] language.

    The usual case of a function declared on its own will produce a
    [Fset_of_closures] containing a single closure.  The closure itself may
    be accessed using [Fclosure] and specifies which variables (by name, not
    by any numeric offset) are free in the corresponding function definition.
    Occurrences of these free variables in the body appear as the usual
    [Fvar] expressions.

    For the case of multiple functions defined together, possibly mutually
    recursive, a [Fset_of_closures] value will be generated containing
    one closure per function.  Each closure may be accessed again using
    [Fclosure], specifying which function's closure is desired.

    As an example, the flambda representation of:

      {[let rec f x = ...
        and g x = ... ]}

    might be (for identifiers [f] and [g] corresponding to the variables of
    the same name in the source text):

      {[Flet( closure, Fset_of_closures { id_f -> ...; id_g -> ... },
              Flet(f, Fclosure { closure = closure; closure_id = id_f },
              Flet(g, Fclosure { closure = closure; closure_id = id_g },
              ...)))]}

    One can also use [Fclosure] to move between closures in the same set of
    closures.  For example for [f] and [g] as above, represented together as a
    set of closures, we might apply [Fclosure] to extract the closure for [g],
    and later decide to use [Fclosure] again on this value (not on the set of
    closures) to access the closure for [f].  This is used when inlining
    mutually-recursive functions as a means of avoiding having to keep around
    a value corresponding to the whole set of closures.  For example,
    continuing from the example above:

      {[ Fclosure { closure = Fvar g; closure_id = id_f;
                    relative_to = Some id_g } ]}

    After closure conversion an inlining pass is performed.  This may
    introduce [Fvar_within_closure] expressions to represent accesses (from
    the body of inlined functions) to variables bound by closures.  Some of
    these [Fvar_within_closure] expressions may survive in the tree after
    inlining has finished.

    Other features of this intermediate language are:

    - Access to constants across modules are performed with respect to
      named symbols in the object file (of type [Symbol.t]).

    - Direct calls are distinguished from indirect calls (as in [Clambda])
      using values of type [call_kind].

    - Nodes making up an expression in the language may be annotated with
      arbitrary values (the ['a] in [type 'a flambda]).

    - "Structured constants" built from the constructors in type [const]
      are not explicitly represented.  Instead, they are converted into
      expressions such as: [Fprim (Pmakeblock(...), ...)].
*)

open Abstract_identifiers

type let_kind =
  | Immutable
  | Mutable

type call_kind =
  | Indirect
  | Direct of Closure_id.t

type const =
  (* Note: no structured constants *)
  | Fconst_base of Asttypes.constant
  | Fconst_pointer of int
  | Fconst_float_array of string list
  | Fconst_immstring of string
  | Fconst_float of float

(* The value of type ['a] may be used for annotation of an flambda expression
   by some optimization pass. *)
type 'a t =
  | Fsymbol of Symbol.t * 'a
  | Fvar of Variable.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a apply * 'a
  | Fset_of_closures of 'a set_of_closures * 'a
  | Fclosure of 'a closure * 'a
  | Fvar_within_closure of 'a var_within_closure * 'a
  | Flet of let_kind * Variable.t * 'a t * 'a t * 'a
  | Fletrec of (Variable.t * 'a t) list * 'a t * 'a
  | Fprim of Lambda.primitive * 'a t list * Debuginfo.t * 'a
  | Fswitch of 'a t * 'a switch * 'a
  (* Restrictions on [Lambda.Lstringswitch] also apply here *)
  | Fstringswitch of 'a t * (string * 'a t) list * 'a t option * 'a
  | Fstaticraise of Static_exception.t * 'a t list * 'a
  | Fstaticcatch of Static_exception.t * Variable.t list * 'a t * 'a t * 'a
  | Ftrywith of 'a t * Variable.t * 'a t * 'a
  | Fifthenelse of 'a t * 'a t * 'a t * 'a
  | Fsequence of 'a t * 'a t * 'a
  | Fwhile of 'a t * 'a t * 'a
  | Ffor of Variable.t * 'a t * 'a t * Asttypes.direction_flag * 'a t * 'a
  | Fassign of Variable.t * 'a t * 'a
  | Fsend of Lambda.meth_kind * 'a t * 'a t * 'a t list * Debuginfo.t * 'a
  | Funreachable of 'a  (** Represents code proved unreachable. *)

and 'a apply = {
  func : 'a t;
  args : 'a t list;
  kind : call_kind;
  dbg : Debuginfo.t;
}

and 'a set_of_closures = {
  function_decls : 'a function_declarations;
  free_vars : 'a t Variable.Map.t;
  (** Parameters known to always alias some variable in the scope of the set
      of closures declaration. For instance, supposing all call sites of f
      are represented in this example,
        [let x = ... in
         let f a b c = ... in
         let y = ... in
         f x y 1;
         f x y 1]
      the specialised arguments of f can (but does not necessarily) contain
      the assotiation [a] -> [x], but cannot contain [b] -> [y] because [f]
      is not in the scope of [y]. If f where the recursive function,
      [let rec f a b c = f a 1 2 in], [a] -> [x] whould still be a valid
      specialised argument because all recursive calls maintain the invariant.

      This information is used for optimisation purpose, if such a binding is
      known, it is possible to specialise the body of the function according
      to its parameter. This is usualy introduced when specialising a recusive
      function, for instance.
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
      with map having [f] -> [succ] in his [specialised_args] field.

      Note that it is usualy not correct to erase this information if the
      argument is used.
  *)
  specialised_args : Variable.t Variable.Map.t;
}

and 'a function_declarations = {
  ident : Set_of_closures_id.t;
  funs : 'a function_declaration Variable.Map.t;
  compilation_unit : Symbol.Compilation_unit.t;
}

and 'a function_declaration = {
  params : Variable.t list;
  body : 'a t;
  (** All variables free in the *body* of the function.  For example, a
      variable that is bound as one of the function's parameters will still
      be included in this set.  This field is present as an optimization. *)
  free_variables : Variable.Set.t;
  (** A stub function is a generated function used to prepare arguments or
      return values to allow indirect calls to functions with a special calling
      convention.  For instance indirect calls to tuplified functions must go
      through a stub.  Stubs will be unconditionally inlined. *)
  stub : bool;
  dbg : Debuginfo.t;
}

and 'a closure = {
  (* CR mshinwell: The [closure] field is confusing.  Can we get this to
     have a variant type?  Not sure *)
  closure : 'a t;
  closure_id : Closure_id.t;
  (** For use when applying [Fclosure] to an existing (that is to say,
      [Fclosure]) closure value rather than a set of closures. *)
  relative_to : Closure_id.t option;
}

and 'a var_within_closure = {
  (** [closure] must yield a closure rather than a set of closures. *)
  closure : 'a t;
  closure_id : Closure_id.t;
  var : Var_within_closure.t;
}

and 'a switch = {  (** Equivalent to the similar type in [Lambda]. *)
  numconsts : Ext_types.Int.Set.t; (** Integer cases *)
  consts : (int * 'a t) list; (** Integer cases *)
  numblocks : Ext_types.Int.Set.t; (** Number of tag block cases *)
  blocks : (int * 'a t) list; (** Tag block cases *)
  failaction : 'a t option; (** Action to take if none matched *)
}
