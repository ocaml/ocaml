(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ext_types
open Abstract_identifiers

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
              Flet(f, Fclosure { fu_closure = closure; fu_fun = id_f },
              Flet(g, Fclosure { fu_closure = closure; fu_fun = id_g },
              ...)))]}

    One can also use [Fclosure] to move between closures in the same set of
    closures.  For example for [f] and [g] as above, represented together as a
    set of closures, we might apply [Fclosure] to extract the closure for [g],
    and later decide to use [Fclosure] again on this value (not on the set of
    closures) to access the closure for [f].  This is used when inlining
    mutually-recursive functions as a means of avoiding having to keep around
    a value corresponding to the whole set of closures.  For example,
    continuing from the example above:

      {[ Fclosure { fu_closure = Fvar g; fu_fun = id_f;
                    fu_relative_to = Some id_g } ]}

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

(* CR mshinwell: These constructors aren't very descriptive.  We should try
   to improve the names.
   pchambart: we want to say that it can be assigned. Maybe Assignable, or
     Mutable/Immutable
 *)
type let_kind =
  | Not_assigned
  | Assigned

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
type 'a flambda =
  | Fsymbol of Symbol.t * 'a
  | Fvar of Variable.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a fapply * 'a
  | Fset_of_closures of 'a fset_of_closures * 'a
  | Fclosure of 'a fclosure * 'a
  (* CR mshinwell for pchambart: rename to [Fvar_within_closure] to match
     [Var_within_closure]?
     XCR pchambda: done *)
  | Fvar_within_closure of 'a fvar_within_closure * 'a
  | Flet of let_kind * Variable.t * 'a flambda * 'a flambda * 'a
  | Fletrec of (Variable.t * 'a flambda) list * 'a flambda * 'a
  | Fprim of Lambda.primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a fswitch * 'a
  (* Restrictions on [Lambda.Lstringswitch] also apply here *)
  | Fstringswitch of 'a flambda * (string * 'a flambda) list *
                     'a flambda option * 'a
  | Fstaticraise of Static_exception.t * 'a flambda list * 'a
  | Fstaticcatch of
      Static_exception.t * Variable.t list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * Variable.t * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of Variable.t * 'a flambda * 'a flambda * Asttypes.direction_flag *
            'a flambda * 'a
  | Fassign of Variable.t * 'a flambda * 'a
  | Fsend of Lambda.meth_kind * 'a flambda * 'a flambda * 'a flambda list *
             Debuginfo.t * 'a
  | Funreachable of 'a
      (** Represents code that has been proved to be unreachable. *)

and 'a t = 'a flambda

and 'a fapply = {
  ap_function : 'a flambda;
  ap_arg : 'a flambda list;
  ap_kind : call_kind;
  ap_dbg : Debuginfo.t;
}

and 'a fset_of_closures = {
  cl_fun : 'a function_declarations;
  cl_free_var : 'a flambda Variable.Map.t;
  cl_specialised_arg : Variable.t Variable.Map.t;
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
      with map having [f] -> [succ] in his [cl_specialised_arg] field.

      Note that it is usualy not correct to erase this information if the
      argument is used.
  *)
}

and 'a function_declarations = {
  ident : Set_of_closures_id.t;
  funs : 'a function_declaration Variable.Map.t;
  compilation_unit : Symbol.Compilation_unit.t;
}

and 'a function_declaration = {
  params : Variable.t list;
  body : 'a flambda;
  free_variables : Variable.Set.t;
  (** All variables free in the *body* of the function.  For example, a
      variable that is bound as one of the function's parameters will still
      be included in this set.  This field is present as an optimization. *)
  stub : bool;
  (** A stub function is a generated function used to prepare arguments or
      return values to allow indirect calls to functions with a special calling
      convention.  For instance indirect calls to tuplified functions must go
      through a stub.  Stubs will be unconditionally inlined. *)
  dbg : Debuginfo.t;
}

and 'a fclosure = {
  (* CR mshinwell: The [fu_closure] field is confusing.  Can we get this to
     have a variant type?  Not sure *)
  fu_closure : 'a flambda;
  fu_fun : Closure_id.t;
  fu_relative_to : Closure_id.t option;
  (** For use when applying [Fclosure] to an existing (that is to say,
      [Fclosure]) closure value rather than a set of closures. *)
}

and 'a fvar_within_closure = {
  (** [vc_closure] must yield a closure rather than a set of closures. *)
  vc_closure : 'a flambda;
  vc_fun : Closure_id.t;
  vc_var : Var_within_closure.t;
}

and 'a fswitch = {  (** Equivalent to the similar type in [Lambda]. *)
  fs_numconsts : Int.Set.t; (** Integer cases *)
  fs_consts : (int * 'a flambda) list; (** Integer cases *)
  fs_numblocks : Int.Set.t; (** Number of tag block cases *)
  fs_blocks : (int * 'a flambda) list; (** Tag block cases *)
  fs_failaction : 'a flambda option; (** Action to take if none matched *)
}
