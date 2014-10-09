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
open Symbol
open Abstract_identifiers

(** A variant of lambda code with explicit closures, where every
    dependency is explicit.

    The particularities are:

    * Symbolic closures: fields are referenced by unique identifiers
      (types [Closure_function.t] and [Closure_variable.t], see
      [Abstract_identifiers]);

    * Explicit external constants access (type [Symbol.t]);

    * Direct calls are explicit;

    * Each node carry a value that can be used for term identifiers;

    * No structured constants: they are converted into
      [Fprim (Pmakeblock(...), ...)].

  *)

  (** There are 2 kind of closures: specified and unspecified ones.
      A closure is first build as unspecified using the Fclosure constructor.
      It represents a block containing code pointers and values (the free
      variables).

      Since a closure can contain multiple functions, an unspecified
      closure can't be directly used, we first need to select (specify)
      which function the closure represents using Ffunction. The 2
      constructors that can be applied on specified closures are

      - Fapply: call the selected function

      - Fvariable_in_closure: access the free variables

      Typical usage when compiling

      {[let rec f x = ...
        and g x = ... ]}

      is to represent it as:

      {[Flet( closure, Fclosure { id_f -> ...; id_g -> ... },
              Flet( f, Ffunction { fu_closure = closure; fu_fun = id_f },
              Flet( g, Ffunction { fu_closure = closure; fu_fun = id_g }, ...)))]}

      Accessing a variable from a closure is done

      - with [Fvar] inside a function declared in the closure

      - with [Fvariable_in_closure] from outside. This kind of access
        is generated when inlining a function.

      It is possible to specify an already specified closure. This can
      happen when inlining a function that access other functions from
      the same closure: For instance, if f from the previous example
      access g and is inlined, calling g will use the closure:

      {[ Ffunction { fu_closure = f; fu_fun = id_g; fu_relative_to = Some id_f } ]}

  *)

type let_kind =
  | Not_assigned
  | Assigned

type call_kind =
  | Indirect
  | Direct of function_within_closure

type const =
  (* no structured constant *)
  | Fconst_base of Asttypes.constant
  | Fconst_pointer of int
  | Fconst_float_array of string list
  | Fconst_immstring of string

(* A data is attached to each node. It is often used to uniquely
   identify an expression *)
type 'a flambda =
    Fsymbol of Symbol.t * 'a
  | Fvar of Variable.t * 'a
  | Fconst of const * 'a
  | Fapply of 'a fapply * 'a
  | Fclosure of 'a fclosure * 'a
  | Ffunction of 'a ffunction * 'a
  | Fvariable_in_closure of 'a fvariable_in_closure * 'a
  | Flet of let_kind * Variable.t * 'a flambda * 'a flambda * 'a
  | Fletrec of (Variable.t * 'a flambda) list * 'a flambda * 'a
  | Fprim of Lambda.primitive * 'a flambda list * Debuginfo.t * 'a
  | Fswitch of 'a flambda * 'a fswitch * 'a
  | Fstaticraise of static_exception * 'a flambda list * 'a
  | Fstaticcatch of
      static_exception * Variable.t list * 'a flambda * 'a flambda * 'a
  | Ftrywith of 'a flambda * Variable.t * 'a flambda * 'a
  | Fifthenelse of 'a flambda * 'a flambda * 'a flambda * 'a
  | Fsequence of 'a flambda * 'a flambda * 'a
  | Fwhile of 'a flambda * 'a flambda * 'a
  | Ffor of Variable.t * 'a flambda * 'a flambda * Asttypes.direction_flag *
            'a flambda * 'a
  | Fassign of Variable.t * 'a flambda * 'a
  | Fsend of Lambda.meth_kind * 'a flambda * 'a flambda * 'a flambda list *
             Debuginfo.t * 'a

  | Fevent of 'a flambda * Lambda.lambda_event * 'a
      (** Only with -g in bytecode. *)

  | Funreachable of 'a
      (** Represent a code that has been proved to be unreachable. *)

and 'a fapply =
  { ap_function: 'a flambda;
    ap_arg: 'a flambda list;
    ap_kind: call_kind;
    ap_dbg: Debuginfo.t }

and 'a fclosure =
  { cl_fun : 'a function_declarations;
    cl_free_var : 'a flambda VarMap.t;
    cl_specialised_arg : Variable.t VarMap.t }

and 'a function_declarations = {
  ident : FunId.t;
  funs : 'a function_declaration VarMap.t;
  (** The ident key correspond to off_id of offset type *)
  compilation_unit : compilation_unit;
}

and 'a function_declaration = {
  stub : bool;
  (** A stub function is a generated function used to prepare
      arguments or return value to allow indirect calls to function
      with a special call convention. For instance indirect calls to
      tuplified function must go through a stub. Stubs will be
      unconditionnaly inlined. *)
  params : Variable.t list;
  free_variables : VarSet.t;
  (** All free variables used in body, including function parameters,
      functions and variables declared in the closure.
      It is present for efficiency reasons. *)
  body : 'a flambda;
  dbg : Debuginfo.t;
}

and 'a ffunction = {
  fu_closure: 'a flambda;
  fu_fun: function_within_closure;
  fu_relative_to: function_within_closure option;
  (** Keeps track of the original function When specifying an already
      specified function. *)
}

and 'a fvariable_in_closure = {
  vc_closure : 'a flambda; (** A selected closure *)
  vc_fun : function_within_closure;
  vc_var : variable_within_closure;
}

and 'a fswitch =
  { fs_numconsts: IntSet.t; (** integer cases *)
    fs_consts: (int * 'a flambda) list; (** Integer cases *)
    fs_numblocks: IntSet.t; (** Number of tag block cases *)
    fs_blocks: (int * 'a flambda) list; (** Tag block cases *)
    fs_failaction: 'a flambda option } (** Action to take if none matched *)



(** Access functions *)

val find_declaration :
  function_within_closure -> 'a function_declarations -> 'a function_declaration
(** [find_declaration f decl] raises Not_found if [f] is not in [decl] *)

val find_declaration_variable :
  function_within_closure -> 'a function_declarations -> Variable.t
(** [find_declaration_variable f decl] raises Not_found if [f] is not in [decl] *)

val find_free_variable :
  variable_within_closure -> 'a fclosure -> 'a flambda
(** [find_free_variable v clos] raises Not_found if [c] is not in [clos] *)


(* utility functions *)

val function_arity : 'a function_declaration -> int

val variables_bound_by_the_closure :
  function_within_closure -> 'a function_declarations -> VarSet.t

val can_be_merged : 'a flambda -> 'a flambda -> bool
(** If [can_be_merged f1 f2] is true, it is safe to merge switch
    branches containing [f1] and [f2] *)

val data_at_toplevel_node : 'a flambda -> 'a

val description_of_toplevel_node : 'a flambda -> string

val recursive_functions : 'a function_declarations -> VarSet.t
