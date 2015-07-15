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

(** Environments and auxiliary structures used during closure conversion. *)

(** Used to remember which [Variable.t] values correspond to which
    [Ident.t] values during closure conversion, and similarly for
     static exception identifiers. *)
module Env : sig
  type t

  val empty : t

  val add_var : t -> Ident.t -> Variable.t -> t
  val add_vars : t -> Ident.t list -> Variable.t list -> t

  val find_var : t -> Ident.t -> Variable.t

  val add_static_exception : t -> int -> Static_exception.t -> t
  val find_static_exception : t -> int -> Static_exception.t
end

(** Used to represent information about a set of function declarations
    during closure conversion.  (The only case in which such a set may
    contain more than one declaration is when processing "let rec".) *)
module Function_decls : sig
  module Function_decl : sig
    type t

    val create
       : let_rec_ident:Ident.t option
      -> closure_bound_var:Variable.t
      -> kind:Lambda.function_kind
      -> params:Ident.t list
      -> body:Lambda.lambda
      -> t

    val let_rec_ident : t -> Ident.t
    val closure_bound_var : t -> Variable.t
    val kind : t -> Lambda.function_kind
    val params : t -> Ident.t list
    val body : t -> Lambda.lambda

    (* [primitive_wrapper t] is [None] iff [t] is not a wrapper for a function
       with default optionnal arguments. Otherwise it is [Some body], where
       [body] is the body of the wrapper. *)
    val primitive_wrapper : t -> Lambda.lambda option

    (* Like [used_idents_by_function], but for just one function. *)
    val used_idents : t -> Lambda.IdentSet.t
  end

  type t = Function_decl.t list

  val create : Function_decl.t list -> t
  val to_list : t -> Function_decl.t list

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t -> Lambda.IdentSet.t

  (* A map from identifiers to their corresponding [Variable.t]s whose domain
     is the set of all identifiers free in the bodies of the declarations that
     are not bound as parameters. *)
  val closure_env_without_parameters : t -> Env.t
end
