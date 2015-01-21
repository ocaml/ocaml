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

(** Various "abstract" identifiers to be used in [Flambda].

    * [Variable.t] is the equivalent of of non-persistent [Ident.t] in
      the [Flambda] tree. It wraps an [Ident.t] together with its
      source [compilation_unit].

      Introducing a new type helps tracing the source of identifier
      when debugging the inliner; and, avoids ident renaming when
      importing cmx files.

    * [ExprId.t] is used to identify nodes in the Flambda tree.

    * [FunId.t] is used to identify a set of recursive functions in
      the Flambda tree.

    The remaining types are abstracted aliases, introduced to avoid
    misconfusion between different usages.

    * [Closure_function.t] and [Closure_variable.t] are abstract aliases
      of [Variable.t] (see [Flambda] for more details).

    * [Static_exception.t] is an abstract alias for [int].

*)

module Variable : sig

  type t

  val create : current_compilation_unit:compilation_unit -> string ->  t

  val unwrap : t -> Ident.t (* For bytecode debugger only *)
  val unique_ident : t -> Ident.t (* For clambdagen only *)
    (* Should we propagate Variable.t into clambda ??? *)

  val rename : current_compilation_unit:compilation_unit ->
    ?append:string -> t -> t

  val in_compilation_unit : compilation_unit -> t -> bool

  include PrintableHashOrdered with type t := t

  val unique_name : t -> string

end

module VarSet : ExtSet with module M := Variable
module VarMap : ExtMap with module M := Variable
module VarTbl : ExtHashtbl with module M := Variable

module IdentMap : ExtMap with module M := Ident


(***********************************************************************)

module Closure_function : sig

  type t

  val wrap : Variable.t -> t
  val unwrap : t -> Variable.t

  val in_compilation_unit : compilation_unit -> t -> bool
  val get_compilation_unit : t -> compilation_unit

  include PrintableHashOrdered with type t := t

  val unique_name : t -> string

end

type function_within_closure = Closure_function.t

module ClosureFunctionSet : ExtSet with module M := Closure_function
module ClosureFunctionMap : ExtMap with module M := Closure_function
module ClosureFunctionTbl : ExtHashtbl with module M := Closure_function


(***********************************************************************)

module Closure_variable : sig

  type t

  val wrap : Variable.t -> t
  val unwrap : t -> Variable.t

  val in_compilation_unit : compilation_unit -> t -> bool

  include PrintableHashOrdered with type t := t

  val unique_name : t -> string

end

type variable_within_closure = Closure_variable.t


module ClosureVariableSet : ExtSet with module M := Closure_variable
module ClosureVariableMap : ExtMap with module M := Closure_variable
module ClosureVariableTbl : ExtHashtbl with module M := Closure_variable


(***********************************************************************)

module ExprId : Id
module FunId : UnitId with module Compilation_unit := Compilation_unit

module ExprSet : ExtSet with module M := ExprId
module ExprMap : ExtMap with module M := ExprId
module ExprTbl : ExtHashtbl with module M := ExprId

module FunSet : ExtSet with module M := FunId
module FunMap : ExtMap with module M := FunId
module FunTbl : ExtHashtbl with module M := FunId


(***********************************************************************)

module Static_exception : sig

  type t

  val create : unit -> t

  val of_int : int -> t
  val to_int : t -> int

  include PrintableHashOrdered with type t := t

end

type static_exception = Static_exception.t

module StaticExceptionSet : ExtSet with module M := Static_exception
module StaticExceptionMap : ExtMap with module M := Static_exception
module StaticExceptionTbl : ExtHashtbl with module M := Static_exception


(***********************************************************************)

module Variable_connected_components :
  Sort_connected_components.S with module Id := Variable
