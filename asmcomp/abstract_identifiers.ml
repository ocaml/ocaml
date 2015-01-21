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

module Variable = struct

  type t = { var_unit : compilation_unit; var_var : Ident.t }

  let create ~current_compilation_unit name =
    { var_unit = current_compilation_unit;
      var_var = Ident.create name }

  let unwrap var = var.var_var
  let unique_ident var =
    let open Ident in
    { var.var_var with
      name =
        Format.asprintf "%a_%s"
          Compilation_unit.print var.var_unit
          var.var_var.name }


  let rename ~current_compilation_unit ?append var =
    let var_var =
      match append with
      | None -> Ident.rename var.var_var
      | Some s -> Ident.create (var.var_var.Ident.name ^ s) in
    { var_unit = current_compilation_unit;
      var_var }

  let in_compilation_unit cu var =
    Compilation_unit.equal cu var.var_unit

  let get_compilation_unit var = var.var_unit


  let compare v1 v2 =
    let c = Ident.compare v1.var_var v2.var_var in
    if c = 0
    then Compilation_unit.compare v1.var_unit v2.var_unit
    else c
  let output c v = Ident.output c v.var_var
  let hash v = Ident.hash v.var_var
  let equal v1 v2 =
    Ident.same v1.var_var v2.var_var &&
    Compilation_unit.equal v1.var_unit v2.var_unit
  let print ppf v =
    Format.fprintf ppf "%a.%a"
      Compilation_unit.print v.var_unit
      Ident.print v.var_var

  let unique_name var = Ident.unique_name var.var_var

end

module VarSet = ExtSet(Variable)
module VarMap = ExtMap(Variable)
module VarTbl = ExtHashtbl(Variable)

module ExprId : Id = Id(struct end)
module ExprMap = ExtMap(ExprId)
module ExprSet = ExtSet(ExprId)
module ExprTbl = ExtHashtbl(ExprId)

module FunInnerid : Id = Id(struct end)
module FunId : UnitId with module Compilation_unit := Compilation_unit
  = UnitId(FunInnerid)(Compilation_unit)
module FunMap = ExtMap(FunId)
module FunSet = ExtSet(FunId)
module FunTbl = ExtHashtbl(FunId)

type static_exception = int

module Static_exception = struct
  include Int
  let of_int x = x
  let to_int x = x
  let create () = Lambda.next_raise_count ()
end

module Closure_element = struct

  include Variable

  let wrap x = x
  let unwrap x = x

end

type function_within_closure = Variable.t
type variable_within_closure = Variable.t

module Closure_function = Closure_element
module Closure_variable = Closure_element

module ClosureFunctionMap = ExtMap(Closure_function)
module ClosureFunctionSet = ExtSet(Closure_function)
module ClosureFunctionTbl = ExtHashtbl(Closure_function)

module ClosureVariableMap = ExtMap(Closure_variable)
module ClosureVariableSet = ExtSet(Closure_variable)
module ClosureVariableTbl = ExtHashtbl(Closure_variable)

module StaticExceptionSet = ExtSet(Static_exception)
module StaticExceptionMap = ExtMap(Static_exception)
module StaticExceptionTbl = ExtHashtbl(Static_exception)

module IdentMap = ExtMap(Ident)

(* let ident_of_function_within_closure { ce_id } = ce_id *)
module Variable_connected_components =
  Sort_connected_components.Make(Variable)

