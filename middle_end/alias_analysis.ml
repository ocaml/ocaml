(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type allocation_point =
  | Symbol of Symbol.t
  | Variable of Variable.t

type constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * Variable.t list
  | Set_of_closures of Flambda.set_of_closures
  | Project_closure of Flambda.project_closure
  | Move_within_set_of_closures of
      Flambda.move_within_set_of_closures
  | Project_var of Flambda.project_var
  | Field of Variable.t * int
  | Symbol_field of Symbol.t * int
  | Const of Flambda.const
  | Symbol of Symbol.t
  | Variable of Variable.t

type initialize_symbol_field = Variable.t option

type definitions =
  {
    variable : constant_defining_value Variable.Map.t;
    initialize_symbol : initialize_symbol_field list Symbol.Map.t;
    symbol : Flambda.constant_defining_value Symbol.Map.t;
    symbol_alias : Variable.t Symbol.Map.t;
  }

let rec resolve_definition
    (definitions: definitions)
    (var: Variable.t)
    (def: constant_defining_value) : allocation_point =
  match def with
  | Allocated_const _
  | Block _
  | Set_of_closures _
  | Project_closure _
  | Const _
  | Move_within_set_of_closures _ ->
    Variable var
  | Project_var {var} ->
    fetch_variable definitions (Var_within_closure.unwrap var)
  | Variable v ->
    fetch_variable definitions v
  | Symbol sym -> begin
      match Symbol.Map.find sym definitions.symbol_alias with
      | exception Not_found ->
        Symbol sym
      | v ->
        fetch_variable definitions v
    end
  | Field (v, n) ->
    begin match fetch_variable definitions v with
    | Symbol s ->
      fetch_symbol_field definitions s n
    | Variable v ->
      fetch_variable_field definitions v n
    end
  | Symbol_field (symbol, field) ->
    fetch_symbol_field definitions symbol field

and fetch_variable
    (definitions: definitions)
    (var: Variable.t) : allocation_point =
  match Variable.Map.find var definitions.variable with
  | exception Not_found -> Variable var
  | def ->
    resolve_definition definitions var def

and fetch_variable_field
    (definitions: definitions)
    (var: Variable.t)
    (field: int) : allocation_point =
  match Variable.Map.find var definitions.variable with
  | Block (_, fields) ->
    begin match List.nth fields field with
    | exception Not_found ->
      (* CR mshinwell for pchambart: Maybe we need to harden this module so that
         it doesn't go wrong when compiling dead code?  (In the same way as
         [Inline_and_simplify])? *)
      Misc.fatal_errorf "No field %i in block %a" field Variable.print var
    | v ->
      fetch_variable definitions v
    end
  | exception Not_found ->
    Misc.fatal_errorf "No definition for field access to %a" Variable.print var
  | Symbol _ | Variable _ | Project_var _ | Field _ | Symbol_field _ ->
    (* Must have been resolved *)
    assert false
  | Const _ | Allocated_const _
  | Set_of_closures _ | Project_closure _ | Move_within_set_of_closures _ ->
    Misc.fatal_errorf "Field access to %a which is not a block" Variable.print var

and fetch_symbol_field
    (definitions: definitions)
    (sym: Symbol.t)
    (field: int) : allocation_point =
  match Symbol.Map.find sym definitions.symbol with
  | Block (_, fields) ->
    begin match List.nth fields field with
    | exception Not_found ->
      Misc.fatal_errorf "No field %i in block %a" field Symbol.print sym
    | Symbol s ->
      Symbol s
    | Const _ ->
      Symbol sym
    end
  | exception Not_found -> begin
      match Symbol.Map.find sym definitions.initialize_symbol with
      | fields -> begin
          match List.nth fields field with
          | None ->
              Misc.fatal_errorf "field access to a not constant %a" Symbol.print sym
          | Some v ->
              fetch_variable definitions v
        end
      | exception Not_found ->
          Misc.fatal_errorf "No definition for field access to %a" Symbol.print sym
    end
  | Allocated_const _ | Set_of_closures _ | Project_closure _ ->
    Misc.fatal_errorf "Field access to %a which is not a block" Symbol.print sym

let run variable initialize_symbol symbol symbol_alias =
  let definitions = { variable; initialize_symbol; symbol; symbol_alias; } in
  Variable.Map.mapi (resolve_definition definitions) definitions.variable
