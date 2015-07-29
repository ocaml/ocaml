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

(* Lift allocated constants out of the main expression.
   * First collect all the constant declarations, separating structured
     constants and constant sets of closures: [collect_constant_declarations]
   * Then share equal constants: [constant_sharing].
     This is done by traversing the structured constants in inverse
     topological order and replacing every existing constant by the previous
     one. (Note: this does not guaranty maximal sharing in cycles)
   * Replace every defining expression of a constant variable with the
     corresponding symbol/integer/etc.
*)

type result = {
  expr : Flambda.t;
  constant_descr : Symbol.t Allocated_const.t Symbol.Map.t;
  assigned_symbols : Symbol.t Variable.Map.t;
  set_of_closures_map : Flambda.set_of_closures Symbol.Map.t;
}

(** Descriptions of the values of allocated constants.  This is effectively
    a subset of [Flambda.named].  After this pass has finished, there will only
    be one such description: symbols. *)
type 'name constant_defining_value =
  | Allocated_const of Allocated_const.t
  | Block of Tag.t * 'name list
  | Symbol of Symbol.t

module Constant_defining_value_map = Map.Make (struct
  type t = Variable.t constant_defining_value

  let compare t1 t2 =
    match t1, t2 with
    | Allocated_const c1, Allocated_const c2 ->
      Allocated_const.compare c1 c2
    | Block (tag1, fields1), Block (tag2, fields2) ->
      let c = Tag.compare tag1 tag2 in
      if c <> 0 then c
      else Variable.compare_list fields1 fields2
    | Symbol sym1, Symbol sym2 -> Symbol.compare sym1 sym2
    | Allocated_const _, _ -> -1
    | Block _, _ -> 1
end)

let constant_defining_value_to_named (const : _ constant_defining_value)
      ~name_to_named : Flambda.named =
  match const with
  | Allocated_const const -> Allocated_const const
  | Block (tag, fields) ->
    Prim (Pmakeblock (tag, List.map name_to_named fields))
  | Symbol symbol -> Symbol symbol

let fresh_symbol var =
  Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ()

let find_and_describe_allocated_constants expr =
  let inconstants =
    Inconstant_idents.inconstants expr
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend
         mshinwell: what is "this"? *)
      ~compilation_unit:(Compilenv.current_unit ())
  in
  let constant_tbl : Flambda.named Variable.Tbl.t = Variable.Tbl.create 10 in
  let set_of_closures_tbl : Flambda.set_of_closures Symbol.Tbl.t =
    Symbol.Tbl.create 10
  in
  let describe_if_constant var (named : Flambda.named) =
    if not (Variable.Set.mem var inconstants.id) then begin
      let add (const : Variable.t constant_defining_value) =
        Variable.Tbl.add constant_tbl var const
      in
      match named with
      | Allocated_const const -> add (Allocated_const const)
      | Prim (Pmakeblock (tag, _), args, _) ->
        add (Block (Tag.create_exn tag, args))
      | Set_of_closures ( { function_decls = { funs; set_of_closures_id } } as set )->
        assert (not (Set_of_closures_id.Set.mem set_of_closures_id
            inconstants.closure));
        let symbol = fresh_symbol var in  (* Will probably never be used. *)
        Symbol.Tbl.add set_of_closures_tbl symbol set;
        add (Symbol symbol);
        (* CR mshinwell: the following seems to be needed because of the behaviour of
           [Closure_conversion_aux.closure_env_without_parameters] and maybe
           [reference_recursive_function_directly].  We should think about this.
           (Could we always use projections instead?  If so we should add an
           invariant check forbidding direct access.) *)
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            Variable.Tbl.add constant_tbl fun_var
              (Symbol (Compilenv.closure_symbol closure_id)))
          funs
      | Move_within_set_of_closures { move_to = closure_id; _ }
      | Project_closure { closure_id; _ } ->
        add (Symbol (Compilenv.closure_symbol closure_id))
      | Prim (Pgetglobal id, _, _) ->
        add (Symbol (Compilenv.symbol_for_global' id))
      | Symbol symbol -> add (Symbol symbol)
      | Prim (Pfield _, _, _) | Prim (Pgetglobalfield _, _, _) -> ()
      | Prim _ ->
        Misc.fatal_errorf "Primitive not expected to be constant: @.%a@."
          Flambda.print_named named
      | Const _ | Project_var _ | Expr _ -> ()
    end
  in
  Flambda_iterators.iter_all_let_and_let_rec_bindings expr
    ~f:describe_if_constant;
  let constant_map =
    Variable.Tbl.fold Variable.Map.add constant_tbl Variable.Map.empty
  in
  constant_map, set_of_closures_tbl

let constant_graph (map : Variable.t constant_defining_value Variable.Map.t) =
  Variable.Map.map (fun (const : Variable.t constant_defining_value) ->
      match const with
      | Block (_, fields) -> Variable.Set.of_list fields
      | constant_defining_value _ | Symbol _ -> Variable.Set.empty)
    map

let rewrite_constant_aliases map aliases =
  let subst var =
    try Variable.Map.find var aliases with
    | Not_found -> var
  in
  let subst_block (const : Variable.t constant_defining_valueant.t)
        : Variable.t constant_defining_valueant.t =
    match const with
    | Block (tag, fields) -> Block (tag, List.map subst fields)
    | constant_defining_value _ | Symbol _ -> const
  in
  Variable.Map.map subst_block map

let all_constants map aliases =
  let allocated_constants = Variable.Map.keys map in
  let aliased_constants =
    Variable.Map.keys
      (Variable.Map.filter (fun _var alias -> Variable.Map.mem alias map)
        aliases)
  in
  Variable.Set.union allocated_constants aliased_constants

let constant_sharing ~constant_map:map ~compare_name ~aliases =
  let module Variable_SCC = Sort_connected_components.Make (Variable) in
  let all_constants = all_constants map aliases in
  let map = rewrite_constant_aliases map aliases in
  let components =
    let graph = constant_graph map in
    Variable_SCC.connected_components_sorted_from_roots_to_leaf graph
  in
  let sorted_symbols =
    List.flatten
      (List.map (function
          | Variable_SCC.Has_loop l -> l
          | Variable_SCC.No_loop v -> [v])
        (List.rev (Array.to_list components)))
  in
  let shared_constants = ref Constant_defining_value_map.empty in
  let constants = ref Variable.Map.empty in
  let equal_constants = ref Variable.Map.empty in
  let find_and_add var cst =
    match Constant_defining_value_map.find cst !shared_constants with
    | exception Not_found ->
      shared_constants :=
        Constant_defining_value_map.add cst var !shared_constants;
      constants := Variable.Map.add var cst !constants;
    | sharing ->
      equal_constants := Variable.Map.add var sharing !equal_constants
  in
  let subst var =
    try Variable.Map.find var !equal_constants with
    | Not_found -> var
  in
  let share var =
    let cst = Variable.Map.find var map in
    match cst with
    | String _ | Float_array _ ->
      (* Strings and float arrays are mutable; we never share them. *)
      constants := Variable.Map.add var cst !constants
    | Float _ | Int32 _ | Int64 _ | Nativeint _ | Immstring _ ->
      find_and_add var cst
    | Block (tag, fields) ->
      find_and_add var (Block (tag, List.map subst fields))
  in
  List.iter share sorted_symbols;
  !constants, !equal_constants

  let assign_symbols var const (descr_map, kind_map) =
    let symbol = fresh_symbol var in
    Symbol.Map.add symbol const descr_map,
      Variable.Map.add var (Symbol symbol) kind_map
  in
  let descr, declared_constants_kind =
    Variable.Map.fold assign_symbols !constants
      (Symbol.Map.empty, Variable.Map.empty)
  in
  let equal_constants_kind =
    Variable.Map.map (fun var ->
        Variable.Map.find var declared_constants_kind)
      !equal_constants
  in
  let declared_and_equal_constants_kind =
    Variable.Map.disjoint_union
      declared_constants_kind
      equal_constants_kind
  in
  let kind =
    Variable.Map.of_set (fun var ->
        let alias =
          try Variable.Map.find var aliases with
          | Not_found -> var
        in
        Variable.Map.find alias declared_and_equal_constants_kind)
      all_constants
  in
  let descr =
    let find_kind var =
      try Variable.Map.find var kind with
      | Not_found ->
        Format.printf "missing %a@."
          Variable.print var;
        raise Not_found
    in
    Symbol.Map.map (Allocated_constants.map ~f:find_kind) descr
  in
  descr, kind

let replace_constant_defining_exprs_with_symbols expr aliases
      set_of_closures_tbl constant_descr
      (assigned_symbols : constant Variable.Map.t)
      ~backend =
  let is_a_constant var =
    let var =
      try Variable.Map.find var aliases with
      | Not_found -> var
    in
    Variable.Map.mem var kind
  in
  let replacement_for_defining_expr var : Flambda.named =
    let var =
      try Variable.Map.find var aliases with
      | Not_found -> var
    in
    Variable.Map.find var kind
  in
  let replace_constant_defining_exprs (expr : Flambda.t) : Flambda.t =
    match expr with
    | Let (_, var, _defining_expr, body) when is_a_constant var ->
      Let (Immutable, var, replacement_for_defining_expr var, body)
    | Let_rec (defs, body) ->
      let defs =
        List.map (fun (var, defining_expr) ->
            let defining_expr =
              if is_a_constant var then replacement_for_defining_expr var
              else defining_expr
            in
            var, defining_expr)
          defs
      in
      Let_rec (defs, body)
    | Let _ | Var _ | Apply _ | Send _ | Assign _ | If_then_else _ | Switch _
    | String_switch _ | Static_raise _ | Static_catch _ | Try_with _
    | While _ | For _ | Proved_unreachable -> expr
  in
  (* In order to run [Inline_and_simplify], below, we need the defining values
     of symbols we have assigned to constants.  Since these values may
     reference other constants, which must be done using [Variable.t]s (due
     to the type of [Flambda.named]), we must also provide bindings for all
     such variables to symbols. *)
  let variable_definitions =
    Variable.Map.fold assigned_symbols (fun symbol -> Symbol symbol)
  in
  let symbol_definitions =
    Symbol.Map.disjoint_union
      (Symbol.Map.map (fun const ->
          constant_defining_value_to_named const ~name_to_var:(fun symbol ->
              ...))  (* lookup in reverse [assigned_symbols] *)
        constant_descr)
      (Symbol.Map.map (fun set -> Flambda.Set_of_closures set)
        set_of_closures_map)
  in
  let rewrite_expr expr =
    expr
    |> Flambda_iterators.map_expr replace_constant_defining_exprs
    (* We need to rerun [Inline_and_simplify] because strings and float arrays
       (bindings of which would have had unknown approximations during
       [Inline_and_simplify.simplify_free_variable]) that are now known to be
       constant will now have been assigned symbols.  Symbols are immutable,
       meaning that we should be able to cause more sets of closures to
       become closed. *)
    |> Inline_and_simplify.run ~never_inline:true ~backend
        ~symbol_definitions ~variable_definitions
  in
  let expr = rewrite_expr expr in
  let free_variables = Free_variables.calculate expr in
  if not (Variable.Set.for_all is_a_constant free_variables) then begin
    Misc.fatal_errorf "Lift_constants: toplevel expression contains free \
        variables that are not constant: %a"
      Flambda.print expr
  end;
  let bind_constant var body : Flambda.t =
    assert (is_a_constant var);
    match get_kind var with
    | Int i -> Let (Immutable, var, Const (Const_base (Const_int i)), body)
    | Const_pointer p -> Let (Immutable, var, Const (Const_pointer p), body)
    | Symbol s -> Let (Immutable, var, Symbol s, body)
    | exception Not_found -> body
  in
  let expr = Variable.Set.fold bind_constant free_variables expr in
  let set_of_closures_map =
    Symbol.Tbl.fold (fun symbol set_of_closures map ->
        let set_of_closures =
          Flambda_iterators.map_function_bodies set_of_closures ~f:rewrite_expr
        in
        Symbol.Map.add symbol set_of_closures map)
      set_of_closures_tbl Symbol.Map.empty
  in
  Format.eprintf "lift_constants output:@ %a\n" Flambda.print expr;
  { expr;
    constant_descr;
    assigned_symbols;
    set_of_closures_map;
  }

let lift_constants expr ~backend =
  Format.eprintf "lift_constants input:@ %a\n" Flambda.print expr;
  let constant_map, set_of_closures_tbl =
    find_and_describe_allocated_constants expr
  in
  let aliases = Alias_analysis.run expr in
  let constants, kind =
    Share_constants.constant_sharing ~constant_map
      ~compare_name:Variable.compare
      ~aliases
  in
  replace_constant_defining_exprs_with_symbols expr aliases
    set_of_closures_tbl constants kind ~backend
