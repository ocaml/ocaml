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

(** Lift allocated constants out of an expression and assign them to
    symbols.
   * First collect all the constant declarations, separating structured
     constants and constant sets of closures.
   * Then share equal constants.
     This is done by traversing the structured constants in inverse
     topological order and replacing every existing constant by the previous
     one. (Note: this does not guaranty maximal sharing in cycles)
   * Replace every defining expression of a constant variable, where the
     defining expression was an allocated constant, with the corresponding
     symbol.
*)

(** To be independent of the order of traversal we do a first pass that
    assigns symbols to all [let]-bound variables that are known to be
    constant.  This either involves generating new symbols, or exploiting
    fixed mappings between (e.g.) closure IDs and symbol names.  At the same
    time we note down the mapping from set of closures IDs to symbols. *)
let assign_symbols_to_constant_let_bound_variables ~expr ~inconstants =
  let fresh_symbol var =
    Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ()
  in
  let var_to_symbol_tbl : Symbol.t Variable.Tbl.t = Variable.Tbl.create 42 in
  let set_of_closures_id_to_symbol_tbl = Set_of_closures_id.Tbl.create 42 in
  let assign_symbol var (named : Flambda.named) =
    match named with
    | Allocated_const _ | Prim (Pmakeblock (_, _), _, _) ->
      let symbol = fresh_symbol var in
      Variable.Tbl.add var_to_symbol_tbl var symbol
    | Set_of_closures (
        { function_decls = { funs; set_of_closures_id; _ }; _ } as set) ->
      assert (not (Set_of_closures_id.Set.mem set_of_closures_id
          inconstants.closure));
      let set_of_closures_symbol = fresh_symbol var in
      Variable.Tbl.add var_to_symbol_tbl var set_of_closures_symbol;
      Set_of_closures_id.Tbl.add set_of_closures_id_to_symbol_tbl
        set_of_closures_id set_of_closures_symbol;
      (* CR mshinwell: the following seems to be needed because of the
         behaviour of [Closure_conversion_aux.closure_env_without_parameters]
         and maybe [reference_recursive_function_directly].  We should think
         about this.  (Could we always use projections instead?  If so we
         should add an invariant check forbidding direct access.) *)
      Variable.Map.iter (fun fun_var _ ->
          let closure_id = Closure_id.wrap fun_var in
          let closure_symbol = Compilenv.closure_symbol closure_id in
          Variable.Tbl.add var_to_symbol_tbl var closure_symbol)
        funs
    | Move_within_set_of_closures { closure = _; start_from = _; move_to; } ->
      Variable.Tbl.add var_to_symbol_tbl var
        (Compilenv.closure_symbol move_to)
    | Project_closure { set_of_closures = _; closure_id; } ->
      Variable.Tbl.add var_to_symbol_tbl var
        (Compilenv.closure_symbol closure_id)
    | Prim (Pgetglobal id, _, _) ->
      let global_symbol = Compilenv.symbol_for_global' id in
      Variable.Tbl.add var_to_symbol_tbl var global_symbol
    | Prim (Pfield _, _, _) | Prim (Pgetglobalfield _, _, _) -> ()
    | Prim _ ->
      Misc.fatal_errorf "Primitive not expected to be constant: @.%a@."
        Flambda.print_named named
    | Symbol _ | Const _ | Project_var _ | Expr _ -> ()
  in
  Flambda_iterators.iter_all_let_and_let_rec_bindings expr ~f:assign_symbol;
  Variable.Tbl.fold Variable.Map.add constant_tbl Variable.Map.empty

(** Produce [Flambda.constant_defining_value]s for all of the constants whose
    definitions are to be lifted.  These are indexed by [Symbol]s. *)
let compute_definitions_of_symbols ~expr ~inconstants ~var_to_symbol_map =
  let constant_tbl : Flambda.named Symbol.Tbl.t = Symbol.Tbl.create 42 in
  let describe_if_constant var (named : Flambda.named) =
    if not (Variable.Set.mem var inconstants.id) then begin
      let find_symbol var = Variable.Map.find var_to_symbol_map var in
      match named with
      | Allocated_const const ->
        Symbol.Tbl.add constant_tbl (find_symbol var) (Allocated_const const)
      | Prim (Pmakeblock (tag, _), args, _) ->
        Symbol.Tbl.add constant_tbl (find_symbol var)
          (Block (Tag.create_exn tag, List.map find_symbol args))
      | Set_of_closures set_of_closures ->
        let set_of_closures_symbol = find_symbol var in
        Symbol.Tbl.add constant_tbl set_of_closures_symbol
          (Set_of_closures set_of_closures);
        Variable.Map.iter (fun fun_var _ ->
            Symbol.Tbl.add constant_tbl (find_symbol fun_var)
              (Closure (set_of_closures_symbol, Closure_id.wrap fun_var)))
          funs
      | Move_within_set_of_closures _ | Project_closure _
      | Prim (Pgetglobal _, _, _) ->
        (* In these cases, there is no definition to lift: it's already a
           symbol (uniquely determined as per [assigned_symbol], above). *)
        ()
      | Prim (Pfield _, _, _) | Prim (Pgetglobalfield _, _, _) -> ()
      | Prim _ -> assert false  (* see [assign_symbol], above *)
      | Symbol _ | Const _ | Project_var _ | Expr _ -> ()
      in
    end
  in
  Flambda_iterators.iter_all_let_and_let_rec_bindings expr
    ~f:describe_if_constant;
  Symbol.Tbl.fold Symbol.Map.add constant_tbl Symbol.Map.empty

let constant_graph (map : constant_defining_value Variable.Map.t) =
  Variable.Map.map (fun (const : constant_defining_value) ->
      match const with
      | Block (_, fields) -> Variable.Set.of_list fields
      | constant_defining_value _ | Symbol _ -> Variable.Set.empty)
    map

let rewrite_constant_aliases map aliases =
  let subst var =
    try Variable.Map.find var aliases with
    | Not_found -> var
  in
  let subst_block (const : constant_defining_value)
        : constant_defining_value =
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

let share_constants ~constant_map:map ~compare_name ~aliases =
  let module Symbol_SCC = Sort_connected_components.Make (Symbol) in
  let all_constants = all_constants map aliases in
  let map = rewrite_constant_aliases map aliases in
  let components =
    let graph = constant_graph map in
    Symbol_SCC.connected_components_sorted_from_roots_to_leaf graph
  in
  let sorted_symbols =
    List.flatten
      (List.map (function
          | Symbol_SCC.Has_loop l -> l
          | Symbol_SCC.No_loop v -> [v])
        (List.rev (Array.to_list components)))
  in
  let shared_constants = ref Constant_defining_value_map.empty in
  (* We use a list for [constants] to preserve the reverse top-sort order.
     This enables us to be sure that, when [Inline_and_simplify] (called
     below) computes approximations of the defining values of the constants,
     it can perform a single traversal and be sure that there will be no
     undefined inter-constant references. *)
  (* CR mshinwell: we should add a check to make sure our Let_symbol
     defining values don't accidentally reference undefined symbols *)
  let constants = ref [] in
  let equal_constants = ref Variable.Map.empty in
  let find_and_add var cst =
    match Constant_defining_value_map.find cst !shared_constants with
    | exception Not_found ->
      shared_constants :=
        Constant_defining_value_map.add cst var !shared_constants;
      constants := (var, cst)::!constants
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
      constants := (var, cst)::!constants
    | Float _ | Int32 _ | Int64 _ | Nativeint _ | Immstring _ ->
      find_and_add var cst
    | Block (tag, fields) ->
      find_and_add var (Block (tag, List.map subst fields))
  in
  List.iter share sorted_symbols;
  let constants = !constants in
  let equal_constants = !equal_constants in
  let assign_symbols (descr_map, kind_map) (var, const) =
    let symbol = fresh_symbol var in
    (symbol, const)::descr_map,
      Variable.Map.add var (Symbol symbol) kind_map
  in
  let descr, declared_constants_kind =
    List.fold_left assign_symbols
      ([], Variable.Map.empty)
      !constants
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
    List.map (fun (symbol, const) ->
        symbol, Allocated_constants.map ~f:find_kind) descr
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
    Variable.Tbl.find var var_to_symbol_tbl
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
    Flambda_iterators.map_expr replace_constant_defining_exprs
  in
  let expr = rewrite_expr expr in
  let bind_constant var body : Flambda.t =
    assert (is_a_constant var);
    Let (Immutable, var, replacement_for_defining_expr var, body)
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

(** Add [Let_symbol] bindings for symbols corresponding to constants whose
    definitions we have lifted. *)
let add_definitions_of_symbols ~expr ~constant_defining_values =
  List.fold_left (fun expr symbol constant_defining_value ->
      Flambda.Let_symbol (symbol, constant_defining_value, expr))
    (Flambda.Entry_point expr)
    (List.rev constant_defining_values)

let lift_constants expr ~backend =
  Format.eprintf "lift_constants input:@ %a\n" Flambda.print expr;
  let inconstants =
    Inconstant_idents.inconstants expr
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend
         mshinwell: what is "this"? *)
      ~compilation_unit:(Compilenv.current_unit ())
  in
  let var_to_symbol_map =
    assign_symbols_to_constant_let_bound_variables ~expr ~inconstants
  in
  let constant_defining_values =
    compute_definitions_of_symbols ~expr ~inconstants ~var_to_symbol_map
  in
  let constants, kind = share_constants ~constant_map in
  let with_symbols =
    replace_constant_defining_exprs_with_symbols expr aliases
      set_of_closures_tbl constants kind ~backend
  in
  let program =
    add_definitions_of_symbols ~expr:with_symbols
      ~constant_defining_values
  in
  (* We need to rerun [Inline_and_simplify] because strings and float arrays
     (bindings of which would have had unknown approximations during
     [Inline_and_simplify.simplify_free_variable]) that are now known to be
     constant will now have been assigned symbols.  Symbols are immutable,
     meaning that we should be able to cause more sets of closures to
     become closed. *)
  Inline_and_simplify.run program ~never_inline:true ~backend
