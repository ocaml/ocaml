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

(** The aim of this pass is to assign symbols to values known to be
    constant (in other words, whose values we know at compile time), with
    appropriate sharing of constants, and replace the occurrences of the
    constants with their corresponding symbols.

    This pass uses the results of two other passes, [Inconstant_idents] and
    [Alias_analysis].  The relationship between these two deserves some
    attention.

    [Inconstant_idents] is a "backwards" analysis that propagates implications
    about inconstantness of variables and set of closures IDs.

    [Alias_analysis] is a "forwards" analysis that is analagous to the
    propagation of [Simple_value_approx.t] values during [Inline_and_simplify].
    It gives us information about relationships between values but not actually
    about their constantness.

    Combining these two into a single pass has been attempted previously,
    but was not thought to be successful; this experiment could be repeated in
    the future.  (If "constant" is considered as "top" and "inconstant" is
    considered as "bottom", then [Alias_analysis] corresponds to a least fixed
    point and [Inconstant_idents] corresponds to a greatest fixed point.)

    At a high level, this pass operates as follows.  Symbols are assigned to
    variables known to be constant and their defining expressions examined.
    Based on the results of [Alias_analysis], we simplify the destructive
    elements within the defining expressions (specifically, projection of
    fields from blocks), to eventually yield [Flambda.constant_defining_value]s
    that are entirely constructive.  These will be bound to symbols in the
    resulting program.

    Another approach to this pass could be to only use the results of
    [Inconstant_idents] and then repeatedly lift constants and run
    [Inline_and_simplify] until a fixpoint.  It was thought more robust to
    instead use [Alias_analysis], where the fixpointing involves a less
    complicated function.

    We still run [Inline_and_simplify] once after this pass since the lifting
    of constants may enable more functions to become closed; the simplification
    pass provides an easy way of cleaning up (e.g. making sure [free_vars]
    maps in sets of closures are correct).
*)

(** Like [Flambda.constant_defining_value], but also permitting projections
    from blocks, which we will entirely simplify away in this pass. *)
type proto_constant_defining_value =
  | Constructive of Flambda.constant_defining_value
  | Field of Symbol.t * int

(** To be independent of the order of traversal we do a first pass that
    assigns symbols to all [let]-bound variables that are known to be
    constant.  This either involves generating new symbols, or exploiting
    fixed mappings between (e.g.) closure IDs and symbol names.  At the same
    time we note down the mapping from set of closures IDs to symbols. *)
let assign_symbols_to_constant_let_bound_variables ~expr
      ~(inconstants : Inconstant_idents.result) =
  let fresh_symbol var =
    Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ()
  in
  let var_to_symbol_tbl : Symbol.t Variable.Tbl.t = Variable.Tbl.create 42 in
  let set_of_closures_id_to_symbol_tbl = Set_of_closures_id.Tbl.create 42 in
  let assign_symbol var (named : Flambda.named) =
    match named with
    (* XXX this needs to collect [Const] too, but they don't need
       symbols. *)
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
  Flambda_iterators.iter_all_let_and_let_rec_bindings expr ~f:assign_symbol

(* Split constructive/deconstructive and reduce to constructive only. *)

(* Future unification of Consts, variables and symbols, differing only in
   what they are equal to *)

(* Look at Translmod again. *)

(** Produce [Flambda.constant_defining_value]s for all of the constants whose
    definitions are to be lifted.  These are indexed by [Symbol]s.  When
    going inside a closure, we need to apply the [free_vars] mapping, so
    that we know the correct assignment of variables found within the bodies
    of functions inside the closure to symbols. *)
let rec compute_definitions_of_symbols ~expr ~(inconstants : Inconstants.result)
      ~var_to_symbol_tbl =
  let constant_tbl : Flambda.named Symbol.Tbl.t = Symbol.Tbl.create 42 in
  let find_symbol var = Variable.Tbl.find var_to_symbol_tbl var in
  let is_constant var = not (Variable.Set.Mem var inconstants.id) in
  let compute_definition var named =
    assert (is_constant var);
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
  let traverse (expr : Flambda.t) =
    match expr with
    | Let (_, var, named, body) ->
      if is_constant var then begin
        compute_definition var named
      end;
      begin match named with
      | Set_of_closures set_of_closures ->
        (* When descending into a closure, we create a new [var_to_symbol_tbl],
           which reflects the [free_vars] mapping of the closure. *)
        let var_to_symbol_tbl = Variable.Tbl.create 42 in
        Variable.Map.iter (fun inner_var outer_var ->
            match find_symbol outer_var with
            | exception Not_found -> ()
            | symbol -> Variable.Tbl.add var_to_symbol_tbl inner_var symbol)
          set_of_closures.free_vars;
        Variable.Map.iter (fun fun_var function_decl ->
            compute_definitions_of_symbols ~expr:function_decl.body
              ~inconstants ~var_to_symbol_tbl)
          set_of_closures.function_decls.funs
      | _ -> ()
      end
    | Let_rec _ -> ...

    | _ -> ()
  in
  Flambda_iterators.iter_toplevel expr traverse (fun _ -> ())

(** Find constants assigned to multiple symbols and choose a unique symbol
    for them, thus sharing constants. *)
(* XXX presumably to do this properly, we should actually traverse the
   incoming program, and look at the [Let_symbol] bindings in effect when
   processing each expression.  This may not be useful at the moment
   because we probably won't rerun Lift_constants, but it seems like the
   right thing to do.
   Need to be careful that existing symbols remain the ones that are used
   as the canonical ones.  Just comes down to pre-populating the
   [constant_to_symbol_tbl], probably.  Maybe then these functions should
   stay as operating on [expr].  In effect it's a fold over the program nodes
   replacing each one that has an [expr] by a sequence of new nodes.
   However we also need to do this globally in some sense.  What about the
   sequential substitution thing?  Keep an environment and remove Let_symbol
   bindings as necessary (and substitute through the expressions) *)
let share_constants ~var_to_symbol_tbl ~symbol_to_constant_tbl =
  let new_var_to_symbol_tbl = Variable.Tbl.create 42 in
  let new_symbol_to_constant_tbl = Symbol.Tbl.create 42 in
  let constant_to_symbol_tbl = Constant_defining_value.Tbl.create 42 in
  let cannot_share (const : Flambda.constant_defining_value) =
    match const with
    (* Strings and float arrays are mutable; we never share them. *)
    | Allocated_const ((String _) | (Float_array _)) -> true
    | Allocated_const _ | Set_of_closures _ | Project_closure _ | Block _ ->
      false
  in
  Variable.Tbl.iter (fun var symbol ->
      let constant_defining_value = Symbol.Tbl.find constant_tbl symbol in
      let symbol =
        match
          Constant_defining_value.Tbl.find constant_to_symbol_tbl
            constant_defining_value
        with
        | exception Not_found -> symbol
        | existing_symbol when cannot_share constant_defining_value -> symbol
        | existing_symbol -> existing_symbol
      in
      Variable.Tbl.add new_var_to_symbol_tbl var symbol;
      Symbol.Tbl.replace new_symbol_to_constant_tbl symbol
        constant_defining_value;
      Constant_defining_value.Tbl.replace constant_to_symbol_tbl
        constant_defining_value symbol)
    var_to_symbol_tbl;
  new_var_to_symbol_tbl, new_symbol_to_constant_tbl

(** Compute an order in which to emit [Let_symbol] bindings such that there
    are no references to unbound symbols. *)
let compute_order_of_let_symbol_bindings ~symbol_to_constant_tbl =
  let constant_graph =
    Symbol.Tbl.fold
      (fun symbol (const : Flambda.constant_defining_value) graph ->
        let children =
          match const with
          | Allocated_const _ -> Symbol.Set.empty
          | Block (_, fields) -> Symbol.Set.of_list fields
          | Set_of_closures _ ->
            (* Sets of closures used as [constant_defining_value]s are
               always closed. *)
            Symbol.Set.empty
          | Project_closure _ -> Symbol.Set.empty
        in
        Symbol.Map.add symbol children graph)
      map
      Symbol.Map.empty
  in
  let module Symbol_SCC = Sort_connected_components.Make (Symbol) in
  let components =
    Symbol_SCC.connected_components_sorted_from_roots_to_leaf
      constant_graph
  in
  List.flatten
    (List.map (function
        | Symbol_SCC.Has_loop l -> l
        | Symbol_SCC.No_loop v -> [v])
      (List.rev (Array.to_list components)))

(** Substitute the defining expressions of all constant variables with the
    corresponding symbols.  Then bind all remaining free variables of the
    expression, which must be constant, to their corresponding symbols. *)
let replace_constant_defining_exprs_with_symbols program ~var_to_symbol_tbl =
  let is_constant var = Variable.Tbl.mem var var_to_symbol_tbl in
  let symbol_for_var var = Variable.Tbl.find var var_to_symbol_tbl in
  let replace var defining_expr : Flambda.named =
    match symbol_for_var var with
    | symbol -> Symbol symbol
    | exception Not_found -> defining_expr  (* [var] is inconstant *)
  in
  let expr =
    Flambda_iterators.map_all_let_and_let_rec_bindings expr ~f:replace
  in
  Variable.Set.fold (fun free_var expr ->
      if not (is_constant free_var) then begin
        Misc.fatal_errorf "The expression formed by replacing definitions of \
            constants by their corresponding symbols contains free variables \
            that are not known to be constant: free_vars = %a: %a"
          Variable.Set.print free_vars
          Flambda.print expr
      end else begin
        Let (Immutable, free_var, Symbol (symbol_for_var free_var), expr)
      end)
    (Free_variables.calculate expr)
    expr

(** Add [Let_symbol] bindings for symbols corresponding to constants whose
    definitions we have lifted.  The bindings are inserted in a correct
    order such that there are no references to unbound symbols. *)
let add_definitions_of_symbols program ~symbols_in_order
      ~symbol_to_constant_tbl =
  List.fold_left (fun program symbol ->
      let constant_defining_value =
        Symbol.Tbl.find symbol_to_constant_tbl symbol
      in
      Flambda.Let_symbol (symbol, constant_defining_value, program))
    program
    (List.rev symbols_in_order)

let lift_constants program ~backend =
  Format.eprintf "lift_constants input:@ %a\n" Flambda.print_program program;
  let inconstants =
    Inconstant_idents.inconstants program
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend
         mshinwell: what is "this"? *)
      ~compilation_unit:(Compilenv.current_unit ())
  in
  let var_to_symbol_tbl =
    assign_symbols_to_constant_let_bound_variables ~program ~inconstants
  in
  let symbol_to_constant_tbl =
    compute_definitions_of_symbols ~program ~inconstants ~var_to_symbol_tbl
  in
  let var_to_symbol_tbl, symbol_to_constant_tbl =
    share_constants ~var_to_symbol_tbl ~symbol_to_constant_tbl
  in
  let symbols_in_order =
    compute_order_of_let_symbol_bindings ~symbol_to_constant_tbl
  in
  program
  |> replace_constant_defining_exprs_with_symbols ~var_to_symbol_tbl
  |> add_definitions_of_symbols ~symbols_in_order ~symbol_to_constant_tbl
  |> (fun program ->
    Format.eprintf "lift_constants before simplify:@ %a\n"
      Flambda.print_program program; program)
  |> Inline_and_simplify.run ~never_inline:true ~backend
