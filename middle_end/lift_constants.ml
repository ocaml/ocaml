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

(* CR mshinwell: check comment is up to date *)
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

open Alias_analysis

let rec tail_variable : Flambda.t -> Variable.t option = function
  | Var v -> Some v
  | Let_rec (_, e)
  | Let_mutable (_, _, e)
  | Let { body = e; _ } -> tail_variable e
  | _ -> None

let closure_symbol ~(backend:(module Backend_intf.S)) closure_id =
  let module Backend = (val backend) in
  Backend.closure_symbol closure_id

(* CR chambart: Copied from lift_let_to_initialize_symobl: to factorize *)
let make_variable_symbol prefix var =
  Symbol.create (Compilation_unit.get_current_exn ())
    (Linkage_name.create
       (prefix ^ Variable.unique_name (Variable.freshen var)))

(** Traverse the given expression assigning symbols to [let]- and [let rec]-
    bound constant variables.  At the same time collect the definitions of
    such variables. *)
let assign_symbols_and_collect_constant_definitions
    ~(backend:(module Backend_intf.S))
    ~program
    ~(inconstants : Inconstant_idents.result) =
  let var_to_symbol_tbl = Variable.Tbl.create 42 in
  let var_to_definition_tbl = Variable.Tbl.create 42 in
  let assign_symbol var (named : Flambda.named) =
    if not (Variable.Set.mem var inconstants.id) then begin
      let assign_symbol () =
        let symbol = make_variable_symbol "" var in
        (* Format.eprintf "assign_symbol %a -> %a@." *)
        (*   Variable.print var *)
        (*   Symbol.print symbol; *)
        Variable.Tbl.add var_to_symbol_tbl var symbol
      in
      let assign_existing_symbol = Variable.Tbl.add var_to_symbol_tbl var in
      let record_definition = Variable.Tbl.add var_to_definition_tbl var in
      match named with
      | Symbol symbol ->
        assign_existing_symbol symbol;
        record_definition (Symbol symbol)
      | Const const -> record_definition (Const const)
      | Allocated_const const ->
        assign_symbol ();
        record_definition (Allocated_const const)
      | Read_mutable _ -> () (* CR mshinwell: should be assert false? *)
      | Prim (Pmakeblock (tag, _), fields, _) ->
        assign_symbol ();
        record_definition (Block (Tag.create_exn tag, fields))
      | Read_symbol_field (symbol, field) ->
        record_definition (Symbol_field (symbol, field))
      | Set_of_closures (
          { function_decls = { funs; set_of_closures_id; _ };
            _ } as set) ->
        assert (not (Set_of_closures_id.Set.mem set_of_closures_id
                       inconstants.closure));
        assign_symbol ();
        record_definition (Set_of_closures set);
        (* CR mshinwell: the following seems to be needed because of the
           behaviour of [Closure_conversion_aux.closure_env_without_parameters]
           and maybe [reference_recursive_function_directly].  We should think
           about this.  (Could we always use projections instead?  If so we
           should add an invariant check forbidding direct access.) *)
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            Variable.Tbl.add var_to_symbol_tbl fun_var closure_symbol;
            let project_closure =
              Alias_analysis.Project_closure
                { set_of_closures = var; closure_id }
            in
            Variable.Tbl.add var_to_definition_tbl fun_var
              project_closure)
          funs
      | Move_within_set_of_closures ({ closure = _; start_from = _; move_to; } as move) ->
        assign_existing_symbol (closure_symbol ~backend  move_to);
        record_definition (Move_within_set_of_closures move)
      | Project_closure ({ closure_id } as project_closure) ->
        assign_existing_symbol (closure_symbol ~backend  closure_id);
        record_definition (Project_closure project_closure)
      (* | Prim (Pgetglobal id, _, _) -> *)
      (*   let symbol = Compilenv.symbol_for_global' id in *)
      (*   assign_existing_symbol symbol; *)
      (*   record_definition (Symbol symbol) *)
      | Prim (Pfield index, [block], _) ->
        (* | Prim (Pgetglobalfield index, [block], _) -> *)
        record_definition (Field (block, index))
      | Prim (Pfield _, _, _) ->
        Misc.fatal_errorf "[Pfield] with the wrong number of arguments"
          Flambda.print_named named
      (* | Prim (Pgetglobalfield _, _, _) -> *)
      (*   Misc.fatal_errorf "[Pgetglobalfield] with the wrong number of arguments" *)
      (*     Flambda.print_named named *)
      | Prim _ ->
        Misc.fatal_errorf "Primitive not expected to be constant: @.%a@."
          Flambda.print_named named
      | Project_var project_var ->
        record_definition (Project_var project_var)
      | Expr e -> begin
          match tail_variable e with
          | None -> () (* Fail ? *)
          | Some v -> record_definition (Variable v)
        end
    end
  in
  let assign_symbol_program expr =
    Flambda_iterators.iter_all_immutable_let_and_let_rec_bindings expr
      ~f:assign_symbol
  in
  Flambda_iterators.iter_exprs_at_toplevel_of_program
    ~f:assign_symbol_program
    program;
  let initialize_symbol_to_definition_tbl = Symbol.Tbl.create 42 in
  let rec collect_initialize_declaration (program:Flambda.program) =
    match program with
    | Let_symbol (_,_,program)
    | Let_rec_symbol (_,program)
    | Import_symbol (_,program)
    | Effect (_,program) ->
        collect_initialize_declaration program
    | Initialize_symbol (symbol,_tag,fields,program) ->
        collect_initialize_declaration program;
        let fields = List.map tail_variable fields in
        Symbol.Tbl.add initialize_symbol_to_definition_tbl symbol fields
    | End _ -> ()
  in
  collect_initialize_declaration program;
  let record_set_of_closure_equalities (set_of_closures:Flambda.set_of_closures) =
    Variable.Map.iter (fun arg var ->
        if not (Variable.Set.mem arg inconstants.id) then
          Variable.Tbl.add var_to_definition_tbl arg (Variable var))
      set_of_closures.free_vars;
    Variable.Map.iter (fun arg var ->
        if not (Variable.Set.mem arg inconstants.id) then
          Variable.Tbl.add var_to_definition_tbl arg (Variable var))
      set_of_closures.specialised_args
  in
  Flambda_iterators.iter_on_set_of_closures_of_program program
    ~f:record_set_of_closure_equalities;
  Flambda_iterators.iter_constant_sets_of_closures_of_program ~f:(fun set_of_closures ->
      Variable.Map.iter (fun fun_var _ ->
          let closure_id = Closure_id.wrap fun_var in
          let closure_symbol = closure_symbol ~backend closure_id in
          Variable.Tbl.add var_to_definition_tbl fun_var
            (Symbol closure_symbol);
          Variable.Tbl.add var_to_symbol_tbl fun_var closure_symbol)
        set_of_closures.Flambda.function_decls.funs)
    program;
  var_to_symbol_tbl,
  var_to_definition_tbl,
  initialize_symbol_to_definition_tbl

let variable_field_definition
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    (var:Variable.t) : Flambda.constant_defining_value_block_field =
  try
    Symbol (Variable.Tbl.find var_to_symbol_tbl var)
  with Not_found ->
    match Variable.Tbl.find var_to_definition_tbl var with
    | Const c -> Const c
    | _ ->
      Misc.fatal_errorf "Unexpected pattern for a constant %a"
        Variable.print var;
      assert false
    | exception Not_found ->
      Misc.fatal_errorf "No assotiated symbol for the constant %a"
        Variable.print var

let resolve_variable
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    (var:Variable.t) : Flambda.constant_defining_value_block_field =
  match Variable.Map.find var aliases with
  | exception Not_found ->
    (* Format.eprintf "no alias for %a@." *)
    (*   Variable.print var; *)
    variable_field_definition var_to_symbol_tbl var_to_definition_tbl var
  | Symbol s ->
    (* Format.eprintf "symbol alias %a -> %a@." *)
    (*   Variable.print var *)
    (*   Symbol.print s; *)
    Symbol s
  | Variable aliased_variable ->
    (* Format.eprintf "variable alias %a -> %a@." *)
    (*   Variable.print var *)
    (*   Variable.print aliased_variable; *)
    variable_field_definition var_to_symbol_tbl var_to_definition_tbl aliased_variable

let translate_set_of_closures
    (inconstants:Inconstant_idents.result)
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    (set_of_closures:Flambda.set_of_closures) =
  let f var (named:Flambda.named) : Flambda.named =
    if Variable.Set.mem var inconstants.id then
      named
    else
      let resolved =
        resolve_variable
          aliases
          var_to_symbol_tbl
          var_to_definition_tbl
          var
      in
      match resolved with
      | Symbol s -> Symbol s
      | Const c -> Const c
  in
  (* let f_body body = *)
  (*   let body = Flambda_iterators.map_all_let_and_let_rec_bindings ~f body in *)
  (*   let free_variable = Flambda.free_variables body in *)
  (* in *)
  Flambda_iterators.map_function_bodies set_of_closures
    ~f:(Flambda_iterators.map_all_immutable_let_and_let_rec_bindings ~f)

let translate_constant_set_of_closures
    (inconstants:Inconstant_idents.result)
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    (constant_defining_values:Flambda.constant_defining_value Symbol.Map.t) =
  Symbol.Map.map (fun (const:Flambda.constant_defining_value) ->
      match const with
      | Flambda.Allocated_const _
      | Flambda.Block _
      | Flambda.Project_closure _ ->
        const
      | Flambda.Set_of_closures set_of_closures ->
        let set_of_closures =
          translate_set_of_closures
            (inconstants:Inconstant_idents.result)
            (aliases:Alias_analysis.allocation_point Variable.Map.t)
            (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
            (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
            (set_of_closures:Flambda.set_of_closures)
        in
        Flambda.Set_of_closures set_of_closures)
    constant_defining_values

let find_original_set_of_closure
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    project_closure_map
    var =
  let rec loop var =
    match Variable.Map.find var aliases with
    | Variable var -> begin
        match Variable.Tbl.find var_to_definition_tbl var with
        | Project_closure { set_of_closures = var }
        | Move_within_set_of_closures { closure = var } ->
          loop var
        | Set_of_closures _ -> begin
            match Variable.Tbl.find var_to_symbol_tbl var with
            | s ->
              s
            | exception Not_found ->
              Format.eprintf "var: %a@." Variable.print var;
              assert false
          end
        | _ -> assert false
      end
    | Symbol s ->
      match Symbol.Map.find s project_closure_map with
      | exception Not_found ->
        assert false
      | s -> s
  in
  loop var

let translate_definition_and_resolve_alias
    inconstants
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    (project_closure_map:Symbol.t Symbol.Map.t)
    (definition:Alias_analysis.constant_defining_value)
  : Flambda.constant_defining_value option =
  match definition with
  | Block (tag, fields) ->
    Some (Flambda.Block (tag, List.map (resolve_variable aliases var_to_symbol_tbl var_to_definition_tbl) fields))
  | Allocated_const c -> Some (Flambda.Allocated_const c)
  | Project_closure { set_of_closures; closure_id } ->
    begin match Variable.Map.find set_of_closures aliases with
    | Symbol s ->
      Some (Flambda.Project_closure (s, closure_id))
    (* If a closure projection is a constant, the set of closures must
       be assigned to a symbol. *)
    | exception Not_found ->
      assert false
    | Variable v ->
      match Variable.Tbl.find var_to_symbol_tbl v with
      | s ->
        Some (Flambda.Project_closure (s, closure_id))
      | exception Not_found ->
        Format.eprintf "var: %a@." Variable.print v;
        assert false
    end
  | Move_within_set_of_closures { closure; move_to } ->
    let set_of_closure_symbol =
      find_original_set_of_closure
        aliases
        var_to_symbol_tbl
        var_to_definition_tbl
        project_closure_map
        closure
    in
    Some (Flambda.Project_closure (set_of_closure_symbol, move_to))
  | Set_of_closures set_of_closures ->
    let set_of_closures =
      translate_set_of_closures
        inconstants
        aliases
        var_to_symbol_tbl
        var_to_definition_tbl
        set_of_closures
    in
    Some (Flambda.Set_of_closures set_of_closures)

  | Project_var _ -> None
  | Field (_,_) | Symbol_field _ -> None
  | Const _ -> None
  | Symbol _ -> None
  | Variable _ -> None

let translate_definitions_and_resolve_alias
    inconstants
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    project_closure_map =
  Variable.Tbl.fold (fun var def map ->
      match translate_definition_and_resolve_alias inconstants aliases
              var_to_symbol_tbl var_to_definition_tbl
              project_closure_map def with
      | None -> map
      | Some def ->
        let symbol = Variable.Tbl.find var_to_symbol_tbl var in
        Symbol.Map.add symbol def map)
    var_to_definition_tbl Symbol.Map.empty

(* Resorting of graph including Initialize_symbol *)
let constant_dependencies ~backend:_ (const:Flambda.constant_defining_value) =
  let closure_dependencies (set_of_closures:Flambda.set_of_closures) =
    let set = ref Symbol.Set.empty in
    Flambda_iterators.iter_symbols_on_named ~f:(fun s ->
        set := Symbol.Set.add s !set)
      (Set_of_closures set_of_closures);
    (* (\* A set of closures do not depend on the closure it define *\) *)
    (* let closure_ids = *)
    (*   Symbol.Set.of_list *)
    (*     (List.map (fun var -> *)
    (*          closure_symbol ~backend (Closure_id.wrap var)) *)
    (*         (Variable.Set.elements *)
    (*            (Variable.Map.keys set_of_closures.function_decls.funs))) *)
    (* in *)
    (* Symbol.Set.diff !set closure_ids *)
    !set
  in
  match const with
  | Allocated_const _ -> Symbol.Set.empty
  | Block (_, fields) ->
    let symbol_fields = Misc.filter_map
        (function
          | (Symbol s:Flambda.constant_defining_value_block_field) -> Some s
          | Flambda.Const _ -> None)
        fields
    in
    Symbol.Set.of_list symbol_fields
  | Set_of_closures set_of_closures ->
    closure_dependencies set_of_closures
  | Project_closure (s, _) ->
    Symbol.Set.singleton s

let expression_symbol_dependencies (expr:Flambda.t) =
  let set = ref Symbol.Set.empty in
  Flambda_iterators.iter_symbols ~f:(fun s ->
      set := Symbol.Set.add s !set)
    expr;
  !set

let program_graph
    ~backend
    imported_symbols symbol_to_constant
    (initialize_symbol_tbl : (Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t)
    (effect_tbl : (Flambda.t * Symbol.t option) Symbol.Tbl.t) =
  let graph_with_only_constant_parts =
    Symbol.Map.map (fun const ->
        Symbol.Set.diff (constant_dependencies ~backend const) imported_symbols)
      symbol_to_constant
  in
  let graph_with_initialisation =
    Symbol.Tbl.fold (fun sym (_tag, fields, previous) ->
        let order_dep =
          match previous with
          | None -> Symbol.Set.empty
          | Some previous -> Symbol.Set.singleton previous
        in
        let deps = List.fold_left (fun set field ->
            Symbol.Set.union (expression_symbol_dependencies field) set)
            order_dep fields
        in
        let deps = Symbol.Set.diff deps imported_symbols in
        Symbol.Map.add sym deps)
      initialize_symbol_tbl graph_with_only_constant_parts
  in
  let graph =
    Symbol.Tbl.fold (fun sym (expr, previous) ->
        let order_dep =
          match previous with
          | None -> Symbol.Set.empty
          | Some previous -> Symbol.Set.singleton previous
        in
        (* Format.printf "effect dep: %a %a@." Symbol.print sym Flambda.print expr; *)
        let deps = Symbol.Set.union (expression_symbol_dependencies expr) order_dep in
        let deps = Symbol.Set.diff deps imported_symbols in
        Symbol.Map.add sym deps
      )
      effect_tbl graph_with_initialisation
  in
  let module Symbol_SCC = Sort_connected_components.Make (Symbol) in
  let components =
    Symbol_SCC.connected_components_sorted_from_roots_to_leaf
      graph
  in
  components

(* rebuilding the program *)
let add_definition_of_symbol constant_definitions
    (initialize_symbol_tbl : (Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t)
    (effect_tbl:(Flambda.t * Symbol.t option) Symbol.Tbl.t)
    program component : Flambda.program =
  (* Format.eprintf "add_definition_of_symbols@."; *)
  let symbol_declaration sym =
    (* A symbol declared through an Initialize_symbol construct
       cannot be recursive, this is not allowed in the construction.
       This also couldn't have been introduced by this pass, so we can
       safely assert that this is not possible here *)
    assert(not (Symbol.Tbl.mem initialize_symbol_tbl sym));
    (* Format.eprintf "add_definition_of_symbol %a@." Symbol.print sym; *)
    (sym, Symbol.Map.find sym constant_definitions)
  in
  let module Symbol_SCC = Sort_connected_components.Make (Symbol) in
  match component with
  | Symbol_SCC.Has_loop l ->
    let l = List.map symbol_declaration l in
    Let_rec_symbol (l, program)
  | Symbol_SCC.No_loop sym ->
    (* Format.eprintf "no loop component %a@." Symbol.print sym; *)
    match Symbol.Tbl.find initialize_symbol_tbl sym with
    | (tag, fields, _previous) ->
      (* Format.eprintf "initialize@."; *)
      Initialize_symbol (sym, tag, fields, program)
    | exception Not_found ->
      match Symbol.Tbl.find effect_tbl sym with
      | (expr, _previous) ->
        (* Format.eprintf "effect@."; *)
        Effect (expr, program)
      | exception Not_found ->
        (* Format.eprintf "symbol@."; *)
        let decl = Symbol.Map.find sym constant_definitions in
        Let_symbol (sym, decl, program)

let add_definitions_of_symbols constant_definitions initialize_symbol_tbl
    effect_tbl program components =
  Array.fold_left
    (add_definition_of_symbol constant_definitions initialize_symbol_tbl effect_tbl)
    program components

let introduce_free_variables_in_set_of_closures
    (var_to_block_field_tbl:Flambda.constant_defining_value_block_field Variable.Tbl.t)
    { Flambda.function_decls; free_vars; specialised_args } =
  let add_definition_and_make_substitution var (expr, subst) =
    match Variable.Tbl.find var_to_block_field_tbl var with
    | def ->
      let fresh = Variable.freshen var in
      let named : Flambda.named = match def with
        | Symbol sym -> Symbol sym
        | Const c -> Const c
      in
      (Flambda.create_let fresh named expr), Variable.Map.add var fresh subst
    | exception Not_found ->
      (* The variable is bound by the closure or the arguments or not
         constant. In either case it does not need to be binded *)
      expr, subst
  in
  let function_decls : Flambda.function_declarations =
    Flambda.update_function_declarations function_decls
      ~funs:(Variable.Map.mapi
          (fun _fun_var (ffun : Flambda.function_declaration) ->
(*
             Format.printf "introduce in %a@."
               Variable.print fun_var;
*)
             let variables_to_bind =
               (* Closures from the same set must not be bound *)
               Variable.Set.diff
                 ffun.free_variables
                 (Variable.Map.keys function_decls.funs)
             in
             let body, subst =
               Variable.Set.fold
                 add_definition_and_make_substitution
                 variables_to_bind
                 (ffun.body, Variable.Map.empty)
             in
             let body =
               Flambda_utils.toplevel_substitution subst body
             in
             Flambda.create_function_declaration
               ~params:ffun.params
               ~body
               ~stub:ffun.stub
               ~dbg:ffun.dbg)
          function_decls.funs)
  in
  let free_vars =
    (* Keep only those that are not rewriten to constants *)
    Variable.Map.filter
      (fun v _ ->
(*
        if Variable.Tbl.mem var_to_block_field_tbl v then begin
          Format.eprintf "dropping variable %a rewritten to a constant\n"
            Variable.print v;
        end;
*)
        not (Variable.Tbl.mem var_to_block_field_tbl v))
      free_vars
  in
  Flambda.create_set_of_closures ~function_decls ~free_vars
    ~specialised_args

let rewrite_project_var
      (var_to_block_field_tbl
        : Flambda.constant_defining_value_block_field Variable.Tbl.t)
      flam =
  Flambda_iterators.map_project_var_to_named_opt flam
    ~f:(fun (project_var : Flambda.project_var) ->
      let var = Var_within_closure.unwrap project_var.var in
      match Variable.Tbl.find var_to_block_field_tbl var with
      | exception Not_found -> None
      | Symbol sym -> Some ((Symbol sym) : Flambda.named)
      | Const const -> Some ((Const const) : Flambda.named))

let introduce_free_variables_in_sets_of_closures
    (var_to_block_field_tbl:Flambda.constant_defining_value_block_field Variable.Tbl.t)
    (translate_definition:Flambda.constant_defining_value Symbol.Map.t) =
  Symbol.Map.map (fun (def:Flambda.constant_defining_value) ->
      match def with
      | Allocated_const _
      | Block _
      | Project_closure _ -> def
      | Set_of_closures set_of_closures ->
        Flambda.Set_of_closures
          (introduce_free_variables_in_set_of_closures
             var_to_block_field_tbl
             set_of_closures))
    translate_definition

let var_to_block_field
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
  =
  let var_to_block_field_tbl = Variable.Tbl.create 42 in
  Variable.Tbl.iter (fun var _ ->
      let def =
        resolve_variable aliases var_to_symbol_tbl var_to_definition_tbl var
      in
      Variable.Tbl.add var_to_block_field_tbl var def
    )
    var_to_definition_tbl;
  var_to_block_field_tbl

let program_symbols ~backend program =
  let new_fake_symbol =
    let r = ref 0 in
    fun () ->
      incr r;
      Symbol.create (Compilation_unit.get_current_exn ())
        (Linkage_name.create ("fake_effect_symbol_" ^ string_of_int !r))
  in
  let initialize_symbol_tbl = Symbol.Tbl.create 42 in
  let effect_tbl = Symbol.Tbl.create 42 in
  let symbol_definition_tbl = Symbol.Tbl.create 42 in
  let add_project_closure_definitions def_symbol (const:Flambda.constant_defining_value) =
    match const with
    | Set_of_closures { function_decls = { funs } } ->
        Variable.Map.iter (fun fun_var _ ->
            let closure_id = Closure_id.wrap fun_var in
            let closure_symbol = closure_symbol ~backend closure_id in
            let project_closure =
              Flambda.Project_closure (def_symbol, closure_id)
            in
            Symbol.Tbl.add symbol_definition_tbl closure_symbol
              project_closure)
          funs
    | Project_closure _
    | Allocated_const _
    | Block _ -> ()
  in
  let rec loop (program:Flambda.program) previous_effect =
    match program with
    | Flambda.Let_symbol (symbol,def,program) ->
      add_project_closure_definitions symbol def;
      Symbol.Tbl.add symbol_definition_tbl symbol def;
      loop program previous_effect
    | Flambda.Let_rec_symbol (defs,program) ->
      List.iter (fun (symbol, def) ->
          add_project_closure_definitions symbol def;
          Symbol.Tbl.add symbol_definition_tbl symbol def)
        defs;
      loop program previous_effect
    | Flambda.Import_symbol (_,program) ->
      loop program previous_effect
    | Flambda.Initialize_symbol (symbol,tag,fields,program) ->
      (* previous_effect is used to keep the order of initialize and effect
         values. Their effects order must be kept ordered.
         it is used as an extra dependency when sorting the symbols. *)
      (* CR pchambart: if the fields expressions are pure, we could drop
         this dependency *)
      Symbol.Tbl.add initialize_symbol_tbl symbol (tag,fields,previous_effect);
      loop program (Some symbol)
    | Flambda.Effect (expr,program) ->
      (* Used to ensure that effects are correctly ordered *)
      let fake_effect_symbol = new_fake_symbol () in
      Symbol.Tbl.add effect_tbl fake_effect_symbol (expr,previous_effect);
      loop program (Some fake_effect_symbol)
    | Flambda.End _ -> ()
  in
  loop program None;
  initialize_symbol_tbl, symbol_definition_tbl, effect_tbl

let replace_definitions_in_initialize_symbol_and_effects
    (inconstants:Inconstant_idents.result)
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    (initialize_symbol_tbl:(Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t)
    (effect_tbl:(Flambda.t * Symbol.t option) Symbol.Tbl.t) =
  let f var (named:Flambda.named) : Flambda.named =
    if Variable.Set.mem var inconstants.id then
      named
    else
      let resolved =
        resolve_variable
          aliases
          var_to_symbol_tbl
          var_to_definition_tbl
          var
      in
      match resolved with
      | Symbol s -> Symbol s
      | Const c -> Const c
  in
  Symbol.Map.iter (fun symbol (tag, fields, previous) ->
      let fields =
        List.map (Flambda_iterators.map_all_immutable_let_and_let_rec_bindings ~f) fields
      in
      Symbol.Tbl.replace initialize_symbol_tbl symbol (tag,fields,previous))
    (Symbol.Tbl.to_map initialize_symbol_tbl);
  Symbol.Map.iter (fun symbol (expr, previous) ->
      let expr =
        Flambda_iterators.map_all_immutable_let_and_let_rec_bindings ~f expr
      in
      Symbol.Tbl.replace effect_tbl symbol (expr,previous))
    (Symbol.Tbl.to_map effect_tbl)

let project_closure_map symbol_definition_map =
  Symbol.Map.fold (fun sym (const:Flambda.constant_defining_value) acc ->
      match const with
      | Project_closure (set_of_closures, _) ->
        Symbol.Map.add sym set_of_closures acc
      | Set_of_closures _
      | Allocated_const _
      | Block _ -> acc)
    symbol_definition_map
    Symbol.Map.empty

let lift_constants program ~backend =
  (* Format.eprintf "lift_constants input:@ %a\n" Flambda.print_program program; *)
  let inconstants =
    Inconstant_idents.inconstants_on_program program
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend
         mshinwell: what is "this"?
         pchambart: The access to compilenv *)
      ~compilation_unit:(Compilation_unit.get_current_exn ())
  in
  let initialize_symbol_tbl, symbol_definition_tbl, effect_tbl =
    program_symbols ~backend program
  in
  let var_to_symbol_tbl, var_to_definition_tbl, initialize_symbol_to_definition_tbl =
    assign_symbols_and_collect_constant_definitions ~backend ~program ~inconstants
  in
  let aliases =
    let var_map = Variable.Tbl.to_map var_to_definition_tbl in
    let initialize_symbol_map =
      Symbol.Tbl.to_map initialize_symbol_to_definition_tbl in
    let sym_map = Flambda_utils.all_lifted_constants_as_map program in
    let var_to_sym_map = Symbol.Map.empty (* Variable.Tbl.to_map var_to_symbol_tbl *) in
    Alias_analysis.second_take var_map initialize_symbol_map sym_map var_to_sym_map
  in
  replace_definitions_in_initialize_symbol_and_effects
      (inconstants:Inconstant_idents.result)
      (aliases:Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
      initialize_symbol_tbl
      effect_tbl;
  let symbol_definition_map =
    translate_constant_set_of_closures
      (inconstants:Inconstant_idents.result)
      (aliases:Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
      (Symbol.Tbl.to_map symbol_definition_tbl)
  in
  let project_closure_map =
    project_closure_map symbol_definition_map
  in
  let translated_definitions =
    translate_definitions_and_resolve_alias
      inconstants
      (aliases:Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
      project_closure_map
  in
  let var_to_block_field_tbl =
    var_to_block_field
      (aliases:Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
  in
  let translated_definitions =
    introduce_free_variables_in_sets_of_closures
      var_to_block_field_tbl
      translated_definitions
  in
  let constant_definitions =
(*
    let inter =
      Symbol.Set.inter
        (Symbol.Map.keys symbol_definition_map)
        (Symbol.Map.keys translated_definitions)
    in
    Format.eprintf "symbol intersection %a@."
      Symbol.Set.print inter;
*)
    (* Add previous Let_symbol to the newly discovered ones *)
    Symbol.Map.union_merge
      (fun
        (c1:Flambda.constant_defining_value)
        (c2:Flambda.constant_defining_value) ->
        match c1, c2 with
        | Project_closure (s1, closure_id1),
          Project_closure (s2, closure_id2) when
            Symbol.equal s1 s2 &&
            Closure_id.equal closure_id1 closure_id2 ->
          c1
        | Project_closure (s1, closure_id1),
          Project_closure (s2, closure_id2) ->
          Format.eprintf "not equal project closure@. s %a %a@. cid %a %a@."
            Symbol.print s1 Symbol.print s2
            Closure_id.print closure_id1 Closure_id.print closure_id2;
          assert false
        | _ ->
          assert false
      )
      symbol_definition_map
      translated_definitions
  in
  let constant_definitions =
    Symbol.Map.map (fun (const:Flambda.constant_defining_value) ->
        match const with
        | Allocated_const _
        | Block _
        | Project_closure _ ->
          const
        | Set_of_closures set_of_closures ->
          let set_of_closures =
            Flambda_iterators.map_function_bodies
              ~f:(Flambda_iterators.map_sets_of_closures
                    ~f:(introduce_free_variables_in_set_of_closures
                          var_to_block_field_tbl))
              set_of_closures
          in
          Flambda.Set_of_closures
            (introduce_free_variables_in_set_of_closures
               var_to_block_field_tbl set_of_closures)
      )
      constant_definitions
  in
  let effect_tbl =
    Symbol.Tbl.map effect_tbl
      (fun (effect, dep) ->
         let effect =
           Flambda_iterators.map_sets_of_closures
             ~f:(introduce_free_variables_in_set_of_closures
                   var_to_block_field_tbl)
             effect
         in
         effect, dep)
  in
  let initialize_symbol_tbl =
    Symbol.Tbl.map initialize_symbol_tbl
      (fun (tag, fields, dep) ->
         let fields =
           List.map
             (Flambda_iterators.map_sets_of_closures
                ~f:(introduce_free_variables_in_set_of_closures
                      var_to_block_field_tbl))
             fields
         in
         tag, fields, dep)
  in
  (* CR mshinwell: tidy this up once correct.
     If a variable bound by a closure gets replaced by a symbol and
     thus eliminated from the [free_vars] set of the closure, we need to
     rewrite any subsequent [Project_var] expressions that project that
     variable. *)
  let constant_definitions =
    Symbol.Map.map (fun (const:Flambda.constant_defining_value) ->
        match const with
        | Allocated_const _
        | Block _
        | Project_closure _ ->
          const
        | Set_of_closures set_of_closures ->
          Flambda.Set_of_closures (Flambda_iterators.map_function_bodies
            ~f:(rewrite_project_var var_to_block_field_tbl)
            set_of_closures)
      )
      constant_definitions
  in
  let effect_tbl =
    Symbol.Tbl.map effect_tbl
      (fun (effect, dep) ->
         let effect = rewrite_project_var var_to_block_field_tbl effect in
         effect, dep)
  in
  let initialize_symbol_tbl =
    Symbol.Tbl.map initialize_symbol_tbl
      (fun (tag, fields, dep) ->
         let fields =
           List.map
             (rewrite_project_var var_to_block_field_tbl)
             fields
         in
         tag, fields, dep)
  in
  (* End of part to tidy up *)
  let imported_symbols = Flambda_utils.imported_symbols program in
  let components = program_graph ~backend imported_symbols constant_definitions
      initialize_symbol_tbl effect_tbl in
  let program =
    add_definitions_of_symbols constant_definitions
      initialize_symbol_tbl
      effect_tbl
      (Flambda.End (Flambda_utils.root_symbol program))
      components
  in
  let program = Flambda_utils.introduce_needed_import_symbols program in
  (* Format.eprintf "@.lift_constants output:@ %a\n" Flambda.print_program program; *)
  program

