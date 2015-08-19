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

open Alias_analysis

type result = Flambda.program

(** Like [Flambda.constant_defining_value] but using [Variable.t] for
    inter-constant references rather than [Symbol.t].  This type also permits
    projections from blocks (which we will entirely simplify away in this pass)
    and non-allocated constants (which will never be assigned symbols). *)
(* type constant_defining_value = Alias_analysis.constant_defining_value *)

let rec tail_variable : Flambda.t -> Variable.t option = function
  | Var v -> Some v
  | Let_rec (_, e)
  | Let (_,_,_,e) -> tail_variable e
  | _ -> None

(** Traverse the given expression assigning symbols to [let]- and [let rec]-
    bound constant variables.  At the same time collect the definitions of
    such variables. *)
let assign_symbols_and_collect_constant_definitions ~program
    ~(inconstants : Inconstant_idents.result) =
  let var_to_symbol_tbl = Variable.Tbl.create 42 in
  let var_to_definition_tbl = Variable.Tbl.create 42 in
  let assign_symbol var (named : Flambda.named) =
    if not (Variable.Set.mem var inconstants.id) then begin
      let assign_symbol () =
        Variable.Tbl.add var_to_symbol_tbl var
          (Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ())
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
      | Prim (Pmakeblock (tag, _), fields, _) ->
        assign_symbol ();
        record_definition (Block (Tag.create_exn tag, fields))
      | Set_of_closures (
          { function_decls = { funs; set_of_closures_id; _ };
            specialised_args; _ } as set) ->
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
            let closure_symbol = Compilenv.closure_symbol closure_id in
            Variable.Tbl.add var_to_symbol_tbl fun_var closure_symbol;
            Variable.Tbl.add var_to_definition_tbl fun_var
              (Symbol closure_symbol))
          funs;
        Variable.Map.iter (fun arg var ->
            Variable.Tbl.add var_to_definition_tbl arg (Variable var))
          specialised_args
      | Move_within_set_of_closures { closure = _; start_from = _; move_to; } ->
        let symbol = Compilenv.closure_symbol move_to in
        assign_existing_symbol symbol;
        record_definition (Symbol symbol)
      | Project_closure ({ closure_id } as project_closure) ->
        assign_existing_symbol (Compilenv.closure_symbol closure_id);
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
      | Predefined_exn exn ->
        record_definition (Predefined_exn exn)
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
    Flambda_iterators.iter_all_let_and_let_rec_bindings expr ~f:assign_symbol
  in
  Flambda_iterators.iter_exprs_at_toplevel_of_program
    ~f:assign_symbol_program
    program;
  var_to_symbol_tbl,
  var_to_definition_tbl

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
    variable_field_definition var_to_symbol_tbl var_to_definition_tbl var
  | Symbol s -> Symbol s
  | Variable aliased_variable ->
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
  Flambda_iterators.map_function_bodies set_of_closures
    ~f:(Flambda_iterators.map_all_let_and_let_rec_bindings ~f)

let translate_definition_and_resolve_alias
    inconstants
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
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
  | Field (_,_) -> None
  | Const _ -> None
  | Symbol _ -> None
  | Predefined_exn _ -> failwith "TODO"
  | Variable _ -> None

let translate_definitions_and_resolve_alias
    inconstants
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t) =
  Variable.Tbl.fold (fun var def map ->
      match translate_definition_and_resolve_alias inconstants aliases
              var_to_symbol_tbl var_to_definition_tbl def with
      | None -> map
      | Some def ->
        let symbol = Variable.Tbl.find var_to_symbol_tbl var in
        Symbol.Map.add symbol def map)
    var_to_definition_tbl Symbol.Map.empty

(* Resorting of graph including Initialize_symbol *)
let constant_dependencies (const:Flambda.constant_defining_value) =
  let closure_dependencies (set_of_closures:Flambda.set_of_closures) =
    let set = ref Symbol.Set.empty in
    Flambda_iterators.iter_symbols_on_named ~f:(fun s ->
        set := Symbol.Set.add s !set)
      (Set_of_closures set_of_closures);
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

let program_graph symbol_to_constant
    (initialize_symbol_tbl : (Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t) =
  let graph_with_only_constant_parts =
    Symbol.Map.map constant_dependencies symbol_to_constant
  in
  let graph =
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
        Symbol.Map.add sym deps)
      initialize_symbol_tbl graph_with_only_constant_parts
  in
  Format.eprintf "@.dep graph:@ %a@."
    (Symbol.Map.print Symbol.Set.print) graph;
  let module Symbol_SCC = Sort_connected_components.Make (Symbol) in
  let components =
    Symbol_SCC.connected_components_sorted_from_roots_to_leaf
      graph
  in
  components

(* rebuilding the program *)
let add_definition_of_symbol constant_definitions
    (initialize_symbol_tbl : (Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t)
    program component : Flambda.program =
  let symbol_declaration sym =
    (* A symbol declared through an Initialize_symbol construct
       cannot be recursive, this is not allowed in the construction.
       This also couldn't have been introduced by this pass, so we can
       safely assert that this is not possible here *)
    assert(not (Symbol.Tbl.mem initialize_symbol_tbl sym));
    (sym, Symbol.Map.find sym constant_definitions)
  in
  let module Symbol_SCC = Sort_connected_components.Make (Symbol) in
  match component with
  | Symbol_SCC.Has_loop l ->
    let l = List.map symbol_declaration l in
    Let_rec_symbol (l, program)
  | Symbol_SCC.No_loop sym ->
    match Symbol.Tbl.find initialize_symbol_tbl sym with
    | (tag, fields, _previous) ->
      Initialize_symbol (sym, tag, fields, program)
    | exception Not_found ->
      let decl = Symbol.Map.find sym constant_definitions in
      Let_symbol (sym, decl, program)

let add_definitions_of_symbols constant_definitions initialize_symbol_tbl
    program components =
  Array.fold_left (add_definition_of_symbol constant_definitions initialize_symbol_tbl)
    program components

let introduce_free_variables_in_set_of_closures
    (var_to_block_field_tbl:Flambda.constant_defining_value_block_field Variable.Tbl.t)
    { Flambda.function_decls; free_vars; specialised_args } =
  (* TODO substitute free variable names: They could be used in
     multiple closure in the set and it is forbidden to have a
     variable bound multiple times. *)
  let add_definition expr var : Flambda.t =
    match Variable.Tbl.find var_to_block_field_tbl var with
    | Symbol sym ->
      Let (Immutable, var, Symbol sym, expr)
    | Const c ->
      Let (Immutable, var, Const c, expr)
    | exception Not_found ->
      (* The variable is bound by the closure or the arguments *)
      expr
  in
  let function_decls : Flambda.function_declarations =
    { function_decls with
      funs = Variable.Map.map
          (fun (ffun : Flambda.function_declaration) ->
             let body =
               List.fold_left
                 add_definition
                 ffun.body
                 (Variable.Set.elements ffun.free_variables)
             in
             Flambda.create_function_declaration
               ~params:ffun.params
               ~body
               ~stub:ffun.stub
               ~dbg:ffun.dbg)
          function_decls.funs;
    }
  in
  Flambda.create_set_of_closures ~function_decls ~free_vars
    ~specialised_args

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

let program_symbols program =
  let initialize_symbol_tbl = Symbol.Tbl.create 42 in
  let symbol_definition_tbl = Symbol.Tbl.create 42 in
  let rec loop (program:Flambda.program) previous_initialize =
    match program with
    | Flambda.Let_symbol (symbol,def,program) ->
      Symbol.Tbl.add symbol_definition_tbl symbol def;
      loop program previous_initialize
    | Flambda.Let_rec_symbol (defs,program) ->
      List.iter (fun (symbol, def) ->
          Symbol.Tbl.add symbol_definition_tbl symbol def)
        defs;
      loop program previous_initialize
    | Flambda.Import_symbol (_,program) ->
      loop program previous_initialize
    | Flambda.Initialize_symbol (symbol,tag,fields,program) ->
      Symbol.Tbl.add initialize_symbol_tbl symbol (tag,fields,previous_initialize);
      loop program (Some symbol)
    | Flambda.End -> ()
  in
  loop program None;
  initialize_symbol_tbl, symbol_definition_tbl

let replace_definitions_in_initialize_symbol
    (inconstants:Inconstant_idents.result)
    (aliases:Alias_analysis.allocation_point Variable.Map.t)
    (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
    (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
    (initialize_symbol_tbl:(Tag.t * Flambda.t list * Symbol.t option) Symbol.Tbl.t) =
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
        List.map (Flambda_iterators.map_all_let_and_let_rec_bindings ~f) fields
      in
      Symbol.Tbl.replace initialize_symbol_tbl symbol (tag,fields,previous))
    (Symbol.Tbl.to_map initialize_symbol_tbl)

let lift_constants program ~backend:_ =
  Format.eprintf "lift_constants input:@ %a\n" Flambda.print_program program;
  let inconstants =
    Inconstant_idents.inconstants_on_program program
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend
         mshinwell: what is "this"?
         pchambart: The access to compilenv *)
      ~compilation_unit:(Compilenv.current_unit ())
  in
  let initialize_symbol_tbl, symbol_definition_tbl = program_symbols program in
  let var_to_symbol_tbl, var_to_definition_tbl =
    assign_symbols_and_collect_constant_definitions ~program ~inconstants
  in
  let aliases =
    let var_map = Variable.Tbl.to_map var_to_definition_tbl in
    let sym_map = Symbol.Map.empty in (* TODO: program toplevel *)
    let var_to_sym_map = Symbol.Map.empty (* Variable.Tbl.to_map var_to_symbol_tbl *) in
    Alias_analysis.second_take var_map sym_map var_to_sym_map
  in
  replace_definitions_in_initialize_symbol
      (inconstants:Inconstant_idents.result)
      (aliases:Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
      initialize_symbol_tbl;
  let translated_definitions =
    translate_definitions_and_resolve_alias
      inconstants
      (aliases:Alias_analysis.allocation_point Variable.Map.t)
      (var_to_symbol_tbl:Symbol.t Variable.Tbl.t)
      (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t)
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
    (* Add previous Let_symbol to the newly discovered ones *)
    Symbol.Tbl.fold (fun symbol def map ->
        Symbol.Map.add symbol def map)
      symbol_definition_tbl
      translated_definitions
  in
  let components = program_graph constant_definitions initialize_symbol_tbl in
  let program =
    add_definitions_of_symbols constant_definitions
      initialize_symbol_tbl
      Flambda.End components
  in
  Format.eprintf "@.lift_constants output:@ %a\n" Flambda.print_program program;
  program

