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

module Int = Ext_types.Int
module ET = Export_info

type general_env =
  { sym : Export_id.t Symbol.Map.t;
    ex_table : Export_info.descr Export_id.Map.t ref;
  }

type env =
  { var : Export_info.approx Variable.Map.t;
    sym : Export_id.t Symbol.Map.t;
    ex_table : Export_info.descr Export_id.Map.t ref;
  }

let create_empty_env () =
  { sym = Symbol.Map.empty;
    ex_table = ref Export_id.Map.empty;
  }

let add_empty_env (env:general_env) =
  { var = Variable.Map.empty;
    sym = env.sym;
    ex_table = env.ex_table;
  }

let extern_id_descr ex =
  let export = Compilenv.approx_env () in
  try Some (Export_info.find_description export ex)
  with Not_found -> None

let extern_symbol_descr sym =
  if Compilenv.is_predefined_exception sym
  then None
  else
    let export =
      Compilenv.approx_for_global (Symbol.compilation_unit sym)
    in
    try
      let id = Symbol.Map.find sym export.symbol_id in
      let descr = Export_info.find_description export id in
      Some descr
    with
    | Not_found -> None

let get_symbol_descr env sym =
  try
    let ex = Symbol.Map.find sym env.sym in
    Some (Export_id.Map.find ex !(env.ex_table))
  with Not_found ->
    extern_symbol_descr sym

let get_descr env (approx : Export_info.approx) =
  match approx with
  | Value_unknown -> None
  | Value_id ex ->
    (try Some (Export_id.Map.find ex !(env.ex_table)) with
     | Not_found ->
       extern_id_descr ex)
  | Value_symbol sym ->
    get_symbol_descr env sym

let fresh_id () =
  Export_id.create (Compilenv.current_unit ())

let record_descr env id (descr:Export_info.descr) =
  env.ex_table := Export_id.Map.add id descr !(env.ex_table)

let new_descr env (descr:Export_info.descr) =
  let id = fresh_id () in
  record_descr env id descr;
  id

let describe_constant env (c:Flambda.constant_defining_value_block_field) : Export_info.approx =
  match c with
  | Symbol s -> Value_symbol s
  (* [Const_pointer] is an immediate value of a type whose values may be
     boxed (typically a variant type with both constant and non-constant
     constructors). *)
  | Const (Int i) -> Value_id (new_descr env (Value_int i))
  | Const (Char c) -> Value_id (new_descr env (Value_int (Char.code c)))
  | Const (Const_pointer i) -> Value_id (new_descr env (Value_int i))

let describe_allocated_constant
      (c : Allocated_const.t) : Export_info.descr =
  match c with
  | Float f -> Value_float f
  | Int32 i -> Value_boxed_int (Int32, i)
  | Int64 i -> Value_boxed_int (Int64, i)
  | Nativeint i -> Value_boxed_int (Nativeint, i)
  | String s ->
    let v_string : Export_info.value_string =
      { size = String.length s; contents = Unknown_or_mutable }
    in
    Value_string v_string
  | Immstring c ->
    let v_string : Export_info.value_string =
      { size = String.length c; contents = Contents c }
    in
    Value_string v_string
  | Float_array a -> Value_float_array (List.length a)

let find_approx env var : Export_info.approx =
  try Variable.Map.find var env.var with
  | Not_found -> Value_unknown

let rec describe (env : env) (flam : Flambda.t) : Export_info.approx =
  match flam with
  | Var var ->
    find_approx env var

  | Let { var = id; defining_expr = lam; body; _ } ->
    let approx = describe_named env lam in
    let env = { env with var = Variable.Map.add id approx env.var } in
    describe env body

  | Let_mutable (_mut_var, _var, body) ->
    describe env body

  | Let_rec(defs, body) ->
    let env =
      List.fold_left (fun env (var, def) ->
          let approx = describe_named env def in
          { env with var = Variable.Map.add var approx env.var }
        )
        env defs
    in
    describe env body

  | Apply { func; kind } ->
    begin match kind with
    | Indirect -> Value_unknown
    | Direct closure_id' ->
      match get_descr env (find_approx env func) with
      | Some(Value_closure { closure_id; set_of_closures = { results } }) ->
        assert (Closure_id.equal closure_id closure_id');
        assert (Closure_id.Map.mem closure_id results);
        Closure_id.Map.find closure_id results
      | _ -> Value_unknown
    end

  | Assign _ ->
    Value_id (new_descr env (Value_constptr 0))

  | For _ ->
    Value_id (new_descr env (Value_constptr 0))

  | While _ ->
    Value_id (new_descr env (Value_constptr 0))

  | Static_raise _ ->
    Value_unknown

  | Static_catch _ ->
    Value_unknown

  | Try_with _ ->
    Value_unknown

  | If_then_else _ ->
    Value_unknown

  | Switch _ ->
    Value_unknown

  | String_switch _ ->
    Value_unknown

  | Send _ ->
    Value_unknown

  | Proved_unreachable ->
    Value_unknown

and describe_named (env : env) (named : Flambda.named) : Export_info.approx =
  match named with
  | Expr e ->
    describe env e

  | Symbol sym ->
    Value_symbol sym

  | Read_mutable _ -> Value_unknown

  | Read_symbol_field (sym, i) -> begin
      match get_symbol_descr env sym with
      | Some (Value_block (_, fields)) when Array.length fields > i ->
        fields.(i)
      | _ ->
        Value_unknown
    end

  | Const c -> begin
      match c with
      | Int i ->
        Value_id (new_descr env (Value_int i))
      | Char c ->
        Value_id (new_descr env (Value_int (Char.code c)))
      | Const_pointer i ->
        Value_id (new_descr env (Value_constptr i))
    end
  | Allocated_const c ->
    let descr = describe_allocated_constant c in
    Value_id (new_descr env descr)

  | Prim(Pmakeblock(tag, Immutable), args, _dbg) ->
    let approxs = List.map (find_approx env) args in
    let descr = Export_info.Value_block (Tag.create_exn tag, Array.of_list approxs) in
    Value_id (new_descr env descr)

  | Prim(Pfield i, [arg], _) -> begin
      match get_descr env (find_approx env arg) with
      | Some (Value_block (_, fields)) when Array.length fields > i ->
        fields.(i)
      | _ ->
        Value_unknown
    end

  | Prim(Pgetglobal id, _, _) ->
    Value_symbol (Compilenv.symbol_for_global' id)

  | Prim(_, _, _) ->
    Value_unknown

  | Set_of_closures set ->
    let descr =
      Export_info.Value_set_of_closures (describe_set_of_closures env set)
    in
    Value_id (new_descr env descr)

  | Project_closure { set_of_closures; closure_id } -> begin
      match get_descr env (find_approx env set_of_closures) with
      | Some(Value_set_of_closures set_of_closures) ->
        let descr = Export_info.Value_closure { closure_id = closure_id; set_of_closures } in
        Value_id (new_descr env descr)
      | _ ->
        (* CR pchambart: This should be [assert false], but currently there are a
           few cases where this is less precise than inline_and_simplify. *)
        Value_unknown
    end

  | Move_within_set_of_closures { closure; start_from; move_to } -> begin
      match get_descr env (find_approx env closure) with
      | Some(Value_closure { set_of_closures; closure_id }) ->
        assert(Closure_id.equal closure_id start_from);
        let descr = Export_info.Value_closure { closure_id = move_to; set_of_closures } in
        Value_id (new_descr env descr)
      | _ -> Value_unknown
    end

  | Project_var { closure; closure_id = closure_id'; var } ->
    begin match get_descr env (find_approx env closure) with
    | Some (Value_closure { set_of_closures = { bound_vars }; closure_id }) ->
      assert (Closure_id.equal closure_id closure_id');
      if not (Var_within_closure.Map.mem var bound_vars) then begin
        Misc.fatal_errorf "Project_var from %a (closure ID %a) of \
            variable %a that is not bound by the closure.  \
            Variables bound by the closure are: %a"
          Variable.print closure
          Closure_id.print closure_id
          Var_within_closure.print var
          (Var_within_closure.Map.print (fun _ _ -> ())) bound_vars
      end;
      Var_within_closure.Map.find var bound_vars
    | _ -> Value_unknown
    end

and describe_set_of_closures env (set : Flambda.set_of_closures)
      : Export_info.value_set_of_closures =
  let bound_vars_approx = Variable.Map.map (find_approx env) set.free_vars in
  let specialised_args_approx =
    Variable.Map.map (find_approx env) set.specialised_args
  in
  let closures_approx =
    (* To build an approximation of the results, we need an
       approximation of the functions. The first one we can build is
       one where every function returns something unknown.

       CR pchambart: we could improve a bit on that by building a
       recursive approximation of the closures: The value_closure
       description contains a [value_set_of_closures]. We could replace
       this field by a [Expr_id.t] or an [approx]. *)
    let initial_value_set_of_closure =
      { Export_info.set_of_closures_id = set.function_decls.set_of_closures_id;
        bound_vars = Var_within_closure.wrap_map bound_vars_approx;
        results =
          Closure_id.wrap_map
            (Variable.Map.map (fun _ -> Export_info.Value_unknown)
               set.function_decls.funs);
        aliased_symbol = None;
      }
    in
    Variable.Map.mapi (fun var
          (function_decl : Flambda.function_declaration) ->
        (* CR mshinwell: consider moving this check into Flambda_invariants *)
        let free_vars_that_are_not_params_or_fun_vars =
          Variable.Set.diff function_decl.free_variables
            (Variable.Set.union (Variable.Set.of_list function_decl.params)
              (Variable.Map.keys set.function_decls.funs))
        in
        let bound_vars = Variable.Map.keys bound_vars_approx in
        if not (Variable.Set.subset free_vars_that_are_not_params_or_fun_vars
          bound_vars) then
        begin
          Misc.fatal_errorf "Build_export_info.describe_set_of_closures: \
              %a function declaration's [free_variables] set %a is wrong \
              (%a should be subset of %a).  Set of closures: %a"
            Variable.print var
            Variable.Set.print function_decl.free_variables
            Variable.Set.print free_vars_that_are_not_params_or_fun_vars
            Variable.Set.print bound_vars
            Flambda.print_set_of_closures set
        end;
        let descr =
          Export_info.Value_closure
            { closure_id = Closure_id.wrap var;
              set_of_closures = initial_value_set_of_closure;
            }
        in
        Export_info.Value_id (new_descr env descr))
      set.function_decls.funs
  in
  let closure_env =
    { env with
      var =
        Variable.Map.fold Variable.Map.add closures_approx
          (Variable.Map.fold Variable.Map.add bound_vars_approx
             (Variable.Map.fold Variable.Map.add specialised_args_approx env.var))
    }
  in
  let result_approx _var (function_declaration:Flambda.function_declaration) =
    describe closure_env function_declaration.body
  in
  let results =
    Variable.Map.mapi result_approx set.function_decls.funs
  in
  { set_of_closures_id = set.function_decls.set_of_closures_id;
    bound_vars = Var_within_closure.wrap_map bound_vars_approx;
    results = Closure_id.wrap_map results;
    aliased_symbol = None;
  }

let describe_constant_defining_value env id symbol
      (const : Flambda.constant_defining_value) =
  let local_env = add_empty_env env in
  match const with
  | Allocated_const alloc_const ->
    let descr = describe_allocated_constant alloc_const in
    record_descr local_env id descr
  | Block (tag, fields) ->
    let approxs = List.map (describe_constant local_env) fields in
    record_descr local_env id (Value_block (tag, Array.of_list approxs))
  | Set_of_closures set_of_closures ->
    let descr =
      Export_info.Value_set_of_closures
        { (describe_set_of_closures local_env set_of_closures) with
          aliased_symbol = Some symbol }
    in
    record_descr local_env id descr
  | Project_closure (sym, closure_id) -> begin
      match get_symbol_descr local_env sym with
      | Some (Value_set_of_closures set_of_closures) ->
        let descr =
          Export_info.Value_closure
            { closure_id = closure_id; set_of_closures }
        in
        record_descr local_env id descr
      | None ->
        Misc.fatal_errorf
          "Cannot project symbol %a to closure_id %a.  \
            No available description@."
          Symbol.print sym
          Closure_id.print closure_id
      | Some (Value_closure _) ->
        Misc.fatal_errorf
          "Cannot project symbol %a to closure_id %a.  \
            Closure instead of set of closure@."
          Symbol.print sym
          Closure_id.print closure_id
      | Some _ ->
        Misc.fatal_errorf
          "Cannot project symbol %a to closure_id %a.  Not a set of closures@."
          Symbol.print sym
          Closure_id.print closure_id
    end

let rec describe_program (env:general_env) (program : Flambda.program) =
  match program with
  | Let_symbol (symbol, constant_defining_value, program) ->
    let id = fresh_id () in
    let env = { env with sym = Symbol.Map.add symbol id env.sym } in
    describe_constant_defining_value env id symbol constant_defining_value;
    describe_program env program
  | Let_rec_symbol (defs, program) ->
    let env, defs =
      List.fold_left (fun ((env:general_env), defs) (symbol, def) ->
          let id = fresh_id () in
          { env with sym = Symbol.Map.add symbol id env.sym },
          (id, symbol, def) :: defs)
        (env, []) defs
    in
    (* [Project_closure]s are separated to be handled last.  They are the
       only values that need a description for their argument. *)
    let projects_closure, other_constants =
      List.partition (function
          | _, _, Flambda.Project_closure _ -> true
          | _ -> false)
        defs
    in
    List.iter (fun (id, symbol, def) ->
        describe_constant_defining_value env id symbol def)
      other_constants;
    List.iter (fun (id, symbol, def) ->
        describe_constant_defining_value env id symbol def)
      projects_closure;
    describe_program env program
  | Import_symbol (_symbol, program) ->
    describe_program env program
  | Initialize_symbol (symbol, tag, fields, program) ->
    let local_env = add_empty_env env in
    let field_approxs = List.map (describe local_env) fields in
    let descr = Export_info.Value_block (tag, Array.of_list field_approxs) in
    let id = new_descr local_env descr in
    let env = { env with sym = Symbol.Map.add symbol id env.sym } in
    describe_program env program
  | Effect (_expr, program) ->
    describe_program env program
  | End symbol ->
    symbol, env

let build_export_info ~(backend:(module Backend_intf.S))
      (program:Flambda.program) : Export_info.t =
  if !Clflags.opaque then
    Export_info.empty
  else
    (* CR pchambart: Should probably use that instead of the ident of
       the module as global identifier.
       mshinwell: Is "that" the variable "_global_symbol"? *)
    let _global_symbol, env = describe_program (create_empty_env ()) program in
    let globals =
      Ident.Map.singleton (Compilenv.current_unit_id ()) root_approx
    in
    let set_of_closures_map =
      let r = ref Set_of_closures_id.Map.empty in
      Flambda_iterators.iter_on_set_of_closures_of_program program
        ~f:(fun s ->
            r := Set_of_closures_id.Map.add
                s.function_decls.set_of_closures_id s !r);
      !r
    in
    let sets_of_closures =
      Set_of_closures_id.Map.map
        (fun { Flambda.function_decls } -> function_decls)
        set_of_closures_map
    in
    let closures =
      let aux_fun ffunctions off_id _ map =
        let fun_id = Closure_id.wrap off_id in
        Closure_id.Map.add fun_id ffunctions map in
      let aux _ ({ function_decls } : Flambda.set_of_closures) map =
        Variable.Map.fold (aux_fun function_decls) function_decls.funs map
      in
      Set_of_closures_id.Map.fold aux set_of_closures_map Closure_id.Map.empty
    in
    let invariant_arguments =
      Set_of_closures_id.Map.map
        (fun { Flambda.function_decls } ->
           Invariant_params.unchanging_params_in_recursion
             ~backend function_decls
        ) set_of_closures_map
    in
    let root_approx : Export_info.approx =
      Value_symbol (Compilenv.current_unit_symbol ())
    in
    Export_info.create
      ~values:(Export_info.nest_eid_map !(env.ex_table))
      ~globals
      ~symbol_id:env.sym
      ~offset_fun:Closure_id.Map.empty
      ~offset_fv:Var_within_closure.Map.empty
      ~sets_of_closures
      ~closures
      ~constant_sets_of_closures:Set_of_closures_id.Set.empty
      ~invariant_arguments:invariant_arguments
