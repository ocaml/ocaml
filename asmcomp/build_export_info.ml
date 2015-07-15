module Int = Ext_types.Int
module ET = Flambdaexport_types
type env = ET.approx Variable.Map.t

let ex_table : ET.descr Export_id.Map.t ref = ref Export_id.Map.empty
let symbol_table : Export_id.t Symbol.Map.t ref = ref Symbol.Map.empty
let global_approx : ET.approx Int.Map.t ref = ref Int.Map.empty

let reset () =
  ex_table := Export_id.Map.empty;
  symbol_table := Symbol.Map.empty;
  global_approx := Int.Map.empty

let extern_id_descr ex =
  let export = Compilenv.approx_env () in
  try Some (Flambdaexport.find_description ex export)
  with Not_found -> None

let extern_symbol_descr sym =
  if Compilenv.is_predefined_exception sym
  then None
  else
    let export =
      Compilenv.approx_for_global (Symbol.compilation_unit sym)
    in
    try
      let id = Symbol.Map.find sym export.ex_symbol_id in
      let descr = Flambdaexport.find_description id export in
      Some descr
    with
    | Not_found -> None

let get_descr (approx : ET.approx) =
  match approx with
  | Value_unknown -> None
  | Value_id ex ->
    (try Some (Export_id.Map.find ex !ex_table) with
     | Not_found ->
       extern_id_descr ex)
  | Value_symbol sym ->
    try
      let ex = Symbol.Map.find sym !symbol_table in
      Some (Export_id.Map.find ex !ex_table)
    with Not_found ->
      extern_symbol_descr sym

let new_descr descr =
  let id = Export_id.create (Compilenv.current_unit ()) in
  ex_table := Export_id.Map.add id descr !ex_table;
  id

let find_approx env var : ET.approx =
  begin try Variable.Map.find var env with
  | Not_found -> Value_unknown
  end

let rec describe (env : env) (flam : Flambda.t) : ET.approx =
  match flam with
  | Var var ->
    find_approx env var

  | Let(kind, id, lam, body) ->
    let approx = match kind with
      | Immutable -> describe_named env lam
      | Mutable -> ET.Value_unknown
    in
    let env = Variable.Map.add id approx env in
    describe env body

  | Let_rec(defs, body) ->
    let env =
      List.fold_left (fun env (var, def) ->
          match def with
          | Flambda.Symbol symbol ->
            Variable.Map.add var (ET.Value_symbol symbol) env
          | _ ->
            env
        )
        env defs
    in
    let env =
      List.fold_left (fun env (var, def) ->
          match def with
          | Flambda.Symbol _ ->
            env
          | _ ->
            Variable.Map.add var (describe_named env def) env
        )
        env defs
    in
    describe env body

  | Apply { func; kind } ->
    begin match kind with
    | Indirect -> Value_unknown
    | Direct closure_id ->
      match get_descr (find_approx env func) with
      | Some(Value_closure { fun_id; set_of_closures = { results } }) ->
        assert(Closure_id.equal closure_id fun_id);
        Closure_id.Map.find fun_id results
      | _ -> Value_unknown
    end

  | Assign _ ->
    Value_id (new_descr (Value_constptr 0))

  | For _ ->
    Value_id (new_descr (Value_constptr 0))

  | While _ ->
    Value_id (new_descr (Value_constptr 0))

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

and describe_named (env : env) (named : Flambda.named) : ET.approx =
  match named with
  | Expr e ->
    describe env e

  | Symbol sym ->
    Value_symbol sym

  | Const (Const_base c) -> begin
      match c with
      | Const_int i ->
        Value_id (new_descr (Value_int i))
      | Const_char c ->
        Value_id (new_descr (Value_int (Char.code c)))
      | Const_float s ->
        Value_id (new_descr (Value_float (float_of_string s)))
      | Const_int32 i ->
        Value_id (new_descr (Value_boxed_int (Int32, i)))
      | Const_int64 i ->
        Value_id (new_descr (Value_boxed_int (Int64, i)))
      | Const_nativeint i ->
        Value_id (new_descr (Value_boxed_int (Nativeint, i)))
      | Const_string (s,_) ->
        let v_string : ET.value_string =
          { size = String.length s; contents = None }
        in
        Value_id (new_descr (Value_string v_string))
    end
  | Const (Const_float f) ->
    Value_id (new_descr (Value_float f))
  | Const (Const_pointer c) ->
    Value_id (new_descr (Value_constptr c))
  | Const (Const_float_array c) ->
    Value_id (new_descr (Value_float_array (List.length c)))
  | Const (Const_immstring c) ->
    let v_string : ET.value_string =
      { size = String.length c; contents = Some c }
    in
    Value_id (new_descr (Value_string v_string))

  | Prim(Pmakeblock(tag, Immutable), args, _dbg) ->
    let approxs = List.map (find_approx env) args in
    let descr = ET.Value_block (Tag.create_exn tag, Array.of_list approxs) in
    Value_id (new_descr descr)

  | Prim(Pfield i, [arg], _) -> begin
      match get_descr (find_approx env arg) with
      | Some (Value_block (_, fields)) when Array.length fields > i ->
        fields.(i)
      | _ ->
        Value_unknown
    end

  | Prim(Lambda.Pgetglobal id, _, _) ->
    Value_symbol (Compilenv.symbol_for_global' id)

  | Prim(Lambda.Pgetglobalfield(id,i), _, _) -> begin
      if id = Compilenv.current_unit_id () then
        match Int.Map.find i !global_approx with
        | exception Not_found ->
          Misc.fatal_error (Format.asprintf "no global %i" i)
        | approx -> approx
      else
        match extern_symbol_descr (Compilenv.symbol_for_global' id) with
        | None ->
          Value_unknown
        | Some (Value_block (_, fields)) ->
          if i >= Array.length fields then
            Misc.fatal_error (Format.asprintf "no field %i in global %a" i Ident.print id);
          fields.(i)
        | Some _ ->
          Misc.fatal_error (Format.asprintf "global %a is not a block" Ident.print id)
    end

  | Prim(Lambda.Psetglobalfield (_, i), [arg], _) ->
    global_approx := Int.Map.add i (find_approx env arg) !global_approx;
    Value_unknown

  | Prim(_, _, _) ->
    Value_unknown

  | Set_of_closures set ->
    Value_id (describe_set_of_closures env set)

  | Project_closure { set_of_closures; closure_id } -> begin
      match get_descr (find_approx env set_of_closures) with
      | Some(Value_set_of_closures set_of_closures) ->
        let descr = ET.Value_closure { fun_id = closure_id; set_of_closures } in
        Value_id (new_descr descr)
      | _ ->
        (* CR pchambart: This should be [assert false], but currently there are a
           few cases where this is less precise than inline_and_simplify. *)
        Value_unknown
    end

  | Move_within_set_of_closures { closure; start_from; move_to } -> begin
      match get_descr (find_approx env closure) with
      | Some(Value_closure { set_of_closures; fun_id }) ->
        assert(Closure_id.equal fun_id start_from);
        let descr = ET.Value_closure { fun_id = move_to; set_of_closures } in
        Value_id (new_descr descr)
      | _ -> Value_unknown
    end

  | Project_var { closure; closure_id; var } -> begin
      match get_descr (find_approx env closure) with
      | Some(Value_closure { set_of_closures = { bound_vars }; fun_id }) ->
        assert(Closure_id.equal fun_id closure_id);
        Var_within_closure.Map.find var bound_vars
      | _ -> Value_unknown
    end

and describe_set_of_closures env (set:Flambda.set_of_closures) : Export_id.t =
  let bound_vars_approx = Variable.Map.map (find_approx env) set.free_vars in
  let specialised_args_approx =
    Variable.Map.map (find_approx env) set.specialised_args
  in

  let closures_approx =
    (* To build an approximation of the results, we need an
       approximation of the functions. The first one we can build is
       one where every function return somthing unknown.

       CR pchambart: we could improve a bit on that by building a
       recursive approximation of the closures: The value_closure
       description contains a [value_set_of_closures]. We could replace
       this field by a [Expr_id.t] or an [approx]. *)
    let initial_value_set_of_closure =
      { ET.set_of_closures_id = set.function_decls.set_of_closures_id;
        bound_vars = Var_within_closure.wrap_map bound_vars_approx;
        results =
          Closure_id.wrap_map
            (Variable.Map.map (fun _ -> ET.Value_unknown)
               set.function_decls.funs);
      }
    in
    Variable.Map.mapi (fun var _ ->
        let descr =
          ET.Value_closure
            { fun_id = Closure_id.wrap var;
              set_of_closures = initial_value_set_of_closure }
        in
        ET.Value_id (new_descr descr))
      set.function_decls.funs
  in
  let closure_env =
    Variable.Map.fold Variable.Map.add closures_approx
      (Variable.Map.fold Variable.Map.add bound_vars_approx
         (Variable.Map.fold Variable.Map.add specialised_args_approx env))
  in
  let result_approx (function_declaration:Flambda.function_declaration) =
    describe closure_env function_declaration.body
  in
  let results =
    Variable.Map.map result_approx set.function_decls.funs
  in
  let descr =
    ET.Value_set_of_closures {
      set_of_closures_id = set.function_decls.set_of_closures_id;
      bound_vars = Var_within_closure.wrap_map bound_vars_approx;
      results = Closure_id.wrap_map results;
    }
  in
  new_descr descr

let build_export_info (lifted_constants:Lift_constants.result) : ET.exported =
  reset ();
  (* let constant_approx = *)
  (*   Variable.Map.map (fun set_of_closures -> *)
  (*       describe_set_of_closures Variable.Map.empty set_of_closures) *)
  (*     lifted_constants.map *)
  (* in *)

  (* let _constant_closure_approx = *)
  (*   Variable.Map.map (fun set_of_closures -> *)
  (*       describe_set_of_closures Variable.Map.empty set_of_closures) *)
  (*     lifted_constants.set_of_closures_map *)
  (* in *)

  let _root_description = describe Variable.Map.empty lifted_constants.expr in

  (* build the approximation of the root module *)
  let root_id =
    let size_global =
      1 + (Int.Map.fold (fun k _ acc -> max k acc) !global_approx (-1))
    in
    let fields =
      Array.init size_global (fun i ->
          try Int.Map.find i !global_approx with
          | Not_found -> ET.Value_unknown
        )
    in
    new_descr (Value_block (Tag.zero,fields))
  in

  let root_approx : ET.approx =
    Value_id root_id
  in

  (* build the symbol to id and id to symbol maps *)
  let module_symbol =
    Compilenv.current_unit_symbol ()
  in

  let ex_symbol_id =
    Symbol.Map.add module_symbol root_id !symbol_table
  in

  (* TODO *)
  let ex_id_symbol = Export_id.Map.empty in

  (* TODO *)
  let ex_functions = Set_of_closures_id.Map.empty in

  (* TODO *)
  let ex_functions_off = Closure_id.Map.empty in

  (* TODO *)
  let constant_closures = Set_of_closures_id.Set.empty in

  (* TODO *)
  let ex_kept_arguments = Set_of_closures_id.Map.empty in

  let export : ET.exported =
    { Flambdaexport.empty_export with
      ex_values = Flambdaexport.nest_eid_map !ex_table;
      ex_globals =
        Ident.Map.singleton
          (Compilenv.current_unit_id ()) root_approx;
      ex_symbol_id = ex_symbol_id;
      ex_id_symbol = Flambdaexport.nest_eid_map ex_id_symbol;
      ex_functions = ex_functions;
      ex_functions_off = ex_functions_off;
      ex_constant_closures = constant_closures;
      ex_kept_arguments = ex_kept_arguments }
  in
  export

