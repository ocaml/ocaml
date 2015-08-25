
(* type backend = (module Backend_intf.S) *)

(* To move elswhere and share *)
let make_variable_symbol var =
  Symbol.create (Compilation_unit.get_current_exn ())
    (Linkage_name.create
       ("lifted_" ^ Variable.unique_name (Variable.freshen var)))

let substitute_variable_to_symbol var symbol expr =
  let bind fresh_var (expr:Flambda.t) : Flambda.t =
    let intermediate_var = Variable.freshen fresh_var in
    Let(Immutable, intermediate_var, Symbol symbol,
        Let(Immutable, fresh_var,
            Prim (Pfield 0, [intermediate_var], Debuginfo.none),
            expr))
  in
  let substitute_named fresh (named:Flambda.named) : Flambda.named =
    let sb to_substitute =
      if Variable.equal to_substitute var then
        fresh
      else
        to_substitute
    in
    match named with
    | Symbol _ | Const _ | Expr _ -> named
    | Allocated_const _ | Predefined_exn _ -> named
    | Set_of_closures set_of_closures ->
      let set_of_closures =
        Flambda.create_set_of_closures
          ~function_decls:set_of_closures.function_decls
          ~free_vars:(Variable.Map.map sb set_of_closures.free_vars)
          ~specialised_args:
            (Variable.Map.map sb set_of_closures.specialised_args)
      in
      Set_of_closures set_of_closures
    | Project_closure project_closure ->
      Project_closure {
        project_closure with
        set_of_closures = sb project_closure.set_of_closures;
      }
    | Move_within_set_of_closures move_within_set_of_closures ->
      Move_within_set_of_closures {
        move_within_set_of_closures with
        closure = sb move_within_set_of_closures.closure;
      }
    | Project_var project_var ->
      Project_var {
        project_var with
        closure = sb project_var.closure;
      }
    | Prim (prim, args, dbg) ->
      Prim (prim, List.map sb args, dbg)
  in
  let f (expr:Flambda.t) : Flambda.t =
    match expr with
    | Var v when Variable.equal v var ->
      let fresh = Variable.freshen var in
      bind fresh (Var fresh)
    | Var _ -> expr
    | Let (kind, v, named, body) ->
      if Variable.Set.mem var (Flambda.free_variables_named named) then
        let fresh = Variable.freshen var in
        let named = substitute_named fresh named in
        bind fresh (Let (kind, v, named, body))
      else
        expr
    | Let_rec (defs, body) ->
      let need_substitution =
        List.exists (fun (_, named) -> Variable.Set.mem var (Flambda.free_variables_named named))
          defs
      in
      if need_substitution then
        let fresh = Variable.freshen var in
        let defs =
          List.map (fun (v, named) -> v, substitute_named fresh named) defs
        in
        bind fresh (Let_rec (defs, body))
      else
        expr
    | If_then_else (cond, ifso, ifnot) when Variable.equal cond var ->
      let fresh = Variable.freshen var in
      bind fresh (If_then_else (fresh, ifso, ifnot))
    | If_then_else _ ->
      expr
    | Switch (cond, sw) when Variable.equal cond var ->
      let fresh = Variable.freshen var in
      bind fresh (Switch (fresh, sw))
    | Switch _ ->
      expr
    | String_switch (cond, sw, def) when Variable.equal cond var ->
      let fresh = Variable.freshen var in
      bind fresh (String_switch (fresh, sw, def))
    | String_switch _ ->
      expr
    | Assign { being_assigned; new_value } when Variable.equal new_value var ->
      let fresh = Variable.freshen var in
      bind fresh (Assign { being_assigned; new_value = fresh })
    | Assign _ ->
      expr
    | Static_raise (_exn, (_arg:Flambda.t list)) ->
      (* If the type change to variable, this needs to be
         updated with substitution *)
      expr
    | For { bound_var; from_value; to_value; direction; body }
      when Variable.equal var from_value || Variable.equal var to_value ->
      let fresh = Variable.freshen var in
      let from_value =
        if Variable.equal var from_value then fresh else from_value
      in
      let to_value =
        if Variable.equal var to_value then fresh else to_value
      in
      bind fresh (For { bound_var; from_value; to_value; direction; body })
    | For _ ->
      expr
    | Apply { func; args; kind; dbg }
      when Variable.equal var func
           || List.exists (Variable.equal var) args ->
      let fresh = Variable.freshen var in
      let func =
        if Variable.equal var func then fresh else func
      in
      let args =
        List.map (fun arg -> if Variable.equal var arg then fresh else arg) args
      in
      bind fresh (Apply { func; args; kind; dbg })
    | Apply _ ->
      expr
    | Send { kind; meth; obj; args; dbg }
      when Variable.equal var meth
           || Variable.equal var obj
           || List.exists (Variable.equal var) args ->
      let fresh = Variable.freshen var in
      let meth =
        if Variable.equal var meth then fresh else meth
      in
      let obj =
        if Variable.equal var obj then fresh else obj
      in
      let args =
        List.map (fun arg -> if Variable.equal var arg then fresh else arg) args
      in
      bind fresh (Send { kind; meth; obj; args; dbg })
    | Send _ ->
      expr
    | Proved_unreachable
    | While _
    | Try_with _
    | Static_catch _ ->
      (* No variables directly used in those expressions *)
      expr
  in
  Flambda_iterators.map_toplevel f (fun v -> v) expr

type ('a, 'b) kind =
  | Initialisation of 'a
  | Effect of 'b

let should_copy (named:Flambda.named) =
  match named with
  | Expr (Var _)
  | Symbol _
  | Prim (Pfield _, _, _)
  | Const _ ->
    true
  | _ ->
    false

let rec split_let free_variables_map (expr:Flambda.t) =
  match expr with
  | Let(_, _, _, Var _) ->
    None
  | Let(Immutable, var, named, body) when should_copy named ->
    (* Format.printf "look variable %a@." *)
    (*   Variable.print var; *)
    (* Those are the cases that are better to duplicate than to lift.
       Symbol and Pfield must be duplicated to avoid relifting the
       code introduced to lift a variable (and ending up looping infinitely). *)
    let copy_definition expr =
      let fresh = Variable.freshen var in
      let expr =
        Flambda_utils.toplevel_substitution
          (Variable.Map.singleton var fresh)
          expr
      in
      Flambda.Let(Immutable, fresh, named, expr)
    in
    let reintroduce_lets ~def_free_vars ~body_free_vars ~def ~body =
        let named_free_vars = Flambda.free_variables_named named in
        match Variable.Set.mem var def_free_vars, Variable.Set.mem var body_free_vars with
        | false, false ->
          (* Unused everywhere: drop everywhere *)
          def_free_vars, body_free_vars, def, body
        | true, false ->
          (* Unused only once: no substitution needed *)
          let def_free_vars = Variable.Set.union def_free_vars named_free_vars in
          def_free_vars, body_free_vars,
          Flambda.Let(Immutable, var, named, def),
          body
        | false, true ->
          (* Unused only once: no substitution needed *)
          let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
          def_free_vars, body_free_vars,
          def,
          Flambda.Let(Immutable, var, named, body)
        | true, true ->
          (* Used, in both: substitute in one.
             We assume the new definition is smaller, hence substitute in there *)
          let def = copy_definition def in
          let def_free_vars = Variable.Set.union def_free_vars named_free_vars in
          let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
          def_free_vars, body_free_vars,
          def,
          Flambda.Let(Immutable, var, named, body)
    in
    begin match split_let free_variables_map body with
    | None -> None
    | Some (def_free_vars, body_free_vars, Effect effect, expr) ->
      let (def_free_vars, body_free_vars, effect, expr) =
        reintroduce_lets ~def_free_vars ~body_free_vars ~def:effect ~body:expr
      in
      Some (def_free_vars, body_free_vars, Effect effect, expr)
    | Some (def_free_vars, body_free_vars, Initialisation (sym, tag, [field]), expr) ->
      let (def_free_vars, body_free_vars, field, expr) =
        reintroduce_lets ~def_free_vars ~body_free_vars ~def:field ~body:expr
      in
      Some (def_free_vars, body_free_vars, Initialisation (sym, tag, [field]), expr)
    | Some (def_free_vars, body_free_vars, Initialisation (sym, tag, fields), expr) ->
      begin match Variable.Set.mem var def_free_vars, Variable.Set.mem var body_free_vars with
      | false, false ->
        Some (def_free_vars, body_free_vars, Initialisation (sym, tag, fields), expr)
      | false, true ->
        let named_free_vars = Flambda.free_variables_named named in
        let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
        Some (def_free_vars, body_free_vars,
              Initialisation (sym, tag, fields),
              Flambda.Let(Immutable, var, named, expr))
      | true, (true | false) ->
        let named_free_vars = Flambda.free_variables_named named in
        let def_free_vars = Variable.Set.union def_free_vars named_free_vars in
        let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
        let fields =
          List.map copy_definition fields
        in
        Some (def_free_vars, body_free_vars,
              Initialisation (sym, tag, fields),
              Flambda.Let(Immutable, var, named, expr))
      end
    end
  | Let(Immutable, var, def, body) ->
    (* Format.printf "not copy variable %a@." *)
    (*   Variable.print var; *)
    (* It is ok to precompute the free variables even if we modify the expression:
       the fact that those variables are free or not is not affected *)
    let body_free_variables = Variable.Map.find var free_variables_map in
    (* let body_free_variables = Flambda.free_variables body in *)
    if Variable.Set.mem var body_free_variables then begin
      let symbol = make_variable_symbol var in
      let expr =
        let var' = Variable.freshen var in
        Flambda.Let(Immutable, var', def, Var var')
      in
      let body' = substitute_variable_to_symbol var symbol body in
      (* Format.printf "@.introduce sym %a var %a@.def@ %a@.subst@ %a@.substituted@ %a@.@." *)
      (*   Symbol.print symbol Variable.print var Flambda.print_named def *)
      (*   Flambda.print body Flambda.print body'; *)
      let body' = Lift_code.lift_lets_expr body' in
      let def_free_vars = Flambda.free_variables expr in
      Some (def_free_vars, body_free_variables,
            Initialisation (symbol, Tag.create_exn 0, [expr]), body')
    end
    else begin
      let expr =
        let var' = Variable.freshen var in
        Flambda.Let(Immutable, var', def, Var var')
      in
      let def_free_vars = Flambda.free_variables expr in
      Some (def_free_vars, body_free_variables, Effect expr, body)
    end
  | _ -> None

let introduce_symbols expr =
  let free_variables_map = Flambda.free_variables_by_let expr in
  let rec loop expr =
    match split_let free_variables_map expr with
    | None -> [], expr
    | Some (_, _, extracted, body) -> begin match extracted with
        | Initialisation (_symbol, _tag, _def) ->
          Format.printf "extracted initialize %a:@.@."
            Symbol.print _symbol;
          (* Format.printf "extracted initialize %a:@ %a@." *)
          (*   Symbol.print _symbol *)
          (*   (Format.pp_print_list Flambda.print) _def; *)
          (* Flambda.Initialize_symbol *)
          (*   (symbol, tag, def, *)
          (*    program) *)
        | Effect _effect ->
          Format.printf "extracted effect@.@.";
          (* Flambda.Effect (effect, program)) *)
      end;
      let l, res = loop body in
      extracted :: l, res
  in
  loop expr

let add_extracted introduced program =
  Format.printf "add extracted@.";
  List.fold_right (fun extracted program ->
      match extracted with
      | Initialisation (symbol, tag, def) ->
        Flambda.Initialize_symbol
          (symbol, tag, def,
           program)
      | Effect effect ->
        Flambda.Effect (effect, program))
    introduced program

let rec split_program (program:Flambda.program) : Flambda.program =
  match program with
  | End s -> End s
  | Import_symbol(s, program) ->
    Import_symbol(s, split_program program)
  | Let_symbol (s, def, program) ->
    Let_symbol (s, def, split_program program)
  | Let_rec_symbol (defs, program) ->
    Let_rec_symbol (defs, split_program program)
  | Effect (expr, program) ->
    let program = split_program program in
    let introduced, expr = introduce_symbols expr in
    add_extracted introduced (Flambda.Effect (expr, program))

  | Initialize_symbol(symbol, tag, ((_::_::_) as fields), program) ->
    (* TMP: removed to lighten debug *)
    Initialize_symbol(symbol, tag, fields, split_program program)

  (* | Initialize_symbol(sym, _, [], _) -> *)
  (*   Misc.fatal_errorf "initialize an empty symbol %a" Symbol.print sym *)

  | Initialize_symbol(sym, tag, [], program) ->
    Initialize_symbol(sym, tag, [], split_program program)

  | Initialize_symbol(symbol, tag, [field], program) ->
    let program = split_program program in
    let introduced, field = introduce_symbols field in
    add_extracted introduced
      (Flambda.Initialize_symbol(symbol, tag, [field], program))


let lift ~backend:_ (f:Flambda.program) =
  (* Format.printf "@.before lift@ %a@." Flambda.print_program f; *)
  let f = split_program f in
  (* Format.printf "@.after lift@ %a@." Flambda.print_program f; *)
  Format.printf "@.after lift_let_to_initialize_symbol@.@.";
  f
