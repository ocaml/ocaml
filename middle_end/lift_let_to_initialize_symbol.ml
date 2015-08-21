
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
        Let (kind, v, named, body)
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
    | _ ->
      (* TODO !!! *)
      Format.printf "unhandled %a@." Flambda.print expr;
      assert false
  in
  Flambda_iterators.map_toplevel f (fun v -> v) expr

type ('a, 'b) kind =
  | Initialisation of 'a
  | Effect of 'b

let should_copy (named:Flambda.named) =
  match named with
  | Symbol _
  | Prim (Pfield _, _, _)
  | Const _ ->
    true
  | _ ->
    false

let rec split_let (expr:Flambda.t) =
  match expr with
  | Let(_, _, _, Var _) ->
    None
  | Let(Immutable, var, named, body) when should_copy named ->
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
    begin match split_let body with
    | None -> None
    | Some (Effect effect, expr) ->
      Some (Effect (copy_definition effect),
            Flambda.Let(Immutable, var, named, expr))
    | Some (Initialisation (sym, tag, fields), expr) ->
      let fields =
        List.map copy_definition fields
      in
      Some (Initialisation (sym, tag, fields),
            Flambda.Let(Immutable, var, named, expr))
    end
  | Let(Immutable, var, def, body) ->
    if Variable.Set.mem var (Flambda.free_variables body) then begin
      let symbol = make_variable_symbol var in
      let expr =
        let var' = Variable.freshen var in
        Flambda.Let(Immutable, var', def, Var var')
      in
      let body' = substitute_variable_to_symbol var symbol body in
      (* Format.printf "introduce sym %a var %a@.def@ %a@.subst@ %a@.substituted@ %a@." *)
      (*   Symbol.print symbol Variable.print var Flambda.print_named def *)
      (*   Flambda.print body Flambda.print body'; *)
      Some (Initialisation (symbol, Tag.create_exn 0, [expr]), body')
    end
    else begin
      let expr =
        let var' = Variable.freshen var in
        Flambda.Let(Immutable, var', def, Var var')
      in
      Some (Effect expr, body)
    end
  | _ -> None

let rec introduce_symbols expr =
  match split_let expr with
  | None -> [], expr
  | Some (extracted, body) ->
    let l, res = introduce_symbols body in
    extracted :: l, res

let add_extracted introduced program =
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
  f
