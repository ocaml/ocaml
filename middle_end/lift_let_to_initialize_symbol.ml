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

type ('a, 'b) kind =
  | Initialisation of 'a
  | Effect of 'b

let should_copy (named:Flambda.named) =
  match named with
  | Expr (Var _)
  | Symbol _
  | Prim (Pfield _, _, _)
  | Read_symbol_field _
  | Const _ ->
    true
  | _ ->
    false

let rec split_let (expr:Flambda.t) =
  match expr with
  | Let { body = Var _; _ } -> None
  | Let { var; defining_expr = named; body; _ } when should_copy named ->
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
      Flambda.create_let fresh named expr
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
          (Flambda.create_let var named def),
          body
        | false, true ->
          (* Unused only once: no substitution needed *)
          let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
          def_free_vars, body_free_vars,
          def,
          Flambda.create_let var named body
        | true, true ->
          (* Used, in both: substitute in one.
             We assume the new definition is smaller, hence substitute in there *)
          let def = copy_definition def in
          let def_free_vars = Variable.Set.union def_free_vars named_free_vars in
          let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
          def_free_vars, body_free_vars,
          def,
          Flambda.create_let var named body
    in
    begin match split_let body with
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
              Flambda.create_let var named expr)
      | true, (true | false) ->
        let named_free_vars = Flambda.free_variables_named named in
        let def_free_vars = Variable.Set.union def_free_vars named_free_vars in
        let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
        let fields =
          List.map copy_definition fields
        in
        Some (def_free_vars, body_free_vars,
              Initialisation (sym, tag, fields),
              Flambda.create_let var named expr)
      end
    end
  | Let { var; defining_expr = def; body; free_vars_of_body; } ->
    (* Format.printf "not copy variable %a@." *)
    (*   Variable.print var; *)
    if Variable.Set.mem var free_vars_of_body then begin
      let symbol = Flambda_utils.make_variable_symbol var in
      let expr =
        let var' = Variable.freshen var in
        Flambda.create_let var' def (Var var')
      in
      let body' =
        Flambda_utils.substitute_variable_to_symbol var symbol body
      in
      (* Format.printf "@.introduce sym %a var %a@.def@ %a@.subst@ %a@.substituted@ %a@.@." *)
      (*   Symbol.print symbol Variable.print var Flambda.print_named def *)
      (*   Flambda.print body Flambda.print body'; *)
      let body' = Lift_code.lift_lets_expr body' in
      let def_free_vars = Flambda.free_variables expr in
      Some (def_free_vars, free_vars_of_body,
            Initialisation (symbol, Tag.create_exn 0, [expr]), body')
    end
    else begin
      let expr =
        let var' = Variable.freshen var in
        Flambda.create_let var' def (Var var')
      in
      let def_free_vars = Flambda.free_variables expr in
      Some (def_free_vars, free_vars_of_body, Effect expr, body)
    end
  | _ -> None

let introduce_symbols expr =
  let rec loop expr ~all_extracted_rev =
    match split_let expr with
    | None -> all_extracted_rev, expr
    | Some (_, _, extracted, body) -> (*begin match extracted with
        | Initialisation (_symbol, _tag, _def) ->
(*
          Format.printf "extracted initialize %a:@.@."
            Symbol.print _symbol;
*)
          (* Format.printf "extracted initialize %a:@ %a@." *)
          (*   Symbol.print _symbol *)
          (*   (Format.pp_print_list Flambda.print) _def; *)
          (* Flambda.Initialize_symbol *)
          (*   (symbol, tag, def, *)
          (*    program) *)
        | Effect _effect ->
(*
          Format.printf "extracted effect@.@.";
*)
          (* Flambda.Effect (effect, program)) *)
      end;*)
      loop body ~all_extracted_rev:(extracted :: all_extracted_rev)
  in
  let all_extracted_rev, expr = loop expr ~all_extracted_rev:[] in
  (List.rev all_extracted_rev), expr

let add_extracted introduced program =
(*  Format.printf "add extracted@.";*)
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
(*  Format.printf "@.after lift_let_to_initialize_symbol@.@.";*)
  f
