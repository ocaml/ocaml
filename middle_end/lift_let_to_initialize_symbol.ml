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
  | Read_symbol_field _
  | Const _ ->
    true
  | _ ->
    false

type split_let_result =
  (Variable.Set.t *
   Variable.Set.t *
   (Variable.t * Symbol.t * Tag.t * Flambda.t list, Flambda.t) kind *
   Flambda.t)
    option

let rec split_let_k : type t. Flambda.t -> (split_let_result -> t) -> t =
  fun (expr:Flambda.t) (k:split_let_result -> t) (* continuation *) ->
  match expr with
  | Let { body = Var _; _ } -> k None
  | Let { var; defining_expr = named; body;
          free_vars_of_defining_expr = named_free_vars; _ }
      when should_copy named ->
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
    (split_let_k body @@ fun res ->
     let res =
       match res with
       | None -> None
       | Some (def_free_vars, body_free_vars, Effect effect, expr) ->
         let (def_free_vars, body_free_vars, effect, expr) =
           reintroduce_lets ~def_free_vars ~body_free_vars ~def:effect ~body:expr
         in
         Some (def_free_vars, body_free_vars, Effect effect, expr)
       | Some (def_free_vars, body_free_vars, Initialisation (init_var, sym, tag, [field]), expr) ->
         let (def_free_vars, body_free_vars, field, expr) =
           reintroduce_lets ~def_free_vars ~body_free_vars ~def:field ~body:expr
         in
         Some (def_free_vars, body_free_vars, Initialisation (init_var, sym, tag, [field]), expr)
       | Some (def_free_vars, body_free_vars, Initialisation (init_var, sym, tag, fields), expr) ->
         begin match Variable.Set.mem var def_free_vars, Variable.Set.mem var body_free_vars with
         | false, false ->
           Some (def_free_vars, body_free_vars, Initialisation (init_var, sym, tag, fields), expr)
         | false, true ->
           let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
           Some (def_free_vars, body_free_vars,
                 Initialisation (init_var, sym, tag, fields),
                 Flambda.create_let var named expr)
         | true, (true | false) ->
           let def_free_vars = Variable.Set.union def_free_vars named_free_vars in
           let body_free_vars = Variable.Set.union body_free_vars named_free_vars in
           let fields =
             List.map copy_definition fields
           in
           Some (def_free_vars, body_free_vars,
                 Initialisation (init_var, sym, tag, fields),
                 Flambda.create_let var named expr)
         end
     in
     k res)
  | Let { var; defining_expr = def; body; free_vars_of_body; } ->
    (* This [Let] is to be lifted (to either [Initialize_symbol] or
       [Effect]). *)
    let res =
      if Variable.Set.mem var free_vars_of_body then begin
        let expr =
          let var' = Variable.freshen var in
          Flambda.create_let var' def (Var var')
        in
        let symbol = Flambda_utils.make_variable_symbol var in
        let def_free_vars = Flambda.free_variables expr in
        Some (def_free_vars, free_vars_of_body,
              Initialisation (var, symbol, Tag.create_exn 0, [expr]), body)
      end
      else begin
        let expr =
          let var' = Variable.freshen var in
          Flambda.create_let var' def (Var var')
        in
        let def_free_vars = Flambda.free_variables expr in
        Some (def_free_vars, free_vars_of_body, Effect expr, body)
      end
    in
    k res
  | _ -> k None

and split_let expr =
  split_let_k expr (fun x -> x)

let introduce_symbols expr =
  let rec loop expr ~all_extracted_rev =
    match split_let expr with
    | None -> all_extracted_rev, expr
    | Some (_, _, extracted, body) ->
      loop body ~all_extracted_rev:(extracted :: all_extracted_rev)
  in
  let all_extracted_rev, expr = loop expr ~all_extracted_rev:[] in
  let to_substitute =
    List.fold_left (fun to_substitute extracted ->
        match extracted with
        | Initialisation (init_var, symbol, _tag, _def) ->
          Variable.Map.add init_var symbol to_substitute
        | Effect _effect ->
          to_substitute)
      Variable.Map.empty all_extracted_rev
  in
  let expr =
    Flambda_utils.substitute_variable_to_symbol to_substitute expr
  in
  (List.rev all_extracted_rev), to_substitute, expr

let add_extracted introduced to_substitute program =
  List.fold_right (fun extracted program ->
      match extracted with
      | Initialisation (_init_var, symbol, tag, def) ->
          let def =
            List.map
              (Flambda_utils.substitute_variable_to_symbol to_substitute)
              def
          in
        Flambda.Initialize_symbol
          (symbol, tag, def,
           program)
      | Effect effect ->
        let effect =
          Flambda_utils.substitute_variable_to_symbol to_substitute effect
        in
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
    let introduced, to_substitute, expr = introduce_symbols expr in
    add_extracted introduced to_substitute
      (Flambda.Effect (expr, program))

  | Initialize_symbol(symbol, tag, ((_::_::_) as fields), program) ->
    (* TMP: removed to lighten debug *)
    Initialize_symbol(symbol, tag, fields, split_program program)

  (* | Initialize_symbol(sym, _, [], _) -> *)
  (*   Misc.fatal_errorf "initialize an empty symbol %a" Symbol.print sym *)

  | Initialize_symbol(sym, tag, [], program) ->
    Initialize_symbol(sym, tag, [], split_program program)

  | Initialize_symbol(symbol, tag, [field], program) ->
    let program = split_program program in
    let introduced, to_substitute, field = introduce_symbols field in
    add_extracted introduced to_substitute
      (Flambda.Initialize_symbol(symbol, tag, [field], program))

let lift ~backend:_ (f:Flambda.program) =
  (* Format.printf "@.before lift@ %a@." Flambda.print_program f; *)
  let f = split_program f in
  (* Format.printf "@.after lift@ %a@." Flambda.print_program f; *)
(*  Format.printf "@.after lift_let_to_initialize_symbol@.@.";*)
  f
