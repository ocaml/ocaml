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
  | Symbol _
  | Read_symbol_field _
  | Const _ ->
    true
  | _ ->
    false

type accumulated =
  { copied_lets : (Variable.t * Flambda.named) list;
    extracted_lets : (Variable.t * Flambda.t) list;
    terminator : Flambda.expr }

let rec accumulate ~substitution ~copied_lets ~extracted_lets (expr:Flambda.t) =
  match expr with
  | Let { var; body = Var var'; _ } when Variable.equal var var' ->
      { copied_lets; extracted_lets;
        terminator = Flambda_utils.toplevel_substitution substitution expr }
  | Let { var; defining_expr = Expr (Var alias); body; _ } ->
      let alias =
        match Variable.Map.find alias substitution with
        | exception Not_found -> alias
        | original_alias -> original_alias
      in
      accumulate
        ~substitution:(Variable.Map.add var alias substitution)
        ~copied_lets
        ~extracted_lets
        body
  | Let { var; defining_expr = named; body; _ } ->
      if should_copy named then
        accumulate
          ~substitution
          ~copied_lets:((var, named)::copied_lets)
          ~extracted_lets
          body
      else
        let extracted =
          let renamed = Variable.rename var in
          Flambda_utils.toplevel_substitution substitution
            (Flambda.create_let renamed named (Var renamed))
        in
        accumulate
          ~substitution
          ~copied_lets
          ~extracted_lets:((var, extracted)::extracted_lets)
          body
  | _ ->
      { copied_lets; extracted_lets;
        terminator = Flambda_utils.toplevel_substitution substitution expr }

let rebuild_expr
    ~(extracted_definitions:Symbol.t Variable.Map.t)
    ~(copied_definitions:Flambda.named Variable.Map.t)
    ~(substitute:bool)
    (expr:Flambda.t) =
  let expr_with_read_symbols =
    Flambda_utils.substitute_read_symbol_field_for_variables
      extracted_definitions expr
  in
  let free_variables =
    Flambda.free_variables expr_with_read_symbols
  in
  let substitution =
    if substitute then
      Variable.Map.of_set (fun x -> Variable.rename x) free_variables
    else
      Variable.Map.of_set (fun x -> x) free_variables
  in
  let expr_with_read_symbols =
    Flambda_utils.toplevel_substitution substitution
      expr_with_read_symbols
  in
  Variable.Map.fold (fun var declaration body ->
      let definition = Variable.Map.find var copied_definitions in
      Flambda.create_let declaration definition body)
    substitution expr_with_read_symbols

let rebuild (used_variables:Variable.Set.t) (accumulated:accumulated) =
  let copied_definitions = Variable.Map.of_list accumulated.copied_lets in
  let extracted_definitions =
    Variable.Map.mapi
      (fun var _ -> Flambda_utils.make_variable_symbol var)
      (Variable.Map.of_list accumulated.extracted_lets)
  in
  let extracted =
    List.map (fun (var, decl) ->
        let expr =
          rebuild_expr ~extracted_definitions ~copied_definitions
            ~substitute:true decl
        in
        if Variable.Set.mem var used_variables then
          Initialisation
            (Variable.Map.find var extracted_definitions,
             Tag.create_exn 0,
             [expr])
        else
          Effect expr)
      accumulated.extracted_lets
  in
  let terminator =
    (* We don't need to substitute the variables in the terminator, we
       suppose that we did for every other occurence. Avoiding this
       substitution allows this transformation to be idempotent. *)
    rebuild_expr ~extracted_definitions ~copied_definitions
      ~substitute:false accumulated.terminator
  in
  List.rev extracted,
  terminator

let introduce_symbols expr =
  let accumulated =
    accumulate
      ~substitution:Variable.Map.empty
      ~copied_lets:[] ~extracted_lets:[] expr
  in
  let used_variables = Flambda.used_variables expr in
  let extracted, terminator = rebuild used_variables accumulated in
  extracted, terminator

let add_extracted introduced program =
  List.fold_right (fun extracted program ->
      match extracted with
      | Initialisation (symbol, tag, def) ->
          Flambda.Initialize_symbol (symbol, tag, def, program)
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
    add_extracted introduced
      (Flambda.Effect (expr, program))
  | Initialize_symbol(symbol, tag, ((_::_::_) as fields), program) ->
    (* CR pchambart: currently the only initialize_symbol with more
       than 1 field is the module block. This could evold, in that case
       this pattern should be handled properly. *)
    Initialize_symbol(symbol, tag, fields, split_program program)
  | Initialize_symbol(sym, tag, [], program) ->
    Let_symbol(sym, Block (tag, []), split_program program)
  | Initialize_symbol(symbol, tag, [field], program) ->
    let program = split_program program in
    let introduced, field = introduce_symbols field in
    add_extracted introduced
      (Flambda.Initialize_symbol(symbol, tag, [field], program))

let lift ~backend:_ (f : Flambda.program) =
  split_program f
