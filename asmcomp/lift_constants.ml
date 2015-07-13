
type block = int * Variable.t list

type result = {
  expr : Flambda.t;
  block_tbl : block Variable.Tbl.t;
  set_of_closures_tbl : Flambda.set_of_closures Variable.Tbl.t;
}

let lift_constants expr =
  let inconstant =
    Inconstant_idents.inconstants
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend *)
      ~compilation_unit:(Compilenv.current_unit ())
      expr
  in
  let aliases =
    Alias_analysis.run expr
  in
  let fresh_symbol (_:Variable.t) = assert false in
  let symbol_tbl : Symbol.t Variable.Tbl.t = Variable.Tbl.create 10 in
  let block_tbl : block Variable.Tbl.t = Variable.Tbl.create 10 in
  let set_of_closures_tbl : Flambda.set_of_closures Variable.Tbl.t = Variable.Tbl.create 10 in
  let constant_named var (named:Flambda.named) : Flambda.named =
    match named with
    | Prim (Lambda.Pmakeblock (tag, _), args, _) ->
      let symbol = fresh_symbol var in
      Variable.Tbl.add block_tbl var (tag, args);
      Variable.Tbl.add symbol_tbl var symbol;
      Symbol symbol
    | Set_of_closures ( { function_decls = { set_of_closures_id } } as set )->
      assert(not (Set_of_closures_id.Set.mem set_of_closures_id inconstant.closure));
      (* Will probably never be used *)
      let symbol = fresh_symbol var in
      Variable.Tbl.add set_of_closures_tbl var set;
      Variable.Tbl.add symbol_tbl var symbol;
      Symbol symbol
    | Move_within_set_of_closures { move_to = closure_id }
    | Project_closure { closure_id } ->
      Symbol (Compilenv.closure_symbol closure_id)
    | _ -> named
  in
  let to_symbol_if_constant var named =
    if Variable.Set.mem var inconstant.id then
      named
    else
      constant_named var named
  in
  let expr =
    Flambdaiter.map (function
        | Let (kind, var, named, body) ->
          Let (kind, var, to_symbol_if_constant var named, body)
        | Let_rec (defs, body) ->
          let defs =
            List.map
              (fun (var, named) -> var, to_symbol_if_constant var named)
              defs
          in
          Let_rec (defs, body)
        | expr -> expr)
      (fun x -> x)
      expr
  in
  let find_symbol_alias var named : Flambda.named =
    match Variable.Map.find var aliases with
    | exception Not_found -> named
    | alias ->
      match Variable.Tbl.find symbol_tbl alias with
      | exception Not_found -> named
      | symbol -> Symbol symbol
  in
  let expr =
    Flambdaiter.map (function
        | Let (kind, var, named, body) ->
          Let (kind, var, find_symbol_alias var named, body)
        | Let_rec (defs, body) ->
          let defs =
            List.map
              (fun (var, named) -> var, find_symbol_alias var named)
              defs
          in
          Let_rec (defs, body)
        | expr -> expr)
      (fun x -> x)
      expr
  in
  { expr;
    block_tbl;
    set_of_closures_tbl }
