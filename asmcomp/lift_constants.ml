
type constant =
  | Symbol of Symbol.t
  | Int of int
  | Const_pointer of int

type result = {
  expr : Flambda.t;
  tbl : Flambda.named Variable.Tbl.t;
  constant_tbl : constant Variable.Tbl.t;
  set_of_closures_map : Flambda.set_of_closures Variable.Map.t;
}

let extract_constant_declarations expr =
  let inconstant =
    Inconstant_idents.inconstants
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend *)
      ~compilation_unit:(Compilenv.current_unit ())
      expr
  in
  let fresh_symbol var =
    Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ()
  in
  let constant_tbl : constant Variable.Tbl.t = Variable.Tbl.create 10 in
  let tbl : Flambda.named Variable.Tbl.t = Variable.Tbl.create 10 in
  let set_of_closures_tbl : Flambda.set_of_closures Variable.Tbl.t = Variable.Tbl.create 10 in
  let constant_named var (named:Flambda.named) : Flambda.named =
    match named with
    | Const (Const_base (Const_int i)) ->
      Variable.Tbl.add constant_tbl var (Int i);
      named
    | Const (Const_base (Const_char c)) ->
      Variable.Tbl.add constant_tbl var (Int (Char.code c));
      named
    | Const (Const_pointer p) ->
      Variable.Tbl.add constant_tbl var (Const_pointer p);
      named
    | Const (Const_float_array _ | Const_immstring _ | Const_float _
            | Const_base
               (Const_string _ | Const_float _
               | Const_int32 _ | Const_int64 _ | Const_nativeint _)) ->
      let symbol = fresh_symbol var in
      Variable.Tbl.add tbl var named;
      Variable.Tbl.add constant_tbl var (Symbol symbol);
      Symbol symbol
    | Prim (Lambda.Pmakeblock _, _, _) ->
      let symbol = fresh_symbol var in
      Variable.Tbl.add tbl var named;
      Variable.Tbl.add constant_tbl var (Symbol symbol);
      Symbol symbol
    | Set_of_closures ( { function_decls = { set_of_closures_id } } as set )->
      assert(not (Set_of_closures_id.Set.mem set_of_closures_id inconstant.closure));
      (* Will probably never be used *)
      let symbol = fresh_symbol var in
      Variable.Tbl.add set_of_closures_tbl var set;
      Variable.Tbl.add constant_tbl var (Symbol symbol);
      Symbol symbol
    | Move_within_set_of_closures { move_to = closure_id }
    | Project_closure { closure_id } ->
      Symbol (Compilenv.closure_symbol closure_id)
    | Prim _ -> named
    | Symbol symbol ->
      Variable.Tbl.add constant_tbl var (Symbol symbol);
      named
    | Project_var _ ->
      named
    | Expr _ ->
      named
  in
  let to_symbol_if_constant var named =
    if Variable.Set.mem var inconstant.id then
      named
    else
      constant_named var named
  in
  let expr =
    Flambda_iterators.map (function
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
  expr,
  constant_tbl,
  tbl,
  set_of_closures_tbl

let rewrite_constant_access (expr, constant_tbl, tbl, set_of_closures_tbl) =
  let aliases =
    Alias_analysis.run expr
  in
  let find_symbol_alias var named : Flambda.named =
    match Variable.Map.find var aliases with
    | exception Not_found -> named
    | alias ->
      match Variable.Tbl.find constant_tbl alias with
      | exception Not_found -> named
      | Symbol symbol -> Symbol symbol
      | Const_pointer p -> Const (Const_pointer p)
      | Int i -> Const (Const_base (Const_int i))
  in
  let rewrite : Flambda.t -> Flambda.t = function
    | Let (kind, var, named, body) ->
      Let (kind, var, find_symbol_alias var named, body)
    | Let_rec (defs, body) ->
      let defs =
        List.map
          (fun (var, named) -> var, find_symbol_alias var named)
          defs
      in
      Flambda.Let_rec (defs, body)
    | expr -> expr
  in
  let expr = Flambda_iterators.map rewrite (fun x -> x) expr in
  let set_of_closures_map =
    Variable.Tbl.fold (fun var (set_of_closures:Flambda.set_of_closures) map ->
        let update_function_decl (function_declaration:Flambda.function_declaration) =
          { function_declaration with
            Flambda.body =
              Flambda_iterators.map rewrite (fun x -> x)
                function_declaration.body;
          }
        in
        let function_decls = {
          set_of_closures.function_decls with
          funs =
            Variable.Map.map update_function_decl
              set_of_closures.function_decls.funs
        }
        in
        let set_of_closures = { set_of_closures with function_decls } in
        Variable.Map.add var set_of_closures map
      )
      set_of_closures_tbl Variable.Map.empty
  in
  { expr;
    tbl;
    constant_tbl;
    set_of_closures_map }

let lift_constants expr =
  rewrite_constant_access
    (extract_constant_declarations expr)
