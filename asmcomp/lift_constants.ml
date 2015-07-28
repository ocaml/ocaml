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

(* Extract constants out of the main expression.
   * First collect all the constant declarations, separating structured
     constants and constant sets of closures: [collect_constant_declarations]
   * Then share equal constants: [constant_sharing].
     This is done by traversing the structured constants in inverse
     topological order and replacing every existing constant by the previous
     one. (Note: this does not guaranty maximal sharing in cycles)
   * Replace every defining expression of a constant variable with the
     corresponding symbol/integer/etc.
*)

(* Values of constants at definition point *)

module Constant_descr = struct
  type t = constant_descr

  let compare_floats x1 x2 =
    Int64.compare (Int64.bits_of_float x1) (Int64.bits_of_float x2)

  (* let rec compare_float_lists l1 l2 = *)
  (*   match l1, l2 with *)
  (*   | [], [] -> 0 *)
  (*   | [], _::_ -> -1 *)
  (*   | _::_, [] -> 1 *)
  (*   | h1::t1, h2::t2 -> *)
  (*     let c = compare_floats h1 h2 in *)
  (*     if c <> 0 then c else compare_float_lists t1 t2 *)

  let compare (x:t) (y:t) = match x, y with
    | Int x, Int y -> compare x y
    | Const_pointer x, Const_pointer y -> compare x y
    | Float x, Float y -> compare_floats x y
    | Int32 x, Int32 y -> compare x y
    | Int64 x, Int64 y -> compare x y
    | Nativeint x, Nativeint y -> compare x y
    | Float_array _, Float_array _ ->
      assert false
    (* should not be added to the map *)
    (* | Float_array x, Float_array y -> *)
    (*   compare_float_lists x y *)
    | String _, String _ ->
      assert false
      (* should not be added to the map *)
    (* | String x, String y -> *)
    (*   compare x y *)
    | Immstring x, Immstring y -> compare x y
    | Symbol x, Symbol y -> compare x y
    | Block (tag1, fields1), Block (tag2, fields2) ->
      let c = Tag.compare tag1 tag2 in
      if c <> 0 then
        c
      else
        Variable.compare_lists fields1 fields2
    | Int _, _ -> -1
    | _, Int _ -> 1
    | Const_pointer _, _ -> -1
    | _, Const_pointer _ -> 1
    | Float _, _ -> -1
    | _, Float _ -> 1
    | Int32 _, _ -> -1
    | _, Int32 _ -> 1
    | Int64 _, _ -> -1
    | _, Int64 _ -> 1
    | Nativeint _, _ -> -1
    | _, Nativeint _ -> 1
    | Float_array _, _ -> -1
    | _, Float_array _ -> 1
    | String _, _ -> -1
    | _, String _ -> 1
    | Immstring _, _ -> -1
    | _, Immstring _ -> 1
    | Symbol _, _ -> -1
    | _, Symbol _ -> 1
end

module Constant_descr_map = Map.Make(Constant_descr)

type result = {
  expr : Flambda.t;
  constant_descr : Symbol.t Flambda.allocated_constant Symbol.Map.t;
  kind : Flambda.named Variable.Map.t;
  set_of_closures_map : Flambda.set_of_closures Symbol.Map.t;
}

let fresh_symbol var =
  Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ()

let collect_constant_declarations expr =
  let inconstant =
    Inconstant_idents.inconstants
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend
         mshinwell: what is "this"? *)
      ~compilation_unit:(Compilenv.current_unit ())
      expr
  in
  let constant_tbl : constant_descr Variable.Tbl.t = Variable.Tbl.create 10 in
  let set_of_closures_tbl : Flambda.set_of_closures Symbol.Tbl.t =
    Symbol.Tbl.create 10 in
  let constant_named var (named:Flambda.named) : unit =
    let add (descr:constant_descr) = Variable.Tbl.add constant_tbl var descr in
    match named with
    | Const (Const_base (Const_int i)) ->
      add (Int i)
    | Const (Const_base (Const_char c)) ->
      add (Int (Char.code c))
    | Const (Const_pointer p) ->
      add (Const_pointer p)
    | Const (Const_float_array array) ->
      add (Float_array (List.map float_of_string array))
    | Const (Const_immstring s) ->
      add (Immstring s)
    | Const (Const_float f) ->
      add (Float f)
    | Const (Const_base (Const_string (s,_))) ->
      add (String s)
    | Const (Const_base (Const_float s)) ->
      add (Float (float_of_string s))
    | Const (Const_base (Const_int32 i)) ->
      add (Int32 i)
    | Const (Const_base (Const_int64 i)) ->
      add (Int64 i)
    | Const (Const_base (Const_nativeint i)) ->
      add (Nativeint i)
    | Prim (Lambda.Pmakeblock (tag, _), args, _) ->
      add (Block (Tag.create_exn tag, args))
    | Set_of_closures ( { function_decls = { funs; set_of_closures_id } } as set )->
      assert(not (Set_of_closures_id.Set.mem set_of_closures_id inconstant.closure));
      (* Will probably never be used *)
      let symbol = fresh_symbol var in
      Symbol.Tbl.add set_of_closures_tbl symbol set;
      add (Symbol symbol);
      (* CR mshinwell: the following seems to be needed because of the behaviour of
         [Closure_conversion_aux.closure_env_without_parameters] and maybe
         [reference_recursive_function_directly].  We should think about this.
         (Could we always use projections instead?  If so we should add an
         invariant check forbidding direct access.)
      *)
      Variable.Map.iter (fun fun_var _ ->
          let closure_id = Closure_id.wrap fun_var in
          Variable.Tbl.add constant_tbl fun_var
            (Symbol (Compilenv.closure_symbol closure_id)))
        funs
    | Move_within_set_of_closures { move_to = closure_id }
    | Project_closure { closure_id } ->
      add (Symbol (Compilenv.closure_symbol closure_id))
    | Prim(Lambda.Pgetglobal id, _, _) ->
      let sym = Compilenv.symbol_for_global' id in
      add (Symbol sym)
    | Symbol symbol ->
      add (Symbol symbol)
    | Prim(Pfield _, _, _) ->
      ()
    | Prim (Pgetglobalfield _, _, _) ->
      ()
    | Prim _ ->
      Format.printf "err:@.%a@." Flambda.print_named named;
      assert false
    | Project_var _ ->
      ()
    | Expr _ ->
      ()
  in
  let to_symbol_if_constant var named =
    if not (Variable.Set.mem var inconstant.id) then
      constant_named var named
  in
  Flambda_iterators.iter (function
      | Let (_, var, named, _) ->
        to_symbol_if_constant var named
      | Let_rec (defs, _) ->
        List.iter
          (fun (var, named) -> to_symbol_if_constant var named)
          defs
      | _ -> ())
    (fun _ -> ())
    expr;
  constant_tbl,
  set_of_closures_tbl

let all_constants map aliases =
  let structured_constants = Variable.Map.keys map in
  let aliased_constants =
    Variable.Map.keys
      (Variable.Map.filter (fun _var alias ->
           Variable.Map.mem alias map)
          aliases)
  in
  Variable.Set.union
    structured_constants
    aliased_constants

module Variable_SCC = Sort_connected_components.Make (Variable)

let constant_graph (map:constant_descr Variable.Map.t) =
  Variable.Map.map (fun (descr:constant_descr) ->
      match descr with
      | Block (_, var) -> Variable.Set.of_list var
      | _ -> Variable.Set.empty)
    map

let rewrite_constant_aliases map aliases =
  let subst var =
    try Variable.Map.find var aliases with
    | Not_found -> var
  in
  let subst_block (descr:constant_descr) : constant_descr =
    match descr with
    | Block (tag, fields) ->
      Block (tag, List.map subst fields)
    | c -> c
  in
  Variable.Map.map subst_block map

let constant_sharing map aliases =
  let all_constants = all_constants map aliases in
  let map = rewrite_constant_aliases map aliases in
  let graph = constant_graph map in
  let components = Variable_SCC.connected_components_sorted_from_roots_to_leaf graph in
  let sorted_symbols =
    List.flatten
      (List.map (function
           | Variable_SCC.Has_loop l -> l
           | Variable_SCC.No_loop v -> [v])
          (List.rev (Array.to_list components)))
  in
  let shared_constants = ref Constant_descr_map.empty in
  let constants = ref Variable.Map.empty in
  let equal_constants = ref Variable.Map.empty in
  let find_and_add var cst =
    match Constant_descr_map.find cst !shared_constants with
    | exception Not_found ->
      shared_constants := Constant_descr_map.add cst var !shared_constants;
      constants := Variable.Map.add var cst !constants;
    | sharing ->
      equal_constants := Variable.Map.add var sharing !equal_constants
  in
  let subst var =
    try Variable.Map.find var !equal_constants with
    | Not_found -> var
  in
  let share var =
    let cst = Variable.Map.find var map in
    match cst with
    | String _
    | Float_array _ ->
      (* Not shared constants *)
      constants := Variable.Map.add var cst !constants
    | Int _
    | Const_pointer _
    | Float _
    | Int32 _
    | Int64 _
    | Nativeint _
    | Immstring _
    | Symbol _ ->
      find_and_add var cst
    | Block (tag, fields) ->
      find_and_add var (Block (tag, List.map subst fields))
  in
  List.iter share sorted_symbols;
  let assign_symbols var descr (descr_map, kind_map) =
    let assign_symbol var (allocated_cst:Variable.t Allocated_constants.t) =
      let symbol = fresh_symbol var in
      Symbol.Map.add symbol allocated_cst descr_map,
      Variable.Map.add var (Symbol symbol:constant) kind_map
    in
    match descr with
    | Int i ->
      descr_map, Variable.Map.add var (Const (Int i)) kind_map
    | Char i ->
      descr_map, Variable.Map.add var (Const (Char i)) kind_map
    | Const_pointer i ->
      descr_map, Variable.Map.add var (Const (Const_pointer i)) kind_map
    | Symbol s ->
      descr_map, Variable.Map.add var (Symbol s) kind_map
    | Float f -> assign_symbol var (Float f)
    | Float_array a -> assign_symbol var (Float_array a)
    | Block (tag, fields) -> assign_symbol var (Block (tag, fields))
    | Int32 i -> assign_symbol var (Int32 i)
    | Int64 i -> assign_symbol var (Int64 i)
    | Nativeint i -> assign_symbol var (Nativeint i)
    | String s -> assign_symbol var (String s)
    | Immstring s -> assign_symbol var (Immstring s)
  in
  let descr, declared_constants_kind =
    Variable.Map.fold assign_symbols !constants (Symbol.Map.empty, Variable.Map.empty)
  in
  let equal_constants_kind =
    Variable.Map.map (fun var -> Variable.Map.find var declared_constants_kind) !equal_constants
  in
  let declared_and_equal_constants_kind =
    Variable.Map.disjoint_union
      declared_constants_kind
      equal_constants_kind
  in
  let kind =
    Variable.Map.of_set (fun var ->
        let alias =
          try Variable.Map.find var aliases with
          | Not_found -> var
        in
        Variable.Map.find alias declared_and_equal_constants_kind)
      all_constants
  in
  let descr =
    let find_kind var =
      try Variable.Map.find var kind with
      | Not_found ->
        Format.printf "missing %a@."
          Variable.print var;
        raise Not_found
    in
    Symbol.Map.map (Allocated_constants.map find_kind)
      descr
  in
  descr, kind

let rewrite_constant_access expr aliases set_of_closures_tbl constant_descr
      (kind : constant Variable.Map.t) ~backend =
  let is_a_constant var =
    let var =
      try Variable.Map.find var aliases with
      | Not_found -> var
    in
    Variable.Map.mem var kind
  in
  let replacement_for_defining_expr var : Flambda.named =
    let var =
      try Variable.Map.find var aliases with
      | Not_found -> var
    in
    Variable.Map.find var kind
  in
  let replace_constant_defining_exprs (expr : Flambda.t) : Flambda.t =
    match expr with
    | Let (_, var, _defining_expr, body) when is_a_constant var ->
      Let (Immutable, var, replacement_for_defining_expr var, body)
    | Let_rec (defs, body) ->
      let defs =
        List.map (fun (var, defining_expr) ->
            let defining_expr =
              if is_a_constant var then replacement_for_defining_expr var
              else defining_expr
            in
            var, defining_expr)
          defs
      in
      Let_rec (defs, body)
    | Let _ | Var _ | Apply _ | Send _ | Assign _ | If_then_else _ | Switch _
    | String_switch _ | Static_raise _ | Static_catch _ | Try_with _
    | While _ | For _ | Proved_unreachable -> expr
  in
  let rewrite_expr expr =
    expr
    |> Flambda_iterators.map_expr replace_constant_defining_exprs
    (* We need to rerun [Inline_and_simplify] because mutable string literals
       (bindings of which would have had unknown approximations during
       [Inline_and_simplify.simplify_free_variable]) that are now known to be
       constant will now have been assigned symbols.  Symbols are immutable,
       meaning that we should be able to cause more sets of closures to
       become closed. *)
    |> Inline_and_simplify.run ~never_inline:true ~backend
  in
  let expr = rewrite_expr expr in
  let free_variables = Free_variables.calculate expr in
  if not (Variable.Set.for_all is_a_constant free_variables) then begin
    Misc.fatal_errorf "Lift_constants: toplevel expression contains free \
        variables that are not constant: %a"
      Flambda.print expr
  end;
  let bind_constant var body : Flambda.t =
    assert (is_a_constant var);
    match get_kind var with
    | Int i -> Let (Immutable, var, Const (Const_base (Const_int i)), body)
    | Const_pointer p -> Let (Immutable, var, Const (Const_pointer p), body)
    | Symbol s -> Let (Immutable, var, Symbol s, body)
    | exception Not_found -> body
  in
  let expr = Variable.Set.fold bind_constant free_variables expr in
  let set_of_closures_map =
    Symbol.Tbl.fold (fun symbol set_of_closures map ->
        let set_of_closures =
          Flambda_iterators.map_function_bodies set_of_closures ~f:rewrite_expr
        in
        Symbol.Map.add symbol set_of_closures map)
      set_of_closures_tbl Symbol.Map.empty
  in
  Format.eprintf "lift_constants output:@ %a\n" Flambda.print expr;
  { expr;
    constant_descr;
    kind;
    set_of_closures_map;
  }

let lift_constants expr ~backend =
  Format.eprintf "lift_constants input:@ %a\n" Flambda.print expr;
  let constant_tbl, set_of_closures_tbl =
    collect_constant_declarations expr
  in
  let constant_map =
    Variable.Tbl.fold Variable.Map.add constant_tbl Variable.Map.empty
  in
  let aliases =
    Alias_analysis.run expr
  in
  let constants, kind = constant_sharing constant_map aliases in
  rewrite_constant_access expr aliases set_of_closures_tbl constants kind
    ~backend
