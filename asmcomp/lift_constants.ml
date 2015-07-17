(* Extract constants out of the main expression.
   * First collect all the constant declarations, separating structured
     constants and constant sets of closures: [collect_constant_declarations]
   * Then share equal constants: [constant_sharing].
     This is done by traversing the structured constants in inverse
     topological order and replacing every existing constant by the previous
     one. (Note: this does not guaranty maximal sharing in cycles)
   * Finaly, remove every let declaration of constants (or aliased to a constant).

   Since the representation does not allow to substitute symbol for variables
   (only variables are accepted in certain positions), those variables are unbound
   in the expression.

   The invariants are:
   * every variable is either bound in the expression or bound in the [kind] field
     of the [result] type.
   * every symbol from the current compilation unit referenced by the [kind] field
     should be bound by either
     - [constant_descr] if it is a structured constant
     - [set_of_closures_map] if it is a set of closures
     - a closure in a set of [set_of_closures_map] if it is a closures

   PR pchambart: those invariants should be checked by a modified Flambda_invariants
   for this representations.

*)

(* Values of constants at use point *)

type constant =
  | Symbol of Symbol.t
  | Int of int
  | Const_pointer of int

(* Values of constants at definition point *)

module Allocated_constants = struct

  (* subset of constants that can be assigned a symbol.
     There are more cases than what can be described by
     clambda to be able to build an approximation *)

  type 'a t =
    | Float of float
    | Int32 of int32
    | Int64 of int64
    | Nativeint of nativeint
    | Float_array of float list
    | String of string
    | Immstring of string
    | Block of Tag.t * 'a list

  let map : ('a -> 'b) -> 'a t -> 'b t = fun f t ->
    match t with
    | Float v -> Float v
    | Int32 v -> Int32 v
    | Int64 v -> Int64 v
    | Nativeint v -> Nativeint v
    | Float_array v -> Float_array v
    | String v -> String v
    | Immstring v -> Immstring v
    | Block (tag, fields) ->
      Block (tag, List.map f fields)

end

(* All the kinds of constants. *)

type constant_descr =
  | Int of int
  | Const_pointer of int
  | Float of float
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
  | Float_array of float list
  | String of string
  | Immstring of string
  | Block of Tag.t * Variable.t list
  | Symbol of Symbol.t

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

  let rec compare_variable_lists l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _::_ -> -1
    | _::_, [] -> 1
    | h1::t1, h2::t2 ->
      let c = Variable.compare h1 h2 in
      if c <> 0 then
        c
      else
        compare_variable_lists t1 t2

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
        compare_variable_lists fields1 fields2

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
  constant_descr : constant Allocated_constants.t Symbol.Map.t;
  kind : constant Variable.Map.t;
  set_of_closures_map : Flambda.set_of_closures Symbol.Map.t;
}

let fresh_symbol var =
  Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ()

let collect_constant_declarations expr =
  let inconstant =
    Inconstant_idents.inconstants
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend *)
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
    | Set_of_closures ( { function_decls = { set_of_closures_id } } as set )->
      assert(not (Set_of_closures_id.Set.mem set_of_closures_id inconstant.closure));
      (* Will probably never be used *)
      let symbol = fresh_symbol var in
      Symbol.Tbl.add set_of_closures_tbl symbol set;
      add (Symbol symbol)
    | Move_within_set_of_closures { move_to = closure_id }
    | Project_closure { closure_id } ->
      add (Symbol (Compilenv.closure_symbol closure_id))
    | Prim(Lambda.Pgetglobal id, _, _) ->
      let sym = Compilenv.symbol_for_global' id in
      add (Symbol sym)
    | Symbol symbol ->
      add (Symbol symbol)
    | Prim (Pgetglobalfield _, _, _) ->
      ()
    | Prim _ ->
      Format.printf "err:@.%a@." Flambda_printers.named named;
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
  let alias = ref Variable.Map.empty in
  let find_and_add var cst =
    match Constant_descr_map.find cst !shared_constants with
    | exception Not_found ->
      shared_constants := Constant_descr_map.add cst var !shared_constants;
      constants := Variable.Map.add var cst !constants;
      alias := Variable.Map.add var var !alias
    | sharing ->
      alias := Variable.Map.add var sharing !alias
  in
  let subst var =
    match Variable.Map.find var !alias with
    | exception Not_found ->
      var
    | subst ->
      subst
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
    let add var (allocated_cst:Variable.t Allocated_constants.t) =
      let symbol = fresh_symbol var in
      Symbol.Map.add symbol allocated_cst descr_map,
      Variable.Map.add var (Symbol symbol:constant) kind_map
    in
    match descr with
    | Int i ->
      descr_map, Variable.Map.add var (Int i:constant) kind_map
    | Const_pointer i ->
      descr_map, Variable.Map.add var (Const_pointer i:constant) kind_map
    | Symbol s ->
      descr_map, Variable.Map.add var (Symbol s:constant) kind_map
    | Float f ->
      add var (Float f)
    | Float_array a ->
      add var (Float_array a)
    | Block (tag, fields) ->
      add var (Block (tag, fields))
    | Int32 i ->
      add var (Int32 i)
    | Int64 i ->
      add var (Int64 i)
    | Nativeint i ->
      add var (Nativeint i)
    | String s ->
      add var (String s)
    | Immstring s ->
      add var (Immstring s)
  in
  let descr, kind =
    Variable.Map.fold assign_symbols !constants (Symbol.Map.empty, Variable.Map.empty)
  in
  let kind =
    Variable.Map.map (fun var -> Variable.Map.find var kind) !alias
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

let rewrite_constant_access expr aliases set_of_closures_tbl constant_descr (kind:constant Variable.Map.t) =
  let is_a_constant var =
    let var =
      try Variable.Map.find var aliases with
      | Not_found -> var
    in
    Variable.Map.mem var kind
  in
  let rewrite : Flambda.t -> Flambda.t = function
    | Let (kind, var, named, body) ->
      if is_a_constant var then
        body
      else
        Let (kind, var, named, body)
    | Let_rec (defs, body) ->
      let defs = List.filter (fun (var, _) -> is_a_constant var) defs in
      begin match defs with
      | [] -> body
      | _ -> Flambda.Let_rec (defs, body)
      end
    | expr -> expr
  in
  let expr = Flambda_iterators.map rewrite (fun x -> x) expr in
  let set_of_closures_map =
    Symbol.Tbl.fold (fun symbol (set_of_closures:Flambda.set_of_closures) map ->
        let update_function_decl (function_declaration:Flambda.function_declaration) =
          let body =
            Flambda_iterators.map rewrite (fun x -> x)
              function_declaration.body
          in
          { function_declaration with Flambda.body }
        in
        let function_decls = {
          set_of_closures.function_decls with
          funs =
            Variable.Map.map update_function_decl
              set_of_closures.function_decls.funs
        }
        in
        let set_of_closures = {
          Flambda.function_decls;
          free_vars = Variable.Map.empty;
          specialised_args = Variable.Map.empty;
        } in
        Symbol.Map.add symbol set_of_closures map
      )
      set_of_closures_tbl Symbol.Map.empty
  in
  { expr;
    constant_descr;
    kind;
    set_of_closures_map }

let lift_constants expr =
  let constant_tbl, set_of_closures_tbl =
    collect_constant_declarations expr
  in
  let constant_map = Variable.Tbl.fold Variable.Map.add constant_tbl Variable.Map.empty in
  let aliases =
    Alias_analysis.run expr
  in
  let constants, kind = constant_sharing constant_map aliases in
  rewrite_constant_access expr aliases set_of_closures_tbl constants kind
