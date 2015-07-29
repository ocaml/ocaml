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

(* Lift allocated constants out of the main expression.
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

module Constant_descr_map = Map.Make(Constant_descr)

type result = {
  expr : Flambda.t;
  constant_descr : Symbol.t Allocated_const.tant Symbol.Map.t;
  kind : Symbol.t Variable.Map.t;
  set_of_closures_map : Flambda.set_of_closures Symbol.Map.t;
}

let fresh_symbol var =
  Compilenv.new_const_symbol' ~name:(Variable.unique_name var) ()

let collect_constant_declarations expr =
  let inconstant =
    Inconstant_idents.inconstants expr
      ~for_clambda:true
      (* CR pchambart: get rid of this.
         This should be available in backend
         mshinwell: what is "this"? *)
      ~compilation_unit:(Compilenv.current_unit ())
  in
  let constant_tbl : Flambda.named Variable.Tbl.t = Variable.Tbl.create 10 in
  let set_of_closures_tbl : Flambda.set_of_closures Symbol.Tbl.t =
    Symbol.Tbl.create 10
  in
  let assign_symbol_if_constant var (named : Flambda.named) =
    if not (Variable.Set.mem var inconstant.id) then begin
      let add const = Variable.Tbl.add constant_tbl var const in
      match named with
      | Allocated_const const -> add const
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
      | Prim (Lambda.Pgetglobal id, _, _) ->
        add (Symbol (Compilenv.symbol_for_global' id))
      | Symbol symbol -> add (Symbol symbol)
      | Prim (Pfield _, _, _) | Prim (Pgetglobalfield _, _, _) -> ()
      | Prim _ ->
        Misc.fatal_errorf "Primitive not expected to be constant: @.%a@."
          Flambda.print_named named
      | Const _ | Project_var _ | Expr _ -> ()
    end
  in
  Flambda_iterators.iter (function
      | Let (_, var, named, _) -> assign_symbol_if_constant var named
      | Let_rec (defs, _) ->
        List.iter (fun (var, named) ->
            assign_symbol_if_constant var named)
          defs
      | _ -> ())
    (fun _ -> ())
    expr;
  constant_tbl, set_of_closures_tbl

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
  let symbol_defining_exprs =
    Symbol.Map.disjoint_union
      (Symbol.Map.map (fun const -> Flambda.Allocated_const const)
        constant_descr)
      (Symbol.Map.map (fun set -> Flambda.Set_of_closures set)
        set_of_closures_map)
  in
  let rewrite_expr expr =
    expr
    |> Flambda_iterators.map_expr replace_constant_defining_exprs
    (* We need to rerun [Inline_and_simplify] because strings and float arrays
       (bindings of which would have had unknown approximations during
       [Inline_and_simplify.simplify_free_variable]) that are now known to be
       constant will now have been assigned symbols.  Symbols are immutable,
       meaning that we should be able to cause more sets of closures to
       become closed. *)
    |> Inline_and_simplify.run ~never_inline:true ~backend
        ~symbol_defining_exprs
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
  let constant_tbl, set_of_closures_tbl = collect_constant_declarations expr in
  let constant_map =
    Variable.Tbl.fold Variable.Map.add constant_tbl Variable.Map.empty
  in
  let aliases = Alias_analysis.run expr in
  let constants, kind = constant_sharing constant_map aliases in
  rewrite_constant_access expr aliases set_of_closures_tbl constants kind
    ~backend
