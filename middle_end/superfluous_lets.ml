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

(* CR mshinwell: improve bound name representations or find some other
   way of fixing this? *)

let substitute_free_occurrences tree find replace =
  let handle_var var =
    if Variable.equal var find then replace else var
  in
  let rec aux (tree : Flambda.t) : Flambda.t =
    match tree with
    | Var var -> Var (handle_var var)
    | Apply { func; args; kind; dbg; } ->
      Apply {
        func = handle_var func;
        args = List.map handle_var args;
        kind;
        dbg;
      }
    | Assign (var, lam) ->
      let lam = aux lam in
      Assign (handle_var var, lam)
    | Proved_unreachable -> tree
    | Let (let_kind, var, defining_expr, body) ->
      let defining_expr = aux_named defining_expr in
      if Variable.equal find var then
        Let (let_kind, var, defining_expr, body)
      else
        Let (let_kind, var, defining_expr, aux body)
    | Let_rec (defs, body) ->
      let stop = ref false in
      let defs =
        List.map (fun (var, lam) ->
            if Variable.equal find var then stop := true;
            var, aux_named lam)
          defs
      in
      if !stop then Let_rec (defs, body)
      else Let_rec (defs, aux body)
    | Switch (arg, sw) ->
      let arg = aux arg in
      let sw =
        { sw with
          failaction = Misc.may_map aux sw.failaction;
          consts = List.map (fun (i,v) -> i, aux v) sw.consts;
          blocks = List.map (fun (i,v) -> i, aux v) sw.blocks;
        }
      in
      Switch (arg, sw)
    | String_switch (arg, sw, def) ->
      let arg = aux arg in
      let sw = List.map (fun (i,v) -> i, aux v) sw in
      let def = Misc.may_map aux def in
      String_switch (arg, sw, def)
    | Static_raise (i, args) ->
      let args = List.map aux args in
      Static_raise (i, args)
    | Static_catch (i, vars, body, handler) ->
      let body = aux body in
      if List.exists (fun var -> Variable.equal find var) vars then
        Static_catch (i, vars, body, handler)
      else
        Static_catch (i, vars, body, aux handler)
    | Try_with (body, var, handler) ->
      let body = aux body in
      if Variable.equal find var then
        Try_with (body, var, handler)
      else
        Try_with (body, var, aux handler)
    | If_then_else (arg, ifso, ifnot) ->
      let arg = aux arg in
      let ifso = aux ifso in
      let ifnot = aux ifnot in
      If_then_else (arg, ifso, ifnot)
    | While (cond, body) ->
      let cond = aux cond in
      let body = aux body in
      While (cond, body)
    | Send (kind, met, obj, args, dbg) ->
      let met = aux met in
      let obj = aux obj in
      let args = List.map aux args in
      Send (kind, met, obj, args, dbg)
    | For (var, lo, hi, dir, body) ->
      let lo = aux lo in
      let hi = aux hi in
      if Variable.equal find var then
        For (var, lo, hi, dir, body)
      else
        For (var, lo, hi, dir, aux body)
  and aux_named (named : Flambda.named) : Flambda.named =
    match named with
    | Symbol _ | Const _  -> named
    | Project_closure project_closure ->
      Project_closure {
        project_closure with
        set_of_closures = handle_var project_closure.set_of_closures;
      }
    | Move_within_set_of_closures move_within_set_of_closures ->
      Move_within_set_of_closures {
        move_within_set_of_closures with
        closure = handle_var move_within_set_of_closures.closure;
      }
    | Project_var project_var ->
      Project_var {
        project_var with
        closure = handle_var project_var.closure;
      }
    | Prim (prim, args, dbg) ->
      Prim (prim, List.map handle_var args, dbg)
    | Set_of_closures { function_decls; free_vars; specialised_args } ->
      Set_of_closures {
        function_decls;
        free_vars;
        specialised_args = Variable.Map.map handle_var specialised_args;
      }
    | Expr expr -> Expr (aux expr)
  in
  aux tree

let remove tree =
  let rec aux (tree : Flambda.t) : Flambda.t =
    match tree with
    | Var _ | Apply _ -> tree
    | Assign (id, lam) ->
      let lam = aux lam in
      Assign (id, lam)
    | Proved_unreachable -> tree
    | Let (Immutable, var1, defining_expr1, body1) ->
      let defining_expr1 = aux_named defining_expr1 in
      let body1 = aux body1 in
      let defining_expr1 =
        match defining_expr1 with
        | Expr (Let (Immutable, var2, defining_expr2, Var var2'))
            when Variable.equal var2 var2' -> defining_expr2
        | defining_expr1 -> defining_expr1
      in
      begin match defining_expr1 with
      | Expr (Var var) -> substitute_free_occurrences body1 var1 var
      | defining_expr1 -> Let (Immutable, var1, defining_expr1, body1)
      end
    | Let (Mutable, var, defining_expr, body) ->
      Let (Mutable, var, aux_named defining_expr, aux body)
    | Let_rec (defs, body) ->
      let defs = List.map (fun (id, lam) -> id, aux_named lam) defs in
      let body = aux body in
      Let_rec (defs, body)
    | Switch (arg, sw) ->
      let arg = aux arg in
      let sw =
        { sw with
          failaction = Misc.may_map aux sw.failaction;
          consts = List.map (fun (i,v) -> i, aux v) sw.consts;
          blocks = List.map (fun (i,v) -> i, aux v) sw.blocks;
        }
      in
      Switch (arg, sw)
    | String_switch (arg, sw, def) ->
      let arg = aux arg in
      let sw = List.map (fun (i,v) -> i, aux v) sw in
      let def = Misc.may_map aux def in
      String_switch (arg, sw, def)
    | Static_raise (i, args) ->
      let args = List.map aux args in
      Static_raise (i, args)
    | Static_catch (i, vars, body, handler) ->
      let body = aux body in
      let handler = aux handler in
      Static_catch (i, vars, body, handler)
    | Try_with (body, id, handler) ->
      let body = aux body in
      let handler = aux handler in
      Try_with (body, id, handler)
    | If_then_else (arg, ifso, ifnot) ->
      let arg = aux arg in
      let ifso = aux ifso in
      let ifnot = aux ifnot in
      If_then_else (arg, ifso, ifnot)
    | While (cond, body) ->
      let cond = aux cond in
      let body = aux body in
      While (cond, body)
    | Send (kind, met, obj, args, dbg) ->
      let met = aux met in
      let obj = aux obj in
      let args = List.map aux args in
      Send (kind, met, obj, args, dbg)
    | For (id, lo, hi, dir, body) ->
      let lo = aux lo in
      let hi = aux hi in
      let body = aux body in
      For (id, lo, hi, dir, body)
  and aux_named (named : Flambda.named) : Flambda.named =
    match named with
    | Symbol _ | Const _ | Project_closure _ | Move_within_set_of_closures _
    | Project_var _ | Prim _ -> named
    | Set_of_closures { function_decls; free_vars; specialised_args } ->
      let function_decls : Flambda.function_declarations =
        { function_decls with
          funs = Variable.Map.map
              (fun (ffun : Flambda.function_declaration) ->
                { ffun with body = aux ffun.body })
            function_decls.funs;
        }
      in
      Set_of_closures { function_decls; free_vars; specialised_args }
    | Expr expr -> Expr (aux expr)
  in
  aux tree
