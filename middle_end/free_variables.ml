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

let calculate tree =
  let free = ref Variable.Set.empty in
  let bound = ref Variable.Set.empty in
  let mark_free id = free := Variable.Set.add id !free in
  let mark_bound id = bound := Variable.Set.add id !bound in
  let rec aux (flam : Flambda.t) : unit =
    match flam with
    | Var var -> mark_free var
    | Apply { func; args; kind = _; dbg = _} ->
      mark_free func;
      List.iter mark_free args
    | Let ( _, var, defining_expr, body) ->
      mark_bound var;
      aux_named defining_expr;
      aux body
    | Let_rec (bindings, body) ->
      List.iter (fun (var, defining_expr) ->
          mark_bound var;
          aux_named defining_expr)
        bindings;
      aux body
    | Switch (e1, switch) ->
      aux e1;
      List.iter (fun (_, e) -> aux e) switch.consts;
      List.iter (fun (_, e) -> aux e) switch.blocks;
      Misc.may aux switch.failaction
    | String_switch (e1, cases, failaction) ->
      aux e1;
      List.iter (fun (_, e) -> aux e) cases;
      Misc.may aux failaction
    | Static_raise (_, es) ->
      List.iter aux es
    | Static_catch (_, vars, e1, e2) ->
      List.iter mark_bound vars;
      aux e1;
      aux e2
    | Try_with (e1, var, e2) ->
      aux e1;
      mark_bound var;
      aux e2
    | If_then_else (e1, e2, e3) ->
      aux e1;
      aux e2;
      aux e3
    | While (e1, e2) ->
      aux e1;
      aux e2
    | For (var, e1, e2, _, e3) ->
      mark_bound var;
      aux e1;
      aux e2;
      aux e3
    | Assign (id, e) ->
      mark_free id;
      aux e
    | Send (_, e1, e2, es, _) ->
      aux e1;
      aux e2;
      List.iter aux es
    | Proved_unreachable -> ()
  and aux_named (named : Flambda.named) =
    match named with
    | Symbol _ | Const _ -> ()
    | Set_of_closures { specialised_args; _ } ->
      (* CR mshinwell for pchambart: mark_free comment explaining why
         the [free_variables] inside [Set_of_closures] isn't counted
         here.  Shouldn't this go into the body as well? *)
      Variable.Map.iter (fun _ var -> mark_free var) specialised_args
    | Project_closure { set_of_closures; closure_id = _ } ->
      mark_free set_of_closures
    | Project_var { closure; closure_id = _; var = _ } ->
      mark_free closure
    | Move_within_set_of_closures { closure; start_from = _; move_to = _ } ->
      mark_free closure
    | Prim (_, args, _) -> List.iter mark_free args
    | Expr flam -> aux flam
  in
  aux tree;
  Variable.Set.diff !free !bound
