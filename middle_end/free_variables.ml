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

(* CR mshinwell: this doesn't seem to cope with shadowed identifiers
   properly.  Check the original version.  Why don't we just do the
   subtraction as we pass back over binding points? *)

let iter tree ~free_variable ~bound_variable =
  let rec aux (flam : Flambda.t) : unit =
    match flam with
    | Var var -> free_variable var
    | Apply { func; args; kind = _; dbg = _} ->
      free_variable func;
      List.iter free_variable args
    | Let (_, var, defining_expr, body) ->
      bound_variable var;
      aux_named defining_expr;
      aux body
    | Let_rec (bindings, body) ->
      List.iter (fun (var, defining_expr) ->
          bound_variable var;
          aux_named defining_expr)
        bindings;
      aux body
    | Switch (scrutinee, switch) ->
      free_variable scrutinee;
      List.iter (fun (_, e) -> aux e) switch.consts;
      List.iter (fun (_, e) -> aux e) switch.blocks;
      Misc.may aux switch.failaction
    | String_switch (scrutinee, cases, failaction) ->
      free_variable scrutinee;
      List.iter (fun (_, e) -> aux e) cases;
      Misc.may aux failaction
    | Static_raise (_, es) ->
      List.iter aux es
    | Static_catch (_, vars, e1, e2) ->
      List.iter bound_variable vars;
      aux e1;
      aux e2
    | Try_with (e1, var, e2) ->
      aux e1;
      bound_variable var;
      aux e2
    | If_then_else (var, e1, e2) ->
      free_variable var;
      aux e1;
      aux e2
    | While (e1, e2) ->
      aux e1;
      aux e2
    | For { bound_var; from_value; to_value; direction = _; body; } ->
      bound_variable bound_var;
      free_variable from_value;
      free_variable to_value;
      aux body
    | Assign { being_assigned; new_value; } ->
      free_variable being_assigned;
      free_variable new_value
    | Send { kind = _; meth; obj; args; dbg = _ } ->
      free_variable meth;
      free_variable obj;
      List.iter free_variable args;
    | Proved_unreachable -> ()
  and aux_named (named : Flambda.named) =
    match named with
    | Symbol _ | Const _ -> ()
    | Set_of_closures { free_vars; specialised_args; _ } ->
      (* Sets of closures are, well, closed---except for the specialised
         argument list, which may identify variables currently in scope
         outside of the closure. *)
      Variable.Map.iter (fun _ renamed_to -> free_variable renamed_to)
        free_vars;
      Variable.Map.iter (fun _ var -> free_variable var) specialised_args
    | Project_closure { set_of_closures; closure_id = _ } ->
      free_variable set_of_closures
    | Project_var { closure; closure_id = _; var = _ } ->
      free_variable closure
    | Move_within_set_of_closures { closure; start_from = _; move_to = _ } ->
      free_variable closure
    | Prim (_, args, _) -> List.iter free_variable args
    | Expr flam -> aux flam
  in
  aux tree

let calculate tree =
  let free = ref Variable.Set.empty in
  let bound = ref Variable.Set.empty in
  let free_variable id = free := Variable.Set.add id !free in
  let bound_variable id = bound := Variable.Set.add id !bound in
  iter tree ~free_variable ~bound_variable;
  Variable.Set.diff !free !bound

let calculate_named tree =
  let var = Variable.create "dummy" in
  calculate (Let (Immutable, var, tree, Var var))
