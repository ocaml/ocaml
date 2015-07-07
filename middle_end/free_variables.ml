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
  let rec aux (flam : _ Flambda.t) : unit =
    match flam with
    | Fvar (var, _) -> mark_free var
    | Fapply ({ func; args; kind = _; dbg = _}, _) ->
      aux func;
      List.iter mark_free args
    | Fproject_var ({ closure; closure_id = _; var = _ }, _) ->
      mark_free closure
    | Flet ( _, var, defining_expr, body, _) ->
      mark_bound var;
      aux_named defining_expr;
      aux body
    | Fletrec (bindings, body, _) ->
      List.iter (fun (var, defining_expr) ->
          mark_bound var;
          aux_named defining_expr)
        bindings;
      aux body
    | Fseq_prim (_, args, _, _) ->
      List.iter aux args
    | Fswitch (e1, switch, _) ->
      aux e1;
      List.iter (fun (_, e) -> aux e) switch.consts;
      List.iter (fun (_, e) -> aux e) switch.blocks;
      Misc.may aux switch.failaction
    | Fstringswitch (e1, cases, failaction, _) ->
      aux e1;
      List.iter (fun (_, e) -> aux e) cases;
      Misc.may aux failaction
    | Fstaticraise (_, es, _) ->
      List.iter aux es
    | Fstaticcatch (_, vars, e1, e2, _) ->
      List.iter mark_bound vars;
      aux e1;
      aux e2
    | Ftrywith (e1, var, e2, _) ->
      aux e1;
      mark_bound var;
      aux e2
    | Fifthenelse (e1, e2, e3, _) ->
      aux e1;
      aux e2;
      aux e3
    | Fsequence (e1, e2, _) ->
      aux e1;
      aux e2
    | Fwhile (e1, e2, _) ->
      aux e1;
      aux e2
    | Ffor (var, e1, e2, _, e3, _) ->
      mark_bound var;
      aux e1;
      aux e2;
      aux e3
    | Fassign (id, e, _) ->
      mark_free id;
      aux e
    | Fsend (_, e1, e2, es, _, _) ->
      aux e1;
      aux e2;
      List.iter aux es
    | Funreachable _ -> ()
  and aux_named (named : _ Flambda.named) =
    match named with
    | Fsymbol _ | Fconst _ -> ()
    | Fset_of_closures ({specialised_args},_) ->
      (* CR mshinwell for pchambart: mark_free comment explaining why
         the [free_variables] inside [Fset_of_closures] isn't counted
         here.  Shouldn't this go into the body as well? *)
      Variable.Map.iter (fun _ var -> mark_free var) specialised_args
    | Fproject_closure ({ set_of_closures; closure_id = _}, _) ->
      mark_free set_of_closures
    | Fmove_within_set_of_closures
        ({ closure; start_from = _; move_to = _ }, _) ->
      mark_free closure
    | Fprim (_, args, _, _) ->
      List.iter mark_free args
    | Fexpr flam -> aux flam
  in
  aux tree;
  Variable.Set.diff !free !bound
