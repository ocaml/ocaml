(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env

type projection_defns = Flambda.expr Variable.Map.t list

type result = {
  projection_defns : projection_defns;
  new_function_body : Flambda.expr;
  additional_free_vars : Variable.t Variable.Map.t;
  benefit : Inlining_cost.Benefit.t;
}

type extracted_var_within_closure = {
  new_var : Variable.t;
  closure_id : Closure_id.t;
  outside_var : Variable.t;
}

type extracted_closure = {
  new_var : Variable.t;
  start_from : Closure_id.t;
  outside_var : Variable.t;
}

type extracted_field = {
  new_var : Variable.t;
  outside_var : Variable.t;
}

type extracted =
  | Var_within_closure of extracted_var_within_closure
  (* [Closure] comes from [Project_closure] or [Move_within_set_of_closures]
     expressions. *)
  | Closure of extracted_closure
  | Field of extracted_field

let freshened_var env v =
  Freshening.apply_variable (E.freshening env) v

module B = Inlining_cost.Benefit
module VAP = Projection.Var_and_projectee

let collect_projections ~env ~which_variables =
  Variable.Map.fold (fun inside_var outside_var collected ->
      let approx = E.find_exn env (freshened_var env outside_var) in
      (* First determine if the variable is bound to a closure. *)
      match A.check_approx_for_closure approx with
      | Ok (value_closure, _approx_var, _approx_sym, value_set_of_closures) ->
        let collected =
          Var_within_closure.Map.fold (fun bound_var _ collected ->
              let new_var =
                Variable.create (Var_within_closure.unique_name bound_var)
              in
              let extracted : extracted_var_within_closure =
                { new_var;
                  closure_id = value_closure.closure_id;
                  outside_var;
                }
              in
              VAP.Map.add (inside_var, Project_var bound_var)
                (Var_within_closure extracted) collected)
            value_set_of_closures.bound_vars collected
        in
        Variable.Map.fold (fun fun_var _ collected ->
            let new_var = Variable.rename fun_var in
            let start_from = value_closure.closure_id in
            let move_to = Closure_id.wrap fun_var in
            (* For the moment represent all projections of closures as
               moves from the original closure ID.  [Inline_and_simplify]
               can deal with this if simplification is possible. *)
            let extracted : extracted_closure =
              { new_var;
                start_from;
                outside_var;
              }
            in
            VAP.Map.add (inside_var, Closure move_to)
              (Closure extracted) collected)
          value_set_of_closures.function_decls.funs closure_acc
      | Wrong ->  (* The variable is not bound to a closure. *)
        match A.check_approx_for_block approx with
        | Wrong -> acc  (* Ignore if not bound to a closure or block. *)
        | Ok (_tag, fields) ->
          let (_field_index : int), collected =
            Array.fold_left (fun (field_index, collected) approx ->
                (* CR-soon pchambart: should we restrict only to cases
                   when the field is aliased to a variable outside
                   of the closure (i.e. when we can certainly remove
                   the allocation of the block) ?
                   Note that this may prevent cases with imbricated
                   closures from benefiting from this transformations.
                   mshinwell: What word was "imbricated" supposed to be?
                *)
                let collected =
                  match approx.A.var with
                  | Some var when E.mem env var ->
                    let new_var =
                      Variable.create
                        (Variable.unique_name inside_var ^ "_field_"
                          ^ string_of_int field_index)
                    in
                    let extracted : extracted_field =
                      { new_var;
                        outside_var;
                      }
                    in
                    VAP.Map.add (inside_var, Field field_index)
                      (Field extracted) collected
                  | None | Some _ -> collected
                in
                field_index + 1, collected)
              fields
              (0, collected)
          in
          collected)
    which_variables
    VAP.Map.empty

let from_function_decl ~which_variables ~env
      ~(function_decl : Flambda.function_declaration) : result =
  let collected = collect_projections ~env ~which_variables in
  if VAP.Map.cardinal collected = 0 then
    None
  else
    let used_new_vars = Variable.Tbl.create 42 in
    let benefit = ref B.zero in
    let new_function_body =
      Flambda_iterators.map_toplevel_projections_to_expr_opt
        ~f:(fun (projection : Projection.t) ->
          let this_benefit = B.of_projection projection in
          match projection with
          | Project_var { closure; var; closure_id = _; } ->
            begin match
              VAP.Map.find (closure, Var_within_closure var) collected
            with
            | exception Not_found -> None
            | { new_var; _ } ->
              benefit := B.(+) !benefit this_benefit;
              Variable.Tbl.add used_new_vars new_var ();
              Some (Flambda.Var new_var)
            end
          | Project_closure _project_closure ->
            (* CR-soon mshinwell: implement this *)
            None
          | Move_within_set_of_closures
              { closure; move_to; start_from = _; } ->
            begin match
              VAP.Map.find (closure, Closure move_to) collected
            with
            | exception Not_found -> None
            | { new_var; _ } ->
              benefit := B.(+) !benefit this_benefit;
              Variable.Tbl.add used_new_vars new_var ();
              Some (Flambda.Var new_var)
            end
          | Field (field_index, var) ->
            begin match
              VAP.Map.find (var, Field field_index) collected
            with
            | exception Not_found -> None
            | { new_var; _ } ->
              benefit := B.(+) !benefit this_benefit;
              Variable.Tbl.add used_new_vars new_var ();
              Some (Flambda.Var new_var)
            end)
        function_decl.body
    in
    let benefit = !benefit in
    let additional_free_vars, new_bindings =
      VAP.Map.fold (fun (projecting_from, projectee : Projection.Projectee.t)
            extracted (free_vars, new_bindings) ->
          let record ~new_var ~intermediate_var ~defining_expr =
            let free_vars =
              Variable.Map.add new_var intermediate_var free_vars
            in
            let new_bindings =
              (* Quotient [new_bindings] by equivalence of [projecting_from]
                 to get the nice grouping behaviour supported by
                 [Augment_specialised_args]. *)
              let map_for_projecting_from =
                match Variable.Map.find projecting_from new_bindings with
                | exception Not_found -> Variable.Map.empty
                | map -> map
              in
              Variable.Map.add projecting_from
                (Variable.Map.add intermediate_var defining_expr
                  map_for_projecting_from)
                new_bindings
            in
            free_vars, new_bindings
          in
          match projectee, extracted with
          | Project_var var_within_closure,
            Var_within_closure { new_var; closure_id; outside_var; } ->
            (* CR-soon mshinwell: improve "intermediate_var" naming. *)
            let intermediate_var = Variable.rename new_var in
            if Variable.Tbl.mem used_new_vars new_var then
              let defining_expr : Flambda.named =
                Project_var {
                  closure = outside_var;
                  closure_id;
                  var = var_within_closure;
                }
              in
              record ~new_var ~intermediate_var ~defining_expr
            else
              free_vars, new_bindings
          | Closure move_to,
            Closure { new_var; start_from; outside_var; }->
            let intermediate_var = Variable.rename new_var in
            if Variable.Tbl.mem used_new_vars new_var then
              let defining_expr : Flambda.named =
                Move_within_set_of_closures {
                  closure = outside_var;
                  start_from;
                  move_to;
                }
              in
              record ~new_var ~intermediate_var ~defining_expr
            else
              free_vars, new_bindings
          | Field field_index,
            Field { new_var; outside_var; } ->
            let intermediate_var = Variable.rename new_var in
            if Variable.Tbl.mem used_new_vars new_var then
              let defining_expr : Flambda.named =
                Flambda.Prim (Pfield field, [outside_var], Debuginfo.none)
              in
              record ~new_var ~intermediate_var ~defining_expr
            else
              free_vars, add_blocks
          | _ -> assert false)
        collected
        (Variable.Map.empty, Variable.Map.empty)
    in
    let projection_defns = Variable.Map.data new_bindings in
    { projection_defns;
      new_function_body;
      additional_free_vars;
      benefit;
    }
