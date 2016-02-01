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

type projection_defns = Flambda.named Variable.Map.t Variable.Map.t

type result = {
  projection_defns_indexed_by_outer_vars : projection_defns;
  new_inner_to_new_outer_vars : Flambda.specialised_to Variable.Map.t;
}

type extracted_var_within_closure = {
  new_inner_var : Variable.t;
  closure_id : Closure_id.t;
  outer_var : Variable.t;
}

type extracted_closure = {
  new_inner_var : Variable.t;
  start_from : Closure_id.t;
  outer_var : Variable.t;
}

type extracted_field = {
  new_inner_var : Variable.t;
  outer_var : Variable.t;
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
module VAP = Projectee.Var_and_projectee

let collect_projections ~env ~which_variables =
  Variable.Map.fold (fun inside_var (spec_to : Flambda.specialised_to)
            collected ->
      let outer_var = spec_to.var in
      let approx = E.find_exn env (freshened_var env outer_var) in
      (* First determine if the variable is bound to a closure. *)
      match A.check_approx_for_closure approx with
      | Ok (value_closure, _approx_var, _approx_sym, value_set_of_closures) ->
        let collected =
          Var_within_closure.Map.fold (fun bound_var _ collected ->
              let new_inner_var =
                Variable.create (Var_within_closure.unique_name bound_var)
              in
              let extracted : extracted_var_within_closure =
                { new_inner_var;
                  closure_id = value_closure.closure_id;
                  outer_var;
                }
              in
              VAP.Map.add (inside_var, Project_var bound_var)
                (Var_within_closure extracted) collected)
            value_set_of_closures.bound_vars collected
        in
        Variable.Map.fold (fun fun_var _ collected ->
            let new_inner_var = Variable.rename fun_var in
            let start_from = value_closure.closure_id in
            let move_to = Closure_id.wrap fun_var in
            (* For the moment represent all projections of closures as
               moves from the original closure ID.  [Inline_and_simplify]
               can deal with this if simplification is possible. *)
            let extracted : extracted_closure =
              { new_inner_var;
                start_from;
                outer_var;
              }
            in
            VAP.Map.add (inside_var, Closure move_to)
              (Closure extracted) collected)
          value_set_of_closures.function_decls.funs collected
      | Wrong ->  (* The variable is not bound to a closure. *)
        match A.check_approx_for_block approx with
        | Wrong -> collected  (* Ignore if not bound to a closure or block. *)
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
                    let new_inner_var =
                      Variable.create
                        (Variable.unique_name inside_var ^ "_field_"
                          ^ string_of_int field_index)
                    in
                    let extracted : extracted_field =
                      { new_inner_var;
                        outer_var;
                      }
                    in
                    VAP.Map.add (inside_var, Field field_index)
                      (Field extracted) collected
                  | None | Some _ -> collected
                in
                field_index + 1, collected)
              (0, collected)
              fields
          in
          collected)
    which_variables
    VAP.Map.empty

let from_function_decl ~which_variables ~env
      ~(function_decl : Flambda.function_declaration) : result option =
  let collected = collect_projections ~env ~which_variables in
  if VAP.Map.cardinal collected = 0 then
    None
  else
    (* Note that the [collect_projections] pass above doesn't actually look
       to see if a given variable was used in the body.  We keep track of
       that here in [used_new_inner_vars]. *)
    let used_new_inner_vars = Variable.Tbl.create 42 in
    (* CR mshinwell: use "iter" *)
    let _new_function_body =
      Flambda_iterators.map_toplevel_projections_to_expr_opt
        ~f:(fun (projection : Projection.t) ->
          match projection with
          | Project_var { closure; var; closure_id = _; } ->
            begin match
              VAP.Map.find (closure, Project_var var) collected
            with
            | exception Not_found -> None
            | Var_within_closure { new_inner_var; _ } ->
              Variable.Tbl.add used_new_inner_vars new_inner_var ();
              None
            | _ -> assert false
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
            | Closure { new_inner_var; _ } ->
              Variable.Tbl.add used_new_inner_vars new_inner_var ();
              None
            | _ -> assert false
            end
          | Field (field_index, var) ->
            begin match
              VAP.Map.find (var, Field field_index) collected
            with
            | exception Not_found -> None
            | Field { new_inner_var; _ } ->
              Variable.Tbl.add used_new_inner_vars new_inner_var ();
              None
            | _ -> assert false
            end)
        function_decl.body
    in
    let new_inner_to_new_outer_vars, new_bindings =
      VAP.Map.fold (fun (projecting_from, (projectee : Projectee.t))
            extracted (new_inner_to_new_outer_vars, new_bindings) ->
          let record ~new_inner_var ~new_outer_var ~defining_expr =
            let specialised_to : Flambda.specialised_to =
              { var = new_outer_var;
                projectee = Some (projecting_from, projectee);
              }
            in
            let new_inner_to_new_outer_vars =
              Variable.Map.add new_inner_var specialised_to
                new_inner_to_new_outer_vars
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
                (Variable.Map.add new_outer_var defining_expr
                  map_for_projecting_from)
                new_bindings
            in
            new_inner_to_new_outer_vars, new_bindings
          in
          match projectee, extracted with
          | Project_var var_within_closure,
              Var_within_closure { new_inner_var; closure_id; outer_var; } ->
            let new_outer_var = Variable.rename new_inner_var in
            if Variable.Tbl.mem used_new_inner_vars new_inner_var then
              let defining_expr : Flambda.named =
                Project_var {
                  closure = outer_var;
                  closure_id;
                  var = var_within_closure;
                }
              in
              record ~new_inner_var ~new_outer_var ~defining_expr
            else
              new_inner_to_new_outer_vars, new_bindings
          | Closure move_to,
              Closure { new_inner_var; start_from; outer_var; }->
            let new_outer_var = Variable.rename new_inner_var in
            if Variable.Tbl.mem used_new_inner_vars new_inner_var then
              let defining_expr : Flambda.named =
                Move_within_set_of_closures {
                  closure = outer_var;
                  start_from;
                  move_to;
                }
              in
              record ~new_inner_var ~new_outer_var ~defining_expr
            else
              new_inner_to_new_outer_vars, new_bindings
          | Field field_index,
              Field { new_inner_var; outer_var; } ->
            let new_outer_var = Variable.rename new_inner_var in
            if Variable.Tbl.mem used_new_inner_vars new_inner_var then
              let defining_expr : Flambda.named =
                Flambda.Prim (Pfield field_index, [outer_var], Debuginfo.none)
              in
              record ~new_inner_var ~new_outer_var ~defining_expr
            else
              new_inner_to_new_outer_vars, new_bindings
          | _ -> assert false)
        collected
        (Variable.Map.empty, Variable.Map.empty)
    in
    if Variable.Map.cardinal new_bindings < 1 then
      None
    else
      let projection_defns = new_bindings in
      let result =
        { projection_defns_indexed_by_outer_vars = projection_defns;
          new_inner_to_new_outer_vars;
        }
      in
      Some result
