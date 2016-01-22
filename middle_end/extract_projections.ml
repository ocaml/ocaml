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

(* CR-soon mshinwell: Refactor this module to use something like
   [Flambda_iterators.projection] throughout. *)

type var_within_closures_in_free_vars = {
  new_var : Variable.t;
  closure_id : Closure_id.t;
  outside_var : Variable.t;
}

type closures_in_free_vars = {
  new_var : Variable.t;
  start_from : Closure_id.t;
  outside_var : Variable.t;
}

type block_in_free_vars = {
  new_var : Variable.t;
  outside_var : Variable.t;
}

module Var_within_closure_field =
  Identifiable.Make (Identifiable.Pair (Variable) (Var_within_closure))

module Closure_field =
  Identifiable.Make (Identifiable.Pair (Variable) (Closure_id))

module Block_field =
  Identifiable.Make (Identifiable.Pair (Variable) (Numbers.Int))

let freshened_var env v =
  Freshening.apply_variable (E.freshening env) v

let closures_in_variables ~env map acc
      : var_within_closures_in_free_vars Var_within_closure_field.Map.t
          * closures_in_free_vars Closure_field.Map.t
          * block_in_free_vars Block_field.Map.t =
  Variable.Map.fold (fun inside_var outside_var
        (var_within_closure_acc, closure_acc, block_acc) ->
      let approx = E.find_exn env (freshened_var env outside_var) in
      match A.check_approx_for_closure approx with
      | Ok (value_closure, _approx_var, _approx_symbol,
            value_set_of_closures) ->
        let var_within_closure_acc =
          Var_within_closure.Map.fold
            (fun bound_var _ acc ->
              let new_var =
                Variable.create (Var_within_closure.unique_name bound_var)
              in
              Var_within_closure_field.Map.add (inside_var, bound_var)
                { new_var; closure_id = value_closure.closure_id; outside_var;
                }
                acc)
            value_set_of_closures.bound_vars var_within_closure_acc
        in
        let closure_acc =
          Variable.Map.fold (fun fun_var _ acc ->
              let new_var = Variable.rename fun_var in
              let start_from = value_closure.closure_id in
              let move_to = Closure_id.wrap fun_var in
              (* For the moment represent all projections of closures as
                 moves from the original closure ID. *)
              (* CR-someday mshinwell: consider using the set-of-closures-var
                 ("_approx_var" above) instead.  See handling in
                 inline_and_simplify.ml, e.g. simplify_project_var: need
                 to check the variable is in scope. *)
              Closure_field.Map.add (inside_var, move_to)
                ({ new_var; start_from; outside_var; } : closures_in_free_vars)
                acc)
            value_set_of_closures.function_decls.funs closure_acc
        in
        var_within_closure_acc, closure_acc, block_acc
      | Wrong ->
        match A.check_approx_for_block approx with
        | Wrong ->
          acc  (* Ignore free_vars that aren't closures or blocks. *)
        | Ok (_tag, fields) ->
          let block_acc = ref block_acc in
          Array.iteri (fun i approx ->
              (* CR-soon pchambart: should we restrict only to cases
                 when the field is aliased to a variable outside
                 of the closure (i.e. when we can certainly remove
                 the allocation of the block) ?
                 Note that this may prevent cases with imbricated
                 closures from benefiting from this transformations.
                 mshinwell: What word was "imbricated" supposed to be?
              *)
              match approx.A.var with
              | Some v when E.mem env v ->
                let new_var =
                  Variable.create
                    (Variable.unique_name inside_var ^ "_field_"
                      ^ string_of_int i)
                in
                block_acc := Block_field.Map.add (inside_var, i)
                  { new_var; outside_var } !block_acc
              | Some _ -> ()
              | _ -> ())
            fields;
          var_within_closure_acc, closure_acc, !block_acc)
    map
    acc

let rewrite_set_of_closures
    ~env
    ~(set_of_closures:Flambda.set_of_closures) =
  let elts_in_free_vars =
    closures_in_variables ~env
      set_of_closures.free_vars
      (Var_within_closure_field.Map.empty, Closure_field.Map.empty,
        Block_field.Map.empty)
  in
  let elts_in_free_vars_and_specialised_args =
    closures_in_variables ~env
      set_of_closures.specialised_args
      elts_in_free_vars
  in
  let var_within_closures_in_free_vars,
      closures_in_free_vars,
      block_in_free_vars =
    elts_in_free_vars_and_specialised_args
  in
  if Var_within_closure_field.Map.is_empty var_within_closures_in_free_vars
    && Closure_field.Map.is_empty closures_in_free_vars
    && Block_field.Map.is_empty block_in_free_vars
  then
    set_of_closures, Variable.Map.empty, Variable.Map.empty
  else
    let used_new_vars = Variable.Tbl.create 42 in
    let rewrite_function_decl (function_decl : Flambda.function_declaration) =
      let body =
        Flambda_iterators.map_toplevel_projections_to_expr_opt
          ~f:(fun (projection : Flambda_iterators.projection) ->
            match projection with
            | Project_var { closure; var; closure_id = _; } ->
              begin match
                Var_within_closure_field.Map.find (closure, var)
                  var_within_closures_in_free_vars
              with
              | exception Not_found -> None
              | { new_var; _ } ->
                Variable.Tbl.add used_new_vars new_var ();
                Some (Flambda.Var new_var)
              end
            | Project_closure _project_closure ->
              (* CR-soon mshinwell: implement this *)
              None
            | Move_within_set_of_closures
                { closure; move_to; start_from = _; } ->
              begin match
                Closure_field.Map.find (closure, move_to) closures_in_free_vars
              with
              | exception Not_found -> None
              | { new_var; _ } ->
                Variable.Tbl.add used_new_vars new_var ();
                Some (Flambda.Var new_var)
              end
            | Field (i, v) ->
              if not (Block_field.Map.mem (v, i) block_in_free_vars) then
                None
              else
                let { new_var; _ } =
                  Block_field.Map.find (v, i) block_in_free_vars
                in
                Variable.Tbl.add used_new_vars new_var ();
                Some (Flambda.Var new_var))
          function_decl.body
      in
      Flambda.create_function_declaration
        ~body
        ~inline:function_decl.inline
        ~params:function_decl.params
        ~stub:function_decl.stub
        ~dbg:function_decl.dbg
        ~is_a_functor:function_decl.is_a_functor
    in
    let funs =
      Variable.Map.map
        rewrite_function_decl
        set_of_closures.function_decls.funs
    in
    let function_decls =
      Flambda.update_function_declarations ~funs
        set_of_closures.function_decls
    in
    let free_vars, add_closures =
      Var_within_closure_field.Map.fold
        (fun (_var, field) { new_var; closure_id; outside_var; }
              (free_vars, add_closures) ->
          let intermediate_var = Variable.rename new_var in
          if Variable.Tbl.mem used_new_vars new_var then
            let defining_expr : Flambda.named =
              Project_var {
                closure = outside_var;
                closure_id;
                var = field;
              }
            in
            Variable.Map.add new_var intermediate_var free_vars,
              Variable.Map.add intermediate_var defining_expr add_closures
          else
            free_vars, add_closures)
        var_within_closures_in_free_vars
        (set_of_closures.free_vars,
         Variable.Map.empty)
    in
    let free_vars, add_closures =
      Closure_field.Map.fold
        (fun (_var, move_to)
              ({ new_var; start_from; outside_var; } : closures_in_free_vars)
              (free_vars, add_closures) ->
          let intermediate_var = Variable.rename new_var in
          if Variable.Tbl.mem used_new_vars new_var then
            let defining_expr : Flambda.named =
              Move_within_set_of_closures {
                closure = outside_var;
                start_from;
                move_to;
              }
            in
            Variable.Map.add new_var intermediate_var free_vars,
              Variable.Map.add intermediate_var defining_expr add_closures
          else
            free_vars, add_closures)
        closures_in_free_vars
        (free_vars, add_closures)
    in
    let free_vars, add_blocks =
      Block_field.Map.fold
        (fun (_var, field) { new_var; outside_var } (free_vars, add_blocks) ->
           let intermediate_var =
             Variable.rename new_var
           in
           if Variable.Tbl.mem used_new_vars new_var then
             Variable.Map.add new_var intermediate_var free_vars,
             Variable.Map.add intermediate_var
               (Flambda.Prim (Pfield field, [outside_var], Debuginfo.none))
               add_blocks
           else
             free_vars, add_blocks)
        block_in_free_vars
        (free_vars,
         Variable.Map.empty)
    in
    Flambda.create_set_of_closures
      ~function_decls
      ~free_vars
      ~specialised_args:set_of_closures.specialised_args,
    add_closures, add_blocks
