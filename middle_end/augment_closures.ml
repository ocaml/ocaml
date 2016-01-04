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
(*   the GNU Library General Public License version 2.1, with the         *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env

type closures_in_free_vars =
  {
    new_var : Variable.t;
    closure_id : Closure_id.t;
    outside_var : Variable.t;
  }

type block_in_free_vars =
  {
    new_var : Variable.t;
    outside_var : Variable.t;
  }

module Closure_field =
  Identifiable.Make (Identifiable.Pair (Variable) (Var_within_closure))

module Block_field =
  Identifiable.Make (Identifiable.Pair (Variable) (Numbers.Int))

let freshened_var env v =
  Freshening.apply_variable (E.freshening env) v

let closures_in_variables ~env map acc =
  Variable.Map.fold (fun inside_var outside_var acc ->
      let approx = E.find_exn env (freshened_var env outside_var) in
      match A.check_approx_for_closure approx with
      | Ok (value_closure, _approx_var, _approx_symbol,
            value_set_of_closures) ->
        Var_within_closure.Map.fold (fun bound_var _ (closure_acc, block_acc) ->
            let new_var =
              Variable.create (Var_within_closure.unique_name bound_var)
            in
            let closure_acc =
              Closure_field.Map.add (inside_var, bound_var)
                { new_var; closure_id = value_closure.closure_id; outside_var }
                closure_acc
            in
            closure_acc, block_acc)
          value_set_of_closures.bound_vars acc
      | Wrong ->
        match A.check_approx_for_block approx with
        | Wrong ->
          acc  (* Ignore free_vars that aren't closures or blocks. *)
        | Ok (_tag, fields) ->
          let closure_acc, block_acc = acc in
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
                    (Variable.unique_name inside_var ^ "_field_" ^ string_of_int i)
                in
                block_acc :=
                  Block_field.Map.add (inside_var, i) { new_var; outside_var } !block_acc
              | Some _ ->
                ()
              | _ -> ())
            fields;
          closure_acc, !block_acc)
    map
    acc

let rewrite_set_of_closures
    ~env
    ~(set_of_closures:Flambda.set_of_closures) =
  let elts_in_free_vars =
    closures_in_variables ~env
      set_of_closures.free_vars
      (Closure_field.Map.empty, Block_field.Map.empty)
  in
  let elts_in_free_vars_and_specialised_args =
    closures_in_variables ~env
      set_of_closures.specialised_args
      elts_in_free_vars
  in
  let closures_in_free_vars,
      block_in_free_vars =
    elts_in_free_vars_and_specialised_args
  in
  if Closure_field.Map.is_empty closures_in_free_vars
    && Block_field.Map.is_empty block_in_free_vars
  then
    set_of_closures, Variable.Map.empty, Variable.Map.empty
  else
    let used_new_vars = Variable.Tbl.create 42 in
    let rewrite_function_decl
        (function_decl:Flambda.function_declaration) =
      let body =
        Flambda_iterators.map_toplevel_project_var_to_expr_opt
          ~f:(fun project_var ->
            match
              Closure_field.Map.find
                (project_var.closure, project_var.var)
                closures_in_free_vars
            with
            | exception Not_found ->
              None
            | { new_var } ->
              Variable.Tbl.add used_new_vars new_var ();
              Some (Flambda.Var new_var))
          function_decl.body
      in
      let body =
        Flambda_iterators.map_toplevel_named (function
            | (Prim (Pfield i, [v], _)) when
                Block_field.Map.mem (v, i) block_in_free_vars ->
              let { new_var } = Block_field.Map.find (v, i) block_in_free_vars in
              Variable.Tbl.add used_new_vars new_var ();
              Expr (Var new_var)
            | named ->
              named)
          body
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
      Closure_field.Map.fold
        (fun (_var, field) { new_var; closure_id; outside_var } (free_vars, add_closures) ->
           let intermediate_var =
             Variable.rename new_var
           in
           if Variable.Tbl.mem used_new_vars new_var then
             Variable.Map.add new_var intermediate_var free_vars,
             Variable.Map.add intermediate_var
               (Flambda.Project_var { Flambda.closure = outside_var; closure_id; var = field })
               add_closures
           else
             free_vars, add_closures)
        closures_in_free_vars
        (set_of_closures.free_vars,
         Variable.Map.empty)
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

let run ~env ~(set_of_closures:Flambda.set_of_closures) : Flambda.t option =
  if !Clflags.classic_inlining then None
  else
    let set_of_closures, add_closures, add_blocks =
      rewrite_set_of_closures
        ~env ~set_of_closures
    in
    if Variable.Map.is_empty add_closures &&
       Variable.Map.is_empty add_blocks then
      None
    else
      let expr =
        Variable.Map.fold Flambda.create_let
          add_closures
            (Flambda_utils.name_expr (Set_of_closures set_of_closures)
              ~name:"augment_closure")
      in
      let expr =
        Variable.Map.fold Flambda.create_let
          add_blocks expr
      in
      Some expr
