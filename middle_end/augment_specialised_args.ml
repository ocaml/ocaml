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

type add_all_or_none_of_these_specialised_args =
  Flambda.named Variable.Map.t

type what_to_specialise = {
  new_function_body : Flambda.expr;
  removed_free_vars : Variable.Set.t;
  new_specialised_args_indexed_by_new_outer_vars
    : add_all_or_none_of_these_specialised_args list;
  new_inner_to_new_outer_vars : Variable.t Variable.Map.t;
  total_benefit : Inlining_cost.Benefit.t;
}

module type S = sig
  val pass_name : string
  val variable_suffix : string

  val precondition : set_of_closures:Flambda.set_of_closures -> bool

  val what_to_specialise
     : env:Inline_and_simplify_aux.Env.t
    -> closure_id:Closure_id.t
    -> function_decl:Flambda.function_declaration
    -> set_of_closures:Flambda.set_of_closures
    -> what_to_specialise option
end

module Make (T : S) = struct
  let () = Pass_wrapper.register ~pass_name:T.pass_name

  let create_wrapper ~fun_var ~(set_of_closures : Flambda.set_of_closures)
      ~(function_decl : Flambda.function_declaration)
      ~new_specialised_args_indexed_by_new_outer_vars
      ~new_inner_to_new_outer_vars =
    let new_fun_var = Variable.rename fun_var ~append:T.variable_suffix in
    (* To avoid increasing the free variables of the wrapper, for
       general cleanliness, we restate the definitions of the
       newly-specialised arguments in the wrapper itself in terms of the
       original specialised arguments.  The variables bound to these
       definitions are called the "specialised args bound in the wrapper".
       Note that the domain of [params_renaming] is a (non-strict) superset
       of the "inner vars" of the original specialised args. *)
    let params_renaming =
      Variable.Map.of_list
        (List.map (fun param ->
            let new_param = Variable.rename param ~append:T.variable_suffix in
            param, new_param)
          function_decl.params)
    in
    let wrapper_params =
      List.map (fun param -> Variable.Map.find param params_renaming)
        function_decl.params
    in
    (*  1. Renaming of existing specialised arguments: these form the
        parameters of the wrapper.

        Existing specialised    set_of_closures.        Existing outer
        arguments of the        -------------------->   specialised arguments
        main function             spec_args             of the main function

                                                                |
                                                 existing_outer |
                 +                               _vars_to_      |
                                                 wrapper_params |
                                                 _renaming      v

        Other parameters of     -------------------->   Parameters of the
        the main function          params_renaming      wrapper, some of
                                                        which will be
                                                        specialised args
    *)
    let existing_outer_vars_to_wrapper_params_renaming =
      let existing_specialised_args_inverse =
        Variable.Map.transpose_keys_and_data set_of_closures.specialised_args
      in
      Variable.Map.map (fun existing_inner_var ->
          match Variable.Map.find existing_inner_var params_renaming with
          | exception Not_found -> assert false
          | wrapper_param -> wrapper_param)
        existing_specialised_args_inverse
    in
    (*  2. Renaming of newly-introduced specialised arguments: the fresh
        variables are used for the [let]-bindings in the wrapper.

        Specialised args
        bound in the wrapper

                ^
                |
                |
                |

        New specialised args                            New specialised
        inner (which are all    -------------------->   args outer
        parameters of the       new_inner_to_new
        main function)            _outer_vars

    *)
    let new_outer_vars_to_spec_args_bound_in_the_wrapper_renaming =
      (* Bottom right to top left in diagram 2 above. *)
      Variable.Map.fold (fun new_inner_var new_outer_var renaming ->
          let inner_var_of_wrapper =
            Variable.rename new_inner_var ~append:T.variable_suffix
          in
          assert (not (Variable.Map.mem new_outer_var renaming));
          Variable.Map.add new_outer_var inner_var_of_wrapper renaming)
        new_inner_to_new_outer_vars
        Variable.Map.empty
    in
    let spec_args_bound_in_the_wrapper =
      (* N.B.: in the order matching the new specialised argument parameters
         to the main function. *)
      Variable.Map.data
        new_outer_vars_to_spec_args_bound_in_the_wrapper_renaming
    in
    let wrapper_body : Flambda.expr =
      let apply : Flambda.expr =
        Apply {
          func = new_fun_var;
          args = wrapper_params @ spec_args_bound_in_the_wrapper;
          kind = Direct (Closure_id.wrap new_fun_var);
          dbg = Debuginfo.none;
          inline = Default_inline;
        }
      in
      Variable.Map.fold (fun new_outer_var
            defining_expr_using_existing_outer_vars wrapper_body ->
          (* The defining expression is currently in terms of the
             existing outer vars (the variables to which the existing
             specialised args were specialised); we must rewrite it to use
             the parameters of the wrapper. *)
          let defining_expr =
            Flambda_utils.toplevel_substitution_named
              existing_outer_vars_to_wrapper_params_renaming
              defining_expr_using_existing_outer_vars
          in
          match
            Variable.Map.find new_outer_var
              new_outer_vars_to_spec_args_bound_in_the_wrapper_renaming
          with
          | exception Not_found -> assert false
          | new_inner_var_of_wrapper ->
            Flambda.create_let new_inner_var_of_wrapper defining_expr
              wrapper_body)
      new_specialised_args_indexed_by_new_outer_vars
      apply
    in
    let new_function_decl =
      Flambda.create_function_declaration
        ~params:wrapper_params
        ~body:wrapper_body
        ~stub:true
        ~dbg:Debuginfo.none
        ~inline:Default_inline
        ~is_a_functor:false
    in
    new_fun_var, new_function_decl

  let rewrite_function_decl ~env ~backend ~fun_var ~set_of_closures
      ~(function_decl : Flambda.function_declaration) =
    if function_decl.stub then
      None
    else
      let closure_id = Closure_id.wrap fun_var in
      let what_to_specialise =
        T.what_to_specialise ~env ~closure_id ~function_decl ~set_of_closures
      in
      match what_to_specialise with
      | None -> None
      | Some what_to_specialise ->
        let (_ : int), new_specialised_args_indexed_by_new_outer_vars =
          let module Backend = (val backend : Backend_intf.S) in
          let max_args = Backend.max_sensible_number_of_arguments in
          List.fold_left (fun (num_params, new_spec_args) add_all_or_none ->
              (* - It is important to limit the number of arguments added:
                 if arguments end up being passed on the stack, tail call
                 optimization will be disabled (see asmcomp/selectgen.ml).
                 - For each group of new specialised args provided by [T],
                 either all or none of them will be added.  (This is to
                 avoid the situation where we add extra arguments but yet
                 fail to eliminate an original one.) *)
              let num_new_args =
                (* CR mshinwell: For [Unbox_specialised_args] this doesn't
                   take into account the fact that we expect to delete the
                   specialised argument(s) being unboxed (although we might
                   not be able to, so this is currently conservative). *)
                Variable.Map.cardinal add_all_or_none
              in
              let new_num_params = num_params + num_new_args in
              (* CR mshinwell: consider sorting the groups in some way,
                 maybe by decreasing total benefit. *)
              if new_num_params > max_args then
                num_params, new_spec_args
              else
                try
                  let new_spec_args =
                    Variable.Map.disjoint_union new_spec_args add_all_or_none
                  in
                  new_num_params, new_spec_args
                with _exn ->
                  Misc.fatal_error "Augment_specialised_args: groups of \
                      new specialised args overlap")
            (List.length function_decl.params, Variable.Map.empty)
            what_to_specialise.new_specialised_args_indexed_by_new_outer_vars
        in
        if Variable.Map.cardinal
            new_specialised_args_indexed_by_new_outer_vars < 1
        then
          None
        else
          let new_inner_to_new_outer_vars =
            what_to_specialise.new_inner_to_new_outer_vars
          in
          let new_fun_var, wrapper =
            create_wrapper ~fun_var ~set_of_closures ~function_decl
              ~new_specialised_args_indexed_by_new_outer_vars
              ~new_inner_to_new_outer_vars
          in
          let all_params =
            let new_params =
              (* The extra parameters on the main function are named
                 according to the decisions made by [T].  Note that the
                 ordering used here must match [create_wrapper], above. *)
              Variable.Set.elements (Variable.Map.keys
                new_specialised_args_indexed_by_new_outer_vars)
            in
            function_decl.params @ new_params
          in
          let rewritten_function_decl =
            Flambda.create_function_declaration
              ~params:all_params
              ~body:what_to_specialise.new_function_body
              ~stub:function_decl.stub
              ~dbg:function_decl.dbg
              ~inline:function_decl.inline
              ~is_a_functor:function_decl.is_a_functor
          in
          Some (
            new_fun_var, rewritten_function_decl, wrapper,
              new_specialised_args_indexed_by_new_outer_vars,
              new_inner_to_new_outer_vars,
              what_to_specialise.removed_free_vars,
              what_to_specialise.total_benefit)

  let rewrite_set_of_closures_core ~backend ~env
        ~(set_of_closures : Flambda.set_of_closures) =
    if not (T.precondition ~set_of_closures) then
      None
    else
      let funs, new_specialised_arg_defns_indexed_by_new_outer_vars,
          specialised_args, removed_free_vars, total_benefit =
        Variable.Map.fold
          (fun fun_var function_decl
                (funs, new_specialised_args_indexed_by_new_outer_vars,
                 new_inner_to_new_outer_vars, removed_free_vars,
                 total_benefit) ->
            match
              rewrite_function_decl ~backend ~env ~set_of_closures ~fun_var
                ~function_decl
            with
            | None ->
              let funs = Variable.Map.add fun_var function_decl funs in
              funs, new_specialised_args_indexed_by_new_outer_vars,
                new_inner_to_new_outer_vars, removed_free_vars,
                total_benefit
            | Some (
                new_fun_var, rewritten_function_decl, wrapper,
                new_specialised_args_indexed_by_new_outer_vars',
                new_inner_to_new_outer_vars', removed_free_vars',
                benefit) ->
              let funs =
                assert (not (Variable.Map.mem new_fun_var funs));
                Variable.Map.add new_fun_var rewritten_function_decl
                  (Variable.Map.add fun_var wrapper funs)
              in
              let new_specialised_args_indexed_by_new_outer_vars =
                Variable.Map.disjoint_union
                  new_specialised_args_indexed_by_new_outer_vars
                  new_specialised_args_indexed_by_new_outer_vars'
              in
              let new_inner_to_new_outer_vars =
                Variable.Map.disjoint_union new_inner_to_new_outer_vars
                  new_inner_to_new_outer_vars'
              in
              let removed_free_vars =
                Variable.Set.union removed_free_vars removed_free_vars'
              in
              let total_benefit =
                Inlining_cost.Benefit.(+) benefit total_benefit
              in
              funs, new_specialised_args_indexed_by_new_outer_vars,
                new_inner_to_new_outer_vars, removed_free_vars,
                total_benefit)
          set_of_closures.function_decls.funs
          (Variable.Map.empty,
            Variable.Map.empty,
            set_of_closures.specialised_args,
            Variable.Set.empty,
            Inlining_cost.Benefit.zero)
      in
      let function_decls =
        Flambda.update_function_declarations set_of_closures.function_decls
          ~funs
      in
      assert (Variable.Map.cardinal specialised_args
        >= Variable.Map.cardinal set_of_closures.specialised_args);
      let free_vars =
        Variable.Map.filter (fun inner_var _outer_var ->
            not (Variable.Set.mem inner_var removed_free_vars))
          set_of_closures.free_vars
      in
      let set_of_closures =
        Flambda.create_set_of_closures
          ~function_decls
          ~free_vars
          ~specialised_args
      in
      let expr =
        Variable.Map.fold (fun new_outer_var spec_arg_defn expr ->
            Flambda.create_let new_outer_var spec_arg_defn expr)
          new_specialised_arg_defns_indexed_by_new_outer_vars
          (Flambda_utils.name_expr (Set_of_closures set_of_closures)
            ~name:T.pass_name)
      in
      Some (expr, total_benefit)

  let rewrite_set_of_closures ~backend ~env ~set_of_closures =
    Pass_wrapper.with_dump ~pass_name:T.pass_name ~input:set_of_closures
      ~print_input:Flambda.print_set_of_closures
      ~print_output:(fun ppf (expr, _benefit) -> Flambda.print ppf expr)
      ~f:(fun () ->
        rewrite_set_of_closures_core ~backend ~env ~set_of_closures)
end
