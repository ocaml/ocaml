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
  new_inner_to_new_outer_vars : Flambda.specialised_to Variable.Map.t;
}

module type S = sig
  val pass_name : string
  val variable_suffix : string

  type user_data

  val precondition
     : backend:(module Backend_intf.S)
    -> env:Inline_and_simplify_aux.Env.t
    -> set_of_closures:Flambda.set_of_closures
    -> user_data option

  val what_to_specialise
     : env:Inline_and_simplify_aux.Env.t
    -> closure_id:Closure_id.t
    -> function_decl:Flambda.function_declaration
    -> set_of_closures:Flambda.set_of_closures
    -> user_data:user_data
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
    (* CR mshinwell: update comment to reflect the fact that we need to
       also apply the inverse free_vars mapping, since we're pushing
       expressions down into the function body. *)
    let existing_outer_vars_to_wrapper_params_renaming =
      let existing_specialised_args_inverse =
        let specialised_args =
          (* There might be more than one specialised arg coming from
             the same existing outer variable, but that doesn't matter.
             What does matter is that this situation might occur across
             functions (one arg in one function and one arg in another
             both specialised to the same thing), and we must choose the
             correct arg in that case. *)
          (* CR-soon mshinwell: Maybe this nonsense could be improved if
             [Extract_projections] didn't rewrite the definitions to use
             the outer variables? *)
          Variable.Map.filter_map set_of_closures.specialised_args
            ~f:(fun inner_var (spec_to : Flambda.specialised_to) ->
              match Variable.Map.find inner_var params_renaming with
              | exception Not_found -> None
              | _ -> Some spec_to.var)
        in
        Variable.Map.transpose_keys_and_data specialised_args
      in
      let existing_free_vars_inverse =
        Variable.Map.transpose_keys_and_data set_of_closures.free_vars
      in
      let for_spec_args =
        Variable.Map.filter_map existing_specialised_args_inverse
          ~f:(fun _existing_outer_var existing_inner_var ->
            match Variable.Map.find existing_inner_var params_renaming with
            | exception Not_found ->
              (* This specialised argument is not an argument of the
                 current function. *)
Format.eprintf "Existing inner spec arg %a not a parameter of %a\n%!"
  Variable.print existing_inner_var Variable.print fun_var;
              None
            | wrapper_param ->
Format.eprintf "Existing outer spec arg %a (inner spec arg %a) maps to wrapper param %a\n%!"
  Variable.print _existing_outer_var
  Variable.print existing_inner_var Variable.print wrapper_param;
              Some wrapper_param)
      in
      Variable.Map.disjoint_union for_spec_args existing_free_vars_inverse
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
      Variable.Map.fold (fun new_inner_var (spec_to : Flambda.specialised_to)
                renaming ->
          let new_outer_var = spec_to.var in
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
    new_fun_var, new_function_decl, params_renaming

  let rewrite_function_decl ~env ~backend ~fun_var ~set_of_closures
      ~(function_decl : Flambda.function_declaration) ~user_data =
Format.eprintf "ASA.rewrite_function_decl %a\n%!"
  Flambda.print_function_declaration (fun_var, function_decl);
    if function_decl.stub then
      None
    else
      let closure_id = Closure_id.wrap fun_var in
      let what_to_specialise =
        T.what_to_specialise ~env ~closure_id ~function_decl ~set_of_closures
          ~user_data
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
          let new_fun_var, wrapper, params_renaming =
            create_wrapper ~fun_var ~set_of_closures ~function_decl
              ~new_specialised_args_indexed_by_new_outer_vars
              ~new_inner_to_new_outer_vars
          in
          let all_params =
            let new_params =
              Variable.Set.elements (Variable.Map.keys
                new_inner_to_new_outer_vars)
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
Format.eprintf "ASA (%s) rewritten_function_decl %a\n%!"
  T.pass_name Flambda.print_function_declaration (new_fun_var, rewritten_function_decl);
          Some (
            new_fun_var, rewritten_function_decl, wrapper,
              new_specialised_args_indexed_by_new_outer_vars,
              new_inner_to_new_outer_vars,
              what_to_specialise.removed_free_vars,
              params_renaming)

  let rewrite_set_of_closures_core ~backend ~env
        ~(set_of_closures : Flambda.set_of_closures) =
Format.eprintf "Augment_specialised_args (%s)@ \nstarting with %a\n%!"
  T.pass_name Flambda.print_set_of_closures set_of_closures;
    match T.precondition ~backend ~env ~set_of_closures with
    | None -> None
    | Some user_data ->
      let funs, new_specialised_arg_defns_indexed_by_new_outer_vars,
          specialised_args, _removed_free_vars, done_something =
        Variable.Map.fold
          (fun fun_var function_decl
                (funs, new_specialised_args_indexed_by_new_outer_vars,
                 new_inner_to_new_outer_vars, removed_free_vars,
                 done_something) ->
            match
              rewrite_function_decl ~backend ~env ~set_of_closures ~fun_var
                ~function_decl ~user_data
            with
            | None ->
              let funs = Variable.Map.add fun_var function_decl funs in
              funs, new_specialised_args_indexed_by_new_outer_vars,
                new_inner_to_new_outer_vars, removed_free_vars, done_something
            | Some (
                new_fun_var, rewritten_function_decl, wrapper,
                new_specialised_args_indexed_by_new_outer_vars',
                new_inner_to_new_outer_vars', removed_free_vars',
                params_renaming) ->
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
                (* This will form the augmentation to the existing
                   specialised_args of the set of closures.  We must include
                   not only the new arguments requested by [T] but also
                   the parameters of the wrapper corresponding to the
                   existing specialised args (irrespective of whether any
                   particular specialised arg is being augmented or not). *)
                let for_new_arguments =
                  Variable.Map.disjoint_union new_inner_to_new_outer_vars
                    new_inner_to_new_outer_vars'
                in
                let for_existing_arguments =
                  Variable.Map.fold (fun inner_var outer_var for_existing ->
                      match Variable.Map.find inner_var params_renaming with
                      | exception Not_found ->
                        (* Not a parameter of this [function_decl]. *)
                        for_existing
                      | wrapper_param ->
                        assert (not (Variable.Map.mem wrapper_param
                            for_existing));
                        Variable.Map.add wrapper_param outer_var
                          for_existing)
                    set_of_closures.specialised_args
                    Variable.Map.empty
                in
                Variable.Map.disjoint_union for_new_arguments
                  for_existing_arguments
              in
              let removed_free_vars =
                Variable.Set.union removed_free_vars removed_free_vars'
              in
              funs, new_specialised_args_indexed_by_new_outer_vars,
                new_inner_to_new_outer_vars, removed_free_vars, true)
          set_of_closures.function_decls.funs
          (Variable.Map.empty,
            Variable.Map.empty,
            set_of_closures.specialised_args,
            Variable.Set.empty,
            false)
      in
      if not done_something then
        None
      else
        let function_decls =
          Flambda.update_function_declarations set_of_closures.function_decls
            ~funs
        in
        let all_free_variables =
          Variable.Map.fold (fun fun_var function_decl all_free_variables ->
Format.eprintf "DECL %a\n%!" Flambda.print_function_declaration (fun_var, function_decl);
              let free_variables =
                Flambda_utils.variables_bound_by_the_closure
                  (Closure_id.wrap fun_var)
                  function_decls
              in
              Variable.Set.union free_variables all_free_variables)
            function_decls.funs
            Variable.Set.empty
        in
        assert (Variable.Map.cardinal specialised_args
          >= Variable.Map.cardinal set_of_closures.specialised_args);
        let free_vars =
          Variable.Map.filter (fun inner_var _outer_var ->
              Variable.Set.mem inner_var all_free_variables)
            set_of_closures.free_vars
        in
Format.eprintf "all_free_variables { %a } free_vars %a\n%!"
  Variable.Set.print all_free_variables
  (Variable.Map.print Variable.print) free_vars;
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
Format.eprintf "Augment_specialised_args (%s)@ \nresult %a\n%!"
  T.pass_name Flambda.print expr;
        Some expr

  let rewrite_set_of_closures ~backend ~env ~set_of_closures =
    Pass_wrapper.with_dump ~pass_name:T.pass_name ~input:set_of_closures
      ~print_input:Flambda.print_set_of_closures
      ~print_output:Flambda.print
      ~f:(fun () ->
        rewrite_set_of_closures_core ~backend ~env ~set_of_closures)
end
