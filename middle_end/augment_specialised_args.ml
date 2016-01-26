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
  Flambda.expr Variable.Map.t

type what_to_specialise = {
  new_function_body : Flambda.expr;
  new_specialised_args : add_all_or_none_of_these_specialised_args list;
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
  let () = Pass_manager.register ~pass_name

  let create_wrapper ~fun_var ~(set_of_closures : Flambda.set_of_closures)
      ~(function_decl : Flambda.function_declaration) ~new_function_body
      ~new_args ~specialised_args =
    let specialised_args, new_bindings =
      (* For each new specialised argument of the main function
         chosen by [T], create a new variable to hold the definition
         of such argument, which will lie outside of the set of
         closures.  If the definition is itself just an existing
         variable, we could use that directly, but in fact we do not
         (because it means we don't have to worry if more than one new
         specialised arg has the same outer variable).  Add this
         information to the [specialised_args] map that will
         form the specialised args information for the new set of closures.

         Also construct a map giving new [let]-bindings that must be
         added outside of the set of closures, to bind the new outer
         variables. *)
      Variable.Map.fold (fun new_inner_var defn
            (specialised_args, new_bindings) ->
          if Variable.Map.mem specialised_args new_inner_var then
            Misc.fatal_errorf "Augment_specialised_args: inner name chosen \
                for a new specialised arg (%a) clashes with an existing \
                specialised arg: %a"
              Variable.print new_inner_var
              Flambda.print_set_of_closures set_of_closures
          else
            let new_outer_var =
              Variable.rename new_inner_var ~append:T.variable_suffix
            in
            let specialised_args =
              Variable.Map.add new_inner_var new_outer_var
                specialised_args
            in
            let new_bindings =
              assert (not (Variable.Map.mem new_outer_var new_bindings));
              Variable.Map.add new_outer_var defn new_bindings
            in
            specialised_args, new_bindings)
        new_args
        (specialised_args, new_bindings)
    in
    let new_outer_vars = Variable.Map.keys new_bindings in
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
      (* Bottom right to top left in diagram 1 above. *)
      let existing_specialised_args_inverse =
        Variable.Map.transpose_keys_and_data set_of_closures.specialised_args
      in
      Variable.Map.mapi (fun existing_outer_var existing_inner_var ->
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
    let wrapper_body : Flambda.t =
      let apply =
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
            Flambda_utils.toplevel_substitution
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
    new_fun_var, new_function_decl, specialised_args, new_bindings

  let rewrite_function_decl ~env ~backend ~fun_var ~set_of_closures
      ~(function_decl : Flambda.function_declaration)
      ~funs ~specialised_args ~free_vars ~new_bindings =
    let done_nothing () =
      let funs = Variable.Map.add fun_var function_decl funs in
      funs, specialised_args, new_bindings,
        free_vars, Inlining_cost.Benefit.zero
    in
    if function_decl.stub then
      done_nothing ()
    else
      let closure_id = Closure_id.wrap fun_var in
      let what_to_specialise =
        T.what_to_specialise ~env ~closure_id ~function_decl ~set_of_closures
      in
      match what_to_specialise with
      | None -> done_nothing ()
      | Some what_to_specialise ->
        let new_specialised_args =
          let module Backend = (val backend : Backend_intf.S) in
          let max_args = Backend.max_sensible_number_of_args in
          List.fold_left (fun add_all_or_none (num_params, new_spec_args) ->
              (* - It is important to limit the number of arguments added:
                 if arguments end up being passed on the stack, tail call
                 optimization will be disabled (see asmcomp/selectgen.ml).
                 - For each group of new specialised args provided by [T],
                 either all or none of them will be added.  (This is to
                 avoid the situation where we add extra arguments but yet
                 fail to eliminate an original one.) *)
              let num_new_args = Variable.Map.cardinal add_all_or_none in
              let num_params = num_params + num_new_args in
              (* CR mshinwell: consider sorting the groups in some way,
                 maybe by decreasing total benefit. *)
              if num_params > max_args then
                new_spec_args
              else
                try
                  let new_spec_args =
                    Variable.Map.disjoint_union new_spec_args add_all_or_none
                  in
                  num_params, new_spec_args
                with _exn ->
                  Misc.fatal_errorf "Augment_specialised_args: groups of \
                      new specialised args overlap: %a"
                    (Format.pp_print_list (Variable.Map.print Variable.print))
                    what_to_specialise.new_specialised_args)
            what_to_specialise.new_specialised_args
            (List.length function_decl.params, Variable.Map.empty)
        in
        if Variable.Map.cardinal new_specialised_args < 1 then
          done_nothing ()
        else
          (* [new_specialised_args] now maps from the names chosen by [T]
             for the new specialised args to their definitions. *)
          let new_fun_var, wrapper, specialised_args, new_bindings =
            create_wrapper ~fun_var ~set_of_closures ~function_decl
              ~new_args ~specialised_args ~new_bindings
          in
          let all_params =
            let new_params =
              (* The extra parameters on the main function are named
                 according to the decisions made by [T].  Note that the
                 ordering used here must match [create_wrapper], above. *)
              Variable.Set.elements (Variable.Map.keys new_specialised_args)
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
          let funs =
            Variable.Map.add new_fun_var rewritten_function_decl
              (Variable.Map.add fun_var wrapper funs)
          in
          let free_vars =
            try
              Variable.Map.disjoint_union
                what_to_specialise.additional_free_vars
                free_vars
                ~eq:Variable.equal
            with _exn ->
              Misc.fatal_errorf "Augment_specialised_args: non-disjoint \
                  [free_vars] sets: %a vs. %a"
                Variable.Set.print what_to_specialise.free_vars
                Variable.Set.print free_vars
          in
          funs, specialised_args, free_vars, new_bindings,
            what_to_specialise.total_benefit

  let rewrite_set_of_closures_core ~backend
        ~(set_of_closures : Flambda.set_of_closures) =
    if not (T.precondition set_of_closures) then
      None
    else
      let funs, specialised_args, free_vars, new_bindings, total_benefit =
        Variable.Map.mapi
          (fun fun_var function_decl
               (funs, specialised_args, free_vars, new_bindings,
                total_benefit) ->
            let funs, specialised_args, free_vars, new_bindings, benefit =
              rewrite_function_decl ~backend ~set_of_closures ~fun_var
                ~function_decl ~funs ~specialised_args
                ~new_bindings
            in
            let total_benefit =
              Inlining_cost.Benefit.(+) benefit total_benefit
            in
            funs, specialised_args, free_vars, new_bindings, total_benefit)
          (set_of_closures.function_decls.funs,
            Variable.Map.empty,
            set_of_closures.free_vars,
            Variable.Map.empty,
            Inlining_cost.Benefit.zero)
      in
      let function_decls =
        Flambda.update_function_declarations function_decls ~funs
      in
      assert (Variable.Map.cardinal specialised_args
        >= Variable.Map.cardinal set_of_closures.specialised_args);
      let set_of_closures =
        Flambda.create_set_of_closures
          ~function_decls
          ~free_vars
          ~specialised_args
      in
      let expr =
        Variable.Map.fold (fun outside_var spec_arg_defn expr ->
            Flambda.create_let outside_var spec_arg_defn expr)
          bindings
          (Flambda_utils.name_expr (Set_of_closures set_of_closures)
            ~name:T.pass_name)
      in
      Some (expr, total_benefit)

  let rewrite_set_of_closures ~env ~backend ~set_of_closures =
    Pass_manager.with_dump ~pass_name ~input:set_of_closures
      ~print_input:Flambda.print_set_of_closures
      ~print_output:Flambda.print
      ~f:(fun () ->
        rewrite_set_of_closures_core ~env ~backend ~set_of_closures)
end
