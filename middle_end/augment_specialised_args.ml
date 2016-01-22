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

let pass_name = "unbox-closures"
let () = Clflags.all_passes := pass_name :: !Clflags.all_passes

let rewrite_function_decl
    ~closure_id
    ~(function_decls : Flambda.function_declarations)
    ~(function_decl : Flambda.function_declaration)
    ~(free_vars : Variable.t Variable.Map.t)
    ~additional_specialised_args =
  let params_for_vars_bound_by_closure =
    Variable.Map.of_set
      (fun var -> Variable.rename var ~append:T.variable_suffix)
      (Flambda_utils.variables_bound_by_the_closure closure_id function_decls)
  in
  let closure_element_params =
    List.map snd (Variable.Map.bindings params_for_vars_bound_by_closure)
  in
  let all_params = function_decl.params @ closure_element_params in
  let body_using_params_not_closures =
    Flambda_utils.toplevel_substitution
      params_for_vars_bound_by_closure
      function_decl.body
  in
  let additional_specialised_args =
    Variable.Map.fold
      (fun var_bound_by_closure new_param
        additional_specialised_args ->
        let specialised_to =
          Variable.Map.find var_bound_by_closure free_vars
        in
        Variable.Map.add new_param specialised_to additional_specialised_args)
      params_for_vars_bound_by_closure
      additional_specialised_args
  in
  additional_specialised_args,
  Flambda.create_function_declaration
    ~params:all_params
    ~body:body_using_params_not_closures
    ~stub:function_decl.stub
    ~dbg:function_decl.dbg
    ~inline:function_decl.inline
    ~is_a_functor:function_decl.is_a_functor

let create_wrapper
    ~(function_decls : Flambda.function_declarations)
    ~(function_decl : Flambda.function_declaration)
    ~fun_var
    ~specialised_args
    ~additional_specialised_args =
  let closure_id = Closure_id.wrap fun_var in
  let new_fun_var = Variable.rename fun_var ~append:T.variable_suffix in
  let wrapper_params =
    List.map (fun param -> Variable.rename param ~append:"_wrapper_param")
      function_decl.params
  in
  let additional_specialised_args =
    List.fold_left2 (fun additional_specialised_args wrapper_param param ->
        match Variable.Map.find param specialised_args with
        | exception Not_found ->
          additional_specialised_args
        | outside_var ->
          Variable.Map.add wrapper_param outside_var
            additional_specialised_args)
      additional_specialised_args wrapper_params function_decl.params
  in
  let extra_args =
    Variable.Set.elements
      (Flambda_utils.variables_bound_by_the_closure closure_id function_decls)
  in
  let wrapper_body : Flambda.t =
    Apply {
      func = new_fun_var;
      args = wrapper_params @ extra_args;
      kind = Direct (Closure_id.wrap new_fun_var);
      dbg = Debuginfo.none;
      inline = Default_inline;
    }
  in
  additional_specialised_args,
  new_fun_var,
  Flambda.create_function_declaration
    ~params:wrapper_params
    ~body:wrapper_body
    ~stub:true
    ~dbg:Debuginfo.none
    ~inline:Default_inline
    ~is_a_functor:false

let add_wrapper
    ~backend
    ~fun_var
    ~(function_decls : Flambda.function_declarations)
    ~(function_decl : Flambda.function_declaration)
    ~(free_vars : Variable.t Variable.Map.t)
    ~specialised_args
    ~funs
    ~additional_specialised_args =
  let closure_id = Closure_id.wrap fun_var in
  let bound_by_closure =
    Flambda_utils.variables_bound_by_the_closure closure_id function_decls
  in
  if Variable.Set.is_empty bound_by_closure
    || function_decl.stub
    || too_many_arguments ~backend ~function_decl ~bound_by_closure
  then
    let funs = Variable.Map.add fun_var function_decl funs in
    funs, additional_specialised_args
  else
    let additional_specialised_args, rewritten_function_decl =
      rewrite_function_decl ~closure_id ~free_vars ~function_decls ~function_decl
        ~additional_specialised_args
    in
    let additional_specialised_args, new_fun_var, wrapper =
      create_wrapper ~fun_var ~function_decls ~function_decl
        ~additional_specialised_args ~specialised_args
    in
    let funs =
      Variable.Map.add new_fun_var rewritten_function_decl
        (Variable.Map.add fun_var wrapper funs)
    in
    funs, additional_specialised_args

(* It is important to limit the number of arguments added: if arguments
   end up being passed on the stack, tail call optimization will be
   disabled (see asmcomp/selectgen.ml). *)
let would_add_too_many_arguments ~(set_of_closures : Flambda.set_of_closures)
      ~additional_specialised_args =
  let module Backend = (val backend : Backend_intf.S) in
  let max_arguments = Backend.max_sensible_number_of_arguments in
  let num_new_arguments = Variable.Map.cardinal additional_specialised_args in
  Variable.Map.fold (fun fun_var function_decl too_many_arguments ->
      if too_many_arguments then true
      else
        let num_existing_arguments = List.length function_decl.params in
        let num_arguments = num_existing_arguments + num_new_arguments in
        num_arguments > max_arguments)
    set_of_closures.function_decls.funs
    false

let rewrite_set_of_closures ~backend
      ~(set_of_closures : Flambda.set_of_closures)
      ~additional_specialised_args =
  if would_add_too_many_arguments ~set_of_closures ~additional_specialised_args
  then
    None
  else
    let specialised_args =
      try
        Variable.Map.disjoint_union set_of_closures.specialised_args
          additional_specialised_args
          ~eq:Variable.equal
      with _exn ->
        Misc.fatal_errorf "Augment_specialised_args: some/all of the \
            additional specialised args (%a) were already specialised args \
            of the set of closures %a"
          (Format.pp_print_list Variable.print) additional_specialised_args
          Flambda.print_set_of_closures set_of_closures
    in
    let funs =
      Variable.Map.mapi
        (fun fun_var function_decl (funs, additional_specialised_args) ->
           add_wrapper ~backend ~function_decls ~fun_var ~free_vars
             ~specialised_args:set_of_closures.specialised_args
             ~function_decl ~funs ~additional_specialised_args)
        set_of_closures.function_decls.funs
    in
    let function_decls =
      Flambda.update_function_declarations function_decls ~funs
    in
    let set_of_closures =
      Flambda.create_set_of_closures
        ~function_decls
        ~free_vars:set_of_closures.free_vars
        ~specialised_args
    in
    Some set_of_closures

let rewrite_set_of_closures ~backend ~set_of_closures =
  Pass_manager.with_dump ~pass_name ~input:set_of_closures
    ~print_input:Flambda.print_set_of_closures
    ~print_output:Flambda.print_set_of_closures
    ~f:(fun () -> rewrite_set_of_closures ~backend ~set_of_closures)
