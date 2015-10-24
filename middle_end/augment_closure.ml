module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env

type closures_in_free_vars =
  {
    new_var : Variable.t;
    closure_id : Closure_id.t;
    outside_var : Variable.t;
  }

module Closure_field =
  Ext_types.Identifiable.Make (Ext_types.Pair (Variable) (Var_within_closure))

let freshened_var env v =
  Freshening.apply_variable (E.freshening env) v

let closures_in_variables ~env map acc =
  Variable.Map.fold (fun inside_var outside_var acc ->
      let approx = E.find_exn env (freshened_var env outside_var) in
      (* CR pchambart: that could also be done for blocks, but the
           heuristic needs to be addapted *)
      match A.check_approx_for_closure approx with
      | Wrong ->
        acc  (* Ignore free_vars that aren't closures. *)
      | Ok (value_closure, _approx_var, _approx_symbol,
            value_set_of_closures) ->
        Var_within_closure.Map.fold (fun bound_var _ acc ->
            let new_var =
              Variable.create (Var_within_closure.unique_name bound_var)
            in
            (* Format.printf "field: %a@." *)
            (*   Closure_field.print (inside_var, bound_var); *)
            Closure_field.Map.add (inside_var, bound_var)
              { new_var; closure_id = value_closure.closure_id; outside_var } acc)
          value_set_of_closures.bound_vars acc)
    map
    acc

let rewrite_set_of_closures
    ~env
    ~(set_of_closures:Flambda.set_of_closures) =
  let closures_in_free_vars =
    closures_in_variables ~env
      set_of_closures.free_vars
      Closure_field.Map.empty
  in
  let closures_in_free_vars_and_specialised_args =
    closures_in_variables ~env
      set_of_closures.specialised_args
      closures_in_free_vars
  in
  let used_new_vars = ref Variable.Set.empty in
  let rewrite_function_decl
      ~(closures_in_free_vars:closures_in_free_vars Closure_field.Map.t)
      (function_decl:Flambda.function_declaration) =
    let body =
      Flambda_iterators.map_project_var_to_expr_opt ~f:(fun project_var ->
          match
            Closure_field.Map.find
              (project_var.closure, project_var.var)
              closures_in_free_vars
          with
          | exception Not_found -> None
          | { new_var } ->
            used_new_vars := Variable.Set.add
                new_var !used_new_vars;
            Some (Flambda.Var new_var))
        function_decl.body
    in
    Flambda.create_function_declaration
      ~body
      ~inline:function_decl.inline
      ~params:function_decl.params
      ~stub:function_decl.stub
      ~dbg:function_decl.dbg
  in
  let funs =
    Variable.Map.map
      (rewrite_function_decl
         ~closures_in_free_vars:closures_in_free_vars_and_specialised_args)
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
         if Variable.Set.mem new_var !used_new_vars then
           Variable.Map.add new_var intermediate_var free_vars,
           Variable.Map.add intermediate_var
             (Flambda.Project_var { Flambda.closure = outside_var; closure_id; var = field })
             add_closures
         else
           free_vars, add_closures)
      closures_in_free_vars_and_specialised_args
      (set_of_closures.free_vars,
       Variable.Map.empty)
  in
  Flambda.create_set_of_closures
    ~function_decls
    ~free_vars
    ~specialised_args:set_of_closures.specialised_args,
  add_closures

let run ~env ~(set_of_closures:Flambda.set_of_closures) : Flambda.t option =
  let set_of_closures, add_closures =
    rewrite_set_of_closures
      ~env ~set_of_closures
  in
  if Variable.Map.is_empty add_closures then
    None
  else
    let expr =
      Variable.Map.fold Flambda.create_let
        add_closures (Flambda_utils.name_expr (Set_of_closures set_of_closures))
    in
    Some expr
