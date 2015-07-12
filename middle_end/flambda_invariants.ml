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

(* Explicit "ignore" functions.  We name every pattern variable, avoiding
   underscores, to try to avoid accidentally failing to handle (for example)
   a particular variable.
   We also avoid explicit record field access during the checking functions,
   preferring instead to use exhaustive record matches.
*)
let already_added_bound_variable_to_env (_ : Variable.t) = ()
let will_traverse_named_expression_later (_ : Flambda.named) = ()
let ignore_call_kind (_ : Flambda.call_kind) = ()
let ignore_debuginfo (_ : Debuginfo.t) = ()
let ignore_meth_kind (_ : Lambda.meth_kind) = ()
let ignore_int (_ : int) = ()
let ignore_int_set (_ : Ext_types.Int.Set.t) = ()
let ignore_bool (_ : bool) = ()
let ignore_string (_ : string) = ()
let ignore_static_exception (_ : Static_exception.t) = ()
let ignore_direction_flag (_ : Asttypes.direction_flag) = ()
let ignore_symbol (_ : Symbol.t) = ()
let ignore_primitive ( _ : Lambda.primitive) = ()
let ignore_const (_ : Flambda.const) = ()
let ignore_set_of_closures_id (_ : Set_of_closures_id.t) = ()
let ignore_closure_id (_ : Closure_id.t) = ()
let ignore_var_within_closure (_ : Var_within_closure.t) = ()
let ignore_compilation_unit (_ : Compilation_unit.t) = ()

exception Binding_occurrence_not_from_current_compilation_unit of Variable.t
exception Binding_occurrence_of_variable_already_bound of Variable.t
exception Unbound_variable of Variable.t
exception Assignment_to_non_mutable_variable of Variable.t
exception Vars_in_function_body_not_bound_by_closure_or_params of
  Variable.Set.t
exception Function_decls_have_overlapping_parameters of Variable.Set.t
exception Specialised_arg_that_is_not_a_parameter of Variable.t
exception Free_variables_set_is_lying of
  Variable.t * Flambda.function_declaration
exception Set_of_closures_free_vars_map_has_wrong_domain of Variable.Set.t
exception Static_exception_not_caught of Static_exception.t
exception Static_exception_caught_in_multiple_places of Static_exception.t
exception Access_to_global_module_identifier of Lambda.primitive
exception Pidentity_should_not_occur
exception Pdirapply_should_be_expanded
exception Prevapply_should_be_expanded
exception Sequential_logical_operator_primitives_must_be_expanded of
  Lambda.primitive
exception Var_within_closure_bound_multiple_times of Var_within_closure.t
exception Declared_closure_from_another_unit of Compilation_unit.t
exception Closure_id_is_bound_multiple_times of Closure_id.t
exception Unbound_closure_ids of Closure_id.Set.t
exception Unbound_vars_within_closures of Var_within_closure.Set.t

exception Flambda_invariants_failed

let variable_invariants flam =
  let add_binding_occurrence env var is_mutable =
    let compilation_unit = Compilation_unit.get_current_exn () in
    if not (Variable.in_compilation_unit compilation_unit var) then
      raise (Binding_occurrence_not_from_current_compilation_unit var)
    else if Variable.Map.mem var env then
      raise (Binding_occurrence_of_variable_already_bound var)
    else
      Variable.Map.add var is_mutable env
  in
  let add_binding_occurrences env vars is_mutable =
    List.fold_left (fun env var ->
        add_binding_occurrence env var is_mutable)
      env vars
  in
  let check_variable_is_bound env var =
    if not (Variable.Map.mem var env) then raise (Unbound_variable var)
  in
  let check_variable_is_bound_and_get_mutability env var =
    try Variable.Map.find var env
    with Not_found -> raise (Unbound_variable var)
  in
  let check_variables_are_bound env vars =
    List.iter (check_variable_is_bound env) vars
  in
  let rec loop env (flam : Flambda.t) =
    match flam with
    (* Expressions that can bind [Variable.t]s: *)
    | Let (let_kind, var, def, body) ->
      loop_named env def;
      loop (add_binding_occurrence env var let_kind) body
    | Let_rec (defs, body) ->
      let env =
        List.fold_left (fun env (var, def) ->
            will_traverse_named_expression_later def;
            add_binding_occurrence env var Flambda.Immutable)
          env defs
      in
      List.iter (fun (var, def) ->
        already_added_bound_variable_to_env var;
        loop_named env def) defs;
      loop env body
    | For (var, lo, hi, direction, body) ->
      ignore_direction_flag direction;
      loop env lo;
      loop env hi;
      loop (add_binding_occurrence env var Flambda.Immutable) body
    | Static_catch (static_exn, vars, body, handler) ->
      ignore_static_exception static_exn;
      loop env body;
      loop (add_binding_occurrences env vars Flambda.Immutable) handler
    | Try_with (body, var, handler) ->
      loop env body;
      loop (add_binding_occurrence env var Flambda.Immutable) handler
    (* Everything else: *)
    | Var var -> check_variable_is_bound env var
    | Apply { func; args; kind; dbg } ->
      check_variable_is_bound env func;
      check_variables_are_bound env args;
      ignore_call_kind kind;
      ignore_debuginfo dbg;
    | Assign (var, e) ->
      let is_mutable = check_variable_is_bound_and_get_mutability env var in
      (* CR-someday mshinwell: consider if the mutable/immutable distinction
         on variables could be enforced by the type system *)
      begin match (is_mutable : Flambda.let_kind) with
      | Mutable -> loop env e
      | Immutable -> raise (Assignment_to_non_mutable_variable var)
      end
    | Send (meth_kind, e1, e2, es, dbg) ->
      ignore_meth_kind meth_kind;
      loop env e1;
      loop env e2;
      List.iter (loop env) es;
      ignore_debuginfo dbg
    | If_then_else (e1, e2, e3) ->
      loop env e1;
      loop env e2;
      loop env e3
    | Switch (e, { numconsts; consts; numblocks; blocks; failaction; }) ->
      loop env e;
      ignore_int_set numconsts;
      ignore_int_set numblocks;
      List.iter (fun (n, e) ->
          ignore_int n;
          loop env e)
        (consts @ blocks);
      Misc.may (loop env) failaction
    | String_switch (e, cases, e_opt) ->
      loop env e;
      List.iter (fun (label, case) ->
          ignore_string label;
          loop env case)
        cases;
      Misc.may (loop env) e_opt
    | Static_raise (static_exn, es) ->
      ignore_static_exception static_exn;
      List.iter (loop env) es
    | While (e1, e2) ->
      loop env e1;
      loop env e2
    | Proved_unreachable -> ()
  and loop_named env (named : Flambda.named) =
    match named with
    | Symbol symbol -> ignore_symbol symbol
    | Const const -> ignore_const const
    | Set_of_closures { function_decls; free_vars; specialised_args; } ->
      let { Flambda.set_of_closures_id; funs; compilation_unit } =
        function_decls
      in
      ignore_set_of_closures_id set_of_closures_id;
      ignore_compilation_unit compilation_unit;
      let functions_in_closure = Variable.Map.keys funs in
      let variables_in_closure =
        Variable.Map.fold (fun var var_in_closure variables_in_closure ->
            check_variable_is_bound env var;
            Variable.Set.add var_in_closure variables_in_closure)
          free_vars Variable.Set.empty
      in
      let all_params, all_free_vars =
        Variable.Map.fold (fun fun_var function_decl acc ->
            let all_params, all_free_vars = acc in
            let { Flambda.params; body; free_variables; stub; dbg; } =
              function_decl
            in
            assert (Variable.Set.mem fun_var functions_in_closure);
            ignore_bool stub;
            ignore_debuginfo dbg;
            (* Check that [free_variables], which is only present as an
               optimization, is not lying. *)
            let free_variables' = Free_variables.calculate body in
            if not (Variable.Set.equal free_variables free_variables') then
              raise (Free_variables_set_is_lying (fun_var, function_decl));
            (* Check that every variable free in the body of the function is
               bound by either the set of closures or the parameter list. *)
            let acceptable_free_variables =
              Variable.Set.union
                (Variable.Set.union variables_in_closure functions_in_closure)
                (Variable.Set.of_list params)
            in
            let bad =
              Variable.Set.diff free_variables acceptable_free_variables
            in
            if not (Variable.Set.is_empty bad) then begin
              raise (Vars_in_function_body_not_bound_by_closure_or_params bad)
            end;
            (* Check that parameters are unique across all functions in the
               declaration. *)
            let old_all_params_size = Variable.Set.cardinal all_params in
            let params = Variable.Set.of_list params in
            let params_size = Variable.Set.cardinal params in
            let all_params = Variable.Set.union all_params params in
            let all_params_size = Variable.Set.cardinal all_params in
            if all_params_size <> old_all_params_size + params_size then begin
              raise (Function_decls_have_overlapping_parameters all_params)
            end;
            all_params, Variable.Set.union free_variables all_free_vars)
          funs (Variable.Set.empty, Variable.Set.empty)
      in
      (* Check that the free variables rewriting map in the set of closures
         does not contain variables in its domain that are not actually free
         variables of any of the function bodies. *)
      let bad_free_vars =
        Variable.Set.diff (Variable.Map.keys free_vars) all_free_vars
      in
      if not (Variable.Set.is_empty bad_free_vars) then begin
        raise (Set_of_closures_free_vars_map_has_wrong_domain bad_free_vars)
      end;
      (* Check that every "specialised arg" is a parameter of one of the
         functions being declared. *)
      Variable.Map.iter (fun being_specialised specialised_to ->
          if not (Variable.Set.mem being_specialised all_params) then begin
            raise (Specialised_arg_that_is_not_a_parameter being_specialised)
          end;
          check_variable_is_bound env specialised_to)
        specialised_args
    | Project_closure { set_of_closures; closure_id; } ->
      check_variable_is_bound env set_of_closures;
      ignore_closure_id closure_id
    | Move_within_set_of_closures { closure; start_from; move_to; } ->
      check_variable_is_bound env closure;
      ignore_closure_id start_from;
      ignore_closure_id move_to;
    | Project_var { closure; closure_id; var; } ->
      check_variable_is_bound env closure;
      ignore_closure_id closure_id;
      ignore_var_within_closure var
    | Prim (prim, args, dbg) ->
      ignore_primitive prim;
      check_variables_are_bound env args;
      ignore_debuginfo dbg
    | Expr expr ->
      loop env expr
  in
  loop Variable.Map.empty flam

let primitive_invariants flam ~no_access_to_global_module_identifiers =
  Flambdaiter.iter_named (function
      | Prim (prim, _, _) ->
        begin match prim with
        | Psequand | Psequor ->
          raise (Sequential_logical_operator_primitives_must_be_expanded prim)
        | Pgetglobalfield _ | Psetglobalfield _ | Psetglobal _ ->
          if no_access_to_global_module_identifiers then begin
            raise (Access_to_global_module_identifier prim)
          end
        | Pgetglobal id ->
          if no_access_to_global_module_identifiers
            && not (Ident.is_predef_exn id) then
          begin
            raise (Access_to_global_module_identifier prim)
          end
        | Pidentity -> raise Pidentity_should_not_occur
        | Pdirapply _ -> raise Pdirapply_should_be_expanded
        | Prevapply _ -> raise Prevapply_should_be_expanded
        | _ -> ()
        end
      | _ -> ())
    flam

let declared_var_within_closure flam =
  let bound = ref Var_within_closure.Set.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if Var_within_closure.Set.mem var !bound then begin
      bound_multiple_times := Some var
    end;
    bound := Var_within_closure.Set.add var !bound
  in
  Flambdaiter.iter_on_sets_of_closures (fun { Flambda.free_vars; _ } ->
      Variable.Map.iter (fun id _ ->
          let var = Var_within_closure.wrap id in
          add_and_check var)
        free_vars)
    flam;
  !bound, !bound_multiple_times

let no_var_within_closure_is_bound_multiple_times flam =
  match declared_var_within_closure flam with
  | _, Some var -> raise (Var_within_closure_bound_multiple_times var)
  | _, None -> ()

let every_declared_closure_is_from_current_compilation_unit flam =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  Flambdaiter.iter_on_sets_of_closures (fun
        { Flambda. function_decls = { compilation_unit; _ }; _ } ->
      if not (Compilation_unit.equal compilation_unit current_compilation_unit)
      then raise (Declared_closure_from_another_unit compilation_unit))
    flam

let declared_closure_ids flam =
  let bound = ref Closure_id.Set.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if Closure_id.Set.mem var !bound
    then bound_multiple_times := Some var;
    bound := Closure_id.Set.add var !bound
  in
  Flambdaiter.iter_on_sets_of_closures (fun { Flambda. function_decls; _; } ->
      Variable.Map.iter (fun id _ ->
          let var = Closure_id.wrap id in
          add_and_check var)
      function_decls.funs)
    flam;
  !bound, !bound_multiple_times

let no_closure_id_is_bound_multiple_times flam =
  match declared_closure_ids flam with
  | _, Some closure_id ->
    raise (Closure_id_is_bound_multiple_times closure_id)
  | _, None -> ()

let used_closure_ids flam =
  let used = ref Closure_id.Set.empty in
  let f (flam : Flambda.named) =
    match flam with
    | Project_closure { closure_id; _} ->
      used := Closure_id.Set.add closure_id !used;
    | Move_within_set_of_closures { closure = _; start_from; move_to; } ->
      used := Closure_id.Set.add start_from !used;
      used := Closure_id.Set.add move_to !used
    | Project_var { closure = _; closure_id; var = _ } ->
      used := Closure_id.Set.add closure_id !used
    | Set_of_closures _
    | Symbol _ | Const _ | Prim _ | Expr _ -> ()
  in
  Flambdaiter.iter_named f flam;
  !used

let used_vars_within_closures flam =
  let used = ref Var_within_closure.Set.empty in
  let f (flam : Flambda.named) =
    match flam with
    | Project_var { closure = _; closure_id = _; var; } ->
      used := Var_within_closure.Set.add var !used
    | _ -> ()
  in
  Flambdaiter.iter_named f flam;
  !used

let every_used_function_from_current_compilation_unit_is_declared flam =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let declared, _ = declared_closure_ids flam in
  let used = used_closure_ids flam in
  let used_from_current_unit =
    Closure_id.Set.filter
      (Closure_id.in_compilation_unit current_compilation_unit)
      used in
  let counter_examples =
    Closure_id.Set.diff used_from_current_unit declared in
  if Closure_id.Set.is_empty counter_examples
  then ()
  else raise (Unbound_closure_ids counter_examples)

let every_used_var_within_closure_from_current_compilation_unit_is_declared
      flam =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let declared, _ = declared_var_within_closure flam in
  let used = used_vars_within_closures flam in
  let used_from_current_unit =
    Var_within_closure.Set.filter
      (Var_within_closure.in_compilation_unit current_compilation_unit)
      used in
  let counter_examples =
    Var_within_closure.Set.diff used_from_current_unit declared in
  if Var_within_closure.Set.is_empty counter_examples
  then ()
  else raise (Unbound_vars_within_closures counter_examples)

let every_static_exception_is_caught flam =
  let check env (flam : Flambda.t) =
    match flam with 
    | Static_raise (exn, _) ->
      if not (Static_exception.Set.mem exn env)
      then raise (Static_exception_not_caught exn)
    | _ -> ()
  in
  let rec loop env (flam : Flambda.t) =
    match flam with
    | Static_catch (i, _, body, handler) ->
      let env = Static_exception.Set.add i env in
      loop env handler;
      loop env body
    | exp ->
      check env exp;
      Flambdaiter.apply_on_subexpressions (loop env)
        (fun (_ : Flambda.named) -> ()) exp
  in
  loop Static_exception.Set.empty flam

let every_static_exception_is_caught_at_a_single_position flam =
  let caught = ref Static_exception.Set.empty in
  let f (flam : Flambda.t) =
    match flam with
    | Static_catch (i, _, _body, _handler) ->
      if Static_exception.Set.mem i !caught then
        raise (Static_exception_caught_in_multiple_places i);
      caught := Static_exception.Set.add i !caught
    | _ -> ()
  in
  Flambdaiter.iter f (fun (_ : Flambda.named) -> ()) flam

let check_exn ?(flambdasym=false) ?(cmxfile=false) flam =
  try
    variable_invariants flam;
    primitive_invariants flam ~no_access_to_global_module_identifiers:cmxfile;
    every_static_exception_is_caught flam;
    every_static_exception_is_caught_at_a_single_position flam;
    no_var_within_closure_is_bound_multiple_times flam;
    every_declared_closure_is_from_current_compilation_unit flam;
    no_closure_id_is_bound_multiple_times flam;
    if not flambdasym then begin
      every_used_function_from_current_compilation_unit_is_declared flam;
      every_used_var_within_closure_from_current_compilation_unit_is_declared
        flam;
    end
  with exn -> begin
    begin match exn with
    | Binding_occurrence_not_from_current_compilation_unit var ->
      Format.eprintf ">> Binding occurrence of variable marked as not being from \
          the current compilation unit: %a\n%!"
        Variable.print var
    | Binding_occurrence_of_variable_already_bound var ->
      Format.eprintf ">> Binding occurrence of variable that was already \
            bound: %a\n%!"
        Variable.print var
    | Unbound_variable var ->
      Format.eprintf ">> Unbound variable: %a" Variable.print var
    | Assignment_to_non_mutable_variable var ->
      Format.eprintf ">> Assignment to non-mutable variable: %a\n%!"
        Variable.print var
    | Vars_in_function_body_not_bound_by_closure_or_params vars ->
      Format.eprintf ">> Variable in the body of a function declaration that \
          is not bound by either the closure or the function's parameter \
          list: %a\n%!"
        Variable.Set.print vars
    | Function_decls_have_overlapping_parameters vars ->
      Format.eprintf ">> Function declarations whose parameters overlap: \
          %a\n%!"
        Variable.Set.print vars
    | Specialised_arg_that_is_not_a_parameter var ->
      Format.eprintf ">> Variable in [specialised_args] that is not a \
          parameter of any of the function(s) in the corresponding \
          declaration(s): %a\n%!"
        Variable.print var
    | Free_variables_set_is_lying (var, function_decl) ->
      Format.eprintf ">> Function declaration whose [free_variables] set does \
          not coincide with the result of [Free_variables.calculate] applied \
          to the body of the function: %a\n%!"
        Printflambda.function_declaration (var, function_decl)
    | Set_of_closures_free_vars_map_has_wrong_domain vars ->
      Format.eprintf ">> [free_vars] map in set of closures has in its domain \
          variables that are not free variables of the corresponding \
           functions: %a\n%!"
        Variable.Set.print vars
    | Sequential_logical_operator_primitives_must_be_expanded prim ->
      Format.eprintf ">> Sequential logical operator primitives must be \
          expanded (see closure_conversion.ml): %a\n%!"
        Printlambda.primitive prim
    | Var_within_closure_bound_multiple_times var ->
      Format.eprintf ">> Variable within a closure is bound multiple times: \
          %a\n%!"
        Var_within_closure.print var
    | Closure_id_is_bound_multiple_times closure_id ->
      Format.eprintf ">> Closure ID is bound multiple times: %a\n%!"
        Closure_id.print closure_id
    | Declared_closure_from_another_unit compilation_unit ->
      Format.eprintf ">> Closure declared as being from another compilation \
          unit: %a\n%!"
        Compilation_unit.print compilation_unit
    | Unbound_closure_ids closure_ids ->
      Format.eprintf ">> Unbound closure ID(s) from the current compilation \
          unit: %a\n%!"
        Closure_id.Set.print closure_ids
    | Unbound_vars_within_closures vars_within_closures ->
      Format.eprintf ">> Unbound variable(s) within closure(s) from the \
          current compilation_unit: %a\n%!"
        Var_within_closure.Set.print vars_within_closures
    | Static_exception_not_caught static_exn ->
      Format.eprintf ">> Uncaught static exception: %a\n%!"
        Static_exception.print static_exn
    | Static_exception_caught_in_multiple_places static_exn ->
      Format.eprintf ">> Static exception caught in multiple places: %a\n%!"
        Static_exception.print static_exn
    | Access_to_global_module_identifier prim ->
      (* CR mshinwell: backend-specific checks should move to another module,
         in the asmcomp/ directory. *)
      Format.eprintf ">> Forbidden access to a global module identifier (not \
          allowed in Flambda that will be exported to a .cmx file): %a\n%!"
        Printlambda.primitive prim
    | Pidentity_should_not_occur ->
      Format.eprintf ">> The Pidentity primitive should never occur in an \
        Flambda expression (see closure_conversion.ml)\n%!"
    | Pdirapply_should_be_expanded ->
      Format.eprintf ">> The Pdirapply primitive should never occur in an \
        Flambda expression (see closure_conversion.ml); use Apply instead\n%!"
    | Prevapply_should_be_expanded ->
      Format.eprintf ">> The Prevapply primitive should never occur in an \
        Flambda expression (see closure_conversion.ml); use Apply instead\n%!"
    | exn -> raise exn
    end;
    raise Flambda_invariants_failed
  end
