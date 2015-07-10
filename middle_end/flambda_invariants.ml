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

type 'a counter_example =
  | No_counter_example
  | Counter_example of 'a

exception Counter_example_id of Variable.t

(* Explicit "ignore" functions.  We name every pattern variable, avoiding
   underscores, to try to avoid accidentally failing to handle (for example)
   a particular variable.
   We also avoid explicit record field access during the checking functions,
   preferring instead to use exhaustive record matches.
*)
let already_added_bound_variable_to_env (var : Variable.t) = ()
let will_traverse_expression_later (e : Flambda.t) = ()
let ignore_call_kind (_ : Flambda.call_kind) = ()
let ignore_let_kind (_ : Flambda.let_kind) = ()
let ignore_debuginfo (_ : Debuginfo.t) = ()
let ignore_meth_kind (_ : Lambda.meth_kind) = ()
let ignore_int (_ : int) = ()
let ignore_int_set (_ : Ext_types.Int.Set.t) = ()
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

(** Faults found by [binding_invariants]. *)
exception Binding_occurrence_in_different_compilation_unit of Variable.t
exception Binding_occurrence_of_variable_already_bound of Variable.t
exception Unbound_variable of Variable.t
exception Assignment_to_non_mutable_variable of Variable.t

type is_mutable = Mutable | Immutable

let binding_invariants flam =
  let add_binding_occurrence env var ~is_mutable =
    let compilation_unit = Compilation_unit.get_current_exn () in
    if not (Variable.in_compilation_unit var compilation_unit) then
      raise (Binding_occurrence_in_different_compilation_unit var)
    else if Variable.Map.mem var env then
      raise (Binding_occurrence_of_variable_already_bound var)
    else
      Variable.Map.add var is_mutable env
  in
  let check_variable_is_bound env var =
    if not Variable.Map.mem var env then raise (Unbound_variable var)
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
      ignore_let_kind let_kind;
      loop_named env def;
      loop (add_binding_occurrence env var) body
    | Let_rec (defs, body) ->
      let env =
        List.fold_left (fun env (var, def) ->
            will_traverse_expression_later def;
            add_binding_occurrence env var)
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
      loop (add_binding_occurrence env var) body
    | Static_catch (static_exn, vars, body, handler) ->
      ignore_static_exception static_exn;
      loop env body;
      let env = List.fold_right Variable.Set.add vars env in
      loop env handler
    | Try_with (body, var, handler) ->
      loop env body;
      loop (add_binding_occurrence env var) handler
    (* Everything else: *)
    | Apply { func; args; kind; dbg } ->
      check_variable_is_bound env func;
      check_variables_are_bound env args;
      ignore_call_kind kind;
      ignore_debuginfo dbg;
    | Assign (var, e) ->
      let is_mutable = check_variable_is_bound_and_get_mutability env var in
      begin match is_mutable with
      | Mutable -> loop env e
      | Immutable -> raise (Assignment_to_non_mutable_variable var)
      end
    | Send (meth_kind, e1, e2, es, dbg) ->
      ignore_meth_kind meth_kind;
      loop env e1;
      loop env e2;
      List.iter (loop env) es;
      ignore_debug_info dbg
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
          loop env e);
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
      loop env e2;
    | Proved_unreachable -> ()
  in
  and loop_named env (named : Flambda.named) =
    match named with
    | Symbol symbol -> ignore_symbol symbol
    | Const const -> ignore_const const
    | Set_of_closures { function_decls; free_vars; specialised_args; } ->
      let { set_of_closures_id; funs; compilation_unit } = function_decls in
      ignore_set_of_closures_id set_of_closures_id;
      ignore_compilation_unit compilation_unit;
      Variable.Map.iter (fun fun_var function_decl ->
          let { params; body; free_variables; stub; dbg; } = function_decl in

          ignore_bool stub;
          ignore_debuginfo dbg;

        funs;
      Variable.Map.iter (fun _ var -> test var env) free_vars;
      Variable.Map.iter (fun _ var -> test var env) specialised_args
    | Project_closure { set_of_closures; closure_id; } ->
      check_variable_is_bound set_of_closures;
      ignore_closure_id closure_id
    | Move_within_set_of_closures { closure; start_from; move_to; } ->
      check_variable_is_bound closure;
      ignore_closure_id start_from;
      ignore_closure_id move_to;
    | Project_var { closure; closure_id; var; } ->
      check_variable_is_bound closure;
      ignore_closure_id closure_id;
      ignore_var_within_closure var
  in
  loop Variable.Map.empty flam

exception Counter_example_varset of Variable.Set.t

let function_free_variables_are_bound_in_the_closure_and_parameters flam =
  let f { Flambda. function_decls;free_vars} _ =
    let variables_in_closure = Variable.Map.keys free_vars in
    let functions_in_closure =
      Variable.Map.fold (fun id _ env -> Variable.Set.add id env)
        function_decls.funs Variable.Set.empty in
    Variable.Map.iter (fun _ { Flambda. params; free_variables } ->
        let acceptable_free_variables =
          Variable.Set.union
            (Variable.Set.union variables_in_closure functions_in_closure)
            (Variable.Set.of_list params) in
        let counter_examples =
          Variable.Set.diff free_variables acceptable_free_variables in
        if not (Variable.Set.is_empty counter_examples)
        then raise (Counter_example_varset counter_examples))
      function_decls.funs
  in
  try
    Flambdaiter.iter_on_sets_of_closures f flam;
    No_counter_example
  with Counter_example_varset set ->
    Counter_example set

let declared_var_within_closure flam =
  let bound = ref Var_within_closure.Set.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if Var_within_closure.Set.mem var !bound
    then bound_multiple_times := Some var;
    bound := Var_within_closure.Set.add var !bound
  in
  let f { Flambda. free_vars } _ =
    Variable.Map.iter (fun id _ ->
        let var = Var_within_closure.wrap id in
        add_and_check var) free_vars
  in
  Flambdaiter.iter_on_sets_of_closures f flam;
  !bound, !bound_multiple_times

let no_var_within_closure_is_bound_multiple_times flam =
  match declared_var_within_closure flam with
  | _, Some var -> Counter_example var
  | _, None -> No_counter_example

exception Counter_example_cu of Compilation_unit.t

let every_declared_closure_is_from_current_compilation_unit flam =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let f { Flambda. function_decls = { compilation_unit }} _ =
    if not (Compilation_unit.equal compilation_unit current_compilation_unit)
    then raise (Counter_example_cu compilation_unit)
  in
  try
    Flambdaiter.iter_on_sets_of_closures f flam;
    No_counter_example
  with Counter_example_cu cu ->
    Counter_example cu

let declared_closure_id flam =
  let bound = ref Closure_id.Set.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if Closure_id.Set.mem var !bound
    then bound_multiple_times := Some var;
    bound := Closure_id.Set.add var !bound
  in
  let f (flam : Flambda.t) =
    match flam with
    | Set_of_closures ({function_decls},_) ->
        Variable.Map.iter (fun id _ ->
            let var = Closure_id.wrap id in
            add_and_check var) function_decls.funs
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !bound, !bound_multiple_times

let no_closure_id_is_bound_multiple_times flam =
  match declared_closure_id flam with
  | _, Some var -> Counter_example var
  | _, None -> No_counter_example

let used_closure_id flam =
  let used = ref Closure_id.Set.empty in
  let f (flam : Flambda.t) =
    match flam with
    | Project_closure ({closure_id;}, _) ->
      used := Closure_id.Set.add closure_id !used;
    | Move_within_set_of_closures ({ closure = _; start_from; move_to }, _) ->
      used := Closure_id.Set.add start_from !used;
      used := Closure_id.Set.add move_to !used
    | Project_var ({ closure = _; closure_id; var = _ }, _) ->
      used := Closure_id.Set.add closure_id !used
    | Assign _ | Var _ | Set_of_closures _ | Symbol _ | Const _
    | Apply _ | Let _ | Let_rec _ | Prim _ | Fseq_prim _ | Switch _
    | String_switch _ | Static_raise _ | Static_catch _ | Try_with _
    | If_then_else _ | Fsequence _ | While _ | For _ | Send _
    | Proved_unreachable -> ()
  in
  Flambdaiter.iter f flam;
  !used

let used_var_within_closure flam =
  let used = ref Var_within_closure.Set.empty in
  let f (flam : Flambda.t) =
    match flam with
    | Project_var ({ closure = _; closure_id = _; var },_) ->
      used := Var_within_closure.Set.add var !used
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !used

let every_used_function_from_current_compilation_unit_is_declared flam =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let declared, _ = declared_closure_id flam in
  let used = used_closure_id flam in
  let used_from_current_unit =
    Closure_id.Set.filter
      (Closure_id.in_compilation_unit current_compilation_unit)
      used in
  let counter_examples =
    Closure_id.Set.diff used_from_current_unit declared in
  if Closure_id.Set.is_empty counter_examples
  then No_counter_example
  else Counter_example counter_examples

let every_used_var_within_closure_from_current_compilation_unit_is_declared
      flam =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let declared, _ = declared_var_within_closure flam in
  let used = used_var_within_closure flam in
  let used_from_current_unit =
    Var_within_closure.Set.filter
      (Var_within_closure.in_compilation_unit current_compilation_unit)
      used in
  let counter_examples =
    Var_within_closure.Set.diff used_from_current_unit declared in
  if Var_within_closure.Set.is_empty counter_examples
  then No_counter_example
  else Counter_example counter_examples

exception Counter_example_se of Static_exception.t

let every_static_exception_is_caught flam =
  let check env (flam : Flambda.t) =
    match flam with 
    | Static_raise(exn,_,_) ->
        if not (Static_exception.Set.mem exn env)
        then raise (Counter_example_se exn)
    | _ -> ()
  in
  let rec loop env (flam : Flambda.t) =
    match flam with
    | Static_catch (i, _, body, handler,_) ->
        let env = Static_exception.Set.add i env in
        loop env handler;
        loop env body
    | exp ->
        check env exp;
        Flambdaiter.apply_on_subexpressions (loop env) exp
  in
  let env = Static_exception.Set.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_se var ->
    Counter_example var

let every_static_exception_is_caught_at_a_single_position flam =
  let caught = ref Static_exception.Set.empty in
  let f (flam : Flambda.t) =
    match flam with
    | Static_catch (i, _, _body, _handler,_) ->
        if Static_exception.Set.mem i !caught
        then raise (Counter_example_se i);
        caught := Static_exception.Set.add i !caught
    | _ -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_se var ->
    Counter_example var

exception Counter_example_prim of Lambda.primitive

let no_access_to_global_module_identifiers flam =
  let f (flam : Flambda.t) =
    match flam with
    | Prim(Pgetglobalfield _ as p, _, _, _)
    | Prim(Psetglobalfield _ as p, _, _, _)
    | Prim(Psetglobal _ as p, _, _, _) ->
        raise (Counter_example_prim p)
    | Prim(Pgetglobal id as p, _, _, _) ->
        if not (Ident.is_predef_exn id)
        then raise (Counter_example_prim p)
    | _ -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_prim p ->
    Counter_example p

(* CR mshinwell: check that the [free_vars] and [free_variables] fields of
   [set_of_closures] and [function_declaration] respectively are correct. *)

(* CR mshinwell: checks for other disallowed primitives?
   (for example, Prim with Psequand/Psequor) *)

let test result fmt printer =
  match result with
  | No_counter_example -> ()
  | Counter_example ce ->
      Misc.fatal_error (Format.asprintf fmt printer ce)

let check ?(flambdasym=false) ?(cmxfile=false) flam =
  test (every_used_identifier_is_bound flam)
    "Unbound identifier %a" Variable.print;

  test (function_free_variables_are_bound_in_the_closure_and_parameters flam)
    "Variables %a are in function free variables but are not bound in \
     the closure or in function parameters" Variable.Set.print;

  test (no_identifier_bound_multiple_times flam)
    "identifier bound multiple times %a" Variable.print;

  test (every_bound_variable_is_from_current_compilation_unit flam)
    "bound variable %a is attributed to another compilation unit"
    Variable.print;

  test (no_assign_on_variable_of_kind_Immutable flam)
    "variable %a of kind Immutable is assigned" Variable.print;

  test (no_var_within_closure_is_bound_multiple_times flam)
    "variable within closure %a bound multiple times"
    Var_within_closure.print;

  test (no_closure_id_is_bound_multiple_times flam)
    "function within closure %a bound multiple times"
    Closure_id.print;

  test (every_declared_closure_is_from_current_compilation_unit flam)
    "function declare using unit %a which is not the current one"
    Compilation_unit.print;

  if not flambdasym
  then
    test (every_used_function_from_current_compilation_unit_is_declared flam)
      "functions %a from the current compilation unit are used but \
       not declared"
      Closure_id.Set.print;

  if not flambdasym
  then
    test (every_used_var_within_closure_from_current_compilation_unit_is_declared
            flam)
      "variables %a from the current compilation unit are used but \
       not declared"
      Var_within_closure.Set.print;

  if cmxfile
  then
    test (no_access_to_global_module_identifiers flam)
      "access to global identifier using the primitive %a in code \
       exported to th cmx file"
      Printlambda.primitive;

  test (every_static_exception_is_caught flam)
    "static exception %a can't be caught"
    Static_exception.print;

  test (every_static_exception_is_caught_at_a_single_position flam)
    "multiple catch point for exception %a"
    Static_exception.print
