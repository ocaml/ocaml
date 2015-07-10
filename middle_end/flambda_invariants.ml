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

let every_used_identifier_is_bound flam =
  let test var env =
    if not (Variable.Set.mem var env) then raise (Counter_example_id var)
  in
  let check env (flam : Flambda.t) =
    (* CR mshinwell: check this function carefully *)
    match flam with
    (* The non-trivial cases here are all the various types of expressions
       that can contain a free [Variable.t]. *)
    | Assign (var, _, _) | Var (var, _) -> test var env
    | Set_of_closures
        ({ function_decls = _; free_vars; specialised_args }, _) ->
      Variable.Map.iter (fun _ var -> test var env) free_vars;
      Variable.Map.iter (fun _ var -> test var env) specialised_args
    | Project_closure (project_closure, _) ->
      test project_closure.set_of_closures env
    | Move_within_set_of_closures (move_within_set_of_closures, _) ->
      test move_within_set_of_closures.closure env
    | Project_var (project_var, _) ->
      test project_var.closure env
    | Apply ({ func = _; args; kind = _; dbg = _ }, _)
    | Prim (_, args, _, _) ->
      List.iter (fun var -> test var env) args
    | Symbol _ | Const _ | Let _ | Let_rec _ | Fseq_prim _
    | Switch _ | String_switch _ | Static_raise _ | Static_catch _
    | Try_with _ | If_then_else _ | Fsequence _ | While _ | For _ | Send _
    | Proved_unreachable -> ()
  in
  let rec loop env (flam : Flambda.t) =
    match flam with
    (* Expressions that can bind [Variable.t]s: *)
    | Let(_,id,def,body,_) ->
        loop env def;
        loop (Variable.Set.add id env) body
    | Let_rec(defs,body,_) ->
        let env =
          List.fold_left (fun env (id,_) -> Variable.Set.add id env) env defs in
        List.iter (fun (_,def) -> loop env def) defs;
        loop env body
    | Set_of_closures ({function_decls;free_vars = _},_) as exp ->
        check env exp;
        Variable.Map.iter (fun _ { Flambda. free_variables; body } ->
            loop free_variables body)
          function_decls.funs
    | For (id, lo, hi, _, body, _) ->
        loop env lo; loop env hi;
        loop (Variable.Set.add id env) body
    | Static_catch (_i, vars, body, handler,_) ->
        loop env body;
        let env = List.fold_right Variable.Set.add vars env in
        loop env handler
    | Try_with(body, id, handler,_) ->
        loop env body;
        loop (Variable.Set.add id env) handler
    (* Expressions that cannot bind [Variable.t]s, but may have free
       occurrences of them: *)
    | Assign _ | Var _ | Symbol _ | Const _ | Apply _
    | Project_closure _ | Move_within_set_of_closures _ | Project_var _
    | Prim _ | Fseq_prim _ | Switch _ | String_switch _ | Static_raise _
    | If_then_else _ | Fsequence _ | While _ | Send _
    | Proved_unreachable as exp ->
      check env exp;
      Flambdaiter.apply_on_subexpressions (loop env) exp
  in
  try
    loop Variable.Set.empty flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

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

let no_identifier_bound_multiple_times flam =
  let bound = ref Variable.Set.empty in
  let add_and_check id =
    if Variable.Set.mem id !bound
    then raise (Counter_example_id id)
    else bound := Variable.Set.add id !bound
  in
  let f (flam : Flambda.t) =
    match flam with
    | Let(_,id,_,_,_) ->
        add_and_check id
    | Let_rec(defs,_,_) ->
        List.iter (fun (id,_) -> add_and_check id) defs
    | Set_of_closures ({function_decls;free_vars},_) ->
        Variable.Map.iter (fun id _ -> add_and_check id) free_vars;
        Variable.Map.iter (fun _ { Flambda. params } ->
            List.iter add_and_check params)
          function_decls.funs
    | For (id,_,_,_,_,_) ->
        add_and_check id
    | Static_catch (_,vars,_,_,_) ->
        List.iter add_and_check vars
    | Try_with(_, id,_,_) ->
        add_and_check id

    | Assign _ | Var _
    | Symbol _ | Const _ | Apply _ | Project_closure _
    | Project_var _ | Move_within_set_of_closures _
    | Prim _ | Fseq_prim _ | Switch _ | String_switch _ | Static_raise _
    | If_then_else _ | Fsequence _
    | While _ | Send _ | Proved_unreachable
      -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let every_bound_variable_is_from_current_compilation_unit flam =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let check id =
    if not (Variable.in_compilation_unit current_compilation_unit id)
    then raise (Counter_example_id id)
  in
  let f (flam : Flambda.t) =
    match flam with
    | Let(_,id,_,_,_) ->
        check id
    | Let_rec(defs,_,_) ->
        List.iter (fun (id,_) -> check id) defs
    | Set_of_closures ({function_decls;free_vars},_) ->
        Variable.Map.iter (fun id _ -> check id) free_vars;
        Variable.Map.iter (fun _ { Flambda. params } ->
            List.iter check params)
          function_decls.funs
    | For (id,_,_,_,_,_) ->
        check id
    | Static_catch (_,vars,_,_,_) ->
        List.iter check vars
    | Try_with(_, id,_,_) ->
        check id

    | Assign _ | Var _
    | Symbol _ | Const _ | Apply _ | Project_closure _
    | Project_var _ | Move_within_set_of_closures _
    | Prim _ | Fseq_prim _ | Switch _ | String_switch _ | Static_raise _
    | If_then_else _ | Fsequence _
    | While _ | Send _ | Proved_unreachable
      -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let no_assign_on_variable_of_kind_Immutable flam =
  let test var env =
    if not (Variable.Set.mem var env)
    then raise (Counter_example_id var) in
  let check env (flam : Flambda.t) =
    match flam with
    | Assign(id,_,_) -> test id env
    | _ -> ()
  in
  let rec loop env (flam : Flambda.t) =
    match flam with
    | Let(Mutable,id,def,body,_) ->
        loop env def;
        loop (Variable.Set.add id env) body
    | Set_of_closures ({ Flambda. function_decls;free_vars = _},_) ->
        let env = Variable.Set.empty in
        Variable.Map.iter (fun _ { Flambda. body } -> loop env body)
          function_decls.funs
    | Let (Immutable, _, _, _, _)
    | Assign _ | Var _
    | Symbol _ | Const _ | Apply _ | Project_closure _
    | Project_var _ | Move_within_set_of_closures _ | Let_rec _
    | Prim _ | Fseq_prim _ | Switch _ | String_switch _ | Static_raise _
    | Static_catch _ | Try_with _ | If_then_else _ | Fsequence _
    | While _ | For _ | Send _ | Proved_unreachable
      as exp ->
        check env exp;
        Flambdaiter.apply_on_subexpressions (loop env) exp
  in
  let env = Variable.Set.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

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
