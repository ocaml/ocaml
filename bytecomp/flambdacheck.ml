(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Symbol
open Abstract_identifiers
open Flambda

let fatal_error_f fmt = Printf.kprintf Misc.fatal_error fmt

type 'a counter_example =
  | No_counter_example
  | Counter_example of 'a

exception Counter_example_id of Variable.t

let every_used_identifier_is_bound flam =
  let test var env =
    if not (VarSet.mem var env)
    then raise (Counter_example_id var) in
  let check env = function
    | Fassign(id,_,_)
    | Fvar(id,_) -> test id env
    | Fclosure({cl_specialised_arg},_) ->
        VarMap.iter (fun _ id -> test id env) cl_specialised_arg

    | Fsymbol _ | Fconst _ | Fapply _ | Ffunction _
    | Fvariable_in_closure _ | Flet _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Fevent _ | Funreachable _
      -> ()
  in
  let rec loop env = function
    | Flet(_,id,def,body,_) ->
        loop env def;
        loop (VarSet.add id env) body
    | Fletrec(defs,body,_) ->
        let env =
          List.fold_left (fun env (id,_) -> VarSet.add id env) env defs in
        List.iter (fun (_,def) -> loop env def) defs;
        loop env body
    | Fclosure ({cl_fun;cl_free_var},_) as exp ->
        check env exp;
        VarMap.iter (fun _ v -> loop env v) cl_free_var;
        VarMap.iter (fun _ { free_variables; body } -> loop free_variables body)
          cl_fun.funs
    | Ffor (id, lo, hi, _, body, _) ->
        loop env lo; loop env hi;
        loop (VarSet.add id env) body
    | Fstaticcatch (i, vars, body, handler,_) ->
        loop env body;
        let env = List.fold_right VarSet.add vars env in
        loop env handler
    | Ftrywith(body, id, handler,_) ->
        loop env body;
        loop (VarSet.add id env) handler

    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Ffunction _
    | Fvariable_in_closure _
    | Fprim _ | Fswitch _ | Fstaticraise _
    | Fifthenelse _ | Fsequence _
    | Fwhile _ | Fsend _ | Fevent _ | Funreachable _
      as exp ->
        check env exp;
        Flambdaiter.apply_on_subexpressions (loop env) exp
  in
  let env = VarSet.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

exception Counter_example_varset of VarSet.t

let function_free_variables_are_bound_in_the_closure_and_parameters flam =
  let f {cl_fun;cl_free_var} _ =
    let variables_in_closure = VarMap.keys cl_free_var in
    let functions_in_closure =
      VarMap.fold (fun id _ env -> VarSet.add id env)
        cl_fun.funs VarSet.empty in
    VarMap.iter (fun _ { params; free_variables } ->
        let acceptable_free_variables =
          VarSet.union
            (VarSet.union variables_in_closure functions_in_closure)
            (VarSet.of_list params) in
        let counter_examples =
          VarSet.diff free_variables acceptable_free_variables in
        if not (VarSet.is_empty counter_examples)
        then raise (Counter_example_varset counter_examples))
      cl_fun.funs
  in
  try
    Flambdaiter.iter_on_closures f flam;
    No_counter_example
  with Counter_example_varset set ->
    Counter_example set

let no_identifier_bound_multiple_times flam =
  let bound = ref VarSet.empty in
  let add_and_check id =
    if VarSet.mem id !bound
    then raise (Counter_example_id id)
    else bound := VarSet.add id !bound
  in
  let f = function
    | Flet(_,id,_,_,_) ->
        add_and_check id
    | Fletrec(defs,_,_) ->
        List.iter (fun (id,_) -> add_and_check id) defs
    | Fclosure ({cl_fun;cl_free_var},_) ->
        VarMap.iter (fun id _ -> add_and_check id) cl_free_var;
        VarMap.iter (fun _ { params } -> List.iter add_and_check params)
          cl_fun.funs
    | Ffor (id,_,_,_,_,_) ->
        add_and_check id
    | Fstaticcatch (_,vars,_,_,_) ->
        List.iter add_and_check vars
    | Ftrywith(_, id,_,_) ->
        add_and_check id

    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Ffunction _
    | Fvariable_in_closure _
    | Fprim _ | Fswitch _ | Fstaticraise _
    | Fifthenelse _ | Fsequence _
    | Fwhile _ | Fsend _ | Fevent _ | Funreachable _
      -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let every_bound_variable_is_from_current_compilation_unit
    ~current_compilation_unit flam =
  let check id =
    if not (Variable.in_compilation_unit current_compilation_unit id)
    then raise (Counter_example_id id)
  in
  let f = function
    | Flet(_,id,_,_,_) ->
        check id
    | Fletrec(defs,_,_) ->
        List.iter (fun (id,_) -> check id) defs
    | Fclosure ({cl_fun;cl_free_var},_) ->
        VarMap.iter (fun id _ -> check id) cl_free_var;
        VarMap.iter (fun _ { params } -> List.iter check params)
          cl_fun.funs
    | Ffor (id,_,_,_,_,_) ->
        check id
    | Fstaticcatch (_,vars,_,_,_) ->
        List.iter check vars
    | Ftrywith(_, id,_,_) ->
        check id

    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Ffunction _
    | Fvariable_in_closure _
    | Fprim _ | Fswitch _ | Fstaticraise _
    | Fifthenelse _ | Fsequence _
    | Fwhile _ | Fsend _ | Fevent _ | Funreachable _
      -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let no_assign_on_variable_of_kind_Not_assigned flam =
  let test var env =
    if not (VarSet.mem var env)
    then raise (Counter_example_id var) in
  let check env = function
    | Fassign(id,_,_) -> test id env
    | _ -> ()
  in
  let rec loop env = function
    | Flet(Assigned,id,def,body,_) ->
        loop env def;
        loop (VarSet.add id env) body
    | Fclosure ({cl_fun;cl_free_var},_) ->
        VarMap.iter (fun _ v -> loop env v) cl_free_var;
        let env = VarSet.empty in
        VarMap.iter (fun _ { body } -> loop env body) cl_fun.funs

    | Flet (Not_assigned, _, _, _, _)
    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Ffunction _
    | Fvariable_in_closure _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Fevent _ | Funreachable _
      as exp ->
        check env exp;
        Flambdaiter.apply_on_subexpressions (loop env) exp
  in
  let env = VarSet.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_id var ->
    Counter_example var

let declared_variable_within_closure flam =
  let bound = ref ClosureVariableSet.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if ClosureVariableSet.mem var !bound
    then bound_multiple_times := Some var;
    bound := ClosureVariableSet.add var !bound
  in
  let f {cl_fun;cl_free_var} _ =
    VarMap.iter (fun id _ ->
        let var = Closure_variable.wrap id in
        add_and_check var) cl_free_var
  in
  Flambdaiter.iter_on_closures f flam;
  !bound, !bound_multiple_times

let no_variable_within_closure_is_bound_multiple_times flam =
  match declared_variable_within_closure flam with
  | _, Some var -> Counter_example var
  | _, None -> No_counter_example

exception Counter_example_cu of compilation_unit

let every_declared_closure_is_from_current_compilation_unit
    ~current_compilation_unit flam =
  let f {cl_fun = { compilation_unit }} _ =
    if not (Compilation_unit.equal compilation_unit current_compilation_unit)
    then raise (Counter_example_cu compilation_unit)
  in
  try
    Flambdaiter.iter_on_closures f flam;
    No_counter_example
  with Counter_example_cu cu ->
    Counter_example cu

let declared_function_within_closure flam =
  let bound = ref ClosureFunctionSet.empty in
  let bound_multiple_times = ref None in
  let add_and_check var =
    if ClosureFunctionSet.mem var !bound
    then bound_multiple_times := Some var;
    bound := ClosureFunctionSet.add var !bound
  in
  let f = function
    | Fclosure ({cl_fun},_) ->
        VarMap.iter (fun id _ ->
            let var = Closure_function.wrap id in
            add_and_check var) cl_fun.funs
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !bound, !bound_multiple_times

let no_function_within_closure_is_bound_multiple_times flam =
  match declared_function_within_closure flam with
  | _, Some var -> Counter_example var
  | _, None -> No_counter_example

let used_function_within_closure flam =
  let used = ref ClosureFunctionSet.empty in
  let f = function
    | Ffunction ({fu_fun;fu_relative_to},_) ->
        used := ClosureFunctionSet.add fu_fun !used;
        (match fu_relative_to with
         | None -> ()
         | Some rel ->
             used := ClosureFunctionSet.add fu_fun !used)
    | Fvariable_in_closure ({vc_fun},_) ->
        used := ClosureFunctionSet.add vc_fun !used

    | Fassign _ | Fvar _ | Fclosure _
    | Fsymbol _ | Fconst _ | Fapply _
    | Flet _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Fevent _ | Funreachable _
      -> ()
  in
  Flambdaiter.iter f flam;
  !used

let used_variable_within_closure flam =
  let used = ref ClosureVariableSet.empty in
  let f = function
    | Fvariable_in_closure ({vc_var},_) ->
        used := ClosureVariableSet.add vc_var !used
    | _ -> ()
  in
  Flambdaiter.iter f flam;
  !used

let every_used_function_from_current_compilation_unit_is_declared
    ~current_compilation_unit flam =
  let declared, _ = declared_function_within_closure flam in
  let used = used_function_within_closure flam in
  let used_from_current_unit =
    ClosureFunctionSet.filter
      (Closure_function.in_compilation_unit current_compilation_unit)
      used in
  let counter_examples =
    ClosureFunctionSet.diff used_from_current_unit declared in
  if ClosureFunctionSet.is_empty counter_examples
  then No_counter_example
  else Counter_example counter_examples

let every_used_variable_in_closure_from_current_compilation_unit_is_declared
    ~current_compilation_unit flam =
  let declared, _ = declared_variable_within_closure flam in
  let used = used_variable_within_closure flam in
  let used_from_current_unit =
    ClosureVariableSet.filter
      (Closure_variable.in_compilation_unit current_compilation_unit)
      used in
  let counter_examples =
    ClosureVariableSet.diff used_from_current_unit declared in
  if ClosureVariableSet.is_empty counter_examples
  then No_counter_example
  else Counter_example counter_examples

exception Counter_example_se of static_exception

let every_static_exception_is_caught flam =
  let check env = function
    | Fstaticraise(exn,_,_) ->
        if not (StaticExceptionSet.mem exn env)
        then raise (Counter_example_se exn)
    | _ -> ()
  in
  let rec loop env = function
    | Fstaticcatch (i, _, body, handler,_) ->
        let env = StaticExceptionSet.add i env in
        loop env handler;
        loop env body
    | exp ->
        check env exp;
        Flambdaiter.apply_on_subexpressions (loop env) exp
  in
  let env = StaticExceptionSet.empty in
  try
    loop env flam;
    No_counter_example
  with Counter_example_se var ->
    Counter_example var

let every_static_exception_is_caught_at_a_single_position flam =
  let caught = ref StaticExceptionSet.empty in
  let f = function
    | Fstaticcatch (i, _, body, handler,_) ->
        if StaticExceptionSet.mem i !caught
        then raise (Counter_example_se i);
        caught := StaticExceptionSet.add i !caught
    | _ -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_se var ->
    Counter_example var

exception Counter_example_prim of Lambda.primitive

let no_access_to_global_module_identifiers flam =
  let open Lambda in
  let f = function
    | Fprim(Pgetglobalfield _ as p, _, _, _)
    | Fprim(Psetglobalfield _ as p, _, _, _)
    | Fprim(Psetglobal _ as p, _, _, _) ->
        raise (Counter_example_prim p)
    | Fprim(Pgetglobal id as p, _, _, _) ->
        if not (Ident.is_predef_exn id)
        then raise (Counter_example_prim p)
    | _ -> ()
  in
  try
    Flambdaiter.iter f flam;
    No_counter_example
  with Counter_example_prim p ->
    Counter_example p

let test result fmt printer =
  match result with
  | No_counter_example -> ()
  | Counter_example ce ->
      Misc.fatal_error (Format.asprintf fmt printer ce)

let check ~current_compilation_unit ?(flambdasym=false) ?(cmxfile=false) flam =
  test (every_used_identifier_is_bound flam)
    "Unbound identifier %a" Variable.print;

  test (function_free_variables_are_bound_in_the_closure_and_parameters flam)
    "Variables %a are in function free variables but are not bound in \
     the closure or in function parameters" VarSet.print;

  test (no_identifier_bound_multiple_times flam)
    "identifier bound multiple times %a" Variable.print;

  test (every_bound_variable_is_from_current_compilation_unit
          ~current_compilation_unit flam)
    "bound variable %a is attributed to another compilation unit"
    Variable.print;

  test (no_assign_on_variable_of_kind_Not_assigned flam)
    "variable %a of kind Not_assigned is assigned" Variable.print;

  test (no_variable_within_closure_is_bound_multiple_times flam)
    "variable within closure %a bound multiple times"
    Closure_variable.print;

  test (no_function_within_closure_is_bound_multiple_times flam)
    "function within closure %a bound multiple times"
    Closure_function.print;

  test (every_declared_closure_is_from_current_compilation_unit
          ~current_compilation_unit flam)
    "function declare using unit %a which is not the current one"
    Compilation_unit.print;

  if not flambdasym
  then
    test (every_used_function_from_current_compilation_unit_is_declared
            ~current_compilation_unit flam)
      "functions %a from the current compilation unit are used but \
       not declared"
      ClosureFunctionSet.print;

  if not flambdasym
  then
    test (every_used_variable_in_closure_from_current_compilation_unit_is_declared
            ~current_compilation_unit flam)
      "variables %a from the current compilation unit are used but \
       not declared"
      ClosureVariableSet.print;

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
