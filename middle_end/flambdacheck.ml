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
    if not (Variable.Set.mem var env)
    then raise (Counter_example_id var) in
  let check env (flam : _ Flambda.t) =
    match flam with
    | Fassign(id,_,_)
    | Fvar(id,_) -> test id env
    | Fset_of_closures({specialised_args},_) ->
        Variable.Map.iter (fun _ id -> test id env) specialised_args

    | Fsymbol _ | Fconst _ | Fapply _ | Fselect_closure _
    | Fvar_within_closure _ | Flet _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstringswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Funreachable _
      -> ()
  in
  let rec loop env (flam : _ Flambda.t) =
    match flam with
    | Flet(_,id,def,body,_) ->
        loop env def;
        loop (Variable.Set.add id env) body
    | Fletrec(defs,body,_) ->
        let env =
          List.fold_left (fun env (id,_) -> Variable.Set.add id env) env defs in
        List.iter (fun (_,def) -> loop env def) defs;
        loop env body
    | Fset_of_closures ({function_decls;free_vars},_) as exp ->
        check env exp;
        Variable.Map.iter (fun _ v -> loop env v) free_vars;
        Variable.Map.iter (fun _ { Flambda. free_variables; body } ->
            loop free_variables body)
          function_decls.funs
    | Ffor (id, lo, hi, _, body, _) ->
        loop env lo; loop env hi;
        loop (Variable.Set.add id env) body
    | Fstaticcatch (_i, vars, body, handler,_) ->
        loop env body;
        let env = List.fold_right Variable.Set.add vars env in
        loop env handler
    | Ftrywith(body, id, handler,_) ->
        loop env body;
        loop (Variable.Set.add id env) handler

    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Fselect_closure _
    | Fvar_within_closure _
    | Fprim _ | Fswitch _ | Fstringswitch _ | Fstaticraise _
    | Fifthenelse _ | Fsequence _
    | Fwhile _ | Fsend _ | Funreachable _
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
    Flambdaiter.iter_on_closures f flam;
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
  let f (flam : _ Flambda.t) =
    match flam with
    | Flet(_,id,_,_,_) ->
        add_and_check id
    | Fletrec(defs,_,_) ->
        List.iter (fun (id,_) -> add_and_check id) defs
    | Fset_of_closures ({function_decls;free_vars},_) ->
        Variable.Map.iter (fun id _ -> add_and_check id) free_vars;
        Variable.Map.iter (fun _ { Flambda. params } ->
            List.iter add_and_check params)
          function_decls.funs
    | Ffor (id,_,_,_,_,_) ->
        add_and_check id
    | Fstaticcatch (_,vars,_,_,_) ->
        List.iter add_and_check vars
    | Ftrywith(_, id,_,_) ->
        add_and_check id

    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Fselect_closure _
    | Fvar_within_closure _
    | Fprim _ | Fswitch _ | Fstringswitch _ | Fstaticraise _
    | Fifthenelse _ | Fsequence _
    | Fwhile _ | Fsend _ | Funreachable _
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
  let f (flam : _ Flambda.t) =
    match flam with
    | Flet(_,id,_,_,_) ->
        check id
    | Fletrec(defs,_,_) ->
        List.iter (fun (id,_) -> check id) defs
    | Fset_of_closures ({function_decls;free_vars},_) ->
        Variable.Map.iter (fun id _ -> check id) free_vars;
        Variable.Map.iter (fun _ { Flambda. params } ->
            List.iter check params)
          function_decls.funs
    | Ffor (id,_,_,_,_,_) ->
        check id
    | Fstaticcatch (_,vars,_,_,_) ->
        List.iter check vars
    | Ftrywith(_, id,_,_) ->
        check id

    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Fselect_closure _
    | Fvar_within_closure _
    | Fprim _ | Fswitch _ | Fstringswitch _ | Fstaticraise _
    | Fifthenelse _ | Fsequence _
    | Fwhile _ | Fsend _ | Funreachable _
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
  let check env (flam : _ Flambda.t) =
    match flam with
    | Fassign(id,_,_) -> test id env
    | _ -> ()
  in
  let rec loop env (flam : _ Flambda.t) =
    match flam with
    | Flet(Mutable,id,def,body,_) ->
        loop env def;
        loop (Variable.Set.add id env) body
    | Fset_of_closures ({ Flambda. function_decls;free_vars},_) ->
        Variable.Map.iter (fun _ v -> loop env v) free_vars;
        let env = Variable.Set.empty in
        Variable.Map.iter (fun _ { Flambda. body } -> loop env body)
          function_decls.funs
    | Flet (Immutable, _, _, _, _)
    | Fassign _ | Fvar _
    | Fsymbol _ | Fconst _ | Fapply _ | Fselect_closure _
    | Fvar_within_closure _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstringswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Funreachable _
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
  Flambdaiter.iter_on_closures f flam;
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
    Flambdaiter.iter_on_closures f flam;
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
  let f (flam : _ Flambda.t) =
    match flam with
    | Fset_of_closures ({function_decls},_) ->
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
  let f (flam : _ Flambda.t) =
    match flam with
    | Fselect_closure ({closure_id;relative_to},_) ->
        used := Closure_id.Set.add closure_id !used;
        (match relative_to with
         | None -> ()
         | Some _rel ->
             used := Closure_id.Set.add closure_id !used)
    | Fvar_within_closure ({closure_id},_) ->
        used := Closure_id.Set.add closure_id !used

    | Fassign _ | Fvar _ | Fset_of_closures _
    | Fsymbol _ | Fconst _ | Fapply _
    | Flet _ | Fletrec _
    | Fprim _ | Fswitch _ | Fstringswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fifthenelse _ | Fsequence _
    | Fwhile _ | Ffor _ | Fsend _ | Funreachable _
      -> ()
  in
  Flambdaiter.iter f flam;
  !used

let used_var_within_closure flam =
  let used = ref Var_within_closure.Set.empty in
  let f (flam : _ Flambda.t) =
    match flam with
    | Fvar_within_closure ({var},_) ->
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
  let check env (flam : _ Flambda.t) =
    match flam with 
    | Fstaticraise(exn,_,_) ->
        if not (Static_exception.Set.mem exn env)
        then raise (Counter_example_se exn)
    | _ -> ()
  in
  let rec loop env (flam : _ Flambda.t) =
    match flam with
    | Fstaticcatch (i, _, body, handler,_) ->
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
  let f (flam : _ Flambda.t) =
    match flam with
    | Fstaticcatch (i, _, _body, _handler,_) ->
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
  let f (flam : _ Flambda.t) =
    match flam with
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
