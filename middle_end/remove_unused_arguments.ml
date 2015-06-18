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

open Abstract_identifiers

let rename_var var =
  Variable.rename var
    ~current_compilation_unit:(Symbol.Compilation_unit.get_current_exn ())

let nid () = Expr_id.create ()

let remove_params unused (fun_decl: _ Flambda.function_declaration) =
  let unused_params, used_params =
    List.partition (fun v -> Variable.Set.mem v unused) fun_decl.params
  in
  let unused_params = List.filter (fun v ->
      Variable.Set.mem v fun_decl.free_variables) unused_params
  in
  let body = List.fold_left (fun body var ->
      Flambda.Flet(Immutable,
           var,
           Fconst(Fconst_pointer 0, nid ()),
           body,
           nid ()))
      fun_decl.body
      unused_params
  in
  let free_variables =
    Variable.Set.filter (fun var -> not (Variable.Set.mem var unused))
      fun_decl.free_variables
  in
  { fun_decl with
    params = used_params;
    free_variables;
    body;
  }

let make_stub unused var (fun_decl : _ Flambda.function_declaration) =
  let renamed = rename_var var in
  let args' = List.map (fun var -> var, rename_var var) fun_decl.params in
  let args =
    List.map (fun (_, var) -> Flambda.Fvar(var, nid ()))
      (List.filter (fun (var, _) -> not (Variable.Set.mem var unused)) args')
  in
  let kind = Flambda.Direct (Closure_id.wrap renamed) in
  let dbg = fun_decl.dbg in
  let body : _ Flambda.t =
    Fapply(
      { func = Fvar(renamed, nid ());
        args;
        kind;
        dbg },
      nid ())
  in
  let free_variables =
    List.fold_left
      (fun set (_, renamed_arg) -> Variable.Set.add renamed_arg set)
      (Variable.Set.singleton renamed)
      args'
  in
  let decl : _ Flambda.function_declaration = {
    params = List.map snd args';
    body;
    free_variables;
    stub = true;
    dbg = fun_decl.dbg;
  }
  in
  decl, renamed

let separate_unused_arguments (set_of_closures : _ Flambda.set_of_closures) =
  let decl = set_of_closures.function_decls in
  let unused = Invariant_params.unused_arguments decl in
  let non_stub_arguments =
    Variable.Map.fold (fun _ (decl : _ Flambda.function_declaration) acc ->
        if decl.stub then
          acc
        else
          Variable.Set.union acc (Variable.Set.of_list decl.Flambda.params))
      decl.funs Variable.Set.empty
  in
  let unused = Variable.Set.inter non_stub_arguments unused in
  if Variable.Set.is_empty unused
  then None
  else begin
    let funs =
      Variable.Map.fold (fun fun_id (fun_decl : _ Flambda.function_declaration) acc ->
          if List.exists (fun v -> Variable.Set.mem v unused) fun_decl.params
          then begin
            let stub, renamed_fun_id = make_stub unused fun_id fun_decl in
            let cleaned = remove_params unused fun_decl in
            Variable.Map.add fun_id stub
              (Variable.Map.add renamed_fun_id cleaned acc)
          end
          else
            Variable.Map.add fun_id fun_decl acc
        )
        decl.funs Variable.Map.empty
    in
    let specialised_args =
      Variable.Map.filter (fun param _ -> not (Variable.Set.mem param unused))
        set_of_closures.specialised_args
    in
    Some
      { set_of_closures with
        function_decls = { decl with funs };
        specialised_args; }
  end

(* Spliting is not always beneficial. For instance when a function
   is only indirectly called, suppressing unused arguments does not
   benefit, and introduce an useless intermediate call *)
let candidate_for_spliting_for_unused_arguments
    (fun_decl : _ Flambda.function_declarations) =
  let no_recursive_functions =
    Variable.Set.is_empty
      (Flambdautils.recursive_functions fun_decl)
  in
  let number_of_non_stub_functions =
    Variable.Map.cardinal
      (Variable.Map.filter (fun _ { Flambda.stub } -> not stub)
         fun_decl.funs)
  in
  (not no_recursive_functions) ||
  (number_of_non_stub_functions > 1)

let separate_unused_arguments_in_closures tree =
  let aux (expr : _ Flambda.t) : _ Flambda.t =
    match expr with
    | Fset_of_closures (set_of_closures, eid) -> begin
        if candidate_for_spliting_for_unused_arguments
            set_of_closures.function_decls then
          match separate_unused_arguments set_of_closures with
          | None -> expr
          | Some set_of_closures ->
              Fset_of_closures (set_of_closures, eid)
        else
          expr
      end
    | e -> e
  in
  Flambdaiter.map aux tree
