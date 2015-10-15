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

let rename_var var =
  Variable.rename var
    ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let remove_params unused (fun_decl: Flambda.function_declaration) =
  let unused_params, used_params =
    List.partition (fun v -> Variable.Set.mem v unused) fun_decl.params
  in
  let unused_params = List.filter (fun v ->
      Variable.Set.mem v fun_decl.free_variables) unused_params
  in
  let body =
    List.fold_left (fun body var ->
        Flambda.create_let var (Const (Const_pointer 0)) body)
      fun_decl.body
      unused_params
  in
  Flambda.create_function_declaration ~params:used_params ~body
    ~stub:fun_decl.stub ~dbg:fun_decl.dbg

let make_stub unused var (fun_decl : Flambda.function_declaration) =
  let renamed = rename_var var in
  let args' = List.map (fun var -> var, rename_var var) fun_decl.params in
  let args =
    List.map (fun (_, var) -> var)
      (List.filter (fun (var, _) -> not (Variable.Set.mem var unused)) args')
  in
  let kind = Flambda.Direct (Closure_id.wrap renamed) in
  let dbg = fun_decl.dbg in
  let body : Flambda.t =
    Apply {
      func = renamed;
      args;
      kind;
      dbg;
    }
  in
  let function_decl =
    Flambda.create_function_declaration ~params:(List.map snd args') ~body
      ~stub:true ~dbg:fun_decl.dbg
  in
  function_decl, renamed

let separate_unused_arguments (set_of_closures : Flambda.set_of_closures) =
  let function_decls = set_of_closures.function_decls in
  let unused = Invariant_params.unused_arguments function_decls in
  let non_stub_arguments =
    Variable.Map.fold (fun _ (decl : Flambda.function_declaration) acc ->
        if decl.stub then
          acc
        else
          Variable.Set.union acc (Variable.Set.of_list decl.Flambda.params))
      function_decls.funs Variable.Set.empty
  in
  let unused = Variable.Set.inter non_stub_arguments unused in
  if Variable.Set.is_empty unused
  then None
  else begin
    let funs =
      Variable.Map.fold (fun fun_id (fun_decl : Flambda.function_declaration) acc ->
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
        function_decls.funs Variable.Map.empty
    in
    let specialised_args =
      Variable.Map.filter (fun param _ -> not (Variable.Set.mem param unused))
        set_of_closures.specialised_args
    in
    let function_decls =
      Flambda.update_function_declarations function_decls ~funs
    in
    let set_of_closures =
      Flambda.create_set_of_closures ~function_decls
        ~free_vars:set_of_closures.free_vars ~specialised_args
    in
    Some set_of_closures
  end

(* Spliting is not always beneficial. For instance when a function
   is only indirectly called, suppressing unused arguments does not
   benefit, and introduce an useless intermediate call *)
let candidate_for_spliting_for_unused_arguments
    (fun_decls : Flambda.function_declarations)
    ~backend =
  let no_recursive_functions =
    Variable.Set.is_empty
      (Find_recursive_functions.in_function_declarations fun_decls ~backend)
  in
  let number_of_non_stub_functions =
    Variable.Map.cardinal
      (Variable.Map.filter (fun _ { Flambda.stub } -> not stub)
         fun_decls.funs)
  in
  (not no_recursive_functions) || (number_of_non_stub_functions > 1)

let separate_unused_arguments_in_closures_expr ~force tree ~backend =
  let aux_named (named : Flambda.named) : Flambda.named =
    match named with
    | Set_of_closures set_of_closures ->
      if force <> None ||
        candidate_for_spliting_for_unused_arguments
          set_of_closures.function_decls
          ~backend
      then begin
        match separate_unused_arguments set_of_closures with
        | None -> named
        | Some set_of_closures -> Set_of_closures set_of_closures
      end else begin
        named
      end
    | e -> e
  in
  Flambda_iterators.map_named aux_named tree

let separate_unused_arguments_in_closures ?force program ~backend =
  Flambda_iterators.map_exprs_at_toplevel_of_program program ~f:(fun expr ->
    separate_unused_arguments_in_closures_expr ~force expr ~backend)
