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

let pass_name = "remove-unused-arguments"
let () = Clflags.all_passes := pass_name :: !Clflags.all_passes

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
    ~stub:fun_decl.stub ~dbg:fun_decl.dbg ~inline:fun_decl.inline
    ~is_a_functor:fun_decl.is_a_functor

let make_stub unused var (fun_decl : Flambda.function_declaration)
    ~specialised_args ~additional_specialised_args =
  let renamed = rename_var var in
  let args' =
    List.map (fun var -> var, rename_var var) fun_decl.params
  in
  let used_args' =
    List.filter (fun (var, _) -> not (Variable.Set.mem var unused)) args'
  in
  let additional_specialised_args =
    List.fold_left (fun additional_specialised_args (original_arg,arg) ->
        match Variable.Map.find original_arg specialised_args with
        | exception Not_found -> additional_specialised_args
        | outside_var ->
          Variable.Map.add arg outside_var additional_specialised_args)
      additional_specialised_args used_args'
  in
  let args = List.map (fun (_, var) -> var) used_args' in
  let kind = Flambda.Direct (Closure_id.wrap renamed) in
  let dbg = fun_decl.dbg in
  let body : Flambda.t =
    Apply {
      func = renamed;
      args;
      kind;
      dbg;
      inline = Default_inline;
    }
  in
  let function_decl =
    Flambda.create_function_declaration ~params:(List.map snd args') ~body
      ~stub:true ~dbg:fun_decl.dbg ~inline:fun_decl.inline
      ~is_a_functor:fun_decl.is_a_functor
  in
  function_decl, renamed, additional_specialised_args

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
    let funs, additional_specialised_args =
      Variable.Map.fold (fun fun_id (fun_decl : Flambda.function_declaration)
                          (funs, additional_specialised_args) ->
          if List.exists (fun v -> Variable.Set.mem v unused) fun_decl.params
          then begin
            let stub, renamed_fun_id, additional_specialised_args =
              make_stub unused fun_id fun_decl
                ~specialised_args:set_of_closures.specialised_args
                ~additional_specialised_args
            in
            let cleaned = remove_params unused fun_decl in
            Variable.Map.add fun_id stub
              (Variable.Map.add renamed_fun_id cleaned funs),
            additional_specialised_args
          end
          else
            Variable.Map.add fun_id fun_decl funs,
            additional_specialised_args
        )
        function_decls.funs (Variable.Map.empty, Variable.Map.empty)
    in
    let specialised_args =
      Variable.Map.disjoint_union additional_specialised_args
        (Variable.Map.filter (fun param _ -> not (Variable.Set.mem param unused))
           set_of_closures.specialised_args)
    in
    let specialised_args =
      Variable.Map.map (fun (spec_to : Flambda.specialised_to) ->
          match spec_to.projectee with
          | None -> spec_to
          | Some (from, _projectee) ->
            if Variable.Map.mem from specialised_args then
              spec_to
            else
              ({ spec_to with projectee = None; } : Flambda.specialised_to))
        specialised_args
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
  if not !Clflags.remove_unused_arguments then begin
    false
  end else begin
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
  end

let separate_unused_arguments_in_set_of_closures set_of_closures ~backend =
  let dump = Clflags.dumped_pass pass_name in
  if candidate_for_spliting_for_unused_arguments
      set_of_closures.Flambda.function_decls
      ~backend
  then
    match separate_unused_arguments set_of_closures with
    | None ->
      if dump then
        Format.eprintf "No change for Remove_unused_arguments:@ %a@.@."
          Flambda.print_set_of_closures set_of_closures;
      set_of_closures
    | Some result ->
      if dump then
        Format.eprintf "Before Remove_unused_arguments:@ %a@.@.\
                        After Remove_unused_arguments:@ %a@.@."
          Flambda.print_set_of_closures set_of_closures
          Flambda.print_set_of_closures result;
      result
  else set_of_closures

let separate_unused_arguments_in_closures_expr tree ~backend =
  let aux_named (named : Flambda.named) : Flambda.named =
    match named with
    | Set_of_closures set_of_closures ->
      if candidate_for_spliting_for_unused_arguments
          set_of_closures.function_decls ~backend
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

let separate_unused_arguments_in_closures program ~backend =
  Flambda_iterators.map_exprs_at_toplevel_of_program program ~f:(fun expr ->
    separate_unused_arguments_in_closures_expr expr ~backend)
