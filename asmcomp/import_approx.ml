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

module A = Simple_value_approx

let reexported_missing_symbols = Symbol.Tbl.create 0

let import_set_of_closures =
  let import_function_declarations (clos : Flambda.function_declarations)
        : Flambda.function_declarations =
    let orig_var_map (clos : Flambda.function_declarations) =
      Variable.Map.fold (fun id _ acc ->
           let fun_id = Closure_id.wrap id in
           let sym = Compilenv.closure_symbol fun_id in
           Symbol.Map.add sym id acc)
        clos.funs Symbol.Map.empty
    in
    let sym_map = orig_var_map clos in
    let f_named (named : Flambda.named) =
      match named with
      | Symbol sym ->
        begin try Flambda.Expr (Var (Symbol.Map.find sym sym_map)) with
        | Not_found -> named
        end
      | named -> named
    in
    { clos with
      funs =
        Variable.Map.map (fun (function_decl : Flambda.function_declaration) ->
            let body =
              Flambda_iterators.map_toplevel_named f_named function_decl.body
            in
            Flambda.create_function_declaration ~params:function_decl.params
              ~body ~stub:function_decl.stub ~dbg:function_decl.dbg)
          clos.funs;
    }
  in
  let aux set_of_closures_id =
    let ex_info = Compilenv.approx_env () in
    let function_declarations =
      try Set_of_closures_id.Map.find set_of_closures_id ex_info.ex_functions
      with Not_found ->
        Misc.fatal_errorf "[ex_functions] does not map set of closures ID %a. \
            ex_info = %a"
          Set_of_closures_id.print set_of_closures_id
          Flambdaexport.print_all ex_info
    in
    import_function_declarations function_declarations
  in
  Set_of_closures_id.Tbl.memoize Compilenv.imported_sets_of_closures_table aux

let rec import_ex ex =
  ignore (Compilenv.approx_for_global (Export_id.unit ex));
  let ex_info = Compilenv.approx_env () in
  match Flambdaexport.find_description ex ex_info with
  | exception Not_found -> A.value_unknown
  | Value_int i -> A.value_int i
  | Value_constptr i -> A.value_constptr i
  | Value_float f -> A.value_float f
  | Value_float_array size -> A.value_float_array size
  | Flambdaexport_types.Value_boxed_int (t,i) -> A.value_boxed_int t i
  | Value_string { size; contents } -> A.value_string size contents
  | Value_mutable_block _ -> A.value_unknown
  | Value_block (tag, fields) ->
    A.value_block (tag, Array.map import_approx fields)
  | Value_closure { fun_id;
        set_of_closures = { set_of_closures_id; bound_vars } } ->
    let bound_vars = Var_within_closure.Map.map import_approx bound_vars in
    begin match
      Set_of_closures_id.Map.find set_of_closures_id
        ex_info.ex_invariant_arguments
    with
    | exception Not_found ->
      Misc.fatal_error "Set of closures ID not found in ex_invariant_arguments"
    | unchanging_params ->
      let value_set_of_closures : A.value_set_of_closures =
        { function_decls = import_set_of_closures set_of_closures_id;
          bound_vars;
          unchanging_params = unchanging_params;
          specialised_args = Variable.Set.empty;
          freshening = Freshening.Project_var.empty;
        }
      in
      A.value_closure value_set_of_closures fun_id
    end
  | Value_set_of_closures { set_of_closures_id; bound_vars } ->
    let bound_vars = Var_within_closure.Map.map import_approx bound_vars in
    let unchanging_params =
      try
        Set_of_closures_id.Map.find set_of_closures_id
          ex_info.ex_invariant_arguments
      with
      | Not_found ->
        Misc.fatal_errorf "Export description of [Value_set_of_closures] \
            names [set_of_closures_id] %a that is not present in \
            [invariant_arguments]"
          Set_of_closures_id.print set_of_closures_id
    in
    let value_set_of_closures : A.value_set_of_closures =
      { function_decls = import_set_of_closures set_of_closures_id;
        bound_vars;
        unchanging_params = unchanging_params;
        specialised_args = Variable.Set.empty;
        freshening = Freshening.Project_var.empty;
      }
    in
    A.value_set_of_closures value_set_of_closures

and import_approx (ap : Flambdaexport_types.approx) =
  match ap with
  | Value_unknown -> A.value_unknown
  | Value_id ex -> A.value_extern ex
  | Value_symbol sym -> A.value_symbol sym

let import_symbol sym =
  if Compilenv.is_predefined_exception sym then
    A.value_unknown
  else
    let symbol_id_map =
      let global = Symbol.compilation_unit sym in
      (Compilenv.approx_for_global global).ex_symbol_id
    in
    match Symbol.Map.find sym symbol_id_map with
    | approx -> A.augment_with_symbol (import_ex approx) sym
    | exception Not_found ->
      if not (Symbol.Tbl.mem reexported_missing_symbols sym) then begin
        Symbol.Tbl.add reexported_missing_symbols sym ();
        (* CR mshinwell: fix "some_file" *)
        (* CR mshinwell: Shouldn't this be a fatal error if it's the
           current compilation unit? *)
        Location.prerr_warning (Location.in_file "some_file")
          (Warnings.Missing_symbol_information
             (Format.asprintf "%a" Symbol.print sym,
              Format.asprintf "%a" Compilation_unit.print
                (Symbol.compilation_unit sym)));
      end;
      A.value_unresolved sym

let rec really_import (approx : A.descr) =
  match approx with
  | Value_extern ex -> really_import_ex ex
  | Value_symbol sym -> really_import_symbol sym
  | r -> r

and really_import_ex ex =
  really_import (import_ex ex).descr

and really_import_symbol sym =
  really_import (import_symbol sym).descr

let import_global id =
  let unit = Compilenv.unit_for_global id in
  import_approx
    (Ident.Map.find id (Compilenv.approx_for_global unit).ex_globals)

let really_import_approx (approx : Simple_value_approx.t) =
  A.replace_description approx (really_import approx.descr)
