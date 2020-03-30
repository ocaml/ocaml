(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Fu Yong Quah, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module A = Simple_value_approx

type queue_elem =
  | Q_symbol of Symbol.t
  | Q_set_of_closures_id of Set_of_closures_id.t
  | Q_export_id of Export_id.t

type symbols_to_export =
  { symbols                               : Symbol.Set.t;
    export_ids                            : Export_id.Set.t;
    set_of_closure_ids                    : Set_of_closures_id.Set.t;
    set_of_closure_ids_keep_declaration   : Set_of_closures_id.Set.t;
    relevant_imported_closure_ids         : Closure_id.Set.t;
    relevant_local_closure_ids            : Closure_id.Set.t;
    relevant_imported_vars_within_closure : Var_within_closure.Set.t;
    relevant_local_vars_within_closure    : Var_within_closure.Set.t;
  }

let traverse
      ~(sets_of_closures_map :
          Flambda.set_of_closures Set_of_closures_id.Map.t)
      ~(closure_id_to_set_of_closures_id :
          Set_of_closures_id.t Closure_id.Map.t)
      ~(function_declarations_map :
          A.function_declarations Set_of_closures_id.Map.t)
      ~(values : Export_info.descr Export_id.Map.t)
      ~(symbol_id : Export_id.t Symbol.Map.t)
      ~(root_symbol: Symbol.t) =
  let relevant_set_of_closures_declaration_only =
    ref Set_of_closures_id.Set.empty
  in
  let relevant_symbols = ref (Symbol.Set.singleton root_symbol) in
  let relevant_set_of_closures = ref Set_of_closures_id.Set.empty in
  let relevant_export_ids = ref Export_id.Set.empty in
  let relevant_imported_closure_ids = ref Closure_id.Set.empty in
  let relevant_local_closure_ids = ref Closure_id.Set.empty in
  let relevant_imported_vars_within_closure =
    ref Var_within_closure.Set.empty
  in
  let relevant_local_vars_with_closure = ref Var_within_closure.Set.empty in
  let (queue : queue_elem Queue.t) = Queue.create () in
  let conditionally_add_symbol symbol =
    if not (Symbol.Set.mem symbol !relevant_symbols) then begin
      relevant_symbols :=
        Symbol.Set.add symbol !relevant_symbols;
      Queue.add (Q_symbol symbol) queue
    end
  in
  let conditionally_add_set_of_closures_id set_of_closures_id =
    if not (Set_of_closures_id.Set.mem
         set_of_closures_id !relevant_set_of_closures) then begin
      relevant_set_of_closures :=
        Set_of_closures_id.Set.add set_of_closures_id
          !relevant_set_of_closures;
      Queue.add (Q_set_of_closures_id set_of_closures_id) queue
    end
  in
  let conditionally_add_export_id export_id =
    if not (Export_id.Set.mem export_id !relevant_export_ids) then begin
      relevant_export_ids :=
        Export_id.Set.add export_id !relevant_export_ids;
      Queue.add (Q_export_id export_id) queue
    end
  in
  let process_approx (approx : Export_info.approx) =
    match approx with
    | Value_id export_id ->
      conditionally_add_export_id export_id
    | Value_symbol symbol ->
      conditionally_add_symbol symbol
    | Value_unknown -> ()
  in
  let process_value_set_of_closures
        (soc : Export_info.value_set_of_closures) =
    conditionally_add_set_of_closures_id soc.set_of_closures_id;
    Var_within_closure.Map.iter
      (fun _ value -> process_approx value) soc.bound_vars;
    Closure_id.Map.iter
      (fun _ value -> process_approx value) soc.results;
    begin match soc.aliased_symbol with
    | None -> ()
    | Some symbol -> conditionally_add_symbol symbol
    end
  in
  let process_function_body (function_body : A.function_body) =
    Flambda_iterators.iter
      (fun (term : Flambda.t) ->
         match term with
         | Flambda.Apply { kind ; _ } ->
           begin match kind with
           | Indirect -> ()
           | Direct closure_id ->
             begin match
               Closure_id.Map.find
                 closure_id
                 closure_id_to_set_of_closures_id
             with
             | exception Not_found ->
               relevant_imported_closure_ids :=
                 Closure_id.Set.add closure_id
                   !relevant_imported_closure_ids
             | set_of_closures_id ->
               relevant_local_closure_ids :=
                 Closure_id.Set.add closure_id
                   !relevant_local_closure_ids;
               conditionally_add_set_of_closures_id
                 set_of_closures_id
             end
           end
         | _ -> ())
      (fun (named : Flambda.named) ->
         let process_closure_id closure_id =
           match
             Closure_id.Map.find closure_id closure_id_to_set_of_closures_id
           with
           | exception Not_found ->
             relevant_imported_closure_ids :=
               Closure_id.Set.add closure_id !relevant_imported_closure_ids
           | set_of_closure_id ->
             relevant_local_closure_ids :=
               Closure_id.Set.add closure_id !relevant_local_closure_ids;
             relevant_set_of_closures_declaration_only :=
               Set_of_closures_id.Set.add
                 set_of_closure_id
                 !relevant_set_of_closures_declaration_only
         in
         match named with
         | Symbol symbol
         | Read_symbol_field (symbol, _) ->
           conditionally_add_symbol symbol
         | Set_of_closures soc ->
           conditionally_add_set_of_closures_id
             soc.function_decls.set_of_closures_id
         | Project_closure { closure_id; _ } ->
           process_closure_id closure_id
         | Move_within_set_of_closures { start_from; move_to; _ } ->
           process_closure_id start_from;
           process_closure_id move_to
         | Project_var { closure_id ; var; _ } ->
           begin match
             Closure_id.Map.find
               closure_id closure_id_to_set_of_closures_id
           with
           | exception Not_found ->
             relevant_imported_closure_ids :=
               Closure_id.Set.add closure_id
                 !relevant_imported_closure_ids;
             relevant_imported_vars_within_closure :=
               Var_within_closure.Set.add var
                 !relevant_imported_vars_within_closure
           | set_of_closure_id ->
             relevant_local_closure_ids :=
               Closure_id.Set.add closure_id
                 !relevant_local_closure_ids;
             relevant_local_vars_with_closure :=
               Var_within_closure.Set.add var
                 !relevant_local_vars_with_closure;
             relevant_set_of_closures_declaration_only :=
               Set_of_closures_id.Set.add
                 set_of_closure_id
                 !relevant_set_of_closures_declaration_only
           end
         | Prim _
         | Expr _
         | Const _
         | Allocated_const _
         | Read_mutable _ -> ())
      function_body.body
  in
  let rec loop () =
    if Queue.is_empty queue then
      ()
    else begin
      begin match Queue.pop queue with
      | Q_export_id export_id ->
        begin match Export_id.Map.find export_id values with
        | exception Not_found -> ()
        | Value_block (_, approxes) ->
          Array.iter process_approx approxes
        | Value_closure value_closure ->
          process_value_set_of_closures value_closure.set_of_closures
        | Value_set_of_closures soc ->
          process_value_set_of_closures soc
        | _ -> ()
        end
      | Q_symbol symbol ->
        let compilation_unit = Symbol.compilation_unit symbol in
        if Compilation_unit.is_current compilation_unit then begin
          match Symbol.Map.find symbol symbol_id with
          | exception Not_found ->
            Misc.fatal_errorf "cannot find symbol's export id %a\n"
              Symbol.print symbol
          | export_id ->
            conditionally_add_export_id export_id
        end
      | Q_set_of_closures_id set_of_closures_id ->
        begin match
          Set_of_closures_id.Map.find
            set_of_closures_id function_declarations_map
        with
        | exception Not_found -> ()
        | function_declarations ->
          Variable.Map.iter
            (fun (_ : Variable.t) (fun_decl : A.function_declaration) ->
               match fun_decl.function_body with
               | None -> ()
               | Some function_body -> process_function_body function_body)
            function_declarations.funs
        end
      end;
      loop ()
    end
  in
  Queue.add (Q_symbol root_symbol) queue;
  loop ();

  Closure_id.Map.iter (fun closure_id set_of_closure_id ->
      if Set_of_closures_id.Set.mem
           set_of_closure_id !relevant_set_of_closures
      then begin
        relevant_local_closure_ids :=
          Closure_id.Set.add closure_id !relevant_local_closure_ids
      end)
    closure_id_to_set_of_closures_id;

  Set_of_closures_id.Set.iter (fun set_of_closures_id ->
      match
        Set_of_closures_id.Map.find set_of_closures_id sets_of_closures_map
      with
      | exception Not_found -> ()
      | set_of_closures ->
        Variable.Map.iter (fun var _ ->
            relevant_local_vars_with_closure :=
              Var_within_closure.Set.add
                (Var_within_closure.wrap var)
                !relevant_local_vars_with_closure)
          set_of_closures.free_vars)
    !relevant_set_of_closures;

  { symbols                             = !relevant_symbols;
    export_ids                          = !relevant_export_ids;
    set_of_closure_ids                  = !relevant_set_of_closures;
    set_of_closure_ids_keep_declaration =
      !relevant_set_of_closures_declaration_only;
    relevant_imported_closure_ids       = !relevant_imported_closure_ids;
    relevant_local_closure_ids          = !relevant_local_closure_ids;
    relevant_imported_vars_within_closure =
      !relevant_imported_vars_within_closure;
    relevant_local_vars_within_closure =
      !relevant_local_vars_with_closure;
  }
