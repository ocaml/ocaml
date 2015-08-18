

let update_constant_for_sharing sharing_symbol_tbl const : Flambda.constant_defining_value =
  let substitute_symbol sym =
    match Symbol.Tbl.find sharing_symbol_tbl sym with
    | exception Not_found -> sym
    | symbol -> symbol
  in
  match (const:Flambda.constant_defining_value) with
  | Allocated_const _ -> const
  | Block (tag, fields) ->
    let subst_field (field:Flambda.constant_defining_value_block_field) :
      Flambda.constant_defining_value_block_field =
      match field with
      | Const _ -> field
      | Symbol sym ->
        Symbol (substitute_symbol sym)
    in
    let fields = List.map subst_field fields in
    Block (tag, fields)
  | Set_of_closures set_of_closures ->
    Set_of_closures (
      Flambda_iterators.map_symbols_on_set_of_closures
        ~f:substitute_symbol set_of_closures
    )
  | Project_closure (sym, closure_id) ->
    Project_closure (substitute_symbol sym, closure_id)

(** Find constants assigned to multiple symbols and choose a unique symbol
    for them, thus sharing constants. *)
let share_constants ~symbol_to_constant sorted_symbols =
  let sharing_symbol_tbl = Symbol.Tbl.create 42 in
  let symbol_to_definition_tbl = Symbol.Tbl.create 42 in
  let constant_to_symbol_tbl = Constant_defining_value.Tbl.create 42 in
  let cannot_share (const : Flambda.constant_defining_value) =
    match const with
    (* Strings and float arrays are mutable; we never share them. *)
    | Allocated_const ((String _) | (Float_array _)) -> true
    | Allocated_const _ | Set_of_closures _ | Project_closure _ | Block _ ->
      false
  in
  List.iter (fun original_symbol ->
      let constant_defining_value =
        update_constant_for_sharing sharing_symbol_tbl
          (Symbol.Map.find original_symbol symbol_to_constant)
      in
      let symbol =
        match
          Constant_defining_value.Tbl.find constant_to_symbol_tbl
            constant_defining_value
        with
        | exception Not_found -> original_symbol
        | _existing_symbol when cannot_share constant_defining_value -> original_symbol
        | existing_symbol -> existing_symbol
      in
      Symbol.Tbl.add sharing_symbol_tbl original_symbol symbol;
      Symbol.Tbl.replace symbol_to_definition_tbl symbol constant_defining_value;
      Constant_defining_value.Tbl.replace constant_to_symbol_tbl
        constant_defining_value symbol)
    sorted_symbols;
  sharing_symbol_tbl, symbol_to_definition_tbl

let share_constants program =
  

    (* let sorted_constants = *)
  (*   compute_order_of_let_symbol_bindings *)
  (*     (aliases:Alias_analysis.allocation_point Variable.Map.t) *)
  (*     (var_to_symbol_tbl:Symbol.t Variable.Tbl.t) *)
  (*     (var_to_definition_tbl:Alias_analysis.constant_defining_value Variable.Tbl.t) *)
  (*     ~symbol_to_constant:translated_definitions *)
  (* in *)
  (* let sharing_symbol_tbl, symbol_to_constant_tbl = *)
  (*   share_constants ~symbol_to_constant:translated_definitions sorted_constants *)
  (* in *)
  (* let var_substitution = *)
  (*   update_var_to_symbol_and_var_to_definition aliases var_to_symbol_tbl *)
  (*     var_to_definition_tbl sharing_symbol_tbl *)
  (* in *)
  (* let components = program_graph symbol_to_constant_tbl initialize_symbol_tbl in *)
  (* let program = *)
  (*   add_definitions_of_symbols sharing_symbol_tbl symbol_to_constant_tbl *)
  (*     initialize_symbol_tbl *)
  (*     Flambda.End components *)
  (* in *)
  (* let program = *)
  (*   Flambda_iterators.map_named_of_program *)
  (*     ~f:(fun var def -> *)
  (*         match Variable.Tbl.find var_substitution var with *)
  (*         | exception Not_found -> def *)
  (*         | Symbol sym -> Symbol sym *)
  (*         | Const c -> Const c) *)
  (*     program *)
  (* in *)
  (* program *)
