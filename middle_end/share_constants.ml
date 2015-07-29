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

module Variable_SCC = Sort_connected_components.Make (Variable)

let constant_graph (map:constant_descr Variable.Map.t) =
  Variable.Map.map (fun (descr:constant_descr) ->
      match descr with
      | Block (_, var) -> Variable.Set.of_list var
      | _ -> Variable.Set.empty)
    map

let rewrite_constant_aliases map aliases =
  let subst var =
    try Variable.Map.find var aliases with
    | Not_found -> var
  in
  let subst_block (descr:constant_descr) : constant_descr =
    match descr with
    | Block (tag, fields) -> Block (tag, List.map subst fields)
    | c -> c
  in
  Variable.Map.map subst_block map

let constant_sharing ~map ~compare_name ~aliases =
  let all_constants = all_constants map aliases in
  let map = rewrite_constant_aliases map aliases in
  let graph = constant_graph map in
  let components =
    Variable_SCC.connected_components_sorted_from_roots_to_leaf graph
  in
  let sorted_symbols =
    List.flatten
      (List.map (function
           | Variable_SCC.Has_loop l -> l
           | Variable_SCC.No_loop v -> [v])
          (List.rev (Array.to_list components)))
  in
  let shared_constants = ref Constant_descr_map.empty in
  let constants = ref Variable.Map.empty in
  let equal_constants = ref Variable.Map.empty in
  let find_and_add var cst =
    match Constant_descr_map.find cst !shared_constants with
    | exception Not_found ->
      shared_constants := Constant_descr_map.add cst var !shared_constants;
      constants := Variable.Map.add var cst !constants;
    | sharing ->
      equal_constants := Variable.Map.add var sharing !equal_constants
  in
  let subst var =
    try Variable.Map.find var !equal_constants with
    | Not_found -> var
  in
  let share var =
    let cst = Variable.Map.find var map in
    match cst with
    | String _ | Float_array _ ->
      (* Strings and float arrays are mutable; we never share them. *)
      constants := Variable.Map.add var cst !constants
    | Float _ | Int32 _ | Int64 _ | Nativeint _ | Immstring _ ->
      find_and_add var cst
    | Block (tag, fields) ->
      find_and_add var (Block (tag, List.map subst fields))
  in
  List.iter share sorted_symbols;
  let assign_symbols var const (descr_map, kind_map) =
    let symbol = fresh_symbol var in
    Symbol.Map.add symbol const descr_map,
      Variable.Map.add var (Symbol symbol) kind_map
  in
  let descr, declared_constants_kind =
    Variable.Map.fold assign_symbols !constants
      (Symbol.Map.empty, Variable.Map.empty)
  in
  let equal_constants_kind =
    Variable.Map.map (fun var ->
        Variable.Map.find var declared_constants_kind)
      !equal_constants
  in
  let declared_and_equal_constants_kind =
    Variable.Map.disjoint_union
      declared_constants_kind
      equal_constants_kind
  in
  let kind =
    Variable.Map.of_set (fun var ->
        let alias =
          try Variable.Map.find var aliases with
          | Not_found -> var
        in
        Variable.Map.find alias declared_and_equal_constants_kind)
      all_constants
  in
  let descr =
    let find_kind var =
      try Variable.Map.find var kind with
      | Not_found ->
        Format.printf "missing %a@."
          Variable.print var;
        raise Not_found
    in
    Symbol.Map.map (Allocated_constants.map ~f:find_kind) descr
  in
  descr, kind
