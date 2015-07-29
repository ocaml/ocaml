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

module Allocated_const_map = Map.Make (struct
  type t = Variable.t Allocated_const.t
  let compare = Allocated_const.compare ~compare_name:Variable.compare
end)
module Variable_SCC = Sort_connected_components.Make (Variable)

let constant_graph (map : Variable.t Allocated_constant.t Variable.Map.t) =
  Variable.Map.map (fun (const : Variable.t Allocated_constant.t) ->
      match const with
      | Block (_, fields) -> Variable.Set.of_list fields
      | Float _ | Int32 _ | Int64 _ | Nativeint _ | Float_array _
      | String _ | Immstring _ -> Variable.Set.empty)
    map

let rewrite_constant_aliases map aliases =
  let subst var =
    try Variable.Map.find var aliases with
    | Not_found -> var
  in
  let subst_block (const : Variable.t Allocated_constant.t)
        : Variable.t Allocated_constant.t =
    match const with
    | Block (tag, fields) -> Block (tag, List.map subst fields)
    | Float _ | Int32 _ | Int64 _ | Nativeint _ | Float_array _
    | String _ | Immstring _ -> const
  in
  Variable.Map.map subst_block map

let all_constants map aliases =
  let allocated_constants = Variable.Map.keys map in
  let aliased_constants =
    Variable.Map.keys
      (Variable.Map.filter (fun _var alias -> Variable.Map.mem alias map)
        aliases)
  in
  Variable.Set.union allocated_constants aliased_constants

let constant_sharing ~constant_map:map ~compare_name ~aliases =
  let all_constants = all_constants map aliases in
  let map = rewrite_constant_aliases map aliases in
  let components =
    let graph = constant_graph map in
    Variable_SCC.connected_components_sorted_from_roots_to_leaf graph
  in
  let sorted_symbols =
    List.flatten
      (List.map (function
          | Variable_SCC.Has_loop l -> l
          | Variable_SCC.No_loop v -> [v])
        (List.rev (Array.to_list components)))
  in
  let shared_constants = ref Allocated_const_map.empty in
  let constants = ref Variable.Map.empty in
  let equal_constants = ref Variable.Map.empty in
  let find_and_add var cst =
    match Allocated_const_map.find cst !shared_constants with
    | exception Not_found ->
      shared_constants := Allocated_const_map.add cst var !shared_constants;
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
  !constants, !equal_constants
