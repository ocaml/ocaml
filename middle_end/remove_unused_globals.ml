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

let dependency (expr:Flambda.t) =
  let set = ref Symbol.Set.empty in
  Flambda_iterators.iter_symbols ~f:(fun s -> set := Symbol.Set.add s !set)
    expr;
  !set

(* CR pchambart: copied from lift_constant. Need remerging *)
let constant_dependencies (const:Flambda.constant_defining_value) =
  let closure_dependencies (set_of_closures:Flambda.set_of_closures) =
    let set = ref Symbol.Set.empty in
    Flambda_iterators.iter_symbols_on_named ~f:(fun s ->
        set := Symbol.Set.add s !set)
      (Set_of_closures set_of_closures);
    !set
  in
  match const with
  | Allocated_const _ -> Symbol.Set.empty
  | Block (_, fields) ->
    let symbol_fields = Misc.filter_map
        (function
          | (Symbol s:Flambda.constant_defining_value_block_field) -> Some s
          | Flambda.Const _ -> None)
        fields
    in
    Symbol.Set.of_list symbol_fields
  | Set_of_closures set_of_closures ->
    closure_dependencies set_of_closures
  | Project_closure (s, _) ->
    Symbol.Set.singleton s

let let_rec_dep defs dep =
  let add_deps l dep =
    List.fold_left (fun dep (sym, sym_dep) ->
        if Symbol.Set.mem sym dep then
          Symbol.Set.union dep sym_dep
        else
          dep)
      dep l
  in
  let defs_deps =
    List.map (fun (sym, def) ->
        sym, constant_dependencies def)
      defs
  in
  let rec fixpoint dep =
    let new_dep = add_deps defs_deps dep in
    if Symbol.Set.equal dep new_dep then
      dep
    else fixpoint new_dep
  in
  fixpoint dep

let rec loop (program:Flambda.program) : Flambda.program * Symbol.Set.t =
  match program with
  | Let_symbol (sym,def,program) ->
    let program, dep = loop program in
    if Symbol.Set.mem sym dep then
      Let_symbol (sym,def,program),
      Symbol.Set.union dep (constant_dependencies def)
    else
      program, dep
  | Let_rec_symbol (defs,program) ->
    let program, dep = loop program in
    let dep = let_rec_dep defs dep in
    let defs =
      List.filter (fun (sym, _) -> Symbol.Set.mem sym dep) defs
    in
    Let_rec_symbol (defs,program), dep
  | Import_symbol (sym,program) ->
    let program, dep = loop program in
    Import_symbol (sym,program), dep
  | Initialize_symbol (sym,tag,fields,program) ->
    let program, dep = loop program in
    if Symbol.Set.mem sym dep then
      let dep =
        List.fold_left
          (fun dep field -> Symbol.Set.union dep (dependency field))
          dep fields
      in
      Initialize_symbol (sym,tag,fields,program), dep
    else begin
      (* Format.printf "remove initialize %a@." *)
      (*   Symbol.print sym; *)
      List.fold_left
        (fun (program, dep) field ->
           if Effect_analysis.no_effects field then
             (program, dep)
           else
             let new_dep = dependency field in
             let dep = Symbol.Set.union new_dep dep in
             Flambda.Effect (field,program), dep)
        (program, dep) fields
    end
  | Effect (effect,program) ->
    let program, dep = loop program in
    if Effect_analysis.no_effects effect then begin
      (* Format.printf "remove effect@."; *)
      (program, dep)
    end
    else
      let new_dep = dependency effect in
      let dep = Symbol.Set.union new_dep dep in
      Effect (effect,program), dep
  | End symbol ->
    program, Symbol.Set.singleton symbol

let remove_unused_globals program =
  let (program, _) = loop program in
  program
