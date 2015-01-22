(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Symbol
open Abstract_identifiers
open Flambda

type t = {
  (* Claim: the substitution is empty if [active] is [false]. *)
  active : bool;
  sb_var : Variable.t Variable.Map.t;
  sb_sym : Variable.t SymbolMap.t;
  sb_exn : Static_exception.t Static_exception.Map.t;
  back_var : Variable.t list Variable.Map.t;
  back_sym : Symbol.t list Variable.Map.t;
  (* Used to handle substitution sequence: we cannot call
     the substitution recursively because there can be name
     clash *)
}
type subst = t

let empty = {
  active = false;
  sb_var = Variable.Map.empty;
  sb_sym = SymbolMap.empty;
  sb_exn = Static_exception.Map.empty;
  back_var = Variable.Map.empty;
  back_sym = Variable.Map.empty;
}

let new_substitution t = { empty with active = t.active }

let activate t = { t with active = true; }

let add_sb_sym sb sym id' =
  let back_sym =
    let l = try Variable.Map.find id' sb.back_sym with Not_found -> [] in
    Variable.Map.add id' (sym :: l) sb.back_sym in
  { sb with sb_sym = SymbolMap.add sym id' sb.sb_sym;
            back_sym }

let rec add_sb_var sb id id' =
  let sb = { sb with sb_var = Variable.Map.add id id' sb.sb_var } in
  let sb =
    try let pre_vars = Variable.Map.find id sb.back_var in
      List.fold_left (fun sb pre_id -> add_sb_var sb pre_id id') sb pre_vars
    with Not_found -> sb in
  let sb =
    try let pre_sym = Variable.Map.find id sb.back_sym in
      List.fold_left (fun sb pre_sym -> add_sb_sym sb pre_sym id') sb pre_sym
    with Not_found -> sb in
  let back_var =
    let l = try Variable.Map.find id' sb.back_var with Not_found -> [] in
    Variable.Map.add id' (id :: l) sb.back_var in
  { sb with back_var }

let add_sb_exn t i i' =
  { t with sb_exn = Static_exception.Map.add i i' t.sb_exn; }

let sb_exn t i =
  try Static_exception.Map.find i t.sb_exn with Not_found -> i

let new_subst_exn t i =
  if t.active then
    let i' = Static_exception.create () in
    let t = add_sb_exn t i i' in
    i', t
  else i, t

let freshen_var var =
  Variable.rename ~current_compilation_unit:(Compilenv.current_unit ()) var

let new_subst_id t id =
  if t.active then 
    let id' = freshen_var id in
    let t = add_sb_var t id id' in
    id', t
  else id, t

let new_subst_ids t defs =
  List.fold_right (fun (id, lam) (defs, t) ->
      let id', t = new_subst_id t id in
      (id', lam) :: defs, t) defs ([], t)

let new_subst_ids' t ids =
  List.fold_right (fun id (ids, t) ->
      let id', t = new_subst_id t id in
      id' :: ids, t) ids ([], t)

let find_var t id =
  try Variable.Map.find id t.sb_var with
  | Not_found ->
    Misc.fatal_error (Format.asprintf "find_var: can't find %a@."
        Variable.print id)

let find_var_exn t var =
  Variable.Map.find var t.sb_var

let find_symbol_exn t sym =
  SymbolMap.find sym t.sb_sym

let subst_var t id =
  try Variable.Map.find id t.sb_var with
  | Not_found -> id

module Alpha_renaming_map_for_ids_and_bound_vars_of_closures = struct
  (* Tables used for identifiers substitution in
     Fclosure ("ids of closures") and Fvariable_in_closure ("bound vars
     of closures") constructions.
     This information is propagated bottom up. This is
     populated when inlining a function containing a closure
     declaration.

     For instance,
       [let f x =
          let g y = ... x ... in
          ... g.x ...           (Fvariable_in_closure x)
          ... g 1 ...           (FApply (Fclosure g ...))
          ]
     if f is inlined g is renamed. The approximation of g will
     cary this table such that later the access to the field x
     of g and selection of g in the closure can be substituted.
   *)

  type t =
    { ffs_fv : Var_within_closure.t Var_within_closure.Map.t;
      ffs_fun : Closure_id.t Closure_id.Map.t }

  let empty =
    { ffs_fv = Var_within_closure.Map.empty; ffs_fun = Closure_id.Map.empty }

  let new_subst_fv t id subst =
    if subst.active
    then
      let id' = freshen_var id in
      let subst = add_sb_var subst id id' in
      let off = Var_within_closure.wrap id in
      let off' = Var_within_closure.wrap id' in
      let off_sb = Var_within_closure.Map.add off off' t.ffs_fv in
      id', subst, { t with ffs_fv = off_sb; }
    else id, subst, t

  let new_subst_fun t id subst =
    if subst.active
    then
      let id' = freshen_var id in
      let subst = add_sb_var subst id id' in
      let off = Closure_id.wrap id in
      let off' = Closure_id.wrap id' in
      let off_sb = Closure_id.Map.add off off' t.ffs_fun in
      id', subst, { t with ffs_fun = off_sb; }
    else id, subst, t

  (** Returns :
      * The map of new_identifiers -> expression
      * The new environment with added substitution
      * a fresh ffunction_subst with only the substitution of free variables
   *)
  let subst_free_vars fv subst =
    Variable.Map.fold (fun id lam (fv, subst, t) ->
        let id, subst, t = new_subst_fv t id subst in
        Variable.Map.add id lam fv, subst, t)
      fv (Variable.Map.empty, subst, empty)

  (** Returns :
      * The function_declaration with renamed function identifiers
      * The new environment with added substitution
      * The ffunction_subst completed with function substitution

      subst_free_vars must have been used to build off_sb
   *)
  let ffuns_subst t subst ffuns =
    if subst.active
    then
      let subst_ffunction fun_id ffun subst =
        let params, subst = new_subst_ids' subst ffun.params in
        let free_variables =
          Variable.Set.fold (fun id set ->
              Variable.Set.add (find_var subst id) set)
            ffun.free_variables Variable.Set.empty in
        (* It is not a problem to share the substitution of parameter
           names between function: There should be no clash *)
        { ffun with
          free_variables;
          params;
          (* keep code in sync with the closure *)
          body = Flambdaiter.toplevel_substitution subst.sb_var ffun.body;
        }, subst
      in
      let subst, t =
        Variable.Map.fold (fun orig_id ffun (subst, t) ->
            let _id, subst, t = new_subst_fun t orig_id subst in
            subst, t)
          ffuns.funs (subst,t) in
      let funs, subst =
        Variable.Map.fold (fun orig_id ffun (funs, subst) ->
            let ffun, subst = subst_ffunction orig_id ffun subst in
            let id = find_var subst orig_id in
            let funs = Variable.Map.add id ffun funs in
            funs, subst)
          ffuns.funs (Variable.Map.empty, subst) in
      { ident = Set_of_closures_id.create (Compilenv.current_unit ());
        compilation_unit = Compilenv.current_unit ();
        funs }, subst, t
    else ffuns, subst, t

  let fun_off_id t off =
    try Closure_id.Map.find off t.ffs_fun
    with Not_found -> off

  let fv_off_id t off =
    try Var_within_closure.Map.find off t.ffs_fv
    with Not_found -> off
end
