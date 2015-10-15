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

type tbl = {
  sb_var : Variable.t Variable.Map.t;
  sb_mutable_var : Mutable_variable.t Mutable_variable.Map.t;
  sb_exn : Static_exception.t Static_exception.Map.t;
  (* Used to handle substitution sequences: we cannot call the substitution
     recursively because there can be name clashes. *)
  back_var : Variable.t list Variable.Map.t;
  back_mutable_var : Mutable_variable.t list Mutable_variable.Map.t;
}

type t =
  | Inactive
  | Active of tbl

type subst = t

let empty_tbl = {
  sb_var = Variable.Map.empty;
  sb_mutable_var = Mutable_variable.Map.empty;
  sb_exn = Static_exception.Map.empty;
  back_var = Variable.Map.empty;
  back_mutable_var = Mutable_variable.Map.empty;
}

let print ppf = function
  | Inactive -> Format.fprintf ppf "Inactive"
  | Active tbl ->
    Format.fprintf ppf "Active:@ ";
    Variable.Map.iter (fun var1 var2 ->
        Format.fprintf ppf "%a -> %a@ "
          Variable.print var1
          Variable.print var2)
      tbl.sb_var;
    Mutable_variable.Map.iter (fun mut_var1 mut_var2 ->
        Format.fprintf ppf "(mutable) %a -> %a@ "
          Mutable_variable.print mut_var1
          Mutable_variable.print mut_var2)
      tbl.sb_mutable_var;
    Variable.Map.iter (fun var vars ->
        Format.fprintf ppf "%a -> %a@ "
          Variable.print var
          Variable.Set.print (Variable.Set.of_list vars))
      tbl.back_var;
    Mutable_variable.Map.iter (fun mut_var mut_vars ->
        Format.fprintf ppf "(mutable) %a -> %a@ "
          Mutable_variable.print mut_var
          Mutable_variable.Set.print (Mutable_variable.Set.of_list mut_vars))
      tbl.back_mutable_var

let empty = Inactive

let empty_preserving_activation_state = function
  | Inactive -> Inactive
  | Active _ -> Active empty_tbl

let activate = function
  | Inactive -> Active empty_tbl
  | Active _ as t -> t

let rec add_sb_var sb id id' =
  let sb = { sb with sb_var = Variable.Map.add id id' sb.sb_var } in
  let sb =
    try let pre_vars = Variable.Map.find id sb.back_var in
      List.fold_left (fun sb pre_id -> add_sb_var sb pre_id id') sb pre_vars
    with Not_found -> sb in
  let back_var =
    let l = try Variable.Map.find id' sb.back_var with Not_found -> [] in
    Variable.Map.add id' (id :: l) sb.back_var in
  { sb with back_var }

let rec add_sb_mutable_var sb id id' =
  let sb = { sb with sb_mutable_var = Mutable_variable.Map.add id id' sb.sb_mutable_var } in
  let sb =
    try let pre_vars = Mutable_variable.Map.find id sb.back_mutable_var in
      List.fold_left (fun sb pre_id -> add_sb_mutable_var sb pre_id id') sb pre_vars
    with Not_found -> sb in
  let back_mutable_var =
    let l = try Mutable_variable.Map.find id' sb.back_mutable_var with Not_found -> [] in
    Mutable_variable.Map.add id' (id :: l) sb.back_mutable_var in
  { sb with back_mutable_var }

let apply_static_exception t i =
  match t with
  | Inactive ->
    i
  | Active t ->
    try Static_exception.Map.find i t.sb_exn
    with Not_found -> i

let add_static_exception t i =
  match t with
  | Inactive -> i, t
  | Active t ->
    let i' = Static_exception.create () in
    let sb_exn =
      Static_exception.Map.add i i' t.sb_exn
    in
    i', Active { t with sb_exn; }

let active_add_variable t id =
  let id' = Variable.freshen id in
  let t = add_sb_var t id id' in
  id', t

let add_variable t id =
  match t with
  | Inactive -> id, t
  | Active t ->
     let id', t = active_add_variable t id in
     id', Active t

let active_add_variables' t ids =
  List.fold_right (fun id (ids, t) ->
      let id', t = active_add_variable t id in
      id' :: ids, t) ids ([], t)

let add_variables t defs =
  List.fold_right (fun (id, data) (defs, t) ->
      let id', t = add_variable t id in
      (id', data) :: defs, t) defs ([], t)

let add_variables' t ids =
  List.fold_right (fun id (ids, t) ->
      let id', t = add_variable t id in
      id' :: ids, t) ids ([], t)

let active_add_mutable_variable t id =
  let id' = Mutable_variable.freshen id in
  let t = add_sb_mutable_var t id id' in
  id', t

let add_mutable_variable t id =
  match t with
  | Inactive -> id, t
  | Active t ->
     let id', t = active_add_mutable_variable t id in
     id', Active t

let active_find_var_exn t id =
  try Variable.Map.find id t.sb_var with
  | Not_found ->
      Misc.fatal_error (Format.asprintf "find_var: can't find %a@."
          Variable.print id)

let apply_variable t var =
  match t with
  | Inactive -> var
  | Active t ->
   try Variable.Map.find var t.sb_var with
   | Not_found -> var

let apply_mutable_variable t mut_var =
  match t with
  | Inactive -> mut_var
  | Active t ->
   try Mutable_variable.Map.find mut_var t.sb_mutable_var with
   | Not_found -> mut_var

let rewrite_recursive_calls_with_symbols t
      (function_declarations : Flambda.function_declarations)
      ~make_closure_symbol =
  match t with
  | Inactive -> function_declarations
  | Active _ ->
    let closure_symbols = Variable.Map.fold (fun id _ map ->
        let cf = Closure_id.wrap id in
        let sym = make_closure_symbol cf in
        Symbol.Map.add sym id map)
        function_declarations.funs Symbol.Map.empty in
    let funs =
      Variable.Map.map (fun (ffun : Flambda.function_declaration) ->
        let body =
          Flambda_iterators.map_toplevel_named
            (* CR pchambart: This may be worth deep substituting below the closures, but that
               means that we need to take care of functions free variables. *)
            (function
              | Symbol sym when Symbol.Map.mem sym closure_symbols ->
                Expr (Var (Symbol.Map.find sym closure_symbols))
              | e -> e)
            ffun.body
        in
        Flambda.create_function_declaration ~params:ffun.params
          ~body ~stub:ffun.stub ~dbg:ffun.dbg)
        function_declarations.funs
    in
    Flambda.update_function_declarations function_declarations ~funs

module Project_var = struct
  type t =
    { vars_within_closure : Var_within_closure.t Var_within_closure.Map.t;
      closure_id : Closure_id.t Closure_id.Map.t }

  let empty =
    { vars_within_closure = Var_within_closure.Map.empty;
      closure_id = Closure_id.Map.empty;
    }

  let new_subst_fv t id subst =
    match subst with
    | Inactive -> id, subst, t
    | Active subst ->
      let id' = Variable.freshen id in
      let subst = add_sb_var subst id id' in
      let off = Var_within_closure.wrap id in
      let off' = Var_within_closure.wrap id' in
      let off_sb = Var_within_closure.Map.add off off' t.vars_within_closure in
      id', Active subst, { t with vars_within_closure = off_sb; }

  let new_subst_fun t id subst =
    let id' = Variable.freshen id in
    let subst = add_sb_var subst id id' in
    let off = Closure_id.wrap id in
    let off' = Closure_id.wrap id' in
    let off_sb = Closure_id.Map.add off off' t.closure_id in
    id', subst, { t with closure_id = off_sb; }

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
  let func_decls_subst t (subst : subst)
        (func_decls : Flambda.function_declarations) =
    match subst with
    | Inactive -> func_decls, subst, t
    | Active subst ->
      let subst_func_decl _fun_id (func_decl : Flambda.function_declaration)
            subst =
        let params, subst = active_add_variables' subst func_decl.params in
        (* It is not a problem to share the substitution of parameter
           names between function: There should be no clash *)
        (* CR mshinwell: could this violate one of the new invariants in
           Flambda_invariants (about all parameters being distinct within one
           set of function declarations)? *)
        let body =
          Flambda_utils.toplevel_substitution subst.sb_var func_decl.body
        in
        let function_decl =
          Flambda.create_function_declaration ~params
            ~body ~stub:func_decl.stub ~dbg:func_decl.dbg
        in
        function_decl, subst
      in
      let subst, t =
        Variable.Map.fold (fun orig_id _func_decl (subst, t) ->
            let _id, subst, t = new_subst_fun t orig_id subst in
            subst, t)
          func_decls.funs (subst,t) in
      let funs, subst =
        Variable.Map.fold (fun orig_id func_decl (funs, subst) ->
            let func_decl, subst = subst_func_decl orig_id func_decl subst in
            let id = active_find_var_exn subst orig_id in
            let funs = Variable.Map.add id func_decl funs in
            funs, subst)
          func_decls.funs (Variable.Map.empty, subst) in
      let current_unit = Compilation_unit.get_current_exn () in
      let function_decls =
        Flambda.create_function_declarations
          ~set_of_closures_id:(Set_of_closures_id.create current_unit)
          ~compilation_unit:current_unit
          ~funs
      in
      function_decls, Active subst, t

  let apply_closure_id t closure_id =
    try Closure_id.Map.find closure_id t.closure_id
    with Not_found -> closure_id

  let apply_var_within_closure t var_in_closure =
    try Var_within_closure.Map.find var_in_closure t.vars_within_closure
    with Not_found -> var_in_closure
end

let apply_function_decls_and_free_vars t fv func_decls =
  let module I = Project_var in
  let fv, t, of_closures = I.subst_free_vars fv t in
  let func_decls, t, of_closures =
    I.func_decls_subst of_closures t func_decls
  in
  fv, func_decls, t, of_closures
