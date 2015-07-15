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

module IdentSet = Lambda.IdentSet

module Env = struct
  type t = {
    variables : Variable.t Ident.tbl;
    static_exceptions : Static_exception.t Ext_types.Int.Map.t;
  }

  let empty = {
    variables = Ident.empty;
    static_exceptions = Ext_types.Int.Map.empty;
  }

  let add_var t id var = { t with variables = Ident.add id var t.variables }
  let add_vars t ids vars = List.fold_left2 add_var t ids vars

  let find_var t id =
    try Ident.find_same id t.variables
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_var: var "
        ^ Ident.unique_name id)

  let add_static_exception t st_exn fresh_st_exn =
    { t with
      static_exceptions =
        Ext_types.Int.Map.add st_exn fresh_st_exn t.static_exceptions }

  let find_static_exception t st_exn =
    try Ext_types.Int.Map.find st_exn t.static_exceptions
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_static_exception: exn "
        ^ string_of_int st_exn)
end

module Function_decls = struct
  module Function_decl = struct
    type t = {
      let_rec_ident : Ident.t;
      closure_bound_var : Variable.t;
      kind : Lambda.function_kind;
      params : Ident.t list;
      body : Lambda.lambda;
    }

    let create ~let_rec_ident ~closure_bound_var ~kind ~params ~body =
      let let_rec_ident =
        match let_rec_ident with
        | None -> Ident.create "unnamed_function"
        | Some let_rec_ident -> let_rec_ident
      in
      { let_rec_ident;
        closure_bound_var;
        kind;
        params;
        body;
      }

    let let_rec_ident t = t.let_rec_ident
    let closure_bound_var t = t.closure_bound_var
    let kind t = t.kind
    let params t = t.params
    let body t = t.body
    let used_idents t = Lambda.free_variables t.body

    (* CR-someday mshinwell: eliminate "*stub*" *)
    let primitive_wrapper t =
      match t.body with
      | Lprim (Pccall { Primitive.prim_name = "*stub*" }, [body]) -> Some body
      | _ -> None
  end

  type t = Function_decl.t list

  let create t = t
  let to_list t = t

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let let_rec_idents t = List.map Function_decl.let_rec_ident t

  (* All parameters of functions in [ts]. *)
  let all_params t = List.concat (List.map Function_decl.params t)

  (* CR mshinwell for pchambart: Should improve the name of this function.
     How about "free_variables_in_body"?
     pchambart: I wanted to avoid mixing 'ident' and 'var' names. This one
       returns sets of idents. I'm not sure but maybe "free_idents_in_body"
       isn't too strange.
       Also "free_variables_in_body" would suggest that we look at a
       single function. maybe a plural ?
  *)
  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  let used_idents_by_function t =
    List.fold_right (fun decl map ->
        Variable.Map.add (Function_decl.closure_bound_var decl)
          (Function_decl.used_idents decl) map)
      t Variable.Map.empty

  let all_used_idents t =
    Variable.Map.fold (fun _ -> IdentSet.union)
      (used_idents_by_function t) IdentSet.empty

  let set_diff (from : IdentSet.t) (idents : Ident.t list) =
    List.fold_right IdentSet.remove idents from

  let all_free_idents t =
    set_diff (set_diff (all_used_idents t) (all_params t)) (let_rec_idents t)

  let closure_env_without_parameters t =
    let closure_env =
      (* For "let rec"-bound functions. *)
      List.fold_right (fun t env ->
          Env.add_var env (Function_decl.let_rec_ident t)
            (Function_decl.closure_bound_var t))
        t Env.empty
    in
    (* For free variables. *)
    IdentSet.fold (fun id env ->
        Env.add_var env id (Variable.of_ident id))
      (all_free_idents t) closure_env
end
