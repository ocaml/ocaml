(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Alistair O'Brien, University of Cambridge             *)
(*                                                                        *)
(*   Copyright 2026, Alistair O'Brien                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


open Asttypes
open Parsetree
open Types
open Ctype
open Predef


module Approx_env = struct
  type t =
    { env : Env.t
    ; mono_lvl : int
    }

  let create ~env ?(mono_lvl = get_current_level ()) () = { env; mono_lvl }

  (* [unknown t] returns a fresh 'unknown' type which indicates that
     we cannot accurately approximate the type. *)
  let unknown t = newvar2 t.mono_lvl

  let approx_transl t f =
    try
      Typetexp.TyVarEnv.with_any_var_level ~level:t.mono_lvl (fun () ->
          f t.env)
    with
    | _ -> unknown t
end

let with_moddep_param (aenv : Approx_env.t) ~loc ~arg_label ~name ~pack body =
  let (), arrow_ty =
    with_moddep_param
      aenv.env
      ~loc
      ~arg_label
      ~name
      ~pack
      (fun env uid ident -> (), body { aenv with env } uid ident)
  in
  arrow_ty

let match_or_keep_matchee (aenv : Approx_env.t) ty ~matchee =
  try
    unify aenv.env ty matchee;
    matchee
  with
  | _ -> matchee


let type_pattern aenv spat =
  match spat.ppat_desc with
  | Ppat_constraint (_, sty) ->
    Approx_env.approx_transl aenv (fun env ->
        (Typetexp.transl_simple_type env ~closed:false sty).ctyp_type)
  | _ ->
    (* We do not approximate deep within the pattern. *)
    Approx_env.unknown aenv


let type_function_param aenv spat ~ret_ty =
  let label, param_ty =
    match spat with
    | `Cases -> Nolabel, newmono (Approx_env.unknown aenv)
    | `Pat (label, default, spat) ->
      let pat_ty = type_pattern aenv spat in
      let param_ty =
        match label, default with
        | (Nolabel | Labelled _), _ ->
          (match spat.ppat_desc with
          | Ppat_constraint (_, { ptyp_desc = Ptyp_poly _; _ }) ->
            (* If the function has a polymorphic parameter annotation,
               then return the [Tpoly] produced by [type_pattern] *)
            pat_ty
          | _ ->
            (* Otherwise, assume it is monomorphic *)
            newmono pat_ty)
        | Optional _, None ->
          (* The pattern must match on an option type (since no default is
             provided). *)
          let pat_ty =
            match_or_keep_matchee aenv pat_ty ~matchee:(type_option (newvar ()))
          in
          newmono pat_ty
        | Optional _, Some _ ->
          (* Since a default is provided, the pattern only needs to match on
             the type of parameter (and not the optional parameter type). *)
          newmono (type_option pat_ty)
      in
      label, param_ty
  in
  newty (Tarrow (label, param_ty, ret_ty, commu_ok))


let type_constraint aenv sconstraint =
  match sconstraint with
  | Pconstraint pty ->
    let pty =
      Approx_env.approx_transl aenv (fun env ->
        (Typetexp.transl_simple_type env ~closed:false pty).ctyp_type)
    in
    maybe_instance_poly pty
  | Pcoerce (_constrain, coerce) ->
    Approx_env.approx_transl aenv (fun env ->
        (Typetexp.transl_simple_type env ~closed:false coerce).ctyp_type)


let rec type_expression aenv sexp =
  match sexp.pexp_desc with
  (* Questionable legacy approximations *)
  | Pexp_let (_, _, sexp)
  | Pexp_match (_, { pc_rhs = sexp } :: _)
  | Pexp_ifthenelse (_, sexp, _)
  | Pexp_sequence (_, sexp)
  | Pexp_try (sexp, _) -> type_expression aenv sexp
  (* 'Telescope' approximations *)
  | Pexp_function (params, ret_constraint, body) ->
    type_function aenv params ret_constraint body
  | Pexp_tuple components -> type_tuple aenv components
  | Pexp_constraint (_, sconstraint) ->
    type_constraint aenv (Pconstraint sconstraint)
  | Pexp_coerce (_, sty1, sty2) -> type_constraint aenv (Pcoerce (sty1, sty2))
  | Pexp_pack (_, Some ptyp) ->
    let loc = sexp.pexp_loc in
    let sty = Ast_helper.Typ.package ~loc ptyp in
    type_constraint aenv (Pconstraint sty)
  | _ -> Approx_env.unknown aenv


and type_tuple aenv components =
  let labeled_tys =
    List.map
      (fun (label, component) -> label, type_expression aenv component)
      components
  in
  newty (Ttuple labeled_tys)


and type_function aenv params ret_constraint body =
  (* We can approximate types up to the first newtype parameter,
     whereupon we give up. *)
  match params with
  | { pparam_desc =
        Pparam_val
          ( Nolabel
          , _
          , { ppat_desc = Ppat_unpack (({ txt = Some name; loc }), Some pack)
            ; _
            } )
    ; pparam_loc
    }
    :: params ->
    (* This parsetree pattern is a possible module dependent function. *)
    let name = { txt = name; loc } in
    let pack =
      Approx_env.approx_transl aenv (fun env ->
          let pack = Ast_helper.Typ.package ~loc:pack.ppt_loc pack in
          (Typetexp.transl_simple_type env ~closed:false pack).ctyp_type)
    in
    (match get_desc pack with
    | Tpackage pack ->
      with_moddep_param
        aenv
        ~loc:pparam_loc
        ~arg_label:Nolabel
        ~name
        ~pack
        (fun aenv _uid _ident -> type_function aenv params ret_constraint body)
    | _ -> Approx_env.unknown aenv)
  | { pparam_desc = Pparam_newtype _ } :: _params -> Approx_env.unknown aenv
  | { pparam_desc = Pparam_val (label, default, pat) } :: params ->
    type_function_param
      aenv
      (`Pat (label, default, pat))
      ~ret_ty:(type_function aenv params ret_constraint body)
  | [] ->
    (match ret_constraint with
    | Some sconstraint -> type_constraint aenv sconstraint
    | None ->
      (match body with
      | Pfunction_body sbody -> type_expression aenv sbody
      | Pfunction_cases ({ pc_rhs = sbody } :: _, _, _) ->
        type_function_param aenv `Cases ~ret_ty:(type_expression aenv sbody)
      | Pfunction_cases ([], _, _) ->
        (* This case is in fact not reachable. *)
        assert false))
