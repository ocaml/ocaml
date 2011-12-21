(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Type-checking of the module language *)

open Misc
open Longident
open Path
open Asttypes
open Parsetree
open Types
open Typedtree
open Format

type error =
    Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_no_component of Longident.t
  | With_mismatch of Longident.t * Includemod.error list
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_class of Ident.t * class_declaration
  | Non_generalizable_module of module_type
  | Implementation_is_required of string
  | Interface_not_compiled of string
  | Not_allowed_in_functor_body
  | With_need_typeconstr
  | Not_a_packed_module of type_expr
  | Incomplete_packed_module of type_expr
  | Scoping_pack of Longident.t * type_expr

exception Error of Location.t * error

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Signature_expected))

let extract_sig_open env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Structure_expected mty))

(* Compute the environment after opening a module *)

let type_open env loc lid =
  let (path, mty) = Typetexp.find_module env loc lid in
  let sg = extract_sig_open env loc mty in
  Env.open_signature path sg env

(* Record a module type *)
let rm node =
  Stypes.record (Stypes.Ti_mod node);
  node

(* Forward declaration, to be filled in by type_module_type_of *)
let type_module_type_of_fwd 
  : (Env.t -> Parsetree.module_expr -> module_type) ref
  = ref (fun env m -> assert false)

(* Merge one "with" constraint in a signature *)

let rec add_rec_types env = function
    Tsig_type(id, decl, Trec_next) :: rem ->
      add_rec_types (Env.add_type id decl env) rem
  | _ -> env

let check_type_decl env id row_id newdecl decl rs rem =
  let env = Env.add_type id newdecl env in
  let env =
    match row_id with None -> env | Some id -> Env.add_type id newdecl env in
  let env = if rs = Trec_not then env else add_rec_types env rem in
  Includemod.type_declarations env id newdecl decl

let rec make_params n = function
    [] -> []
  | _ :: l -> ("a" ^ string_of_int n) :: make_params (n+1) l

let wrap_param s = {ptyp_desc=Ptyp_var s; ptyp_loc=Location.none}

let make_next_first rs rem =
  if rs = Trec_first then
    match rem with
      Tsig_type (id, decl, Trec_next) :: rem ->
        Tsig_type (id, decl, Trec_first) :: rem
    | Tsig_module (id, mty, Trec_next) :: rem ->
        Tsig_module (id, mty, Trec_first) :: rem
    | _ -> rem
  else rem

let merge_constraint initial_env loc sg lid constr =
  let real_id = ref None in
  let rec merge env sg namelist row_id =
    match (sg, namelist, constr) with
      ([], _, _) ->
        raise(Error(loc, With_no_component lid))
    | (Tsig_type(id, decl, rs) :: rem, [s],
       Pwith_type ({ptype_kind = Ptype_abstract} as sdecl))
      when Ident.name id = s && Typedecl.is_fixed_type sdecl ->
        let decl_row =
          { type_params =
              List.map (fun _ -> Btype.newgenvar()) sdecl.ptype_params;
            type_arity = List.length sdecl.ptype_params;
            type_kind = Type_abstract;
            type_private = Private;
            type_manifest = None;
            type_variance =
              List.map (fun (c,n) -> (not n, not c, not c))
              sdecl.ptype_variance;
            type_loc = Location.none;
	    type_newtype_level = None }
        and id_row = Ident.create (s^"#row") in
        let initial_env = Env.add_type id_row decl_row initial_env in
        let newdecl = Typedecl.transl_with_constraint
                        initial_env id (Some(Pident id_row)) decl sdecl in
        check_type_decl env id row_id newdecl decl rs rem;
        let decl_row = {decl_row with type_params = newdecl.type_params} in
        let rs' = if rs = Trec_first then Trec_not else rs in
        Tsig_type(id_row, decl_row, rs') :: Tsig_type(id, newdecl, rs) :: rem
    | (Tsig_type(id, decl, rs) :: rem, [s], Pwith_type sdecl)
      when Ident.name id = s ->
        let newdecl =
          Typedecl.transl_with_constraint initial_env id None decl sdecl in
        check_type_decl env id row_id newdecl decl rs rem;
        Tsig_type(id, newdecl, rs) :: rem
    | (Tsig_type(id, decl, rs) :: rem, [s], (Pwith_type _ | Pwith_typesubst _))
      when Ident.name id = s ^ "#row" ->
        merge env rem namelist (Some id)
    | (Tsig_type(id, decl, rs) :: rem, [s], Pwith_typesubst sdecl)
      when Ident.name id = s ->
        (* Check as for a normal with constraint, but discard definition *)
        let newdecl =
          Typedecl.transl_with_constraint initial_env id None decl sdecl in
        check_type_decl env id row_id newdecl decl rs rem;
        real_id := Some id;
        make_next_first rs rem
    | (Tsig_module(id, mty, rs) :: rem, [s], Pwith_module lid)
      when Ident.name id = s ->
        let (path, mty') = Typetexp.find_module initial_env loc lid in
        let newmty = Mtype.strengthen env mty' path in
        ignore(Includemod.modtypes env newmty mty);
        Tsig_module(id, newmty, rs) :: rem
    | (Tsig_module(id, mty, rs) :: rem, [s], Pwith_modsubst lid)
      when Ident.name id = s ->
        let (path, mty') = Typetexp.find_module initial_env loc lid in
        let newmty = Mtype.strengthen env mty' path in
        ignore(Includemod.modtypes env newmty mty);
        real_id := Some id;
        make_next_first rs rem
    | (Tsig_module(id, mty, rs) :: rem, s :: namelist, _)
      when Ident.name id = s ->
        let newsg = merge env (extract_sig env loc mty) namelist None in
        Tsig_module(id, Tmty_signature newsg, rs) :: rem
    | (item :: rem, _, _) ->
        item :: merge (Env.add_item item env) rem namelist row_id in
  try
    let names = Longident.flatten lid in
    let sg = merge initial_env sg names None in
    match names, constr with
      [s], Pwith_typesubst sdecl ->
        let id =
          match !real_id with None -> assert false | Some id -> id in
        let lid =
          try match sdecl.ptype_manifest with
          | Some {ptyp_desc = Ptyp_constr (lid, stl)} ->
              let params =
                List.map
                  (function {ptyp_desc=Ptyp_var s} -> s | _ -> raise Exit)
                  stl in
              if List.map (fun x -> Some x) params <> sdecl.ptype_params
	      then raise Exit;
              lid
          | _ -> raise Exit
          with Exit -> raise (Error (sdecl.ptype_loc, With_need_typeconstr))
        in
        let (path, _) =
          try Env.lookup_type lid initial_env with Not_found -> assert false
        in
        let sub = Subst.add_type id path Subst.identity in
        Subst.signature sub sg
    | [s], Pwith_modsubst lid ->
        let id =
          match !real_id with None -> assert false | Some id -> id in
        let (path, _) = Typetexp.find_module initial_env loc lid in
        let sub = Subst.add_module id path Subst.identity in
        Subst.signature sub sg
    | _ ->
        sg
  with Includemod.Error explanation ->
    raise(Error(loc, With_mismatch(lid, explanation)))

(* Add recursion flags on declarations arising from a mutually recursive
   block. *)

let map_rec fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl -> fn Trec_first d1 :: map_end (fn Trec_next) dl rem

let rec map_rec' fn decls rem =
  match decls with
  | (id,_ as d1) :: dl when Btype.is_row_name (Ident.name id) ->
      fn Trec_not d1 :: map_rec' fn dl rem
  | _ -> map_rec fn decls rem

(* Auxiliary for translating recursively-defined module types.
   Return a module type that approximates the shape of the given module
   type AST.  Retain only module, type, and module type
   components of signatures.  For types, retain only their arity,
   making them abstract otherwise. *)

let rec approx_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      let (path, info) = Typetexp.find_modtype env smty.pmty_loc lid in
      Tmty_ident path
  | Pmty_signature ssg ->
      Tmty_signature(approx_sig env ssg)
  | Pmty_functor(param, sarg, sres) ->
      let arg = approx_modtype env sarg in
      let (id, newenv) = Env.enter_module param arg env in
      let res = approx_modtype newenv sres in
      Tmty_functor(id, arg, res)
  | Pmty_with(sbody, constraints) ->
      approx_modtype env sbody
  | Pmty_typeof smod ->
      !type_module_type_of_fwd env smod

and approx_sig env ssg =
  match ssg with
    [] -> []
  | item :: srem ->
      match item.psig_desc with
      | Psig_type sdecls ->
          let decls = Typedecl.approx_type_decl env sdecls in
          let rem = approx_sig env srem in
          map_rec' (fun rs (id, info) -> Tsig_type(id, info, rs)) decls rem
      | Psig_module(name, smty) ->
          let mty = approx_modtype env smty in
          let (id, newenv) = Env.enter_module name mty env in
          Tsig_module(id, mty, Trec_not) :: approx_sig newenv srem
      | Psig_recmodule sdecls ->
          let decls =
            List.map
              (fun (name, smty) ->
                (Ident.create name, approx_modtype env smty))
              sdecls in
          let newenv =
            List.fold_left (fun env (id, mty) -> Env.add_module id mty env)
            env decls in
          map_rec (fun rs (id, mty) -> Tsig_module(id, mty, rs)) decls
                  (approx_sig newenv srem)
      | Psig_modtype(name, sinfo) ->
          let info = approx_modtype_info env sinfo in
          let (id, newenv) = Env.enter_modtype name info env in
          Tsig_modtype(id, info) :: approx_sig newenv srem
      | Psig_open lid ->
          approx_sig (type_open env item.psig_loc lid) srem
      | Psig_include smty ->
          let mty = approx_modtype env smty in
          let sg = Subst.signature Subst.identity
                     (extract_sig env smty.pmty_loc mty) in
          let newenv = Env.add_signature sg env in
          sg @ approx_sig newenv srem
      | Psig_class sdecls | Psig_class_type sdecls ->
          let decls = Typeclass.approx_class_declarations env sdecls in
          let rem = approx_sig env srem in
          List.flatten
            (map_rec
              (fun rs (i1, d1, i2, d2, i3, d3) ->
                [Tsig_cltype(i1, d1, rs);
                 Tsig_type(i2, d2, rs);
                 Tsig_type(i3, d3, rs)])
              decls [rem])
      | _ ->
          approx_sig env srem

and approx_modtype_info env sinfo =
  match sinfo with
    Pmodtype_abstract ->
      Tmodtype_abstract
  | Pmodtype_manifest smty ->
      Tmodtype_manifest(approx_modtype env smty)

(* Additional validity checks on type definitions arising from
   recursive modules *)

let check_recmod_typedecls env sdecls decls =
  let recmod_ids = List.map fst decls in
  List.iter2
    (fun (_, smty) (id, mty) ->
      List.iter
        (fun path ->
          Typedecl.check_recmod_typedecl env smty.pmty_loc recmod_ids
                                         path (Env.find_type path env))
        (Mtype.type_paths env (Pident id) mty))
    sdecls decls

(* Auxiliaries for checking uniqueness of names in signatures and structures *)

module StringSet = Set.Make(struct type t = string let compare = compare end)

let check cl loc set_ref name =
  if StringSet.mem name !set_ref
  then raise(Error(loc, Repeated_name(cl, name)))
  else set_ref := StringSet.add name !set_ref

let check_sig_item type_names module_names modtype_names loc = function
    Tsig_type(id, _, _) ->
      check "type" loc type_names (Ident.name id)
  | Tsig_module(id, _, _) ->
      check "module" loc module_names (Ident.name id)
  | Tsig_modtype(id, _) ->
      check "module type" loc modtype_names (Ident.name id)
  | _ -> ()

let rec remove_values ids = function
    [] -> []
  | Tsig_value (id, _) :: rem
    when List.exists (Ident.equal id) ids -> remove_values ids rem
  | f :: rem -> f :: remove_values ids rem

let rec get_values = function
    [] -> []
  | Tsig_value (id, _) :: rem -> id :: get_values rem
  | f :: rem -> get_values rem

(* Check and translate a module type expression *)

let transl_modtype_longident loc env lid =
  let (path, info) = Typetexp.find_modtype env loc lid in
  path

let rec transl_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      Tmty_ident (transl_modtype_longident smty.pmty_loc env lid)
  | Pmty_signature ssg ->
      Tmty_signature(transl_signature env ssg)
  | Pmty_functor(param, sarg, sres) ->
      let arg = transl_modtype env sarg in
      let (id, newenv) = Env.enter_module param arg env in
      let res = transl_modtype newenv sres in
      Tmty_functor(id, arg, res)
  | Pmty_with(sbody, constraints) ->
      let body = transl_modtype env sbody in
      let init_sg = extract_sig env sbody.pmty_loc body in
      let final_sg =
        List.fold_left
          (fun sg (lid, sdecl) ->
            merge_constraint env smty.pmty_loc sg lid sdecl)
          init_sg constraints in
      Mtype.freshen (Tmty_signature final_sg)
  | Pmty_typeof smod ->
      !type_module_type_of_fwd env smod

and transl_signature env sg =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let rec transl_sig env sg =
    Ctype.init_def(Ident.current_time());
    match sg with
      [] -> []
    | item :: srem ->
        match item.psig_desc with
        | Psig_value(name, sdesc) ->
            let desc = Typedecl.transl_value_decl env item.psig_loc sdesc in
            let (id, newenv) = Env.enter_value name desc env in
            let rem = transl_sig newenv srem in
            if List.exists (Ident.equal id) (get_values rem) then rem
            else Tsig_value(id, desc) :: rem
        | Psig_type sdecls ->
            List.iter
              (fun (name, decl) -> check "type" item.psig_loc type_names name)
              sdecls;
            let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
            let rem = transl_sig newenv srem in
            map_rec' (fun rs (id, info) -> Tsig_type(id, info, rs)) decls rem
        | Psig_exception(name, sarg) ->
            let arg = Typedecl.transl_exception env sarg in
            let (id, newenv) = Env.enter_exception name arg env in
            let rem = transl_sig newenv srem in
            Tsig_exception(id, arg) :: rem
        | Psig_module(name, smty) ->
            check "module" item.psig_loc module_names name;
            let mty = transl_modtype env smty in
            let (id, newenv) = Env.enter_module name mty env in
            let rem = transl_sig newenv srem in
            Tsig_module(id, mty, Trec_not) :: rem
        | Psig_recmodule sdecls ->
            List.iter
              (fun (name, smty) ->
                 check "module" item.psig_loc module_names name)
              sdecls;
            let (decls, newenv) =
              transl_recmodule_modtypes item.psig_loc env sdecls in
            let rem = transl_sig newenv srem in
            map_rec (fun rs (id, mty) -> Tsig_module(id, mty, rs)) decls rem
        | Psig_modtype(name, sinfo) ->
            check "module type" item.psig_loc modtype_names name;
            let info = transl_modtype_info env sinfo in
            let (id, newenv) = Env.enter_modtype name info env in
            let rem = transl_sig newenv srem in
            Tsig_modtype(id, info) :: rem
        | Psig_open lid ->
            transl_sig (type_open env item.psig_loc lid) srem
        | Psig_include smty ->
            let mty = transl_modtype env smty in
            let sg = Subst.signature Subst.identity
                       (extract_sig env smty.pmty_loc mty) in
            List.iter
              (check_sig_item type_names module_names modtype_names
                              item.psig_loc)
              sg;
            let newenv = Env.add_signature sg env in
            let rem = transl_sig newenv srem in
            remove_values (get_values rem) sg @ rem
        | Psig_class cl ->
            List.iter
              (fun {pci_name = name} ->
                 check "type" item.psig_loc type_names name)
              cl;
            let (classes, newenv) = Typeclass.class_descriptions env cl in
            let rem = transl_sig newenv srem in
            List.flatten
              (map_rec
                 (fun rs (i, d, i', d', i'', d'', i''', d''', _, _, _) ->
                    [Tsig_class(i, d, rs);
                     Tsig_cltype(i', d', rs);
                     Tsig_type(i'', d'', rs);
                     Tsig_type(i''', d''', rs)])
                 classes [rem])
        | Psig_class_type cl ->
            List.iter
              (fun {pci_name = name} ->
                 check "type" item.psig_loc type_names name)
              cl;
            let (classes, newenv) = Typeclass.class_type_declarations env cl in
            let rem = transl_sig newenv srem in
            List.flatten
              (map_rec
                 (fun rs (i, d, i', d', i'', d'') ->
                    [Tsig_cltype(i, d, rs);
                     Tsig_type(i', d', rs);
                     Tsig_type(i'', d'', rs)])
                 classes [rem])
    in transl_sig env sg

and transl_modtype_info env sinfo =
  match sinfo with
    Pmodtype_abstract ->
      Tmodtype_abstract
  | Pmodtype_manifest smty ->
      Tmodtype_manifest(transl_modtype env smty)

and transl_recmodule_modtypes loc env sdecls =
  let make_env curr =
    List.fold_left
      (fun env (id, mty) -> Env.add_module id mty env)
      env curr in
  let transition env_c curr =
    List.map2
      (fun (_, smty) (id, mty) -> (id, transl_modtype env_c smty))
      sdecls curr in
  let init =
    List.map
      (fun (name, smty) ->
        (Ident.create name, approx_modtype env smty))
      sdecls in
  let env0 = make_env init in
  let dcl1 = transition env0 init in
  let env1 = make_env dcl1 in
  check_recmod_typedecls env1 sdecls dcl1;
  let dcl2 = transition env1 dcl1 in
(*
  List.iter
    (fun (id, mty) ->
      Format.printf "%a: %a@." Printtyp.ident id Printtyp.modtype mty)
    dcl2;
*)
  let env2 = make_env dcl2 in
  check_recmod_typedecls env2 sdecls dcl2;
  (dcl2, env2)

(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
    Tmod_ident p -> p
  | Tmod_apply(funct, arg, coercion) when !Clflags.applicative_functors ->
      Papply(path_of_module funct, path_of_module arg)
  | _ -> raise Not_a_path

(* Check that all core type schemes in a structure are closed *)

let rec closed_modtype = function
    Tmty_ident p -> true
  | Tmty_signature sg -> List.for_all closed_signature_item sg
  | Tmty_functor(id, param, body) -> closed_modtype body

and closed_signature_item = function
    Tsig_value(id, desc) -> Ctype.closed_schema desc.val_type
  | Tsig_module(id, mty, _) -> closed_modtype mty
  | _ -> true

let check_nongen_scheme env = function
    Tstr_value(rec_flag, pat_exp_list) ->
      List.iter
        (fun (pat, exp) ->
          if not (Ctype.closed_schema exp.exp_type) then
            raise(Error(exp.exp_loc, Non_generalizable exp.exp_type)))
        pat_exp_list
  | Tstr_module(id, md) ->
      if not (closed_modtype md.mod_type) then
        raise(Error(md.mod_loc, Non_generalizable_module md.mod_type))
  | _ -> ()

let check_nongen_schemes env str =
  List.iter (check_nongen_scheme env) str

(* Extract the list of "value" identifiers bound by a signature.
   "Value" identifiers are identifiers for signature components that
   correspond to a run-time value: values, exceptions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)

let rec bound_value_identifiers = function
    [] -> []
  | Tsig_value(id, {val_kind = Val_reg}) :: rem ->
      id :: bound_value_identifiers rem
  | Tsig_exception(id, decl) :: rem -> id :: bound_value_identifiers rem
  | Tsig_module(id, mty, _) :: rem -> id :: bound_value_identifiers rem
  | Tsig_class(id, decl, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

(* Helpers for typing recursive modules *)

let anchor_submodule name anchor =
  match anchor with None -> None | Some p -> Some(Pdot(p, name, nopos))
let anchor_recmodule id anchor =
  Some (Pident id)

let enrich_type_decls anchor decls oldenv newenv =
  match anchor with
    None -> newenv
  | Some p ->
      List.fold_left
        (fun e (id, info) ->
          let info' =
            Mtype.enrich_typedecl oldenv (Pdot(p, Ident.name id, nopos)) info
          in
            Env.add_type id info' e)
        oldenv decls

let enrich_module_type anchor name mty env =
  match anchor with
    None -> mty
  | Some p -> Mtype.enrich_modtype env (Pdot(p, name, nopos)) mty

let check_recmodule_inclusion env bindings =
  (* PR#4450, PR#4470: consider
        module rec X : DECL = MOD  where MOD has inferred type ACTUAL
     The "natural" typing condition
        E, X: ACTUAL |- ACTUAL <: DECL
     leads to circularities through manifest types.
     Instead, we "unroll away" the potential circularities a finite number
     of times.  The (weaker) condition we implement is:
        E, X: DECL,
           X1: ACTUAL,
           X2: ACTUAL{X <- X1}/X1
           ...
           Xn: ACTUAL{X <- X(n-1)}/X(n-1)
        |- ACTUAL{X <- Xn}/Xn <: DECL{X <- Xn}
     so that manifest types rooted at X(n+1) are expanded in terms of X(n),
     avoiding circularities.  The strengthenings ensure that
     Xn.t = X(n-1).t = ... = X2.t = X1.t.
     N can be chosen arbitrarily; larger values of N result in more
     recursive definitions being accepted.  A good choice appears to be
     the number of mutually recursive declarations. *)

  let subst_and_strengthen env s id mty =
    Mtype.strengthen env (Subst.modtype s mty)
                         (Subst.module_path s (Pident id)) in

  let rec check_incl first_time n env s =
    if n > 0 then begin
      (* Generate fresh names Y_i for the rec. bound module idents X_i *)
      let bindings1 =
        List.map
          (fun (id, mty_decl, modl, mty_actual) ->
             (id, Ident.rename id, mty_actual))
          bindings in
      (* Enter the Y_i in the environment with their actual types substituted
         by the input substitution s *)
      let env' =
        List.fold_left
          (fun env (id, id', mty_actual) ->
             let mty_actual' =
               if first_time
               then mty_actual
               else subst_and_strengthen env s id mty_actual in
             Env.add_module id' mty_actual' env)
          env bindings1 in
      (* Build the output substitution Y_i <- X_i *)
      let s' =
        List.fold_left
          (fun s (id, id', mty_actual) ->
             Subst.add_module id (Pident id') s)
          Subst.identity bindings1 in
      (* Recurse with env' and s' *)
      check_incl false (n-1) env' s'
    end else begin
      (* Base case: check inclusion of s(mty_actual) in s(mty_decl)
         and insert coercion if needed *)
      let check_inclusion (id, mty_decl, modl, mty_actual) =
        let mty_decl' = Subst.modtype s mty_decl
        and mty_actual' = subst_and_strengthen env s id mty_actual in
        let coercion =
          try
            Includemod.modtypes env mty_actual' mty_decl'
          with Includemod.Error msg ->
            raise(Error(modl.mod_loc, Not_included msg)) in
        let modl' =
          { mod_desc = Tmod_constraint(modl, mty_decl, coercion);
            mod_type = mty_decl;
            mod_env = env;
            mod_loc = modl.mod_loc } in
        (id, modl') in
      List.map check_inclusion bindings
    end
  in check_incl true (List.length bindings) env Subst.identity

(* Helper for unpack *)

let rec package_constraints env loc mty constrs =
  if constrs = [] then mty
  else let sg = extract_sig env loc mty in
  let sg' =
    List.map
      (function
        | Tsig_type (id, ({type_params=[]} as td), rs) when List.mem_assoc [Ident.name id] constrs ->
            let ty = List.assoc [Ident.name id] constrs in
            Tsig_type (id, {td with type_manifest = Some ty}, rs)
        | Tsig_module (id, mty, rs) ->
            let rec aux = function
              | (m :: ((_ :: _) as l), t) :: rest when m = Ident.name id -> (l, t) :: aux rest
              | _ :: rest -> aux rest
              | [] -> []
            in
            Tsig_module (id, package_constraints env loc mty (aux constrs), rs)
        | item -> item
      )
      sg
  in
  Tmty_signature sg'

let modtype_of_package env loc p nl tl =
  try match Env.find_modtype p env with
  | Tmodtype_manifest mty when nl <> [] ->
      package_constraints env loc mty (List.combine (List.map Longident.flatten nl) tl)
  | _ ->
      if nl = [] then Tmty_ident p
      else raise(Error(loc, Signature_expected))
  with Not_found ->
    raise(Typetexp.Error(loc, Typetexp.Unbound_modtype (Ctype.lid_of_path p)))

let wrap_constraint env arg mty =
  let coercion =
    try
      Includemod.modtypes env arg.mod_type mty
    with Includemod.Error msg ->
      raise(Error(arg.mod_loc, Not_included msg)) in
  { mod_desc = Tmod_constraint(arg, mty, coercion);
    mod_type = mty;
    mod_env = env;
    mod_loc = arg.mod_loc }

(* Type a module value expression *)

let rec type_module sttn funct_body anchor env smod =
  match smod.pmod_desc with
    Pmod_ident lid ->
      let (path, mty) = Typetexp.find_module env smod.pmod_loc lid in
      rm { mod_desc = Tmod_ident path;
           mod_type = if sttn then Mtype.strengthen env mty path else mty;
           mod_env = env;
           mod_loc = smod.pmod_loc }
  | Pmod_structure sstr ->
      let (str, sg, finalenv) =
        type_structure funct_body anchor env sstr smod.pmod_loc in
      rm { mod_desc = Tmod_structure str;
           mod_type = Tmty_signature sg;
           mod_env = env;
           mod_loc = smod.pmod_loc }
  | Pmod_functor(name, smty, sbody) ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name mty env in
      let body = type_module sttn true None newenv sbody in
      rm { mod_desc = Tmod_functor(id, mty, body);
           mod_type = Tmty_functor(id, mty, body.mod_type);
           mod_env = env;
           mod_loc = smod.pmod_loc }
  | Pmod_apply(sfunct, sarg) ->
      let arg = type_module true funct_body None env sarg in
      let path = try Some (path_of_module arg) with Not_a_path -> None in
      let funct =
        type_module (sttn && path <> None) funct_body None env sfunct in
      begin match Mtype.scrape env funct.mod_type with
        Tmty_functor(param, mty_param, mty_res) as mty_functor ->
          let coercion =
            try
              Includemod.modtypes env arg.mod_type mty_param
            with Includemod.Error msg ->
              raise(Error(sarg.pmod_loc, Not_included msg)) in
          let mty_appl =
            match path with
              Some path ->
                Subst.modtype (Subst.add_module param path Subst.identity)
                              mty_res
            | None ->
                try
                  Mtype.nondep_supertype
                    (Env.add_module param arg.mod_type env) param mty_res
                with Not_found ->
                  raise(Error(smod.pmod_loc,
                              Cannot_eliminate_dependency mty_functor))
          in
          rm { mod_desc = Tmod_apply(funct, arg, coercion);
               mod_type = mty_appl;
               mod_env = env;
               mod_loc = smod.pmod_loc }
      | _ ->
          raise(Error(sfunct.pmod_loc, Cannot_apply funct.mod_type))
      end
  | Pmod_constraint(sarg, smty) ->
      let arg = type_module true funct_body anchor env sarg in
      let mty = transl_modtype env smty in
      rm {(wrap_constraint env arg mty) with mod_loc = smod.pmod_loc}

  | Pmod_unpack sexp ->
      if funct_body then
        raise (Error (smod.pmod_loc, Not_allowed_in_functor_body));
      if !Clflags.principal then Ctype.begin_def ();
      let exp = Typecore.type_exp env sexp in
      if !Clflags.principal then begin
        Ctype.end_def ();
        Ctype.generalize_structure exp.exp_type
      end;
      let mty =
        match Ctype.expand_head env exp.exp_type with
          {desc = Tpackage (p, nl, tl)} ->
            if List.exists (fun t -> Ctype.free_variables t <> []) tl then
              raise (Error (smod.pmod_loc,
                            Incomplete_packed_module exp.exp_type));
            if !Clflags.principal &&
              not (Typecore.generalizable (Btype.generic_level-1) exp.exp_type)
            then
              Location.prerr_warning smod.pmod_loc
                (Warnings.Not_principal "this module unpacking");
            modtype_of_package env smod.pmod_loc p nl tl
        | {desc = Tvar _} ->
            raise (Typecore.Error
                     (smod.pmod_loc, Typecore.Cannot_infer_signature))
        | _ ->
            raise (Error (smod.pmod_loc, Not_a_packed_module exp.exp_type))
      in
      rm { mod_desc = Tmod_unpack(exp, mty);
           mod_type = mty;
           mod_env = env;
           mod_loc = smod.pmod_loc }

and type_structure funct_body anchor env sstr scope =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let rec type_struct env sstr =
    Ctype.init_def(Ident.current_time());
    match sstr with
      [] ->
        ([], [], env)
    | {pstr_desc = Pstr_eval sexpr} :: srem ->
        let expr = Typecore.type_expression env sexpr in
        let (str_rem, sig_rem, final_env) = type_struct env srem in
        (Tstr_eval expr :: str_rem, sig_rem, final_env)
    | {pstr_desc = Pstr_value(rec_flag, sdefs); pstr_loc = loc} :: srem ->
        let scope =
          match rec_flag with
          | Recursive -> Some (Annot.Idef {scope with
                                 Location.loc_start = loc.Location.loc_start})
          | Nonrecursive ->
              let start = match srem with
                | [] -> loc.Location.loc_end
                | {pstr_loc = loc2} :: _ -> loc2.Location.loc_start
              in Some (Annot.Idef {scope with Location.loc_start = start})
          | Default -> None
        in
        let (defs, newenv) =
          Typecore.type_binding env rec_flag sdefs scope in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        let bound_idents = let_bound_idents defs in
        (* Note: Env.find_value does not trigger the value_used event. Values
           will be marked as being used during the signature inclusion test. *)
        let make_sig_value id =
          Tsig_value(id, Env.find_value (Pident id) newenv) in
        (Tstr_value(rec_flag, defs) :: str_rem,
         map_end make_sig_value bound_idents sig_rem,
         final_env)
    | {pstr_desc = Pstr_primitive(name, sdesc); pstr_loc = loc} :: srem ->
        let desc = Typedecl.transl_value_decl env loc sdesc in
        let (id, newenv) = Env.enter_value name desc env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_primitive(id, desc) :: str_rem,
         Tsig_value(id, desc) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_type sdecls; pstr_loc = loc} :: srem ->
        List.iter
          (fun (name, decl) -> check "type" loc type_names name)
          sdecls;
        let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
        let newenv' =
          enrich_type_decls anchor decls env newenv in
        let (str_rem, sig_rem, final_env) = type_struct newenv' srem in
        (Tstr_type decls :: str_rem,
         map_rec' (fun rs (id, info) -> Tsig_type(id, info, rs)) decls sig_rem,
         final_env)
    | {pstr_desc = Pstr_exception(name, sarg)} :: srem ->
        let arg = Typedecl.transl_exception env sarg in
        let (id, newenv) = Env.enter_exception name arg env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_exception(id, arg) :: str_rem,
         Tsig_exception(id, arg) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_exn_rebind(name, longid); pstr_loc = loc} :: srem ->
        let (path, arg) = Typedecl.transl_exn_rebind env loc longid in
        let (id, newenv) = Env.enter_exception name arg env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_exn_rebind(id, path) :: str_rem,
         Tsig_exception(id, arg) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_module(name, smodl); pstr_loc = loc} :: srem ->
        check "module" loc module_names name;
        let modl =
          type_module true funct_body (anchor_submodule name anchor) env
            smodl in
        let mty = enrich_module_type anchor name modl.mod_type env in
        let (id, newenv) = Env.enter_module name mty env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_module(id, modl) :: str_rem,
         Tsig_module(id, modl.mod_type, Trec_not) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_recmodule sbind; pstr_loc = loc} :: srem ->
        List.iter
          (fun (name, _, _) -> check "module" loc module_names name)
          sbind;
        let (decls, newenv) =
          transl_recmodule_modtypes loc env
            (List.map (fun (name, smty, smodl) -> (name, smty)) sbind) in
        let bindings1 =
          List.map2
            (fun (id, mty) (name, smty, smodl) ->
              let modl =
                type_module true funct_body (anchor_recmodule id anchor) newenv
                  smodl in
              let mty' =
                enrich_module_type anchor (Ident.name id) modl.mod_type newenv
              in
              (id, mty, modl, mty'))
           decls sbind in
        let bindings2 =
          check_recmodule_inclusion newenv bindings1 in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_recmodule bindings2 :: str_rem,
         map_rec (fun rs (id, modl) -> Tsig_module(id, modl.mod_type, rs))
                 bindings2 sig_rem,
         final_env)
    | {pstr_desc = Pstr_modtype(name, smty); pstr_loc = loc} :: srem ->
        check "module type" loc modtype_names name;
        let mty = transl_modtype env smty in
        let (id, newenv) = Env.enter_modtype name (Tmodtype_manifest mty) env in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (Tstr_modtype(id, mty) :: str_rem,
         Tsig_modtype(id, Tmodtype_manifest mty) :: sig_rem,
         final_env)
    | {pstr_desc = Pstr_open lid; pstr_loc = loc} :: srem ->
        type_struct (type_open env loc lid) srem
    | {pstr_desc = Pstr_class cl; pstr_loc = loc} :: srem ->
         List.iter
           (fun {pci_name = name} -> check "type" loc type_names name)
           cl;
        let (classes, new_env) = Typeclass.class_declarations env cl in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (Tstr_class
           (List.map (fun (i, d, _,_,_,_,_,_, s, m, c) ->
              let vf = if d.cty_new = None then Virtual else Concrete in
              (i, s, m, c, vf)) classes) ::
         Tstr_cltype
           (List.map (fun (_,_, i, d, _,_,_,_,_,_,_) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_,_,_,_, i, d, _,_,_,_,_) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_,_,_,_,_,_, i, d, _,_,_) -> (i, d)) classes) ::
         str_rem,
         List.flatten
           (map_rec
              (fun rs (i, d, i', d', i'', d'', i''', d''', _, _, _) ->
                 [Tsig_class(i, d, rs);
                  Tsig_cltype(i', d', rs);
                  Tsig_type(i'', d'', rs);
                  Tsig_type(i''', d''', rs)])
              classes [sig_rem]),
         final_env)
    | {pstr_desc = Pstr_class_type cl; pstr_loc = loc} :: srem ->
        List.iter
          (fun {pci_name = name} -> check "type" loc type_names name)
          cl;
        let (classes, new_env) = Typeclass.class_type_declarations env cl in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (Tstr_cltype
           (List.map (fun (i, d, _, _, _, _) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_, _, i, d, _, _) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_, _, _, _, i, d) -> (i, d)) classes) ::
         str_rem,
         List.flatten
           (map_rec
              (fun rs (i, d, i', d', i'', d'') ->
                 [Tsig_cltype(i, d, rs);
                  Tsig_type(i', d', rs);
                  Tsig_type(i'', d'', rs)])
              classes [sig_rem]),
         final_env)
    | {pstr_desc = Pstr_include smodl; pstr_loc = loc} :: srem ->
        let modl = type_module true funct_body None env smodl in
        (* Rename all identifiers bound by this signature to avoid clashes *)
        let sg = Subst.signature Subst.identity
                   (extract_sig_open env smodl.pmod_loc modl.mod_type) in
        List.iter
          (check_sig_item type_names module_names modtype_names loc) sg;
        let new_env = Env.add_signature sg env in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (Tstr_include (modl, bound_value_identifiers sg) :: str_rem,
         sg @ sig_rem,
         final_env)
  in
  if !Clflags.annotations
  then List.iter (function {pstr_loc = l} -> Stypes.record_phrase l) sstr;
  type_struct env sstr

let type_module = type_module true false None
let type_structure = type_structure false None

(* Normalize types in a signature *)

let rec normalize_modtype env = function
    Tmty_ident p -> ()
  | Tmty_signature sg -> normalize_signature env sg
  | Tmty_functor(id, param, body) -> normalize_modtype env body

and normalize_signature env = List.iter (normalize_signature_item env)

and normalize_signature_item env = function
    Tsig_value(id, desc) -> Ctype.normalize_type env desc.val_type
  | Tsig_module(id, mty, _) -> normalize_modtype env mty
  | _ -> ()

(* Simplify multiple specifications of a value or an exception in a signature.
   (Other signature components, e.g. types, modules, etc, are checked for
   name uniqueness.)  If multiple specifications with the same name,
   keep only the last (rightmost) one. *)

let rec simplify_modtype mty =
  match mty with
    Tmty_ident path -> mty
  | Tmty_functor(id, arg, res) -> Tmty_functor(id, arg, simplify_modtype res)
  | Tmty_signature sg -> Tmty_signature(simplify_signature sg)

and simplify_signature sg =
  let rec simplif val_names exn_names res = function
    [] -> res
  | (Tsig_value(id, descr) as component) :: sg ->
      let name = Ident.name id in
      simplif (StringSet.add name val_names) exn_names
              (if StringSet.mem name val_names then res else component :: res)
              sg
  | (Tsig_exception(id, decl) as component) :: sg ->
      let name = Ident.name id in
      simplif val_names (StringSet.add name exn_names)
              (if StringSet.mem name exn_names then res else component :: res)
              sg
  | Tsig_module(id, mty, rs) :: sg ->
      simplif val_names exn_names
              (Tsig_module(id, simplify_modtype mty, rs) :: res) sg
  | component :: sg ->
      simplif val_names exn_names (component :: res) sg
  in
    simplif StringSet.empty StringSet.empty [] (List.rev sg)

(* Extract the module type of a module expression *)

let type_module_type_of env smod =
  let mty =
    match smod.pmod_desc with
    | Pmod_ident lid -> (* turn off strengthening in this case *)
        let (path, mty) = Typetexp.find_module env smod.pmod_loc lid in mty
    | _ -> (type_module env smod).mod_type in
  (* PR#5037: clean up inferred signature to remove duplicate specs *)
  let mty = simplify_modtype mty in
  (* PR#5036: must not contain non-generalized type variables *)
  if not (closed_modtype mty) then
    raise(Error(smod.pmod_loc, Non_generalizable_module mty));
  mty

(* For Typecore *)

let rec get_manifest_types = function
    [] -> []
  | Tsig_type (id, {type_params=[]; type_manifest=Some ty}, _) :: rem ->
      (Ident.name id, ty) :: get_manifest_types rem
  | _ :: rem -> get_manifest_types rem

let type_package env m p nl tl =
  (* Same as Pexp_letmodule *)
  (* remember original level *)
  let lv = Ctype.get_current_level () in
  Ctype.begin_def ();
  Ident.set_current_time lv;
  let context = Typetexp.narrow () in
  let modl = type_module env m in
  Ctype.init_def(Ident.current_time());
  Typetexp.widen context;
  let (mp, env) =
    match modl.mod_desc with
      Tmod_ident mp -> (mp, env)
    | _ ->
      let (id, new_env) = Env.enter_module "%M" modl.mod_type env in
      (Pident id, new_env)
  in
  let rec mkpath mp = function
    | Lident name -> Pdot(mp, name, nopos)
    | Ldot (m, name) -> Pdot(mkpath mp m, name, nopos)
    | _ -> assert false
  in
  let tl' =
    List.map (fun name -> Ctype.newconstr (mkpath mp name) []) nl in
  (* go back to original level *)
  Ctype.end_def ();
  if nl = [] then (wrap_constraint env modl (Tmty_ident p), []) else
  let mty = modtype_of_package env modl.mod_loc p nl tl' in
  List.iter2
    (fun n ty ->
      try Ctype.unify env ty (Ctype.newvar ())
      with Ctype.Unify _ -> raise (Error(m.pmod_loc, Scoping_pack (n,ty))))
    nl tl';
  (wrap_constraint env modl mty, tl')

(* Fill in the forward declarations *)
let () =
  Typecore.type_module := type_module;
  Typetexp.transl_modtype_longident := transl_modtype_longident;
  Typetexp.transl_modtype := transl_modtype;
  Typecore.type_open := type_open;
  Typecore.type_package := type_package;
  type_module_type_of_fwd := type_module_type_of

(* Typecheck an implementation file *)

let type_implementation sourcefile outputprefix modulename initial_env ast =
  Typecore.reset_delayed_checks ();
  let (str, sg, finalenv) = type_structure initial_env ast Location.none in
  let simple_sg = simplify_signature sg in
  if !Clflags.print_types then begin
    fprintf std_formatter "%a@." Printtyp.signature simple_sg;
    (str, Tcoerce_none)   (* result is ignored by Compile.implementation *)
  end else begin
    let sourceintf =
      Misc.chop_extension_if_any sourcefile ^ !Config.interface_suffix in
    if Sys.file_exists sourceintf then begin
      let intf_file =
        try
          find_in_path_uncap !Config.load_path (modulename ^ ".cmi")
        with Not_found ->
          raise(Error(Location.none, Interface_not_compiled sourceintf)) in
      let dclsig = Env.read_signature modulename intf_file in
      let coercion = Includemod.compunit sourcefile sg intf_file dclsig in
      Typecore.force_delayed_checks ();
      (* It is important to run these checks after the inclusion test above,
         so that value declarations which are not used internally but exported
         are not reported as being unused. *)
      (str, coercion)
    end else begin
      check_nongen_schemes finalenv str;
      normalize_signature finalenv simple_sg;
      let coercion =
        Includemod.compunit sourcefile sg
                            "(inferred signature)" simple_sg in
      Typecore.force_delayed_checks ();
      (* See comment above. Here the target signature contains all
         the value being exported. We can still capture unused
         declarations like "let x = true;; let x = 1;;", because in this
         case, the inferred signature contains only the last declaration. *)
      if not !Clflags.dont_write_files then
        Env.save_signature simple_sg modulename (outputprefix ^ ".cmi");
      (str, coercion)
    end
  end

(* "Packaging" of several compilation units into one unit
   having them as sub-modules.  *)

let rec package_signatures subst = function
    [] -> []
  | (name, sg) :: rem ->
      let sg' = Subst.signature subst sg in
      let oldid = Ident.create_persistent name
      and newid = Ident.create name in
      Tsig_module(newid, Tmty_signature sg', Trec_not) ::
      package_signatures (Subst.add_module oldid (Pident newid) subst) rem

let package_units objfiles cmifile modulename =
  (* Read the signatures of the units *)
  let units =
    List.map
      (fun f ->
         let pref = chop_extensions f in
         let modname = String.capitalize(Filename.basename pref) in
         let sg = Env.read_signature modname (pref ^ ".cmi") in
         if Filename.check_suffix f ".cmi" &&
            not(Mtype.no_code_needed_sig Env.initial sg)
         then raise(Error(Location.none, Implementation_is_required f));
         (modname, Env.read_signature modname (pref ^ ".cmi")))
      objfiles in
  (* Compute signature of packaged unit *)
  Ident.reinit();
  let sg = package_signatures Subst.identity units in
  (* See if explicit interface is provided *)
  let mlifile =
    chop_extension_if_any cmifile ^ !Config.interface_suffix in
  if Sys.file_exists mlifile then begin
    if not (Sys.file_exists cmifile) then begin
      raise(Error(Location.in_file mlifile, Interface_not_compiled mlifile))
    end;
    let dclsig = Env.read_signature modulename cmifile in
    Includemod.compunit "(obtained by packing)" sg mlifile dclsig
  end else begin
    (* Determine imports *)
    let unit_names = List.map fst units in
    let imports =
      List.filter
        (fun (name, crc) -> not (List.mem name unit_names))
        (Env.imported_units()) in
    (* Write packaged signature *)
    Env.save_signature_with_imports sg modulename cmifile imports;
    Tcoerce_none
  end

(* Error report *)

open Printtyp

let report_error ppf = function
    Cannot_apply mty ->
      fprintf ppf
        "@[This module is not a functor; it has type@ %a@]" modtype mty
  | Not_included errs ->
      fprintf ppf
        "@[<v>Signature mismatch:@ %a@]" Includemod.report_error errs
  | Cannot_eliminate_dependency mty ->
      fprintf ppf
        "@[This functor has type@ %a@ \
           The parameter cannot be eliminated in the result type.@  \
           Please bind the argument to a module identifier.@]" modtype mty
  | Signature_expected -> fprintf ppf "This module type is not a signature"
  | Structure_expected mty ->
      fprintf ppf
        "@[This module is not a structure; it has type@ %a" modtype mty
  | With_no_component lid ->
      fprintf ppf
        "@[The signature constrained by `with' has no component named %a@]"
        longident lid
  | With_mismatch(lid, explanation) ->
      fprintf ppf
        "@[<v>\
           @[In this `with' constraint, the new definition of %a@ \
             does not match its original definition@ \
             in the constrained signature:@]@ \
           %a@]"
        longident lid Includemod.report_error explanation
  | Repeated_name(kind, name) ->
      fprintf ppf
        "@[Multiple definition of the %s name %s.@ \
           Names must be unique in a given structure or signature.@]" kind name
  | Non_generalizable typ ->
      fprintf ppf
        "@[The type of this expression,@ %a,@ \
           contains type variables that cannot be generalized@]" type_scheme typ
  | Non_generalizable_class (id, desc) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (class_declaration id) desc
  | Non_generalizable_module mty ->
      fprintf ppf
        "@[The type of this module,@ %a,@ \
           contains type variables that cannot be generalized@]" modtype mty
  | Implementation_is_required intf_name ->
      fprintf ppf
        "@[The interface %s@ declares values, not just types.@ \
           An implementation must be provided.@]" intf_name
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %s.@]" intf_name
  | Not_allowed_in_functor_body ->
      fprintf ppf
        "This kind of expression is not allowed within the body of a functor."
  | With_need_typeconstr ->
      fprintf ppf
        "Only type constructors with identical parameters can be substituted."
  | Not_a_packed_module ty ->
      fprintf ppf
        "This expression is not a packed module. It has type@ %a"
        type_expr ty
  | Incomplete_packed_module ty ->
      fprintf ppf
        "The type of this packed module contains variables:@ %a"
        type_expr ty
  | Scoping_pack (lid, ty) ->
      fprintf ppf
        "The type %a in this module cannot be exported.@ " longident lid;
      fprintf ppf
        "Its type contains local dependencies:@ %a" type_expr ty
