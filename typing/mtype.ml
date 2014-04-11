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

(* Operations on module types *)

open Asttypes
open Path
open Types


let rec scrape env mty =
  match mty with
    Mty_ident p ->
      begin try
        scrape env (Env.find_modtype_expansion p env)
      with Not_found ->
        mty
      end
  | _ -> mty

let freshen mty =
  Subst.modtype Subst.identity mty

let rec strengthen env mty p =
  match scrape env mty with
    Mty_signature sg ->
      Mty_signature(strengthen_sig env sg p)
  | Mty_functor(param, arg, res)
    when !Clflags.applicative_functors && Ident.name param <> "*" ->
      Mty_functor(param, arg, strengthen env res (Papply(p, Pident param)))
  | mty ->
      mty

and strengthen_sig env sg p =
  match sg with
    [] -> []
  | (Sig_value(id, desc) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Sig_type(id, decl, rs) :: rem ->
      let newdecl =
        match decl.type_manifest, decl.type_private, decl.type_kind with
          Some _, Public, _ -> decl
        | Some _, Private, (Type_record _ | Type_variant _) -> decl
        | _ ->
            let manif =
              Some(Btype.newgenty(Tconstr(Pdot(p, Ident.name id, nopos),
                                          decl.type_params, ref Mnil))) in
            if decl.type_kind = Type_abstract then
              { decl with type_private = Public; type_manifest = manif }
            else
              { decl with type_manifest = manif }
      in
      Sig_type(id, newdecl, rs) :: strengthen_sig env rem p
  | (Sig_typext(id, ext, es) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Sig_module(id, md, rs) :: rem ->
      let str = strengthen_decl env md (Pdot(p, Ident.name id, nopos)) in
      Sig_module(id, str, rs)
      :: strengthen_sig (Env.add_module_declaration id md env) rem p
      (* Need to add the module in case it defines manifest module types *)
  | Sig_modtype(id, decl) :: rem ->
      let newdecl =
        match decl.mtd_type with
          None ->
            {decl with mtd_type = Some(Mty_ident(Pdot(p, Ident.name id, nopos)))}
        | Some _ ->
            decl
      in
      Sig_modtype(id, newdecl) ::
      strengthen_sig (Env.add_modtype id decl env) rem p
      (* Need to add the module type in case it is manifest *)
  | (Sig_class(id, decl, rs) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | (Sig_class_type(id, decl, rs) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p

and strengthen_decl env md p =
  {md with md_type = strengthen env md.md_type p}

let () = Env.strengthen := strengthen

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict

let nondep_supertype env mid mty =

  let rec nondep_mty env va mty =
    match mty with
      Mty_ident p ->
        if Path.isfree mid p then
          nondep_mty env va (Env.find_modtype_expansion p env)
        else mty
    | Mty_alias p ->
        if Path.isfree mid p then
          nondep_mty env va (Env.find_module p env).md_type
        else mty
    | Mty_signature sg ->
        Mty_signature(nondep_sig env va sg)
    | Mty_functor(param, arg, res) ->
        let var_inv =
          match va with Co -> Contra | Contra -> Co | Strict -> Strict in
        Mty_functor(param, Misc.may_map (nondep_mty env var_inv) arg,
                    nondep_mty
                      (Env.add_module ~arg:true param
                         (Btype.default_mty arg) env) va res)

  and nondep_sig env va = function
    [] -> []
  | item :: rem ->
      let rem' = nondep_sig env va rem in
      match item with
        Sig_value(id, d) ->
          Sig_value(id,
                    {d with val_type = Ctype.nondep_type env mid d.val_type})
          :: rem'
      | Sig_type(id, d, rs) ->
          Sig_type(id, Ctype.nondep_type_decl env mid id (va = Co) d, rs)
          :: rem'
      | Sig_typext(id, ext, es) ->
          Sig_typext(id, Ctype.nondep_extension_constructor env mid ext, es)
          :: rem'
      | Sig_module(id, md, rs) ->
          Sig_module(id, {md with md_type=nondep_mty env va md.md_type}, rs)
          :: rem'
      | Sig_modtype(id, d) ->
          begin try
            Sig_modtype(id, nondep_modtype_decl env d) :: rem'
          with Not_found ->
            match va with
              Co -> Sig_modtype(id, {mtd_type=None; mtd_loc=Location.none;
                                     mtd_attributes=[]}) :: rem'
            | _  -> raise Not_found
          end
      | Sig_class(id, d, rs) ->
          Sig_class(id, Ctype.nondep_class_declaration env mid d, rs)
          :: rem'
      | Sig_class_type(id, d, rs) ->
          Sig_class_type(id, Ctype.nondep_cltype_declaration env mid d, rs)
          :: rem'

  and nondep_modtype_decl env mtd =
    {mtd with mtd_type = Misc.may_map (nondep_mty env Strict) mtd.mtd_type}

  in
    nondep_mty env Co mty

let enrich_typedecl env p decl =
  match decl.type_manifest with
    Some ty -> decl
  | None ->
      try
        let orig_decl = Env.find_type p env in
        if orig_decl.type_arity <> decl.type_arity
        then decl
        else {decl with type_manifest =
                Some(Btype.newgenty(Tconstr(p, decl.type_params, ref Mnil)))}
      with Not_found ->
        decl

let rec enrich_modtype env p mty =
  match mty with
    Mty_signature sg ->
      Mty_signature(List.map (enrich_item env p) sg)
  | _ ->
      mty

and enrich_item env p = function
    Sig_type(id, decl, rs) ->
      Sig_type(id,
                enrich_typedecl env (Pdot(p, Ident.name id, nopos)) decl, rs)
  | Sig_module(id, md, rs) ->
      Sig_module(id,
                  {md with
                   md_type = enrich_modtype env
                       (Pdot(p, Ident.name id, nopos)) md.md_type},
                 rs)
  | item -> item

let rec type_paths env p mty =
  match scrape env mty with
    Mty_ident p -> []
  | Mty_alias p -> []
  | Mty_signature sg -> type_paths_sig env p 0 sg
  | Mty_functor(param, arg, res) -> []

and type_paths_sig env p pos sg =
  match sg with
    [] -> []
  | Sig_value(id, decl) :: rem ->
      let pos' = match decl.val_kind with Val_prim _ -> pos | _ -> pos + 1 in
      type_paths_sig env p pos' rem
  | Sig_type(id, decl, _) :: rem ->
      Pdot(p, Ident.name id, nopos) :: type_paths_sig env p pos rem
  | Sig_module(id, md, _) :: rem ->
      type_paths env (Pdot(p, Ident.name id, pos)) md.md_type @
      type_paths_sig (Env.add_module_declaration id md env) p (pos+1) rem
  | Sig_modtype(id, decl) :: rem ->
      type_paths_sig (Env.add_modtype id decl env) p pos rem
  | (Sig_typext _ | Sig_class _) :: rem ->
      type_paths_sig env p (pos+1) rem
  | (Sig_class_type _) :: rem ->
      type_paths_sig env p pos rem

let rec no_code_needed env mty =
  match scrape env mty with
    Mty_ident p -> false
  | Mty_signature sg -> no_code_needed_sig env sg
  | Mty_functor(_, _, _) -> false
  | Mty_alias p -> true

and no_code_needed_sig env sg =
  match sg with
    [] -> true
  | Sig_value(id, decl) :: rem ->
      begin match decl.val_kind with
      | Val_prim _ -> no_code_needed_sig env rem
      | _ -> false
      end
  | Sig_module(id, md, _) :: rem ->
      no_code_needed env md.md_type &&
      no_code_needed_sig (Env.add_module_declaration id md env) rem
  | (Sig_type _ | Sig_modtype _ | Sig_class_type _) :: rem ->
      no_code_needed_sig env rem
  | (Sig_typext _ | Sig_class _) :: rem ->
      false


(* Check whether a module type may return types *)

let rec contains_type env = function
    Mty_ident path ->
      (try Misc.may (contains_type env) (Env.find_modtype path env).mtd_type
       with Not_found -> raise Exit)
  | Mty_signature sg ->
      contains_type_sig env sg
  | Mty_functor (_, _, body) ->
      contains_type env body
  | Mty_alias _ ->
      ()

and contains_type_sig env = List.iter (contains_type_item env)

and contains_type_item env = function
    Sig_type (_,({type_manifest = None} |
                 {type_kind = Type_abstract; type_private = Private}),_)
  | Sig_modtype _ ->
      raise Exit
  | Sig_module (_, {md_type = mty}, _) ->
      contains_type env mty
  | Sig_value _
  | Sig_type _
  | Sig_typext _
  | Sig_class _
  | Sig_class_type _ ->
      ()

let contains_type env mty =
  try contains_type env mty; false with Exit -> true
