(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Operations on module types *)

open Asttypes
open Path
open Types


let rec scrape env mty =
  match mty with
    Tmty_ident p ->
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
    Tmty_signature sg ->
      Tmty_signature(strengthen_sig env sg p)
  | Tmty_functor(param, arg, res) ->
      Tmty_functor(param, arg, strengthen env res (Papply(p, Pident param)))
  | mty ->
      mty

and strengthen_sig env sg p =
  match sg with
    [] -> []
  | (Tsig_value(id, desc) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_type(id, decl, rs) :: rem ->
      let newdecl =
        match decl.type_manifest with
          Some ty when decl.type_private = Public -> decl
        | _ ->
            let manif =
              Some(Btype.newgenty(Tconstr(Pdot(p, Ident.name id, nopos),
                                          decl.type_params, ref Mnil))) in
            if decl.type_kind = Type_abstract then
              { decl with type_private = Public; type_manifest = manif }
            else
              { decl with type_manifest = manif }
      in
      Tsig_type(id, newdecl, rs) :: strengthen_sig env rem p
  | (Tsig_exception(id, d) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | Tsig_module(id, mty, rs) :: rem ->
      Tsig_module(id, strengthen env mty (Pdot(p, Ident.name id, nopos)), rs)
      :: strengthen_sig (Env.add_module id mty env) rem p
      (* Need to add the module in case it defines manifest module types *)
  | Tsig_modtype(id, decl) :: rem ->
      let newdecl =
        match decl with
          Tmodtype_abstract ->
            Tmodtype_manifest(Tmty_ident(Pdot(p, Ident.name id, nopos)))
        | Tmodtype_manifest _ ->
            decl in
      Tsig_modtype(id, newdecl) ::
      strengthen_sig (Env.add_modtype id decl env) rem p
      (* Need to add the module type in case it is manifest *)
  | (Tsig_class(id, decl, rs) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p
  | (Tsig_cltype(id, decl, rs) as sigelt) :: rem ->
      sigelt :: strengthen_sig env rem p

(* In nondep_supertype, env is only used for the type it assigns to id.
   Hence there is no need to keep env up-to-date by adding the bindings
   traversed. *)

type variance = Co | Contra | Strict

let nondep_supertype env mid mty =

  let rec nondep_mty va mty =
    match mty with
      Tmty_ident p ->
        if Path.isfree mid p then
          nondep_mty va (Env.find_modtype_expansion p env)
        else mty
    | Tmty_signature sg ->
        Tmty_signature(nondep_sig va sg)
    | Tmty_functor(param, arg, res) ->
        let var_inv =
          match va with Co -> Contra | Contra -> Co | Strict -> Strict in
        Tmty_functor(param, nondep_mty var_inv arg, nondep_mty va res)

  and nondep_sig va = function
    [] -> []
  | item :: rem ->
      let rem' = nondep_sig va rem in
      match item with
        Tsig_value(id, d) ->
          Tsig_value(id, {val_type = Ctype.nondep_type env mid d.val_type;
                          val_kind = d.val_kind}) :: rem'
      | Tsig_type(id, d, rs) ->
          Tsig_type(id, Ctype.nondep_type_decl env mid id (va = Co) d, rs)
          :: rem'
      | Tsig_exception(id, d) ->
          Tsig_exception(id, List.map (Ctype.nondep_type env mid) d) :: rem'
      | Tsig_module(id, mty, rs) ->
          Tsig_module(id, nondep_mty va mty, rs) :: rem'
      | Tsig_modtype(id, d) ->
          begin try
            Tsig_modtype(id, nondep_modtype_decl d) :: rem'
          with Not_found ->
            match va with
              Co -> Tsig_modtype(id, Tmodtype_abstract) :: rem'
            | _  -> raise Not_found
          end
      | Tsig_class(id, d, rs) ->
          Tsig_class(id, Ctype.nondep_class_declaration env mid d, rs)
          :: rem'
      | Tsig_cltype(id, d, rs) ->
          Tsig_cltype(id, Ctype.nondep_cltype_declaration env mid d, rs)
          :: rem'

  and nondep_modtype_decl = function
      Tmodtype_abstract -> Tmodtype_abstract
    | Tmodtype_manifest mty -> Tmodtype_manifest(nondep_mty Strict mty)

  in
    nondep_mty Co mty

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
    Tmty_signature sg ->
      Tmty_signature(List.map (enrich_item env p) sg)
  | _ ->
      mty

and enrich_item env p = function
    Tsig_type(id, decl, rs) ->
      Tsig_type(id,
                enrich_typedecl env (Pdot(p, Ident.name id, nopos)) decl, rs)
  | Tsig_module(id, mty, rs) ->
      Tsig_module(id,
                  enrich_modtype env (Pdot(p, Ident.name id, nopos)) mty, rs)
  | item -> item

let rec type_paths env p mty =
  match scrape env mty with
    Tmty_ident p -> []
  | Tmty_signature sg -> type_paths_sig env p 0 sg
  | Tmty_functor(param, arg, res) -> []

and type_paths_sig env p pos sg =
  match sg with
    [] -> []
  | Tsig_value(id, decl) :: rem ->
      let pos' = match decl.val_kind with Val_prim _ -> pos | _ -> pos + 1 in
      type_paths_sig env p pos' rem
  | Tsig_type(id, decl, _) :: rem ->
      Pdot(p, Ident.name id, nopos) :: type_paths_sig env p pos rem
  | Tsig_module(id, mty, _) :: rem ->
      type_paths env (Pdot(p, Ident.name id, pos)) mty @
      type_paths_sig (Env.add_module id mty env) p (pos+1) rem
  | Tsig_modtype(id, decl) :: rem ->
      type_paths_sig (Env.add_modtype id decl env) p pos rem
  | (Tsig_exception _ | Tsig_class _) :: rem ->
      type_paths_sig env p (pos+1) rem
  | (Tsig_cltype _) :: rem ->
      type_paths_sig env p pos rem

let rec no_code_needed env mty =
  match scrape env mty with
    Tmty_ident p -> false
  | Tmty_signature sg -> no_code_needed_sig env sg
  | Tmty_functor(_, _, _) -> false  

and no_code_needed_sig env sg =
  match sg with
    [] -> true
  | Tsig_value(id, decl) :: rem ->
      begin match decl.val_kind with
      | Val_prim _ -> no_code_needed_sig env rem
      | _ -> false
      end
  | Tsig_module(id, mty, _) :: rem ->
      no_code_needed env mty &&
      no_code_needed_sig (Env.add_module id mty env) rem
  | (Tsig_type _ | Tsig_modtype _ | Tsig_cltype _) :: rem ->
      no_code_needed_sig env rem
  | (Tsig_exception _ | Tsig_class _) :: rem ->
      false
