(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Type-checking of the module language *)

open Misc
open Path
open Parsetree
open Typedtree


type error =
    Unbound_module of Longident.t
  | Unbound_modtype of Longident.t
  | Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_not_abstract of string
  | With_arity_mismatch of string

exception Error of Location.t * error

(* Merge a set of type definitions in a signature *)

let merge_constraints loc env sg decls =
  let sub = ref Subst.identity in
  let rec merge_one_constraint id decl = function
    [] ->
      [Tsig_type(id, decl)]
  | (Tsig_type(id', decl') as item) :: rem ->
      if Ident.equal id id' then begin
        if decl'.type_kind <> Type_abstract then
          raise(Error(loc, With_not_abstract(Ident.name id)));
        if decl'.type_arity <> decl.type_arity then
          raise(Error(loc, With_arity_mismatch(Ident.name id)));
        sub := Subst.add_type id (Pident id') !sub;
        Tsig_type(id', decl) :: rem
      end else
        item :: merge_one_constraint id decl rem
  | item :: rem ->
      item :: merge_one_constraint id decl rem in
  let rec merge_all_constraints sg = function
    [] ->
      sg
  | (id, decl) :: rem ->
      merge_all_constraints (merge_one_constraint id decl sg) rem in
  let newsig = merge_all_constraints sg decls in
  Subst.signature !sub newsig

(* Lookup and strengthen the type of a module path *)

let type_module_path env loc lid =
  try
    Env.lookup_module lid env
  with Not_found ->
    raise(Error(loc, Unbound_module lid))

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Signature_expected))

let extract_sig_open env loc mty =
  match Mtype.scrape env mty with
    Tmty_signature sg -> sg
  | _ -> raise(Error(loc, Structure_expected mty))

(* Check and translate a module type expression *)

let rec transl_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      begin try
        let (path, info) = Env.lookup_modtype lid env in 
        Tmty_ident path
      with Not_found ->
        raise(Error(smty.pmty_loc, Unbound_modtype lid))
      end
  | Pmty_signature sg ->
      Tmty_signature (transl_signature env sg)
  | Pmty_functor(param, sarg, sres) ->
      let arg = transl_modtype env sarg in
      let (id, newenv) = Env.enter_module param arg env in
      let res = transl_modtype newenv sres in
      Tmty_functor(id, arg, res)
  | Pmty_with(sbody, sdecls) ->
      let body = transl_modtype env sbody in
      let sg = extract_sig env sbody.pmty_loc body in
      let (decls, newenv) =
        Typedecl.transl_type_decl env sdecls in
      Tmty_signature(merge_constraints smty.pmty_loc env sg decls)
      
and transl_signature env sg =
  match sg with
    [] -> []
  | Psig_value(name, sdesc) :: srem ->
      let desc = Typedecl.transl_value_decl env sdesc in
      let (id, newenv) = Env.enter_value name desc env in
      let rem = transl_signature newenv srem in
      Tsig_value(id, desc) :: rem
  | Psig_type sdecls :: srem ->
      let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
      let rem = transl_signature newenv srem in
      map_end (fun (id, info) -> Tsig_type(id, info)) decls rem
  | Psig_exception(name, sarg) :: srem ->
      let arg = Typedecl.transl_exception env sarg in
      let (id, newenv) = Env.enter_exception name arg env in
      let rem = transl_signature newenv srem in
      Tsig_exception(id, arg) :: rem
  | Psig_module(name, smty) :: srem ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name mty env in
      let rem = transl_signature newenv srem in
      Tsig_module(id, mty) :: rem
  | Psig_modtype(name, sinfo) :: srem ->
      let info = transl_modtype_info env sinfo in
      let (id, newenv) = Env.enter_modtype name info env in
      let rem = transl_signature newenv srem in
      Tsig_modtype(id, info) :: rem
  | Psig_open(lid, loc) :: srem ->
      let (path, mty) = type_module_path env loc lid in
      let sg = extract_sig_open env loc mty in
      let newenv = Env.open_signature path sg env in
      transl_signature newenv srem
  | Psig_include smty :: srem ->
      let mty = transl_modtype env smty in
      let sg = extract_sig env smty.pmty_loc mty in
      let newenv = Env.add_signature sg env in
      let rem = transl_signature newenv srem in
      sg @ rem

and transl_modtype_info env sinfo =
  match sinfo with
    Pmodtype_abstract ->
      Tmodtype_abstract
  | Pmodtype_manifest smty ->
      Tmodtype_manifest(transl_modtype env smty)

(* Type a module value expression *)

let rec type_module env smod =
  match smod.pmod_desc with
    Pmod_ident lid ->
      let (path, mty) = type_module_path env smod.pmod_loc lid in
      { mod_desc = Tmod_ident path;
        mod_type = Mtype.strengthen env mty path;
        mod_loc = smod.pmod_loc }
  | Pmod_structure sstr ->
      let (str, sg, _) = type_structure env sstr in
      { mod_desc = Tmod_structure str;
        mod_type = Tmty_signature sg;
        mod_loc = smod.pmod_loc }
  | Pmod_functor(name, smty, sbody) ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name mty env in
      let body = type_module newenv sbody in
      { mod_desc = Tmod_functor(id, mty, body);
        mod_type = Tmty_functor(id, mty, body.mod_type);
        mod_loc = smod.pmod_loc }
  | Pmod_apply(sfunct, sarg) ->
      let funct = type_module env sfunct in
      let arg = type_module env sarg in
      begin match Mtype.scrape env funct.mod_type with
        Tmty_functor(param, mty_param, mty_res) as mty_functor ->
          let coercion =
            try
              Includemod.modtypes env arg.mod_type mty_param
            with Includemod.Error msg ->
              raise(Error(sarg.pmod_loc, Not_included msg)) in
          let mty_appl =
            match arg with
              {mod_desc = Tmod_ident path} ->
                Subst.modtype (Subst.add_module param path Subst.identity)
                               mty_res
            | _ ->
              try
                Mtype.nondep_supertype
                  (Env.add_module param arg.mod_type env) param mty_res
              with Not_found ->
                raise(Error(smod.pmod_loc,
                            Cannot_eliminate_dependency mty_functor)) in
          { mod_desc = Tmod_apply(funct, arg, coercion);
            mod_type = mty_appl;
            mod_loc = smod.pmod_loc }
      | _ ->
          raise(Error(sfunct.pmod_loc, Cannot_apply funct.mod_type))
      end        
  | Pmod_constraint(sarg, smty) ->
      let arg = type_module env sarg in
      let mty = transl_modtype env smty in
      let coercion =
        try
          Includemod.modtypes env arg.mod_type mty
        with Includemod.Error msg ->
          raise(Error(sarg.pmod_loc, Not_included msg)) in
      { mod_desc = Tmod_constraint(arg, mty, coercion);
        mod_type = mty;
        mod_loc = smod.pmod_loc }

and type_structure env = function
    [] ->
      ([], [], env)
  | Pstr_eval sexpr :: srem ->
      let expr = Typecore.type_expression env sexpr in
      let (str_rem, sig_rem, final_env) = type_structure env srem in
      (Tstr_eval expr :: str_rem, sig_rem, final_env)
  | Pstr_value(rec_flag, sdefs) :: srem ->
      let (defs, newenv) =
        Typecore.type_binding env rec_flag sdefs in
      let (str_rem, sig_rem, final_env) = type_structure newenv srem in
      let bound_idents = List.rev(let_bound_idents defs) in
      let make_sig_value id =
        Tsig_value(id, Env.find_value (Pident id) newenv) in
      (Tstr_value(rec_flag, defs) :: str_rem,
       map_end make_sig_value bound_idents sig_rem,
       final_env)
  | Pstr_primitive(name, sdesc) :: srem ->
      let desc = Typedecl.transl_value_decl env sdesc in
      let (id, newenv) = Env.enter_value name desc env in
      let (str_rem, sig_rem, final_env) = type_structure newenv srem in
      (Tstr_primitive(id, desc) :: str_rem,
       Tsig_value(id, desc) :: sig_rem,
       final_env)
  | Pstr_type sdecls :: srem ->
      let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
      let (str_rem, sig_rem, final_env) = type_structure newenv srem in
      (Tstr_type decls :: str_rem,
       map_end (fun (id, info) -> Tsig_type(id, info)) decls sig_rem,
       final_env)
  | Pstr_exception(name, sarg) :: srem ->
      let arg = Typedecl.transl_exception env sarg in
      let (id, newenv) = Env.enter_exception name arg env in
      let (str_rem, sig_rem, final_env) = type_structure newenv srem in
      (Tstr_exception(id, arg) :: str_rem,
       Tsig_exception(id, arg) :: sig_rem,
       final_env)
  | Pstr_module(name, smodl) :: srem ->
      let modl = type_module env smodl in
      let (id, newenv) = Env.enter_module name modl.mod_type env in
      let (str_rem, sig_rem, final_env) = type_structure newenv srem in
      (Tstr_module(id, modl) :: str_rem,
       Tsig_module(id, modl.mod_type) :: sig_rem,
       final_env)
  | Pstr_modtype(name, smty) :: srem ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_modtype name (Tmodtype_manifest mty) env in
      let (str_rem, sig_rem, final_env) = type_structure newenv srem in
      (Tstr_modtype(id, mty) :: str_rem,
       Tsig_modtype(id, Tmodtype_manifest mty) :: sig_rem,
       final_env)
  | Pstr_open(lid, loc) :: srem ->
      let (path, mty) = type_module_path env loc lid in
      let sg = extract_sig_open env loc mty in
      type_structure (Env.open_signature path sg env) srem

(* Error report *)

open Format
open Printtyp

let report_error = function
    Unbound_module lid ->
      print_string "Unbound module "; longident lid
  | Unbound_modtype lid ->
      print_string "Unbound module type "; longident lid
  | Cannot_apply mty ->
      open_hovbox 0;
      print_string "This module is not a functor; it has type";
      print_space(); modtype mty;
      close_box()
  | Not_included errs ->
      open_vbox 0;
      print_string "Signature mismatch:"; print_space();
      Includemod.report_error errs;
      close_box()
  | Cannot_eliminate_dependency mty ->
      open_hovbox 0;
      print_string "This functor has type";
      print_space(); modtype mty; print_space();
      print_string "The parameter cannot be eliminated in the result type.";
      print_space();
      print_string "Please bind the argument to a module identifier.";
      close_box()
  | Signature_expected ->
      print_string "This module type is not a signature"
  | Structure_expected mty ->
      open_hovbox 0;
      print_string "This module is not a structure; it has type";
      print_space(); modtype mty;
      close_box()
  | With_not_abstract s ->
      print_string "The type "; print_string s; print_string " is not abstract"
  | With_arity_mismatch s ->
      print_string "Arity mismatch in `with' constraint over type ";
      print_string s
