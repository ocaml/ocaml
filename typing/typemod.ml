(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Type-checking of the module language *)

open Misc
open Longident
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
  | With_no_component of Longident.t
  | Repeated_name of string * string
  | Non_generalizable of type_expr
  | Non_generalizable_class of Ident.t * class_type

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

(* Lookup the type of a module path *)

let type_module_path env loc lid =
  try
    Env.lookup_module lid env
  with Not_found ->
    raise(Error(loc, Unbound_module lid))

(* Merge one "with" constraint in a signature *)

let merge_constraint env loc sg lid constr =
  let rec merge sg namelist =
    match (sg, namelist, constr) with
      ([], _, _) ->
        raise(Error(loc, With_no_component lid))
    | (Tsig_type(id, decl) :: rem, [s], Pwith_type sdecl)
      when Ident.name id = s ->
        let newdecl = Typedecl.transl_with_constraint env sdecl in
        Tsig_type(id, newdecl) :: rem
    | (Tsig_module(id, mty) :: rem, [s], Pwith_module lid)
      when Ident.name id = s ->
        let (path, mty') = type_module_path env loc lid in
        Tsig_module(id, Mtype.strengthen env mty' path) :: rem
    | (Tsig_module(id, mty) :: rem, s :: namelist, _) when Ident.name id = s ->
        let newsg = merge (extract_sig env loc mty) namelist in
        Tsig_module(id, Tmty_signature newsg) :: rem
    | (item :: rem, _, _) ->
        item :: merge rem namelist in
  merge sg (Longident.flatten lid)

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
      Tmty_signature final_sg
      
and transl_signature env sg =
  match sg with
    [] -> []
  | {psig_desc = Psig_value(name, sdesc)} :: srem ->
      let desc = Typedecl.transl_value_decl env sdesc in
      let (id, newenv) = Env.enter_value name desc env in
      let rem = transl_signature newenv srem in
      Tsig_value(id, desc) :: rem
  | {psig_desc = Psig_type sdecls} :: srem ->
      let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
      let rem = transl_signature newenv srem in
      map_end (fun (id, info) -> Tsig_type(id, info)) decls rem
  | {psig_desc = Psig_exception(name, sarg)} :: srem ->
      let arg = Typedecl.transl_exception env sarg in
      let (id, newenv) = Env.enter_exception name arg env in
      let rem = transl_signature newenv srem in
      Tsig_exception(id, arg) :: rem
  | {psig_desc = Psig_module(name, smty)} :: srem ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name mty env in
      let rem = transl_signature newenv srem in
      Tsig_module(id, mty) :: rem
  | {psig_desc = Psig_modtype(name, sinfo)} :: srem ->
      let info = transl_modtype_info env sinfo in
      let (id, newenv) = Env.enter_modtype name info env in
      let rem = transl_signature newenv srem in
      Tsig_modtype(id, info) :: rem
  | {psig_desc = Psig_open lid; psig_loc = loc} :: srem ->
      let (path, mty) = type_module_path env loc lid in
      let sg = extract_sig_open env loc mty in
      let newenv = Env.open_signature path sg env in
      transl_signature newenv srem
  | {psig_desc = Psig_include smty} :: srem ->
      let mty = transl_modtype env smty in
      let sg = extract_sig env smty.pmty_loc mty in
      let newenv = Env.add_signature sg env in
      let rem = transl_signature newenv srem in
      sg @ rem
  | {psig_desc = Psig_class cl} :: srem ->
      let (classes, newenv) = Typeclass.transl_class_types env cl in
      let rem = transl_signature newenv srem in
      List.flatten
        (map_end
           (fun (i, d, i', d', i'', d'') ->
              [Tsig_class(i, d); Tsig_type(i', d'); Tsig_type(i'', d'')])
           classes [rem])

and transl_modtype_info env sinfo =
  match sinfo with
    Pmodtype_abstract ->
      Tmodtype_abstract
  | Pmodtype_manifest smty ->
      Tmodtype_manifest(transl_modtype env smty)

(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
    Tmod_ident p -> p
  | Tmod_apply(funct, arg, coercion) ->
      Papply(path_of_module funct, path_of_module arg)
  | _ -> raise Not_a_path

(* Check that all type and module identifiers in a structure have
   distinct names (so that access by named paths is unambiguous). *)

module StringSet = Set.Make(struct type t = string let compare = compare end)

let check_unique_names sg =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let check cl loc set_ref name =
    if StringSet.mem name !set_ref
    then raise(Error(loc, Repeated_name(cl, name)))
    else set_ref := StringSet.add name !set_ref in
  let check_item item =
    match item.pstr_desc with
      Pstr_eval exp -> ()
    | Pstr_value(rec_flag, exps) -> ()
    | Pstr_primitive(name, desc) -> ()
    | Pstr_type name_decl_list ->
        List.iter
          (fun (name, decl) -> check "type" item.pstr_loc type_names name)
          name_decl_list
    | Pstr_exception(name, decl) -> ()
    | Pstr_module(name, smod) ->
        check "module" item.pstr_loc module_names name
    | Pstr_modtype(name, decl) ->
        check "module type" item.pstr_loc modtype_names name
    | Pstr_open lid -> ()
    | Pstr_class decl -> ()
  in
    List.iter check_item sg

(* Check that all core type schemes in a structure are closed *)

let check_nongen_schemes env str =
  List.iter 
    (function
        Tstr_value(rec_flag, pat_exp_list) ->
          List.iter
            (fun (pat, exp) ->
              if not (Ctype.closed_schema exp.exp_type) then
                raise(Error(exp.exp_loc, Non_generalizable exp.exp_type)))
            pat_exp_list
      | Tstr_class cl ->
          List.iter
	    (fun (id, imp) ->
	       let desc = Env.find_class (Pident id) env in
	       if not
	         (List.for_all Ctype.closed_schema desc.cty_params
	             &
	          List.for_all Ctype.closed_schema desc.cty_args
	             &
	          Vars.fold (fun _ (_, ty) -> (or) (Ctype.closed_schema ty))
                    desc.cty_vars
      	       	    true)
	       then
	         raise(Error(imp.cl_loc,
      	       	       Non_generalizable_class (id, desc))))
	    cl
      | _ -> ())  (* Sub-structures have been checked before *)
    str

(* Type a module value expression *)

let rec type_module env smod =
  match smod.pmod_desc with
    Pmod_ident lid ->
      let (path, mty) = type_module_path env smod.pmod_loc lid in
      { mod_desc = Tmod_ident path;
        mod_type = Mtype.strengthen env mty path;
        mod_loc = smod.pmod_loc }
  | Pmod_structure sstr ->
      let (str, sg, finalenv) = type_structure env sstr in
      check_nongen_schemes finalenv str;
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
	    try
	      let path = path_of_module arg in
              Subst.modtype (Subst.add_module param path Subst.identity)
      	       	       	    mty_res
	    with Not_a_path ->
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

and type_structure env sstr =
  check_unique_names sstr;
  type_struct env sstr

and type_struct env = function
    [] ->
      ([], [], env)
  | {pstr_desc = Pstr_eval sexpr} :: srem ->
      let expr = Typecore.type_expression env sexpr in
      let (str_rem, sig_rem, final_env) = type_struct env srem in
      (Tstr_eval expr :: str_rem, sig_rem, final_env)
  | {pstr_desc = Pstr_value(rec_flag, sdefs)} :: srem ->
      let (defs, newenv) =
        Typecore.type_binding env rec_flag sdefs in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      let bound_idents = let_bound_idents defs in
      let make_sig_value id =
        Tsig_value(id, Env.find_value (Pident id) newenv) in
      (Tstr_value(rec_flag, defs) :: str_rem,
       map_end make_sig_value bound_idents sig_rem,
       final_env)
  | {pstr_desc = Pstr_primitive(name, sdesc)} :: srem ->
      let desc = Typedecl.transl_value_decl env sdesc in
      let (id, newenv) = Env.enter_value name desc env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_primitive(id, desc) :: str_rem,
       Tsig_value(id, desc) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_type sdecls} :: srem ->
      let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_type decls :: str_rem,
       map_end (fun (id, info) -> Tsig_type(id, info)) decls sig_rem,
       final_env)
  | {pstr_desc = Pstr_exception(name, sarg)} :: srem ->
      let arg = Typedecl.transl_exception env sarg in
      let (id, newenv) = Env.enter_exception name arg env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_exception(id, arg) :: str_rem,
       Tsig_exception(id, arg) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_module(name, smodl)} :: srem ->
      let modl = type_module env smodl in
      let (id, newenv) = Env.enter_module name modl.mod_type env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_module(id, modl) :: str_rem,
       Tsig_module(id, modl.mod_type) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_modtype(name, smty)} :: srem ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_modtype name (Tmodtype_manifest mty) env in
      let (str_rem, sig_rem, final_env) = type_struct newenv srem in
      (Tstr_modtype(id, mty) :: str_rem,
       Tsig_modtype(id, Tmodtype_manifest mty) :: sig_rem,
       final_env)
  | {pstr_desc = Pstr_open lid; pstr_loc = loc} :: srem ->
      let (path, mty) = type_module_path env loc lid in
      let sg = extract_sig_open env loc mty in
      type_struct (Env.open_signature path sg env) srem
  | {pstr_desc = Pstr_class cl; pstr_loc = loc} :: srem ->
      let (classes, new_env) = Typeclass.transl_classes env cl in
      let (str_rem, sig_rem, final_env) = type_struct new_env srem in
      (Tstr_class (List.map (fun (i, _, _, _, _, _, c) -> (i, c)) classes)
       :: Tstr_type (List.map (fun (_, _, i, d, _, _, _) -> (i, d)) classes)
       :: Tstr_type (List.map (fun (_, _, _, _, i, d, _) -> (i, d)) classes)
       :: str_rem,
       List.flatten
         (map_end
      	    (fun (i, d, i', d', i'', d'', _) ->
               [Tsig_class(i, d); Tsig_type(i', d'); Tsig_type(i'', d'')])
            classes [sig_rem]),
       final_env)

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
  | With_no_component lid ->
      print_string "The signature constrained by `with' has no component named";
      print_space(); longident lid
  | Repeated_name(kind, name) ->
      open_hovbox 0;
      print_string "Multiple definition of the "; print_string kind;
      print_string " name "; print_string name; print_string ".";
      print_space();
      print_string "Names must be unique in a given structure.";
      close_box()
  | Non_generalizable typ ->
      open_hovbox 0;
      print_string "The type of this expression,"; print_space();
      type_scheme typ; print_string ","; print_space();
      print_string "contains type variables that cannot be generalized";
      close_box()
  | Non_generalizable_class (id, desc) ->
      open_hovbox 0;
      print_string "The type of this class,"; print_space();
      class_type id desc; print_string ","; print_space();
      print_string "contains type variables that cannot be generalized";
      close_box()
