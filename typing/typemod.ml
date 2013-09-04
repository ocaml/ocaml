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

open Misc
open Longident
open Path
open Asttypes
open Parsetree
open Types
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
  | Extension of string
  | Recursive_module_require_explicit_type

exception Error of Location.t * Env.t * error

open Typedtree

let fst3 (x,_,_) = x

let rec path_concat head p =
  match p with
    Pident tail -> Pdot (Pident head, Ident.name tail, 0)
  | Pdot (pre, s, pos) -> Pdot (path_concat head pre, s, pos)
  | Papply _ -> assert false

(* Extract a signature from a module type *)

let extract_sig env loc mty =
  match Mtype.scrape env mty with
    Mty_signature sg -> sg
  | _ -> raise(Error(loc, env, Signature_expected))

let extract_sig_open env loc mty =
  match Mtype.scrape env mty with
    Mty_signature sg -> sg
  | _ -> raise(Error(loc, env, Structure_expected mty))

(* Compute the environment after opening a module *)

let type_open ?toplevel ovf env loc lid =
  let (path, mty) = Typetexp.find_module env loc lid.txt in
  let sg = extract_sig_open env loc mty in
  path, Env.open_signature ~loc ?toplevel ovf path sg env

(* Record a module type *)
let rm node =
  Stypes.record (Stypes.Ti_mod node);
  node

(* Forward declaration, to be filled in by type_module_type_of *)
let type_module_type_of_fwd :
    (Env.t -> Parsetree.module_expr ->
      Typedtree.module_expr * Types.module_type) ref
  = ref (fun env m -> assert false)

(* Merge one "with" constraint in a signature *)

let rec add_rec_types env = function
    Sig_type(id, decl, Trec_next) :: rem ->
      add_rec_types (Env.add_type id decl env) rem
  | _ -> env

let check_type_decl env loc id row_id newdecl decl rs rem =
  let env = Env.add_type id newdecl env in
  let env =
    match row_id with None -> env | Some id -> Env.add_type id newdecl env in
  let env = if rs = Trec_not then env else add_rec_types env rem in
  Includemod.type_declarations env id newdecl decl;
  Typedecl.check_coherence env loc id newdecl

let rec make_params n = function
    [] -> []
  | _ :: l -> ("a" ^ string_of_int n) :: make_params (n+1) l

let make_next_first rs rem =
  if rs = Trec_first then
    match rem with
      Sig_type (id, decl, Trec_next) :: rem ->
        Sig_type (id, decl, Trec_first) :: rem
    | Sig_module (id, mty, Trec_next) :: rem ->
        Sig_module (id, mty, Trec_first) :: rem
    | _ -> rem
  else rem

let sig_item desc typ env loc = {
  Typedtree.sig_desc = desc; sig_loc = loc; sig_env = env
}

let make p n i =
  let open Variance in
  set May_pos p (set May_neg n (set May_weak n (set Inj i null)))

let merge_constraint initial_env loc sg constr =
  let lid =
    match constr with
    | Pwith_type (lid, _) | Pwith_module (lid, _) -> lid
    | Pwith_typesubst {ptype_name=s} | Pwith_modsubst (s, _) ->
        {loc = s.loc; txt=Lident s.txt}
  in
  let real_id = ref None in
  let rec merge env sg namelist row_id =
    match (sg, namelist, constr) with
      ([], _, _) ->
        raise(Error(loc, env, With_no_component lid.txt))
    | (Sig_type(id, decl, rs) :: rem, [s],
       Pwith_type (_, ({ptype_kind = Ptype_abstract} as sdecl)))
      when Ident.name id = s && Typedecl.is_fixed_type sdecl ->
        let decl_row =
          { type_params =
              List.map (fun _ -> Btype.newgenvar()) sdecl.ptype_params;
            type_arity = List.length sdecl.ptype_params;
            type_kind = Type_abstract;
            type_private = Private;
            type_manifest = None;
            type_variance =
              List.map
                (fun (_, v) ->
                   let (c, n) =  
                     match v with
                     | Covariant -> true, false
                     | Contravariant -> false, true
                     | Invariant -> false, false
                   in
                   make (not n) (not c) false
                )
                sdecl.ptype_params;
            type_loc = sdecl.ptype_loc;
            type_newtype_level = None }
        and id_row = Ident.create (s^"#row") in
        let initial_env = Env.add_type id_row decl_row initial_env in
        let tdecl = Typedecl.transl_with_constraint
                        initial_env id (Some(Pident id_row)) decl sdecl in
        let newdecl = tdecl.typ_type in
        check_type_decl env sdecl.ptype_loc id row_id newdecl decl rs rem;
        let decl_row = {decl_row with type_params = newdecl.type_params} in
        let rs' = if rs = Trec_first then Trec_not else rs in
        (Pident id, lid, Twith_type tdecl),
        Sig_type(id_row, decl_row, rs') :: Sig_type(id, newdecl, rs) :: rem
    | (Sig_type(id, decl, rs) :: rem , [s], Pwith_type (_, sdecl))
      when Ident.name id = s ->
        let tdecl =
          Typedecl.transl_with_constraint initial_env id None decl sdecl in
        let newdecl = tdecl.typ_type in
        check_type_decl env sdecl.ptype_loc id row_id newdecl decl rs rem;
        (Pident id, lid, Twith_type tdecl), Sig_type(id, newdecl, rs) :: rem
    | (Sig_type(id, decl, rs) :: rem, [s], (Pwith_type _ | Pwith_typesubst _))
      when Ident.name id = s ^ "#row" ->
        merge env rem namelist (Some id)
    | (Sig_type(id, decl, rs) :: rem, [s], Pwith_typesubst sdecl)
      when Ident.name id = s ->
        (* Check as for a normal with constraint, but discard definition *)
        let tdecl =
          Typedecl.transl_with_constraint initial_env id None decl sdecl in
        let newdecl = tdecl.typ_type in
        check_type_decl env sdecl.ptype_loc id row_id newdecl decl rs rem;
        real_id := Some id;
        (Pident id, lid, Twith_typesubst tdecl),
        make_next_first rs rem
    | (Sig_module(id, mty, rs) :: rem, [s], Pwith_module (_, lid))
      when Ident.name id = s ->
        let (path, mty') = Typetexp.find_module initial_env loc lid.txt in
        let newmty = Mtype.strengthen env mty' path in
        ignore(Includemod.modtypes env newmty mty);
        (Pident id, lid, Twith_module (path, lid)),
        Sig_module(id, newmty, rs) :: rem
    | (Sig_module(id, mty, rs) :: rem, [s], Pwith_modsubst (_, lid))
      when Ident.name id = s ->
        let (path, mty') = Typetexp.find_module initial_env loc lid.txt in
        let newmty = Mtype.strengthen env mty' path in
        ignore(Includemod.modtypes env newmty mty);
        real_id := Some id;
        (Pident id, lid, Twith_modsubst (path, lid)),
        make_next_first rs rem
    | (Sig_module(id, mty, rs) :: rem, s :: namelist, _)
      when Ident.name id = s ->
        let ((path, path_loc, tcstr), newsg) =
          merge env (extract_sig env loc mty) namelist None in
        (path_concat id path, lid, tcstr),
        Sig_module(id, Mty_signature newsg, rs) :: rem
    | (item :: rem, _, _) ->
        let (cstr, items) = merge (Env.add_item item env) rem namelist row_id
        in
        cstr, item :: items
  in
  try
    let names = Longident.flatten lid.txt in
    let (tcstr, sg) = merge initial_env sg names None in
    let sg =
    match names, constr with
      [s], Pwith_typesubst sdecl ->
        let id =
          match !real_id with None -> assert false | Some id -> id in
        let lid =
          try match sdecl.ptype_manifest with
          | Some {ptyp_desc = Ptyp_constr (lid, stl)}
            when List.length stl = List.length sdecl.ptype_params ->
              let params =
                List.map
                  (function {ptyp_desc=Ptyp_var s} -> s | _ -> raise Exit)
                  stl in
              List.iter2 (fun x (ox, _) ->
                match ox with
                    Some y when x = y.txt -> ()
                  | _ -> raise Exit
              ) params sdecl.ptype_params;
              lid
          | _ -> raise Exit
          with Exit ->
            raise(Error(sdecl.ptype_loc, initial_env, With_need_typeconstr))
        in
        let (path, _) =
          try Env.lookup_type lid.txt initial_env with Not_found -> assert false
        in
        let sub = Subst.add_type id path Subst.identity in
        Subst.signature sub sg
    | [s], Pwith_modsubst (_, lid) ->
        let id =
          match !real_id with None -> assert false | Some id -> id in
        let (path, _) = Typetexp.find_module initial_env loc lid.txt in
        let sub = Subst.add_module id path Subst.identity in
        Subst.signature sub sg
    | _ ->
          sg
    in
    (tcstr, sg)
  with Includemod.Error explanation ->
    raise(Error(loc, initial_env, With_mismatch(lid.txt, explanation)))

(* Add recursion flags on declarations arising from a mutually recursive
   block. *)

let map_rec fn decls rem =
  match decls with
  | [] -> rem
  | d1 :: dl -> fn Trec_first d1 :: map_end (fn Trec_next) dl rem

let map_rec' = map_rec
(*
let rec map_rec' fn decls rem =
  match decls with
  | (id,_ as d1) :: dl when Btype.is_row_name (Ident.name id) ->
      fn Trec_not d1 :: map_rec' fn dl rem
  | _ -> map_rec fn decls rem
*)

let rec map_rec'' fn decls rem =
  match decls with
  | d1 :: dl when Btype.is_row_name (Ident.name d1.typ_id) ->
      fn Trec_not d1 :: map_rec'' fn dl rem
  | _ -> map_rec fn decls rem

(* Auxiliary for translating recursively-defined module types.
   Return a module type that approximates the shape of the given module
   type AST.  Retain only module, type, and module type
   components of signatures.  For types, retain only their arity,
   making them abstract otherwise. *)

let rec approx_modtype env smty =
  match smty.pmty_desc with
    Pmty_ident lid ->
      let (path, info) = Typetexp.find_modtype env smty.pmty_loc lid.txt in
      Mty_ident path
  | Pmty_signature ssg ->
      Mty_signature(approx_sig env ssg)
  | Pmty_functor(param, sarg, sres) ->
      let arg = approx_modtype env sarg in
      let (id, newenv) = Env.enter_module param.txt arg env in
      let res = approx_modtype newenv sres in
      Mty_functor(id, arg, res)
  | Pmty_with(sbody, constraints) ->
      approx_modtype env sbody
  | Pmty_typeof smod ->
      let (_, mty) = !type_module_type_of_fwd env smod in
      mty
  | Pmty_extension (s, _arg) ->
      raise (Error (s.loc, env, Extension s.txt))

and approx_sig env ssg =
  match ssg with
    [] -> []
  | item :: srem ->
      match item.psig_desc with
      | Psig_type sdecls ->
          let decls = Typedecl.approx_type_decl env sdecls in
          let rem = approx_sig env srem in
          map_rec' (fun rs (id, info) -> Sig_type(id, info, rs)) decls rem
      | Psig_module pmd ->
          let mty = approx_modtype env pmd.pmd_type in
          let (id, newenv) = Env.enter_module pmd.pmd_name.txt mty env in
          Sig_module(id, mty, Trec_not) :: approx_sig newenv srem
      | Psig_recmodule sdecls ->
          let decls =
            List.map
              (fun pmd ->
                (Ident.create pmd.pmd_name.txt, approx_modtype env pmd.pmd_type))
              sdecls in
          let newenv =
            List.fold_left (fun env (id, mty) -> Env.add_module id mty env)
            env decls in
          map_rec (fun rs (id, mty) -> Sig_module(id, mty, rs)) decls
                  (approx_sig newenv srem)
      | Psig_modtype d ->
          let info = approx_modtype_info env d.pmtd_type in
          let (id, newenv) = Env.enter_modtype d.pmtd_name.txt info env in
          Sig_modtype(id, info) :: approx_sig newenv srem
      | Psig_open (ovf, lid, _attrs) ->
          let (path, mty) = type_open ovf env item.psig_loc lid in
          approx_sig mty srem
      | Psig_include (smty, _attrs) ->
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
              (fun rs (i1, _, d1, i2, d2, i3, d3, _) ->
                [Sig_class_type(i1, d1, rs);
                 Sig_type(i2, d2, rs);
                 Sig_type(i3, d3, rs)])
              decls [rem])
      | _ ->
          approx_sig env srem

and approx_modtype_info env sinfo =
  match sinfo with
    None ->
      Modtype_abstract
  | Some smty ->
      Modtype_manifest(approx_modtype env smty)

(* Additional validity checks on type definitions arising from
   recursive modules *)

let check_recmod_typedecls env sdecls decls =
  let recmod_ids = List.map fst3 decls in
  List.iter2
    (fun pmd (id, _, mty) ->
       let mty = mty.mty_type in
      List.iter
        (fun path ->
          Typedecl.check_recmod_typedecl env pmd.pmd_type.pmty_loc recmod_ids
                                         path (Env.find_type path env))
        (Mtype.type_paths env (Pident id) mty))
    sdecls decls

(* Auxiliaries for checking uniqueness of names in signatures and structures *)

module StringSet =
  Set.Make(struct type t = string let compare (x:t) y = compare x y end)

let check cl loc set_ref name =
  if StringSet.mem name !set_ref
  then raise(Error(loc, Env.empty, Repeated_name(cl, name)))
  else set_ref := StringSet.add name !set_ref

let check_sig_item type_names module_names modtype_names loc = function
    Sig_type(id, _, _) ->
      check "type" loc type_names (Ident.name id)
  | Sig_module(id, _, _) ->
      check "module" loc module_names (Ident.name id)
  | Sig_modtype(id, _) ->
      check "module type" loc modtype_names (Ident.name id)
  | _ -> ()

let rec remove_duplicates val_ids exn_ids  = function
    [] -> []
  | Sig_value (id, _) :: rem
    when List.exists (Ident.equal id) val_ids ->
      remove_duplicates val_ids exn_ids rem
  | Sig_exception(id, _) :: rem
    when List.exists (Ident.equal id) exn_ids ->
      remove_duplicates val_ids exn_ids rem
  | f :: rem -> f :: remove_duplicates val_ids exn_ids rem

let rec get_values = function
    [] -> []
  | Sig_value (id, _) :: rem -> id :: get_values rem
  | f :: rem -> get_values rem

let rec get_exceptions = function
    [] -> []
  | Sig_exception (id, _) :: rem -> id :: get_exceptions rem
  | f :: rem -> get_exceptions rem


(* Check and translate a module type expression *)

let transl_modtype_longident loc env lid =
  let (path, info) = Typetexp.find_modtype env loc lid in
  path

let mkmty desc typ env loc attrs =
  let mty = {
    mty_desc = desc;
    mty_type = typ;
    mty_loc = loc;
    mty_env = env;
    mty_attributes = attrs;
    } in
  Cmt_format.add_saved_type (Cmt_format.Partial_module_type mty);
  mty

let mksig desc env loc =
  let sg = { sig_desc = desc; sig_loc = loc; sig_env = env } in
  Cmt_format.add_saved_type (Cmt_format.Partial_signature_item sg);
  sg

(* let signature sg = List.map (fun item -> item.sig_type) sg *)

let rec transl_modtype env smty =
  let loc = smty.pmty_loc in
  match smty.pmty_desc with
    Pmty_ident lid ->
      let path = transl_modtype_longident loc env lid.txt in
      mkmty (Tmty_ident (path, lid)) (Mty_ident path) env loc
        smty.pmty_attributes
  | Pmty_signature ssg ->
      let sg = transl_signature env ssg in
      mkmty (Tmty_signature sg) (Mty_signature sg.sig_type) env loc
        smty.pmty_attributes
  | Pmty_functor(param, sarg, sres) ->
      let arg = transl_modtype env sarg in
      let (id, newenv) = Env.enter_module param.txt arg.mty_type env in
      let res = transl_modtype newenv sres in
      mkmty (Tmty_functor (id, param, arg, res))
      (Mty_functor(id, arg.mty_type, res.mty_type)) env loc
        smty.pmty_attributes
  | Pmty_with(sbody, constraints) ->
      let body = transl_modtype env sbody in
      let init_sg = extract_sig env sbody.pmty_loc body.mty_type in
      let (tcstrs, final_sg) =
        List.fold_left
          (fun (tcstrs,sg) sdecl ->
            let (tcstr, sg) = merge_constraint env smty.pmty_loc sg sdecl
            in
            (tcstr :: tcstrs, sg)
        )
        ([],init_sg) constraints in
      mkmty (Tmty_with ( body, tcstrs))
        (Mtype.freshen (Mty_signature final_sg)) env loc
        smty.pmty_attributes
  | Pmty_typeof smod ->
      let tmty, mty = !type_module_type_of_fwd env smod in
      mkmty (Tmty_typeof tmty) mty env loc smty.pmty_attributes
  | Pmty_extension (s, _arg) ->
      raise (Error (s.loc, env, Extension s.txt))


and transl_signature env sg =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let rec transl_sig env sg =
    Ctype.init_def(Ident.current_time());
    match sg with
      [] -> [], [], env
    | item :: srem ->
        let loc = item.psig_loc in
        match item.psig_desc with
        | Psig_value sdesc ->
            let (tdesc, newenv) = Typedecl.transl_value_decl env item.psig_loc sdesc in
            let (trem,rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_value tdesc) env loc :: trem,
            (if List.exists (Ident.equal tdesc.val_id) (get_values rem) then rem
            else Sig_value(tdesc.val_id, tdesc.val_val) :: rem),
              final_env
        | Psig_type sdecls ->
            List.iter
              (fun decl ->
                check "type" item.psig_loc type_names decl.ptype_name.txt)
              sdecls;
            let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_type decls) env loc :: trem,
            map_rec'' (fun rs td ->
                Sig_type(td.typ_id, td.typ_type, rs)) decls rem,
            final_env
        | Psig_exception sarg ->
            let (arg, decl, newenv) = Typedecl.transl_exception env sarg in
            let (trem, rem, final_env) = transl_sig newenv srem in
            let id = arg.cd_id in
            mksig (Tsig_exception arg) env loc :: trem,
            (if List.exists (Ident.equal id) (get_exceptions rem) then rem
             else Sig_exception(id, decl) :: rem),
            final_env
        | Psig_module pmd ->
            check "module" item.psig_loc module_names pmd.pmd_name.txt;
            let tmty = transl_modtype env pmd.pmd_type in
            let mty = tmty.mty_type in
            let (id, newenv) = Env.enter_module pmd.pmd_name.txt mty env in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_module {md_id=id; md_name=pmd.pmd_name; md_type=tmty; md_attributes=pmd.pmd_attributes}) env loc :: trem,
            Sig_module(id, mty, Trec_not) :: rem,
            final_env
        | Psig_recmodule sdecls ->
            List.iter
              (fun pmd ->
                 check "module" item.psig_loc module_names pmd.pmd_name.txt)
              sdecls;
            let (decls, newenv) =
              transl_recmodule_modtypes item.psig_loc env sdecls in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_recmodule decls) env loc :: trem,
            map_rec (fun rs md -> Sig_module(md.md_id, md.md_type.mty_type, rs))
              decls rem,
            final_env
        | Psig_modtype pmtd ->
            let newenv, mtd, sg =
              transl_modtype_decl modtype_names env item.psig_loc pmtd
            in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_modtype mtd) env loc :: trem,
            sg :: rem,
            final_env
        | Psig_open (ovf, lid, attrs) ->
            let (path, newenv) = type_open ovf env item.psig_loc lid in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_open (ovf, path,lid,attrs)) env loc :: trem,
            rem, final_env
        | Psig_include (smty, attrs) ->
            let tmty = transl_modtype env smty in
            let mty = tmty.mty_type in
            let sg = Subst.signature Subst.identity
                       (extract_sig env smty.pmty_loc mty) in
            List.iter
              (check_sig_item type_names module_names modtype_names
                              item.psig_loc)
              sg;
            let newenv = Env.add_signature sg env in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_include (tmty, sg, attrs)) env loc :: trem,
            remove_duplicates (get_values rem) (get_exceptions rem) sg @ rem,
            final_env
        | Psig_class cl ->
            List.iter
              (fun {pci_name = name} ->
                 check "type" item.psig_loc type_names name.txt )
              cl;
            let (classes, newenv) = Typeclass.class_descriptions env cl in
            let (trem, rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_class
                     (List.map2
                        (fun pcl tcl ->
                          let (_, _, _, _, _, _, _, _, _, _, _, tcl) = tcl in
                          tcl)
                        cl classes)) env loc
            :: trem,
            List.flatten
              (map_rec
                 (fun rs (i, _, d, i', d', i'', d'', i''', d''', _, _, _) ->
                   [Sig_class(i, d, rs);
                    Sig_class_type(i', d', rs);
                    Sig_type(i'', d'', rs);
                    Sig_type(i''', d''', rs)])
                 classes [rem]),
            final_env
        | Psig_class_type cl ->
            List.iter
              (fun {pci_name = name} ->
                 check "type" item.psig_loc type_names name.txt)
              cl;
            let (classes, newenv) = Typeclass.class_type_declarations env cl in
            let (trem,rem, final_env) = transl_sig newenv srem in
            mksig (Tsig_class_type (List.map2 (fun pcl tcl ->
              let (_, _, _, _, _, _, _, tcl) = tcl in
              tcl
            ) cl classes)) env loc :: trem,
            List.flatten
              (map_rec
                 (fun rs (i, _, d, i', d', i'', d'', _) ->
                   [Sig_class_type(i, d, rs);
                    Sig_type(i', d', rs);
                    Sig_type(i'', d'', rs)])
                 classes [rem]),
            final_env
        | Psig_attribute x ->
            let (trem,rem, final_env) = transl_sig env srem in
            mksig (Tsig_attribute x) env loc :: trem, rem, final_env
        | Psig_extension ((s, _), _) ->
            raise (Error (s.loc, env, Extension s.txt))
  in
  let previous_saved_types = Cmt_format.get_saved_types () in
  let (trem, rem, final_env) = transl_sig (Env.in_signature env) sg in
  let sg = { sig_items = trem; sig_type =  rem; sig_final_env = final_env } in
  Cmt_format.set_saved_types
    ((Cmt_format.Partial_signature sg) :: previous_saved_types);
  sg

and transl_modtype_decl modtype_names env loc
    {pmtd_name; pmtd_type; pmtd_attributes} =
  check "module type" loc modtype_names pmtd_name.txt;
  let (tinfo, info) = transl_modtype_info env pmtd_type in
  let (id, newenv) = Env.enter_modtype pmtd_name.txt info env in
  let mtd =
    {
     mtd_id=id;
     mtd_name=pmtd_name;
     mtd_type=tinfo;
     mtd_attributes=pmtd_attributes;
    }
  in
  newenv, mtd, Sig_modtype(id, info)

and transl_modtype_info env sinfo =
  match sinfo with
    None ->
      None, Modtype_abstract
  | Some smty ->
      let tmty = transl_modtype env smty in
      Some tmty, Modtype_manifest tmty.mty_type

and transl_recmodule_modtypes loc env sdecls =
  let make_env curr =
    List.fold_left
      (fun env (id, _, mty) -> Env.add_module id mty env)
      env curr in
  let make_env2 curr =
    List.fold_left
      (fun env (id, _, mty) -> Env.add_module id mty.mty_type env)
      env curr in
  let transition env_c curr =
    List.map2
      (fun pmd (id, id_loc, mty) -> (id, id_loc, transl_modtype env_c pmd.pmd_type))
      sdecls curr in
  let ids = List.map (fun x -> Ident.create x.pmd_name.txt) sdecls in
  let approx_env =
    (*
       cf #5965
       We use a dummy module type in order to detect a reference to one
       of the module being defined during the call to approx_modtype.
       It will be detected in Env.lookup_module.
    *)
    List.fold_left
      (fun env id ->
         let dummy = Mty_ident (Path.Pident (Ident.create "#recmod#")) in
         Env.add_module id dummy env
      )
      env ids
  in
  let init =
    List.map2
      (fun id pmd ->
        (id, pmd.pmd_name, approx_modtype approx_env pmd.pmd_type))
      ids sdecls
  in
  let env0 = make_env init in
  let dcl1 = transition env0 init in
  let env1 = make_env2 dcl1 in
  check_recmod_typedecls env1 sdecls dcl1;
  let dcl2 = transition env1 dcl1 in
(*
  List.iter
    (fun (id, mty) ->
      Format.printf "%a: %a@." Printtyp.ident id Printtyp.modtype mty)
    dcl2;
*)
  let env2 = make_env2 dcl2 in
  check_recmod_typedecls env2 sdecls dcl2;
  let dcl2 =
    List.map2
      (fun pmd (id, id_loc, mty) ->
        {md_id=id; md_name=id_loc; md_type=mty; md_attributes=pmd.pmd_attributes})
      sdecls dcl2
  in
  (dcl2, env2)

(* Try to convert a module expression to a module path. *)

exception Not_a_path

let rec path_of_module mexp =
  match mexp.mod_desc with
    Tmod_ident (p,_) -> p
  | Tmod_apply(funct, arg, coercion) when !Clflags.applicative_functors ->
      Papply(path_of_module funct, path_of_module arg)
  | _ -> raise Not_a_path

(* Check that all core type schemes in a structure are closed *)

let rec closed_modtype = function
    Mty_ident p -> true
  | Mty_signature sg -> List.for_all closed_signature_item sg
  | Mty_functor(id, param, body) -> closed_modtype body

and closed_signature_item = function
    Sig_value(id, desc) -> Ctype.closed_schema desc.val_type
  | Sig_module(id, mty, _) -> closed_modtype mty
  | _ -> true

let check_nongen_scheme env str =
  match str.str_desc with
    Tstr_value(rec_flag, pat_exp_list) ->
      List.iter
        (fun {vb_expr=exp} ->
          if not (Ctype.closed_schema exp.exp_type) then
            raise(Error(exp.exp_loc, env, Non_generalizable exp.exp_type)))
        pat_exp_list
  | Tstr_module {mb_expr=md;_} ->
      if not (closed_modtype md.mod_type) then
        raise(Error(md.mod_loc, env, Non_generalizable_module md.mod_type))
  | _ -> ()

let check_nongen_schemes env str =
  List.iter (check_nongen_scheme env) str

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
        (fun e info ->
          let id = info.typ_id in
          let info' =
            Mtype.enrich_typedecl oldenv (Pdot(p, Ident.name id, nopos))
              info.typ_type
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
          (fun (id, _, mty_decl, modl, mty_actual, _attrs) ->
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
      let check_inclusion (id, id_loc, mty_decl, modl, mty_actual, attrs) =
        let mty_decl' = Subst.modtype s mty_decl.mty_type
        and mty_actual' = subst_and_strengthen env s id mty_actual in
        let coercion =
          try
            Includemod.modtypes env mty_actual' mty_decl'
          with Includemod.Error msg ->
            raise(Error(modl.mod_loc, env, Not_included msg)) in
        let modl' =
            { mod_desc = Tmod_constraint(modl, mty_decl.mty_type,
                Tmodtype_explicit mty_decl, coercion);
              mod_type = mty_decl.mty_type;
              mod_env = env;
              mod_loc = modl.mod_loc;
              mod_attributes = [];
             } in
        {
         mb_id = id;
         mb_name = id_loc;
         mb_expr = modl';
         mb_attributes = attrs;
        }
      in
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
        | Sig_type (id, ({type_params=[]} as td), rs)
          when List.mem_assoc [Ident.name id] constrs ->
            let ty = List.assoc [Ident.name id] constrs in
            Sig_type (id, {td with type_manifest = Some ty}, rs)
        | Sig_module (id, mty, rs) ->
            let rec aux = function
              | (m :: ((_ :: _) as l), t) :: rest when m = Ident.name id ->
                  (l, t) :: aux rest
              | _ :: rest -> aux rest
              | [] -> []
            in
            Sig_module (id, package_constraints env loc mty (aux constrs), rs)
        | item -> item
      )
      sg
  in
  Mty_signature sg'

let modtype_of_package env loc p nl tl =
  try match Env.find_modtype p env with
  | Modtype_manifest mty when nl <> [] ->
      package_constraints env loc mty
        (List.combine (List.map Longident.flatten nl) tl)
  | _ ->
      if nl = [] then Mty_ident p
      else raise(Error(loc, env, Signature_expected))
  with Not_found ->
    let error = Typetexp.Unbound_modtype (Ctype.lid_of_path p) in
    raise(Typetexp.Error(loc, env, error))

let wrap_constraint env arg mty explicit =
  let coercion =
    try
      Includemod.modtypes env arg.mod_type mty
    with Includemod.Error msg ->
      raise(Error(arg.mod_loc, env, Not_included msg)) in
  { mod_desc = Tmod_constraint(arg, mty, explicit, coercion);
    mod_type = mty;
    mod_env = env;
    mod_attributes = [];
    mod_loc = arg.mod_loc }

(* Type a module value expression *)

let rec type_module sttn funct_body anchor env smod =
  match smod.pmod_desc with
    Pmod_ident lid ->
      let (path, mty) = Typetexp.find_module env smod.pmod_loc lid.txt in
      rm { mod_desc = Tmod_ident (path, lid);
           mod_type = if sttn then Mtype.strengthen env mty path else mty;
           mod_env = env;
           mod_attributes = smod.pmod_attributes;
           mod_loc = smod.pmod_loc }
  | Pmod_structure sstr ->
      let (str, sg, finalenv) =
        type_structure funct_body anchor env sstr smod.pmod_loc in
      rm { mod_desc = Tmod_structure str;
           mod_type = Mty_signature sg;
           mod_env = env;
           mod_attributes = smod.pmod_attributes;
           mod_loc = smod.pmod_loc }
  | Pmod_functor(name, smty, sbody) ->
      let mty = transl_modtype env smty in
      let (id, newenv) = Env.enter_module name.txt mty.mty_type env in
      let body = type_module sttn true None newenv sbody in
      rm { mod_desc = Tmod_functor(id, name, mty, body);
           mod_type = Mty_functor(id, mty.mty_type, body.mod_type);
           mod_env = env;
           mod_attributes = smod.pmod_attributes;
           mod_loc = smod.pmod_loc }
  | Pmod_apply(sfunct, sarg) ->
      let arg = type_module true funct_body None env sarg in
      let path = try Some (path_of_module arg) with Not_a_path -> None in
      let funct =
        type_module (sttn && path <> None) funct_body None env sfunct in
      begin match Mtype.scrape env funct.mod_type with
        Mty_functor(param, mty_param, mty_res) as mty_functor ->
          let coercion =
            try
              Includemod.modtypes env arg.mod_type mty_param
            with Includemod.Error msg ->
              raise(Error(sarg.pmod_loc, env, Not_included msg)) in
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
                  raise(Error(smod.pmod_loc, env,
                              Cannot_eliminate_dependency mty_functor))
          in
          rm { mod_desc = Tmod_apply(funct, arg, coercion);
               mod_type = mty_appl;
               mod_env = env;
               mod_attributes = smod.pmod_attributes;
               mod_loc = smod.pmod_loc }
      | _ ->
          raise(Error(sfunct.pmod_loc, env, Cannot_apply funct.mod_type))
      end
  | Pmod_constraint(sarg, smty) ->
      let arg = type_module true funct_body anchor env sarg in
      let mty = transl_modtype env smty in
      rm {(wrap_constraint env arg mty.mty_type (Tmodtype_explicit mty)) with
          mod_loc = smod.pmod_loc;
          mod_attributes = smod.pmod_attributes;
         }

  | Pmod_unpack sexp ->
      if funct_body then
        raise (Error (smod.pmod_loc, env, Not_allowed_in_functor_body));
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
              raise (Error (smod.pmod_loc, env,
                            Incomplete_packed_module exp.exp_type));
            if !Clflags.principal &&
              not (Typecore.generalizable (Btype.generic_level-1) exp.exp_type)
            then
              Location.prerr_warning smod.pmod_loc
                (Warnings.Not_principal "this module unpacking");
            modtype_of_package env smod.pmod_loc p nl tl
        | {desc = Tvar _} ->
            raise (Typecore.Error
                     (smod.pmod_loc, env, Typecore.Cannot_infer_signature))
        | _ ->
            raise (Error(smod.pmod_loc, env, Not_a_packed_module exp.exp_type))
      in
      rm { mod_desc = Tmod_unpack(exp, mty);
           mod_type = mty;
           mod_env = env;
           mod_attributes = smod.pmod_attributes;
           mod_loc = smod.pmod_loc }
  | Pmod_extension (s, _arg) ->
      raise (Error (s.loc, env, Extension s.txt))

and type_structure ?(toplevel = false) funct_body anchor env sstr scope =
  let type_names = ref StringSet.empty
  and module_names = ref StringSet.empty
  and modtype_names = ref StringSet.empty in
  let rec type_struct env sstr =
    let previous_saved_types = Cmt_format.get_saved_types () in
    Ctype.init_def(Ident.current_time());
    match sstr with
      [] ->
        ([], [], env)
      | pstr :: srem ->
          let loc = pstr.pstr_loc in
          let mk desc =
            let str = { str_desc = desc; str_loc = loc; str_env = env } in
            Cmt_format.set_saved_types (Cmt_format.Partial_structure_item str
                                        :: previous_saved_types);
            str
          in
            match pstr.pstr_desc with
              | Pstr_eval (sexpr, attrs) ->
                  let expr = Typecore.type_expression env sexpr in
                  let item = mk (Tstr_eval (expr, attrs)) in
                  let (str_rem, sig_rem, final_env) = type_struct env srem in
                  (item :: str_rem, sig_rem, final_env)
              | Pstr_value(rec_flag, sdefs) ->
        let scope =
          match rec_flag with
          | Recursive -> Some (Annot.Idef {scope with
                                 Location.loc_start = loc.Location.loc_start})
          | Nonrecursive ->
              let start = match srem with
                | [] -> loc.Location.loc_end
                | {pstr_loc = loc2} :: _ -> loc2.Location.loc_start
              in Some (Annot.Idef {scope with Location.loc_start = start})
        in
        let (defs, newenv) =
          Typecore.type_binding env rec_flag sdefs scope in
        let item = mk (Tstr_value(rec_flag, defs)) in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        let bound_idents = let_bound_idents defs in
        (* Note: Env.find_value does not trigger the value_used event. Values
           will be marked as being used during the signature inclusion test. *)
        let make_sig_value id =
          Sig_value(id, Env.find_value (Pident id) newenv) in
        (item :: str_rem,
         map_end make_sig_value bound_idents sig_rem,
         final_env)
    | Pstr_primitive sdesc ->
        let (desc, newenv) = Typedecl.transl_value_decl env loc sdesc in
        let item = mk (Tstr_primitive desc) in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (item :: str_rem, Sig_value(desc.val_id, desc.val_val) :: sig_rem, final_env)
    | Pstr_type sdecls ->
        List.iter
          (fun decl -> check "type" loc type_names decl.ptype_name.txt)
          sdecls;
        let (decls, newenv) = Typedecl.transl_type_decl env sdecls in
        let item = mk (Tstr_type decls) in
        let newenv' =
          enrich_type_decls anchor decls env newenv in
        let (str_rem, sig_rem, final_env) = type_struct newenv' srem in
        (item :: str_rem,
         map_rec'' (fun rs info -> Sig_type(info.typ_id, info.typ_type, rs))
           decls sig_rem,
         final_env)
    | Pstr_exception sarg ->
        let (arg, decl, newenv) = Typedecl.transl_exception env sarg in
        let item = mk (Tstr_exception arg) in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (item :: str_rem,
         Sig_exception(arg.cd_id, decl) :: sig_rem,
         final_env)
    | Pstr_exn_rebind(name, longid, attrs) ->
        let (path, arg) = Typedecl.transl_exn_rebind env loc longid.txt in
        let (id, newenv) = Env.enter_exception name.txt arg env in
        let item = mk (Tstr_exn_rebind(id, name, path, longid, attrs)) in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (item :: str_rem,
         Sig_exception(id, arg) :: sig_rem,
         final_env)
    | Pstr_module {pmb_name = name; pmb_expr = smodl; pmb_attributes = attrs} ->
        check "module" loc module_names name.txt;
        let modl =
          type_module true funct_body (anchor_submodule name.txt anchor) env
            smodl in
        let mty = enrich_module_type anchor name.txt modl.mod_type env in
        let (id, newenv) = Env.enter_module name.txt mty env in
        let item = mk
            (Tstr_module
               {
                mb_id=id;
                mb_name=name;
                mb_expr=modl;
                mb_attributes=attrs;
               }
            )
        in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (item :: str_rem,
         Sig_module(id, modl.mod_type, Trec_not) :: sig_rem,
         final_env)
    | Pstr_recmodule sbind ->
        let sbind =
          List.map
            (function
              | {pmb_name = name; pmb_expr = {pmod_desc=Pmod_constraint(expr, typ)}; pmb_attributes = attrs} ->
                  name, typ, expr, attrs
              | mb ->
                  raise (Error (mb.pmb_expr.pmod_loc, env, Recursive_module_require_explicit_type))
            )
            sbind
        in
        List.iter
          (fun (name, _, _, _) -> check "module" loc module_names name.txt)
          sbind;
        let (decls, newenv) =
          transl_recmodule_modtypes loc env
            (List.map (fun (name, smty, smodl, attrs) -> {pmd_name=name; pmd_type=smty; pmd_attributes=attrs}) sbind) in
        let bindings1 =
          List.map2
            (fun {md_id=id; md_type=mty} (name, _, smodl, attrs) ->
              let modl =
                type_module true funct_body (anchor_recmodule id anchor) newenv
                  smodl in
              let mty' =
                enrich_module_type anchor (Ident.name id) modl.mod_type newenv
              in
              (id, name, mty, modl, mty', attrs))
           decls sbind in
        let bindings2 =
          check_recmodule_inclusion newenv bindings1 in
        let item = mk (Tstr_recmodule bindings2) in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (item :: str_rem,
         map_rec (fun rs mb -> Sig_module(mb.mb_id, mb.mb_expr.mod_type, rs))
                 bindings2 sig_rem,
         final_env)
    | Pstr_modtype pmtd ->
        (* check that it is non-abstract *)
        let newenv, mtd, sg =
          transl_modtype_decl modtype_names env loc pmtd
        in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        mk (Tstr_modtype mtd) :: str_rem,
        sg :: sig_rem,
        final_env
    | Pstr_open (ovf, lid, attrs) ->
        let (path, newenv) = type_open ovf ~toplevel env loc lid in
        let item = mk (Tstr_open (ovf, path, lid, attrs)) in
        let (str_rem, sig_rem, final_env) = type_struct newenv srem in
        (item :: str_rem, sig_rem, final_env)
    | Pstr_class cl ->
         List.iter
           (fun {pci_name = name} -> check "type" loc type_names name.txt)
           cl;
        let (classes, new_env) = Typeclass.class_declarations env cl in
        let item =
          mk
            (Tstr_class
               (List.map (fun (i, _, d, _,_,_,_,_,_, s, m, c) ->
                 let vf = if d.cty_new = None then Virtual else Concrete in
                 (* (i, s, m, c, vf) *) (c, m, vf)) classes))
(* TODO: check with Jacques why this is here
           Tstr_class_type
           (List.map (fun (_,_, i, d, _,_,_,_,_,_,c) -> (i, c)) classes) ::
         Tstr_type
           (List.map (fun (_,_,_,_, i, d, _,_,_,_,_) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_,_,_,_,_,_, i, d, _,_,_) -> (i, d)) classes) ::
*)
        in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (item :: str_rem,
         List.flatten
           (map_rec
              (fun rs (i, _, d, i', d', i'', d'', i''', d''', _, _, _) ->
                [Sig_class(i, d, rs);
                 Sig_class_type(i', d', rs);
                 Sig_type(i'', d'', rs);
                 Sig_type(i''', d''', rs)])
              classes [sig_rem]),
         final_env)
    | Pstr_class_type cl ->
        List.iter
          (fun {pci_name = name} -> check "type" loc type_names name.txt)
          cl;
        let (classes, new_env) = Typeclass.class_type_declarations env cl in
        let item =
          mk
            (Tstr_class_type
               (List.map (fun (i, i_loc, d, _, _, _, _, c) ->
                 (i, i_loc, c)) classes))
(*  TODO: check with Jacques why this is here
       Tstr_type
           (List.map (fun (_, _, i, d, _, _) -> (i, d)) classes) ::
         Tstr_type
           (List.map (fun (_, _, _, _, i, d) -> (i, d)) classes) :: *)
        in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (item :: str_rem,
         List.flatten
           (map_rec
              (fun rs (i, _, d, i', d', i'', d'', _) ->
                 [Sig_class_type(i, d, rs);
                  Sig_type(i', d', rs);
                  Sig_type(i'', d'', rs)])
              classes [sig_rem]),
         final_env)
    | Pstr_include (smodl, attrs) ->
        let modl = type_module true funct_body None env smodl in
        (* Rename all identifiers bound by this signature to avoid clashes *)
        let sg = Subst.signature Subst.identity
                   (extract_sig_open env smodl.pmod_loc modl.mod_type) in
        List.iter
          (check_sig_item type_names module_names modtype_names loc) sg;
        let new_env = Env.add_signature sg env in
        let item = mk (Tstr_include (modl, sg, attrs)) in
        let (str_rem, sig_rem, final_env) = type_struct new_env srem in
        (item :: str_rem,
         sg @ sig_rem,
         final_env)
    | Pstr_extension ((s, _), _) ->
        raise (Error (s.loc, env, Extension s.txt))
    | Pstr_attribute x ->
        let (str_rem, sig_rem, final_env) = type_struct env srem in
        mk (Tstr_attribute x) :: str_rem, sig_rem, final_env
  in
  if !Clflags.annotations then
    (* moved to genannot *)
    List.iter (function {pstr_loc = l} -> Stypes.record_phrase l) sstr;
  let previous_saved_types = Cmt_format.get_saved_types () in
  let (items, sg, final_env) = type_struct env sstr in
  let str = { str_items = items; str_type = sg; str_final_env = final_env } in
  Cmt_format.set_saved_types
    (Cmt_format.Partial_structure str :: previous_saved_types);
  str, sg, final_env

let type_toplevel_phrase env s =
  type_structure ~toplevel:true false None env s Location.none
let type_module = type_module true false None
let type_structure = type_structure false None

(* Normalize types in a signature *)

let rec normalize_modtype env = function
    Mty_ident p -> ()
  | Mty_signature sg -> normalize_signature env sg
  | Mty_functor(id, param, body) -> normalize_modtype env body

and normalize_signature env = List.iter (normalize_signature_item env)

and normalize_signature_item env = function
    Sig_value(id, desc) -> Ctype.normalize_type env desc.val_type
  | Sig_module(id, mty, _) -> normalize_modtype env mty
  | _ -> ()

(* Simplify multiple specifications of a value or an exception in a signature.
   (Other signature components, e.g. types, modules, etc, are checked for
   name uniqueness.)  If multiple specifications with the same name,
   keep only the last (rightmost) one. *)

let rec simplify_modtype mty =
  match mty with
    Mty_ident path -> mty
  | Mty_functor(id, arg, res) -> Mty_functor(id, arg, simplify_modtype res)
  | Mty_signature sg -> Mty_signature(simplify_signature sg)

and simplify_signature sg =
  let rec simplif val_names exn_names res = function
    [] -> res
  | (Sig_value(id, descr) as component) :: sg ->
      let name = Ident.name id in
      simplif (StringSet.add name val_names) exn_names
              (if StringSet.mem name val_names then res else component :: res)
              sg
  | (Sig_exception(id, decl) as component) :: sg ->
      let name = Ident.name id in
      simplif val_names (StringSet.add name exn_names)
              (if StringSet.mem name exn_names then res else component :: res)
              sg
  | Sig_module(id, mty, rs) :: sg ->
      simplif val_names exn_names
              (Sig_module(id, simplify_modtype mty, rs) :: res) sg
  | component :: sg ->
      simplif val_names exn_names (component :: res) sg
  in
    simplif StringSet.empty StringSet.empty [] (List.rev sg)

(* Extract the module type of a module expression *)

let type_module_type_of env smod =
  let tmty =
    match smod.pmod_desc with
    | Pmod_ident lid -> (* turn off strengthening in this case *)
        let (path, mty) = Typetexp.find_module env smod.pmod_loc lid.txt in
        rm { mod_desc = Tmod_ident (path, lid);
             mod_type = mty;
             mod_env = env;
             mod_attributes = smod.pmod_attributes;
             mod_loc = smod.pmod_loc }
    | _ -> type_module env smod in
  let mty = tmty.mod_type in
  (* PR#5037: clean up inferred signature to remove duplicate specs *)
  let mty = simplify_modtype mty in
  (* PR#5036: must not contain non-generalized type variables *)
  if not (closed_modtype mty) then
    raise(Error(smod.pmod_loc, env, Non_generalizable_module mty));
  tmty, mty

(* For Typecore *)

let rec get_manifest_types = function
    [] -> []
  | Sig_type (id, {type_params=[]; type_manifest=Some ty}, _) :: rem ->
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
      Tmod_ident (mp,_) -> (mp, env)
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
  if nl = [] then
    (wrap_constraint env modl (Mty_ident p) Tmodtype_implicit, [])
  else let mty = modtype_of_package env modl.mod_loc p nl tl' in
  List.iter2
    (fun n ty ->
      try Ctype.unify env ty (Ctype.newvar ())
      with Ctype.Unify _ ->
        raise (Error(m.pmod_loc, env, Scoping_pack (n,ty))))
    nl tl';
  (wrap_constraint env modl mty Tmodtype_implicit, tl')

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
  Cmt_format.set_saved_types [];
  try
  Typecore.reset_delayed_checks ();
  let (str, sg, finalenv) =
    type_structure initial_env ast (Location.in_file sourcefile) in
  let simple_sg = simplify_signature sg in
  if !Clflags.print_types then begin
    Printtyp.wrap_printing_env initial_env
      (fun () -> fprintf std_formatter "%a@." Printtyp.signature simple_sg);
    (str, Tcoerce_none)   (* result is ignored by Compile.implementation *)
  end else begin
    let sourceintf =
      Misc.chop_extension_if_any sourcefile ^ !Config.interface_suffix in
    if Sys.file_exists sourceintf then begin
      let intf_file =
        try
          find_in_path_uncap !Config.load_path (modulename ^ ".cmi")
        with Not_found ->
          raise(Error(Location.in_file sourcefile, Env.empty,
                      Interface_not_compiled sourceintf)) in
      let dclsig = Env.read_signature modulename intf_file in
      let coercion = Includemod.compunit sourcefile sg intf_file dclsig in
      Typecore.force_delayed_checks ();
      (* It is important to run these checks after the inclusion test above,
         so that value declarations which are not used internally but exported
         are not reported as being unused. *)
      Cmt_format.save_cmt (outputprefix ^ ".cmt") modulename
        (Cmt_format.Implementation str) (Some sourcefile) initial_env None;
      (str, coercion)
    end else begin
      check_nongen_schemes finalenv str.str_items;
      normalize_signature finalenv simple_sg;
      let coercion =
        Includemod.compunit sourcefile sg
                            "(inferred signature)" simple_sg in
      Typecore.force_delayed_checks ();
      (* See comment above. Here the target signature contains all
         the value being exported. We can still capture unused
         declarations like "let x = true;; let x = 1;;", because in this
         case, the inferred signature contains only the last declaration. *)
      if not !Clflags.dont_write_files then begin
        let sg =
          Env.save_signature simple_sg modulename (outputprefix ^ ".cmi") in
        Cmt_format.save_cmt  (outputprefix ^ ".cmt") modulename
          (Cmt_format.Implementation str)
          (Some sourcefile) initial_env (Some sg);
      end;
      (str, coercion)
    end
    end
  with e ->
    Cmt_format.save_cmt  (outputprefix ^ ".cmt") modulename
      (Cmt_format.Partial_implementation
         (Array.of_list (Cmt_format.get_saved_types ())))
      (Some sourcefile) initial_env None;
    raise e


let save_signature modname tsg outputprefix source_file initial_env cmi =
  Cmt_format.save_cmt  (outputprefix ^ ".cmti") modname
    (Cmt_format.Interface tsg) (Some source_file) initial_env (Some cmi)

(* "Packaging" of several compilation units into one unit
   having them as sub-modules.  *)

let rec package_signatures subst = function
    [] -> []
  | (name, sg) :: rem ->
      let sg' = Subst.signature subst sg in
      let oldid = Ident.create_persistent name
      and newid = Ident.create name in
      Sig_module(newid, Mty_signature sg', Trec_not) ::
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
         then raise(Error(Location.none, Env.empty,
                          Implementation_is_required f));
         (modname, Env.read_signature modname (pref ^ ".cmi")))
      objfiles in
  (* Compute signature of packaged unit *)
  Ident.reinit();
  let sg = package_signatures Subst.identity units in
  (* See if explicit interface is provided *)
  let prefix = chop_extension_if_any cmifile in
  let mlifile = prefix ^ !Config.interface_suffix in
  if Sys.file_exists mlifile then begin
    if not (Sys.file_exists cmifile) then begin
      raise(Error(Location.in_file mlifile, Env.empty,
                  Interface_not_compiled mlifile))
    end;
    let dclsig = Env.read_signature modulename cmifile in
    Cmt_format.save_cmt  (prefix ^ ".cmt") modulename
      (Cmt_format.Packed (sg, objfiles)) None Env.initial None ;
    Includemod.compunit "(obtained by packing)" sg mlifile dclsig
  end else begin
    (* Determine imports *)
    let unit_names = List.map fst units in
    let imports =
      List.filter
        (fun (name, crc) -> not (List.mem name unit_names))
        (Env.imported_units()) in
    (* Write packaged signature *)
    if not !Clflags.dont_write_files then begin
      let sg =
        Env.save_signature_with_imports sg modulename
          (prefix ^ ".cmi") imports in
      Cmt_format.save_cmt (prefix ^ ".cmt")  modulename
        (Cmt_format.Packed (sg, objfiles)) None Env.initial (Some sg)
    end;
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
        "@[The interface %a@ declares values, not just types.@ \
           An implementation must be provided.@]"
        Location.print_filename intf_name
  | Interface_not_compiled intf_name ->
      fprintf ppf
        "@[Could not find the .cmi file for interface@ %a.@]"
        Location.print_filename intf_name
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
  | Extension s ->
      fprintf ppf "Uninterpreted extension '%s'." s
  | Recursive_module_require_explicit_type ->
      fprintf ppf "Recursive modules require an explicit module type."

let report_error env ppf err =
  Printtyp.wrap_printing_env env (fun () -> report_error ppf err)
