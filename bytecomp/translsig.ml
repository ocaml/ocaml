(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                  Jun Furuse, University of Tokyo                    *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Types
open Typedtree

exception Not_pure

let rec structure_of_signature env sg =
  let types = ref [] in
  List.fold_right (fun sgitem (st, env) ->
    match sgitem with
    | Tsig_value (id, vdesc) ->
	(* values are accepted only when they are external *)
	begin match vdesc.val_kind with
	| Val_prim primdesc -> 
	    let env' = Env.add_value id vdesc env in
	    Tstr_primitive (id, vdesc) :: st, env'
	| _ -> 
	    (* it defines some val : must exist .ml *)
	    raise Not_pure
	end
    | Tsig_type (id, type_decl, Trec_first) ->
	let env' = Env.add_type id type_decl env in
	let decls = (id, type_decl) :: !types in
	types := [];
	Tstr_type decls :: st, env'
    | Tsig_type (id, type_decl, Trec_next) -> 
	let env' = Env.add_type id type_decl env in
	types := (id, type_decl) :: !types; 
	st, env'
    | Tsig_type (id, type_decl, Trec_not) ->
	let env' = Env.add_type id type_decl env in
	Tstr_type [id, type_decl] :: st, env'
    | Tsig_module (id, modtyp, Trec_not) ->
	let env' = Env.add_module id modtyp env in
	Tstr_module (id, module_of_module_type env' modtyp) :: st, env'
    | Tsig_module (id, modtype, _) -> 
	(* recursive module is not supported yet *)
	Format.eprintf "Translsig: RECURSIVE MODULE %s@." (Ident.unique_name id);
	raise Not_pure
    | Tsig_class (id, classdecl, rec_status) -> raise Not_pure
    | Tsig_exception (id, excdecl) ->
	st, Env.add_exception id excdecl env
    | Tsig_modtype (id, modtype_decl) ->
	st, Env.add_modtype id modtype_decl env
    | Tsig_cltype (id, cltype_decl, _) ->
	st, Env.add_cltype id cltype_decl env) sg ([], env)

and module_of_module_type env t =
  match t with
  | Tmty_ident path ->
      { mod_desc= Tmod_ident path;
	mod_loc= Location.none;
	mod_type= t;
	mod_env= env }
  | Tmty_signature sg ->
      { mod_desc= Tmod_structure (fst (structure_of_signature env sg));
	mod_loc= Location.none;
	mod_type= t;
	mod_env= env }
  | Tmty_functor (fid, argt1, argt2)->
      { mod_desc= Tmod_functor (fid, argt1, module_of_module_type env argt2);
	mod_loc= Location.none;
	mod_type= t;
	mod_env= env }
