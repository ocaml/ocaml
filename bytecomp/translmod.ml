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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Misc
open Asttypes
open Path
open Types
open Typedtree
open Lambda
open Translobj
open Translcore
open Translclass

(* Compile a coercion *)

let rec apply_coercion restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure pos_cc_list ->
      name_lambda arg (fun id ->
        Lprim(Pmakeblock(0, Immutable),
              List.map (apply_coercion_field id) pos_cc_list))
  | Tcoerce_functor(cc_arg, cc_res) ->
      let param = Ident.create "funarg" in
      name_lambda arg (fun id ->
        Lfunction(Curried, [param],
          apply_coercion cc_res
            (Lapply(Lvar id, [apply_coercion cc_arg (Lvar param)]))))
  | Tcoerce_primitive p ->
      transl_primitive p

and apply_coercion_field id (pos, cc) =
  apply_coercion cc (Lprim(Pfield pos, [Lvar id]))

(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | (Tcoerce_structure pc1, Tcoerce_structure pc2) ->
      let v2 = Array.of_list pc2 in
      Tcoerce_structure
        (List.map
          (function (p1, Tcoerce_primitive p) ->
                      (p1, Tcoerce_primitive p)
                  | (p1, c1) ->
                      let (p2, c2) = v2.(p1) in (p2, compose_coercions c1 c2))
             pc1)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(* Record the primitive declarations occuring in the module compiled *)

let primitive_declarations = ref ([] : string list)

(* Keep track of the root path (from the root of the namespace to the
   currently compiled module expression).  Useful for naming exceptions. *)

let global_path glob = Some(Pident glob)
let functor_path path param =
  match path with
    None -> None
  | Some p -> Some(Papply(p, Pident param))
let field_path path field =
  match path with
    None -> None
  | Some p -> Some(Pdot(p, Ident.name field, Path.nopos))

(* Compile a module expression *)

let rec transl_module cc rootpath mexp =
  match mexp.mod_desc with
    Tmod_ident path ->
      apply_coercion cc (transl_path path)
  | Tmod_structure str ->
      transl_structure [] cc rootpath str
  | Tmod_functor(param, mty, body) ->
      let bodypath = functor_path rootpath param in
      begin match cc with
        Tcoerce_none ->
          Lfunction(Curried, [param], transl_module Tcoerce_none bodypath body)
      | Tcoerce_functor(ccarg, ccres) ->
          let param' = Ident.create "funarg" in
          Lfunction(Curried, [param'],
            Llet(Alias, param, apply_coercion ccarg (Lvar param'),
              transl_module ccres bodypath body))
      | _ ->
          fatal_error "Translmod.transl_module"
      end
  | Tmod_apply(funct, arg, ccarg) ->
      apply_coercion cc
        (Lapply(transl_module Tcoerce_none None funct,
                [transl_module ccarg None arg]))
  | Tmod_constraint(arg, mty, ccarg) ->
      transl_module (compose_coercions cc ccarg) rootpath arg

and transl_structure fields cc rootpath = function
    [] ->
      begin match cc with
        Tcoerce_none ->
          Lprim(Pmakeblock(0, Immutable),
                List.map (fun id -> Lvar id) (List.rev fields))
      | Tcoerce_structure pos_cc_list ->
          let v = Array.of_list (List.rev fields) in
          Lprim(Pmakeblock(0, Immutable),
                List.map
                  (fun (pos, cc) ->
                    match cc with
                      Tcoerce_primitive p -> transl_primitive p
                    | _ -> apply_coercion cc (Lvar v.(pos)))
                  pos_cc_list)
      | _ ->
          fatal_error "Translmod.transl_structure"
      end
  | Tstr_eval expr :: rem ->
      Lsequence(transl_exp expr, transl_structure fields cc rootpath rem)
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
      transl_let rec_flag pat_expr_list
                 (transl_structure ext_fields cc rootpath rem)
  | Tstr_primitive(id, descr) :: rem ->
      begin match descr.val_kind with
        Val_prim p -> primitive_declarations :=
                        p.Primitive.prim_name :: !primitive_declarations
      | _ -> ()
      end;
      transl_structure fields cc rootpath rem
  | Tstr_type(decls) :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_exception(id, decl) :: rem ->
      Llet(Strict, id, transl_exception id (field_path rootpath id) decl,
           transl_structure (id :: fields) cc rootpath rem)
  | Tstr_module(id, modl) :: rem ->
      Llet(Strict, id,
           transl_module Tcoerce_none (field_path rootpath id) modl,
           transl_structure (id :: fields) cc rootpath rem)
  | Tstr_modtype(id, decl) :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_open path :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_class cl_list :: rem ->
      let ids = List.map (fun (i, _, _, _) -> i) cl_list in
      Lletrec(List.map
                (fun (id, arity, meths, cl) ->
                  (id, transl_class ids id arity meths cl))
                cl_list,
              transl_structure (List.rev ids @ fields) cc rootpath rem)
  | Tstr_cltype cl_list :: rem ->
      transl_structure fields cc rootpath rem

(* Update forward declaration in Translcore *)
let _ =
  Translcore.transl_module := transl_module

(* Compile an implementation *)

let transl_implementation module_name (str, cc) =
  reset_labels ();
  primitive_declarations := [];
  let module_id = Ident.create_persistent module_name in
  Lprim(Psetglobal module_id,
        [transl_label_init
            (transl_structure [] cc (global_path module_id) str)])

(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

let transl_store_structure glob map prims str =
  let rec transl_store subst = function
    [] ->
      lambda_unit
  | Tstr_eval expr :: rem ->
      Lsequence(subst_lambda subst (transl_exp expr),
                transl_store subst rem)
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let ids = let_bound_idents pat_expr_list in
      let lam = transl_let rec_flag pat_expr_list (store_idents ids) in
      Lsequence(subst_lambda subst lam,
                transl_store (add_idents ids subst) rem)
  | Tstr_primitive(id, descr) :: rem ->
      begin match descr.val_kind with
        Val_prim p -> primitive_declarations :=
                        p.Primitive.prim_name :: !primitive_declarations
      | _ -> ()
      end;
      transl_store subst rem
  | Tstr_type(decls) :: rem ->
      transl_store subst rem
  | Tstr_exception(id, decl) :: rem ->
      let lam = transl_exception id (field_path (global_path glob) id) decl in
      Lsequence(Llet(Strict, id, lam, store_ident id),
                transl_store (add_ident id subst) rem)
  | Tstr_module(id, modl) :: rem ->
      let lam =
        transl_module Tcoerce_none (field_path (global_path glob) id) modl in
      Lsequence(Llet(Strict, id,
                     subst_lambda subst lam, store_ident id),
                transl_store (add_ident id subst) rem)
  | Tstr_modtype(id, decl) :: rem ->
      transl_store subst rem
  | Tstr_open path :: rem ->
      transl_store subst rem
  | Tstr_class cl_list :: rem ->
      let ids = List.map (fun (i, _, _, _) -> i) cl_list in
      let lam =
        Lletrec(List.map
                  (fun (id, arity, meths, cl) ->
                     (id, transl_class ids id arity meths cl))
                  cl_list,
                store_idents ids) in
      Lsequence(subst_lambda subst lam,
                transl_store (add_idents ids subst) rem)
  | Tstr_cltype cl_list :: rem ->
      transl_store subst rem

  and store_ident id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion cc (Lvar id) in
      Lprim(Psetfield(pos, false), [Lprim(Pgetglobal glob, []); init_val])
    with Not_found ->
      fatal_error("Translmod.transl_store_structure: " ^ Ident.unique_name id)

  and store_idents idlist =
    make_sequence store_ident idlist

  and add_ident id subst =
    try
      let (pos, cc) = Ident.find_same id map in
      Ident.add id (Lprim(Pfield pos, [Lprim(Pgetglobal glob, [])])) subst
    with Not_found ->
      fatal_error("Translmod.transl_store_structure: " ^ Ident.unique_name id)

  and add_idents idlist subst =
    List.fold_right add_ident idlist subst

  and store_primitive (pos, prim) cont =
    Lsequence(Lprim(Psetfield(pos, false),
                    [Lprim(Pgetglobal glob, []); transl_primitive prim]),
              cont)

  in List.fold_right store_primitive prims (transl_store Ident.empty str)

(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

let rec defined_idents = function
    [] -> []
  | Tstr_eval expr :: rem -> defined_idents rem
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let_bound_idents pat_expr_list @ defined_idents rem
  | Tstr_primitive(id, descr) :: rem -> defined_idents rem
  | Tstr_type decls :: rem -> defined_idents rem
  | Tstr_exception(id, decl) :: rem -> id :: defined_idents rem
  | Tstr_module(id, modl) :: rem -> id :: defined_idents rem
  | Tstr_modtype(id, decl) :: rem -> defined_idents rem
  | Tstr_open path :: rem -> defined_idents rem
  | Tstr_class cl_list :: rem ->
      List.map (fun (i, _, _, _) -> i) cl_list @ defined_idents rem
  | Tstr_cltype cl_list :: rem -> defined_idents rem

(* Transform a coercion and the list of value identifiers defined by
   a toplevel structure into a table [id -> (pos, coercion)],
   with [pos] being the position in the global block where the value of
   [id] must be stored, and [coercion] the coercion to be applied to it.
   A given identifier may appear several times
   in the coercion (if it occurs several times in the signature); remember
   to assign it the position of its last occurrence.
   Identifiers that are not exported are assigned positions at the
   end of the block (beyond the positions of all exported idents).
   Also compute the total size of the global block,
   and the list of all primitives exported as values. *)

let build_ident_map restr idlist =
  let rec natural_map pos map prims = function
    [] ->
      (map, prims, pos)
  | id :: rem ->
      natural_map (pos+1) (Ident.add id (pos, Tcoerce_none) map) prims rem in
  match restr with
    Tcoerce_none ->
      natural_map 0 Ident.empty [] idlist
  | Tcoerce_structure pos_cc_list ->
      let idarray = Array.of_list idlist in
      let rec export_map pos map prims undef = function
        [] ->
          natural_map pos map prims undef
      | (source_pos, Tcoerce_primitive p) :: rem ->
          export_map (pos + 1) map ((pos, p) :: prims) undef rem
      | (source_pos, cc) :: rem ->
          let id = idarray.(source_pos) in
          export_map (pos + 1) (Ident.add id (pos, cc) map)
                     prims (list_remove id undef) rem
      in export_map 0 Ident.empty [] idlist pos_cc_list
  | _ ->
      fatal_error "Translmod.build_ident_map"

(* Compile an implementation using transl_store_structure 
   (for the native-code compiler). *)

let transl_store_implementation module_name (str, restr) =
  reset_labels ();
  primitive_declarations := [];
  let module_id = Ident.create_persistent module_name in
  let (map, prims, size) = build_ident_map restr (defined_idents str) in
  (size, transl_label_init (transl_store_structure module_id map prims str))

(* Compile a toplevel phrase *)

let transl_toplevel_item = function
    Tstr_eval expr ->
      transl_exp expr
  | Tstr_value(rec_flag, pat_expr_list) ->
      let idents = let_bound_idents pat_expr_list in
      let lam =
        transl_let rec_flag pat_expr_list
          (make_sequence (fun id -> Lprim(Psetglobal id, [Lvar id])) idents) in
      List.iter Ident.make_global idents;
      lam
  | Tstr_primitive(id, descr) ->
      lambda_unit
  | Tstr_type(decls) ->
      lambda_unit
  | Tstr_exception(id, decl) ->
      Ident.make_global id;
      Lprim(Psetglobal id, [transl_exception id None decl])
  | Tstr_module(id, modl) ->
      Ident.make_global id;
      Lprim(Psetglobal id, [transl_module Tcoerce_none None modl])
  | Tstr_modtype(id, decl) ->
      lambda_unit
  | Tstr_open path ->
      lambda_unit
  | Tstr_class cl_list ->
      let ids = List.map (fun (i, _, _, _) -> i) cl_list in
      let lam =
        Lletrec(List.map
                  (fun (id, arity, meths, cl) ->
                     (id, transl_class ids id arity meths cl))
                  cl_list,
                make_sequence
                  (fun (id, _, _, _) -> Lprim(Psetglobal id, [Lvar id]))
                  cl_list)
      in
      List.iter (fun (id, _, _, _) -> Ident.make_global id) cl_list;
      lam
      
  | Tstr_cltype cl_list ->
      lambda_unit

let transl_toplevel_definition str =
  reset_labels ();
  transl_label_init (make_sequence transl_toplevel_item str)
