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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Misc
open Asttypes
open Typedtree
open Lambda
open Translcore


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
      let param = Ident.new "funarg" in
      name_lambda arg (fun id ->
        Lfunction([param],
          apply_coercion cc_res
            (Lapply(Lvar id, [apply_coercion cc_arg (Lvar param)]))))
  | Tcoerce_primitive p ->
      fatal_error "Translmod.apply_coercion"

and apply_coercion_field id (pos, cc) =
  match cc with
    Tcoerce_primitive p -> transl_primitive p
  | _ -> apply_coercion cc (Lprim(Pfield pos, [Lvar id]))

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
        (List.map (fun (p1, c1) ->
                let (p2, c2) = v2.(p1) in (p2, compose_coercions c1 c2))
             pc1)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(* Record the primitive declarations occuring in the module compiled *)

let primitive_declarations = ref ([] : string list)

(* Compile a module expression *)

let rec transl_module cc mexp =
  match mexp.mod_desc with
    Tmod_ident path ->
      apply_coercion cc (transl_path path)
  | Tmod_structure str ->
      transl_structure [] cc str
  | Tmod_functor(param, mty, body) ->
      begin match cc with
        Tcoerce_none ->
          Lfunction([param], transl_module Tcoerce_none body)
      | Tcoerce_functor(ccarg, ccres) ->
          let param' = Ident.new "funarg" in
          Lfunction([param'],
            Llet(Alias, param, apply_coercion ccarg (Lvar param'),
              transl_module ccres body))
      | _ ->
          fatal_error "Translmod.transl_module"
      end
  | Tmod_apply(funct, arg, ccarg) ->
      apply_coercion cc
        (Lapply(transl_module Tcoerce_none funct, [transl_module ccarg arg]))
  | Tmod_constraint(arg, mty, ccarg) ->
      transl_module (compose_coercions cc ccarg) arg

and transl_structure fields cc = function
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
      Lsequence(transl_exp expr, transl_structure fields cc rem)
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
      transl_let rec_flag pat_expr_list (transl_structure ext_fields cc rem)
  | Tstr_primitive(id, descr) :: rem ->
      begin match descr.val_prim with
        None -> ()
      | Some p -> primitive_declarations :=
                    p.Primitive.prim_name :: !primitive_declarations
      end;
      transl_structure fields cc rem
  | Tstr_type(decls) :: rem ->
      transl_structure fields cc rem
  | Tstr_exception(id, decl) :: rem ->
      Llet(Strict, id, transl_exception id decl,
           transl_structure (id :: fields) cc rem)
  | Tstr_module(id, modl) :: rem ->
      Llet(Strict, id, transl_module Tcoerce_none modl,
           transl_structure (id :: fields) cc rem)
  | Tstr_modtype(id, decl) :: rem ->
      transl_structure fields cc rem
  | Tstr_open path :: rem ->
      transl_structure fields cc rem

(* Compile an implementation *)

let transl_implementation module_name str cc =
  primitive_declarations := [];
  let module_id = Ident.new_persistent module_name in
  Lprim(Psetglobal module_id, [transl_structure [] cc str])

(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.
   "map" is a table from idents to (position in global block, coercion).
   "prim" is a list of (position in global block, primitive declaration). *)

let transl_store_structure glob map prims str =
  let rec transl_store = function
    [] ->
      lambda_unit
  | Tstr_eval expr :: rem ->
      Lsequence(transl_exp expr, transl_store rem)
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      transl_let rec_flag pat_expr_list
        (store_idents glob map (let_bound_idents pat_expr_list)
          (transl_store rem))
  | Tstr_primitive(id, descr) :: rem ->
      begin match descr.val_prim with
        None -> ()
      | Some p -> primitive_declarations :=
                    p.Primitive.prim_name :: !primitive_declarations
      end;
      transl_store rem
  | Tstr_type(decls) :: rem ->
      transl_store rem
  | Tstr_exception(id, decl) :: rem ->
      Llet(Strict, id, transl_exception id decl,
           store_ident glob map id (transl_store rem))
  | Tstr_module(id, modl) :: rem ->
      Llet(Strict, id, transl_module Tcoerce_none modl,
           store_ident glob map id (transl_store rem))
  | Tstr_modtype(id, decl) :: rem ->
      transl_store rem
  | Tstr_open path :: rem ->
      transl_store rem

  and store_ident glob map id cont =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion cc (Lvar id) in
      Lsequence
       (Lprim(Psetfield(pos, false), [Lprim(Pgetglobal glob, []); init_val]),
        cont)
    with Not_found ->
      cont

  and store_idents glob map idlist cont =
    List.fold_right (store_ident glob map) idlist cont

  and store_primitive (pos, prim) cont =
    Lsequence(Lprim(Psetfield(pos, false),
                    [Lprim(Pgetglobal glob, []); transl_primitive prim]),
              cont)
  in
    List.fold_right store_primitive prims (transl_store str)

(* Build the list of value identifiers defined by a toplevel structure *)

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

(* Transform a coercion and the list of value identifiers built above
   into a table id -> (pos, coercion), with [pos] being the position
   in the global block where the value of [id] must be stored,
   and [coercion] the coercion to be applied to it.
   A given identifier may appear several times
   in the coercion (if it occurs several times in the signature); remember
   to assign it the position of its last occurrence.
   Also buid a list of primitives and their positions in the global block,
   and the total size of the global block. *)

let build_ident_map restr idlist =
  match restr with
    Tcoerce_none ->
      let rec build_map pos map = function
        [] ->
          (map, [], pos)
      | id :: rem ->
          build_map (pos+1) (Ident.add id (pos, Tcoerce_none) map) rem
      in build_map 0 Ident.empty idlist
  | Tcoerce_structure pos_cc_list ->
      let idarray = Array.of_list idlist in
      let rec build_map pos map prims = function
        [] ->
          (map, prims, pos)
      | (source_pos, Tcoerce_primitive p) :: rem ->
          build_map (pos+1) map ((pos, p) :: prims) rem
      | (source_pos, cc) :: rem ->
          build_map (pos+1) (Ident.add idarray.(source_pos) (pos, cc) map)
                    prims rem
      in build_map 0 Ident.empty [] pos_cc_list
  | _ ->
      fatal_error "Translmod.build_ident_map"
        
(* Compile an implementation using transl_store_structure 
   (for the native-code compiler). *)

let transl_store_implementation module_name str restr =
  primitive_declarations := [];
  let module_id = Ident.new_persistent module_name in
  let (map, prims, size) = build_ident_map restr (defined_idents str) in
  (size, transl_store_structure module_id map prims str)

(* Compile a sequence of expressions *)

let rec make_sequence fn = function
    [] -> lambda_unit
  | [x] -> fn x
  | x::rem -> Lsequence(fn x, make_sequence fn rem)

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
      Lprim(Psetglobal id, [transl_exception id decl])
  | Tstr_module(id, modl) ->
      Ident.make_global id;
      Lprim(Psetglobal id, [transl_module Tcoerce_none modl])
  | Tstr_modtype(id, decl) ->
      lambda_unit
  | Tstr_open path ->
      lambda_unit

let transl_toplevel_definition str =
  make_sequence transl_toplevel_item str
