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
   "map" is a table from idents to (position in global block, coercion). *)

let rec transl_store_structure glob map = function
    [] ->
      lambda_unit
  | Tstr_eval expr :: rem ->
      Lsequence(transl_exp expr, transl_store_structure glob map rem)
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
      transl_let rec_flag pat_expr_list
        (store_idents glob map (let_bound_idents pat_expr_list)
          (transl_store_structure glob map rem))
  | Tstr_primitive(id, descr) :: rem ->
      begin match descr.val_prim with
        None -> ()
      | Some p -> primitive_declarations :=
                    p.Primitive.prim_name :: !primitive_declarations
      end;
      store_ident glob map id (transl_store_structure glob map rem)
  | Tstr_type(decls) :: rem ->
      transl_store_structure glob map rem
  | Tstr_exception(id, decl) :: rem ->
      Llet(Strict, id, transl_exception id decl,
           store_ident glob map id (transl_store_structure glob map rem))
  | Tstr_module(id, modl) :: rem ->
      Llet(Strict, id, transl_module Tcoerce_none modl,
           store_ident glob map id (transl_store_structure glob map rem))
  | Tstr_modtype(id, decl) :: rem ->
      transl_store_structure glob map rem
  | Tstr_open path :: rem ->
      transl_store_structure glob map rem

and store_ident glob map id cont =
  try
    let (pos, cc) = Ident.find_same id map in
    let init_val =
      match cc with
        Tcoerce_primitive p -> transl_primitive p
      | _ -> apply_coercion cc (Lvar id) in
    Lsequence
     (Lprim(Psetfield(pos, false), [Lprim(Pgetglobal glob, []); init_val]),
      cont)
  with Not_found ->
    cont

and store_idents glob map idlist cont =
  List.fold_right (store_ident glob map) idlist cont

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

(* Distribute a coercion over the list of value identifiers built above. *)

let distribute_coercion restr idlist =
  match restr with
    Tcoerce_none ->
      List.map (fun id -> (id, Tcoerce_none)) idlist
  | Tcoerce_structure pos_cc_list ->
      let idarray = Array.of_list idlist in
      List.map (fun (pos, cc) -> (idarray.(pos), cc)) pos_cc_list
  | _->
      fatal_error "Translmod.distribute_coercion"

(* Transform the list (id, coercion) built above into a table
   id -> (pos, coercion). *)

let rec build_ident_map pos = function
    [] -> Ident.empty
  | (id, cc) :: rem -> Ident.add id (pos, cc) (build_ident_map (pos+1) rem)

(* Compile an implementation using transl_store_structure 
   (for the native-code compiler). *)

let transl_store_implementation module_name str cc =
  primitive_declarations := [];
  let module_id = Ident.new_persistent module_name in
  let id_cc_list = distribute_coercion cc (defined_idents str) in
  let map = build_ident_map 0 id_cc_list in
  (List.length id_cc_list, transl_store_structure module_id map str)

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
