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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Misc
open Asttypes
open Longident
open Path
open Types
open Typedtree
open Primitive
open Lambda
open Translobj
open Translcore
open Translclass

type error =
  Circular_dependency of Ident.t

exception Error of Location.t * error

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

(* Utilities for compiling "module rec" definitions *)

let mod_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "CamlinternalMod", name))
                             Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

let undefined_location loc =
  (* Confer Translcore.assert_failed *)
  let fname = match loc.Location.loc_start.Lexing.pos_fname with
              | "" -> !Location.input_name
              | x -> x in
  let pos = loc.Location.loc_start in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Lconst(Const_block(0,
                     [Const_base(Const_string fname);
                      Const_base(Const_int line);
                      Const_base(Const_int char)]))

let init_shape modl =
  let rec init_shape_mod env mty =
    match Mtype.scrape env mty with
      Tmty_ident _ ->
        raise Not_found
    | Tmty_signature sg ->
        Const_block(0, [Const_block(0, init_shape_struct env sg)])
    | Tmty_functor(id, arg, res) ->
        raise Not_found (* can we do better? *)
  and init_shape_struct env sg =
    match sg with
      [] -> []
    | Tsig_value(id, vdesc) :: rem ->
        let init_v =
          match Ctype.expand_head env vdesc.val_type with
            {desc = Tarrow(_,_,_,_)} ->
              Const_pointer 0 (* camlinternalMod.Function *)
          | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_lazy_t ->
              Const_pointer 1 (* camlinternalMod.Lazy *)
          | _ -> raise Not_found in
        init_v :: init_shape_struct env rem
    | Tsig_type(id, tdecl, _) :: rem ->
        init_shape_struct (Env.add_type id tdecl env) rem
    | Tsig_exception(id, edecl) :: rem ->
        raise Not_found
    | Tsig_module(id, mty, _) :: rem ->
        init_shape_mod env mty ::
        init_shape_struct (Env.add_module id mty env) rem
    | Tsig_modtype(id, minfo) :: rem ->
        init_shape_struct (Env.add_modtype id minfo env) rem
    | Tsig_class(id, cdecl, _) :: rem ->
        Const_pointer 2 (* camlinternalMod.Class *)
        :: init_shape_struct env rem
    | Tsig_cltype(id, ctyp, _) :: rem ->
        init_shape_struct env rem
  in
  try
    Some(undefined_location modl.mod_loc,
         Lconst(init_shape_mod modl.mod_env modl.mod_type))
  with Not_found ->
    None

(* Reorder bindings to honor dependencies.  *)

type binding_status = Undefined | Inprogress | Defined

let reorder_rec_bindings bindings =
  let id = Array.of_list (List.map (fun (id,_,_,_) -> id) bindings)
  and loc = Array.of_list (List.map (fun (_,loc,_,_) -> loc) bindings)
  and init = Array.of_list (List.map (fun (_,_,init,_) -> init) bindings)
  and rhs = Array.of_list (List.map (fun (_,_,_,rhs) -> rhs) bindings) in
  let fv = Array.map Lambda.free_variables rhs in
  let num_bindings = Array.length id in
  let status = Array.create num_bindings Undefined in
  let res = ref [] in
  let rec emit_binding i =
    match status.(i) with
      Defined -> ()
    | Inprogress -> raise(Error(loc.(i), Circular_dependency id.(i)))
    | Undefined ->
        if init.(i) = None then begin
          status.(i) <- Inprogress;
          for j = 0 to num_bindings - 1 do
            if IdentSet.mem id.(j) fv.(i) then emit_binding j
          done
        end;
        res := (id.(i), init.(i), rhs.(i)) :: !res;
        status.(i) <- Defined in
  for i = 0 to num_bindings - 1 do
    match status.(i) with
      Undefined -> emit_binding i
    | Inprogress -> assert false
    | Defined -> ()
  done;
  List.rev !res

(* Generate lambda-code for a reordered list of bindings *)

let eval_rec_bindings bindings cont =
  let rec bind_inits = function
    [] ->
      bind_strict bindings
  | (id, None, rhs) :: rem ->
      bind_inits rem
  | (id, Some(loc, shape), rhs) :: rem ->
      Llet(Strict, id, Lapply(mod_prim "init_mod", [loc; shape]),
           bind_inits rem)
  and bind_strict = function
    [] ->
      patch_forwards bindings
  | (id, None, rhs) :: rem ->
      Llet(Strict, id, rhs, bind_strict rem)
  | (id, Some(loc, shape), rhs) :: rem ->
      bind_strict rem
  and patch_forwards = function
    [] ->
      cont
  | (id, None, rhs) :: rem ->
      patch_forwards rem
  | (id, Some(loc, shape), rhs) :: rem ->
      Lsequence(Lapply(mod_prim "update_mod", [shape; Lvar id; rhs]),
                patch_forwards rem)
  in
    bind_inits bindings

let compile_recmodule compile_rhs bindings cont =
  eval_rec_bindings
    (reorder_rec_bindings
      (List.map
        (fun (id, modl) ->
                  (id, modl.mod_loc, init_shape modl, compile_rhs id modl))
        bindings))
    cont

(* Compile a module expression *)

let rec transl_module cc rootpath mexp =
  match mexp.mod_desc with
    Tmod_ident path ->
      apply_coercion cc (transl_path path)
  | Tmod_structure str ->
      transl_structure [] cc rootpath str
  | Tmod_functor(param, mty, body) ->
      let bodypath = functor_path rootpath param in
      oo_wrap mexp.mod_env true
        (function
        | Tcoerce_none ->
            Lfunction(Curried, [param],
                      transl_module Tcoerce_none bodypath body)
        | Tcoerce_functor(ccarg, ccres) ->
            let param' = Ident.create "funarg" in
            Lfunction(Curried, [param'],
                      Llet(Alias, param, apply_coercion ccarg (Lvar param'),
                           transl_module ccres bodypath body))
        | _ ->
            fatal_error "Translmod.transl_module")
        cc
  | Tmod_apply(funct, arg, ccarg) ->
      oo_wrap mexp.mod_env true
        (apply_coercion cc)
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
  | Tstr_exn_rebind(id, path) :: rem ->
      Llet(Strict, id, transl_path path,
           transl_structure (id :: fields) cc rootpath rem)
  | Tstr_module(id, modl) :: rem ->
      Llet(Strict, id,
           transl_module Tcoerce_none (field_path rootpath id) modl,
           transl_structure (id :: fields) cc rootpath rem)
  | Tstr_recmodule bindings :: rem ->
      let ext_fields = List.rev_append (List.map fst bindings) fields in
      compile_recmodule
        (fun id modl ->
          transl_module Tcoerce_none (field_path rootpath id) modl)
        bindings
        (transl_structure ext_fields cc rootpath rem)
  | Tstr_modtype(id, decl) :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_open path :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_class cl_list :: rem ->
      let ids = List.map (fun (i, _, _, _, _) -> i) cl_list in
      Lletrec(List.map
                (fun (id, arity, meths, cl, vf) ->
                  (id, transl_class ids id arity meths cl vf))
                cl_list,
              transl_structure (List.rev ids @ fields) cc rootpath rem)
  | Tstr_cltype cl_list :: rem ->
      transl_structure fields cc rootpath rem
  | Tstr_include(modl, ids) :: rem ->
      let mid = Ident.create "include" in
      let rec rebind_idents pos newfields = function
        [] ->
          transl_structure newfields cc rootpath rem
      | id :: ids ->
          Llet(Alias, id, Lprim(Pfield pos, [Lvar mid]),
               rebind_idents (pos + 1) (id :: newfields) ids) in
      Llet(Strict, mid, transl_module Tcoerce_none None modl,
           rebind_idents 0 fields ids)

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
                transl_store (add_idents false ids subst) rem)
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
                transl_store (add_ident false id subst) rem)
  | Tstr_exn_rebind(id, path) :: rem ->
      let lam = subst_lambda subst (transl_path path) in
      Lsequence(Llet(Strict, id, lam, store_ident id),
                transl_store (add_ident false id subst) rem)
  | Tstr_module(id, modl) :: rem ->
      let lam =
        transl_module Tcoerce_none (field_path (global_path glob) id) modl in
      (* Careful: the module value stored in the global may be different
         from the local module value, in case a coercion is applied.
         If so, keep using the local module value (id) in the remainder of
         the compilation unit (add_ident true returns subst unchanged).
         If not, we can use the value from the global
         (add_ident true adds id -> Pgetglobal... to subst). *)
      Llet(Strict, id, subst_lambda subst lam,
        Lsequence(store_ident id, transl_store(add_ident true id subst) rem))
  | Tstr_recmodule bindings :: rem ->
      let ids = List.map fst bindings in
      compile_recmodule
        (fun id modl ->
          subst_lambda subst
            (transl_module Tcoerce_none
                           (field_path (global_path glob) id) modl))
        bindings
        (Lsequence(store_idents ids,
                   transl_store (add_idents true ids subst) rem))
  | Tstr_modtype(id, decl) :: rem ->
      transl_store subst rem
  | Tstr_open path :: rem ->
      transl_store subst rem
  | Tstr_class cl_list :: rem ->
      let ids = List.map (fun (i, _, _, _, _) -> i) cl_list in
      let lam =
        Lletrec(List.map
                  (fun (id, arity, meths, cl, vf) ->
                     (id, transl_class ids id arity meths cl vf))
                  cl_list,
                store_idents ids) in
      Lsequence(subst_lambda subst lam,
                transl_store (add_idents false ids subst) rem)
  | Tstr_cltype cl_list :: rem ->
      transl_store subst rem
  | Tstr_include(modl, ids) :: rem ->
      let mid = Ident.create "include" in
      let rec store_idents pos = function
        [] -> transl_store (add_idents true ids subst) rem
      | id :: idl ->
          Llet(Alias, id, Lprim(Pfield pos, [Lvar mid]),
               Lsequence(store_ident id, store_idents (pos + 1) idl)) in
      Llet(Strict, mid,
           subst_lambda subst (transl_module Tcoerce_none None modl),
           store_idents 0 ids)

  and store_ident id =
    try
      let (pos, cc) = Ident.find_same id map in
      let init_val = apply_coercion cc (Lvar id) in
      Lprim(Psetfield(pos, false), [Lprim(Pgetglobal glob, []); init_val])
    with Not_found ->
      fatal_error("Translmod.store_ident: " ^ Ident.unique_name id)

  and store_idents idlist =
    make_sequence store_ident idlist

  and add_ident may_coerce id subst =
    try
      let (pos, cc) = Ident.find_same id map in
      match cc with
        Tcoerce_none ->
          Ident.add id (Lprim(Pfield pos, [Lprim(Pgetglobal glob, [])])) subst
      | _ ->
          if may_coerce then subst else assert false
    with Not_found ->
      assert false

  and add_idents may_coerce idlist subst =
    List.fold_right (add_ident may_coerce) idlist subst

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
  | Tstr_exn_rebind(id, path) :: rem -> id :: defined_idents rem
  | Tstr_module(id, modl) :: rem -> id :: defined_idents rem
  | Tstr_recmodule decls :: rem -> List.map fst decls @ defined_idents rem
  | Tstr_modtype(id, decl) :: rem -> defined_idents rem
  | Tstr_open path :: rem -> defined_idents rem
  | Tstr_class cl_list :: rem ->
      List.map (fun (i, _, _, _, _) -> i) cl_list @ defined_idents rem
  | Tstr_cltype cl_list :: rem -> defined_idents rem
  | Tstr_include(modl, ids) :: rem -> ids @ defined_idents rem

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
  transl_store_label_init module_id size
    (transl_store_structure module_id map prims) str
  (*size, transl_label_init (transl_store_structure module_id map prims str)*)

(* Compile a toplevel phrase *)

let toploop_ident = Ident.create_persistent "Toploop"
let toploop_getvalue_pos = 0 (* position of getvalue in module Toploop *)
let toploop_setvalue_pos = 1 (* position of setvalue in module Toploop *)

let aliased_idents = ref Ident.empty

let set_toplevel_unique_name id =
  aliased_idents :=
    Ident.add id (Ident.unique_toplevel_name id) !aliased_idents

let toplevel_name id =
  try Ident.find_same id !aliased_idents
  with Not_found -> Ident.name id

let toploop_getvalue id =
  Lapply(Lprim(Pfield toploop_getvalue_pos,
                 [Lprim(Pgetglobal toploop_ident, [])]),
         [Lconst(Const_base(Const_string (toplevel_name id)))])

let toploop_setvalue id lam =
  Lapply(Lprim(Pfield toploop_setvalue_pos,
                 [Lprim(Pgetglobal toploop_ident, [])]),
         [Lconst(Const_base(Const_string (toplevel_name id))); lam])

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let close_toplevel_term lam =
  IdentSet.fold (fun id l -> Llet(Strict, id, toploop_getvalue id, l))
                (free_variables lam) lam

let transl_toplevel_item = function
    Tstr_eval expr ->
      transl_exp expr
  | Tstr_value(rec_flag, pat_expr_list) ->
      let idents = let_bound_idents pat_expr_list in
      transl_let rec_flag pat_expr_list
                 (make_sequence toploop_setvalue_id idents)
  | Tstr_primitive(id, descr) ->
      lambda_unit
  | Tstr_type(decls) ->
      lambda_unit
  | Tstr_exception(id, decl) ->
      toploop_setvalue id (transl_exception id None decl)
  | Tstr_exn_rebind(id, path) ->
      toploop_setvalue id (transl_path path)
  | Tstr_module(id, modl) ->
      (* we need to use the unique name for the module because of issues
         with "open" (PR#1672) *)
      set_toplevel_unique_name id;
      toploop_setvalue id
                        (transl_module Tcoerce_none (Some(Pident id)) modl)
  | Tstr_recmodule bindings ->
      let idents = List.map fst bindings in
      compile_recmodule
        (fun id modl -> transl_module Tcoerce_none (Some(Pident id)) modl)
        bindings
        (make_sequence toploop_setvalue_id idents)
  | Tstr_modtype(id, decl) ->
      lambda_unit
  | Tstr_open path ->
      lambda_unit
  | Tstr_class cl_list ->
      (* we need to use unique names for the classes because there might
         be a value named identically *)
      let ids = List.map (fun (i, _, _, _, _) -> i) cl_list in
      List.iter set_toplevel_unique_name ids;
      Lletrec(List.map
                (fun (id, arity, meths, cl, vf) ->
                   (id, transl_class ids id arity meths cl vf))
                cl_list,
              make_sequence
                (fun (id, _, _, _, _) -> toploop_setvalue_id id)
                cl_list)
  | Tstr_cltype cl_list ->
      lambda_unit
  | Tstr_include(modl, ids) ->
      let mid = Ident.create "include" in
      let rec set_idents pos = function
        [] ->
          lambda_unit
      | id :: ids ->
          Lsequence(toploop_setvalue id (Lprim(Pfield pos, [Lvar mid])),
                    set_idents (pos + 1) ids) in
      Llet(Strict, mid, transl_module Tcoerce_none None modl, set_idents 0 ids)

let transl_toplevel_item_and_close itm =
  close_toplevel_term (transl_label_init (transl_toplevel_item itm))

let transl_toplevel_definition str =
  reset_labels ();
  make_sequence transl_toplevel_item_and_close str

(* Compile the initialization code for a packed library *)

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, []) 

let transl_package component_names target_name coercion =
  let components =
    match coercion with
      Tcoerce_none ->
        List.map get_component component_names
    | Tcoerce_structure pos_cc_list ->
        let g = Array.of_list component_names in
        List.map
          (fun (pos, cc) -> apply_coercion cc (get_component g.(pos)))
          pos_cc_list
    | _ ->
        assert false in
  Lprim(Psetglobal target_name, [Lprim(Pmakeblock(0, Immutable), components)])

let transl_store_package component_names target_name coercion =
  let rec make_sequence fn pos arg =
    match arg with
      [] -> lambda_unit
    | hd :: tl -> Lsequence(fn pos hd, make_sequence fn (pos + 1) tl) in
  match coercion with
    Tcoerce_none ->
      (List.length component_names,
       make_sequence
         (fun pos id ->
           Lprim(Psetfield(pos, false),
                 [Lprim(Pgetglobal target_name, []);
                  get_component id]))
         0 component_names)
  | Tcoerce_structure pos_cc_list ->
      let id = Array.of_list component_names in
      (List.length pos_cc_list,
       make_sequence
         (fun dst (src, cc) ->
           Lprim(Psetfield(dst, false),
                 [Lprim(Pgetglobal target_name, []);
                  apply_coercion cc (get_component id.(src))]))
         0 pos_cc_list)
  | _ -> assert false

(* Error report *)

open Format

let report_error ppf = function
    Circular_dependency id ->
      fprintf ppf
        "@[Cannot safely evaluate the definition@ of the recursively-defined module %a@]"
        Printtyp.ident id
