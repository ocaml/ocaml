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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Misc
open Asttypes
open Longident
open Path
open Types
open Typedtree
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
            (Lapply(Lvar id, [apply_coercion cc_arg (Lvar param)],
                    Location.none))))
  | Tcoerce_primitive p ->
      transl_primitive Location.none p

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

let primitive_declarations = ref ([] : Primitive.description list)
let record_primitive = function
  | {val_kind=Val_prim p} ->
      primitive_declarations := p :: !primitive_declarations
  | _ -> ()

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
  let (fname, line, char) = Location.get_pos_info loc.Location.loc_start in
  Lconst(Const_block(0,
                     [Const_base(Const_string (fname, None));
                      Const_base(Const_int line);
                      Const_base(Const_int char)]))

let init_shape modl =
  let rec init_shape_mod env mty =
    match Mtype.scrape env mty with
      Mty_ident _ ->
        raise Not_found
    | Mty_signature sg ->
        Const_block(0, [Const_block(0, init_shape_struct env sg)])
    | Mty_functor(id, arg, res) ->
        raise Not_found (* can we do better? *)
  and init_shape_struct env sg =
    match sg with
      [] -> []
    | Sig_value(id, vdesc) :: rem ->
        let init_v =
          match Ctype.expand_head env vdesc.val_type with
            {desc = Tarrow(_,_,_,_)} ->
              Const_pointer 0 (* camlinternalMod.Function *)
          | {desc = Tconstr(p, _, _)} when Path.same p Predef.path_lazy_t ->
              Const_pointer 1 (* camlinternalMod.Lazy *)
          | _ -> raise Not_found in
        init_v :: init_shape_struct env rem
    | Sig_type(id, tdecl, _) :: rem ->
        init_shape_struct (Env.add_type id tdecl env) rem
    | Sig_exception(id, edecl) :: rem ->
        raise Not_found
    | Sig_module(id, mty, _) :: rem ->
        init_shape_mod env mty ::
        init_shape_struct (Env.add_module id mty env) rem
    | Sig_modtype(id, minfo) :: rem ->
        init_shape_struct (Env.add_modtype id minfo env) rem
    | Sig_class(id, cdecl, _) :: rem ->
        Const_pointer 2 (* camlinternalMod.Class *)
        :: init_shape_struct env rem
    | Sig_class_type(id, ctyp, _) :: rem ->
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
      Llet(Strict, id, Lapply(mod_prim "init_mod", [loc; shape], Location.none),
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
      Lsequence(Lapply(mod_prim "update_mod", [shape; Lvar id; rhs],
                       Location.none),
                patch_forwards rem)
  in
    bind_inits bindings

let compile_recmodule compile_rhs bindings cont =
  eval_rec_bindings
    (reorder_rec_bindings
       (List.map
          (fun {mb_id=id; mb_expr=modl; _} ->
            (id, modl.mod_loc, init_shape modl, compile_rhs id modl))
          bindings))
    cont

(* Extract the list of "value" identifiers bound by a signature.
   "Value" identifiers are identifiers for signature components that
   correspond to a run-time value: values, exceptions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg}) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_exception(id, decl) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, mty, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_class(id, decl, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem

(* Compile a module expression *)

let rec transl_module cc rootpath mexp =
  match mexp.mod_desc with
    Tmod_ident (path,_) ->
      apply_coercion cc (transl_path path)
  | Tmod_structure str ->
      transl_struct [] cc rootpath str
  | Tmod_functor( param, _, mty, body) ->
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
                [transl_module ccarg None arg], mexp.mod_loc))
  | Tmod_constraint(arg, mty, _, ccarg) ->
      transl_module (compose_coercions cc ccarg) rootpath arg
  | Tmod_unpack(arg, _) ->
      apply_coercion cc (Translcore.transl_exp arg)

and transl_struct fields cc rootpath str =
  transl_structure fields cc rootpath str.str_items

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
                      Tcoerce_primitive p -> transl_primitive Location.none p
                    | _ -> apply_coercion cc (Lvar v.(pos)))
                  pos_cc_list)
      | _ ->
          fatal_error "Translmod.transl_structure"
      end
  | item :: rem ->
      match item.str_desc with
      | Tstr_eval (expr, _) ->
      Lsequence(transl_exp expr, transl_structure fields cc rootpath rem)
  | Tstr_value(rec_flag, pat_expr_list) ->
      let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
      transl_let rec_flag pat_expr_list
                 (transl_structure ext_fields cc rootpath rem)
  | Tstr_primitive descr ->
      record_primitive descr.val_val;
      transl_structure fields cc rootpath rem
  | Tstr_type(decls) ->
      transl_structure fields cc rootpath rem
  | Tstr_exception decl ->
      let id = decl.cd_id in
      Llet(Strict, id, transl_exception (field_path rootpath id) decl,
           transl_structure (id :: fields) cc rootpath rem)
  | Tstr_exn_rebind( id, _, path, _, _) ->
      Llet(Strict, id, transl_path path,
           transl_structure (id :: fields) cc rootpath rem)
  | Tstr_module mb ->
      let id = mb.mb_id in
      Llet(Strict, id,
           transl_module Tcoerce_none (field_path rootpath id) mb.mb_expr,
           transl_structure (id :: fields) cc rootpath rem)
  | Tstr_recmodule bindings ->
      let ext_fields =
        List.rev_append (List.map (fun mb -> mb.mb_id) bindings) fields
      in
      compile_recmodule
        (fun id modl ->
          transl_module Tcoerce_none (field_path rootpath id) modl)
        bindings
        (transl_structure ext_fields cc rootpath rem)
  | Tstr_class cl_list ->
      let ids = List.map (fun (ci,_,_) -> ci.ci_id_class) cl_list in
      Lletrec(List.map
              (fun (ci, meths, vf) ->
                let id = ci.ci_id_class in
                let cl = ci.ci_expr in
                  (id, transl_class ids id meths cl vf ))
                cl_list,
              transl_structure (List.rev ids @ fields) cc rootpath rem)
  | Tstr_include(modl, sg, _) ->
      let ids = bound_value_identifiers sg in
      let mid = Ident.create "include" in
      let rec rebind_idents pos newfields = function
        [] ->
          transl_structure newfields cc rootpath rem
      | id :: ids ->
          Llet(Alias, id, Lprim(Pfield pos, [Lvar mid]),
               rebind_idents (pos + 1) (id :: newfields) ids) in
      Llet(Strict, mid, transl_module Tcoerce_none None modl,
           rebind_idents 0 fields ids)

  | Tstr_modtype _
  | Tstr_open _
  | Tstr_class_type _
  | Tstr_attribute _ ->
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
            (transl_struct [] cc (global_path module_id) str)])


(* Build the list of value identifiers defined by a toplevel structure
   (excluding primitive declarations). *)

let rec defined_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval (expr, _) -> defined_idents rem
    | Tstr_value(rec_flag, pat_expr_list) ->
      let_bound_idents pat_expr_list @ defined_idents rem
    | Tstr_primitive desc -> defined_idents rem
    | Tstr_type decls -> defined_idents rem
    | Tstr_exception decl -> decl.cd_id :: defined_idents rem
    | Tstr_exn_rebind(id, _, path, _, _) -> id :: defined_idents rem
    | Tstr_module mb -> mb.mb_id :: defined_idents rem
    | Tstr_recmodule decls ->
      List.map (fun mb -> mb.mb_id) decls @ defined_idents rem
    | Tstr_modtype _ -> defined_idents rem
    | Tstr_open _ -> defined_idents rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _, _) -> ci.ci_id_class) cl_list @ defined_idents rem
    | Tstr_class_type cl_list -> defined_idents rem
    | Tstr_include(modl, sg, _) -> bound_value_identifiers sg @ defined_idents rem
    | Tstr_attribute _ -> []

(* second level idents (module M = struct ... let id = ... end),
   and all sub-levels idents *)
let rec more_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval (expr, _attrs) -> more_idents rem
    | Tstr_value(rec_flag, pat_expr_list) -> more_idents rem
    | Tstr_primitive _ -> more_idents rem
    | Tstr_type decls -> more_idents rem
    | Tstr_exception _ -> more_idents rem
    | Tstr_exn_rebind(id, _, path, _, _) -> more_idents rem
    | Tstr_recmodule decls -> more_idents rem
    | Tstr_modtype _ -> more_idents rem
    | Tstr_open _ -> more_idents rem
    | Tstr_class cl_list -> more_idents rem
    | Tstr_class_type cl_list -> more_idents rem
    | Tstr_include(modl, _, _) -> more_idents rem
    | Tstr_module {mb_expr={mod_desc = Tmod_structure str}} ->
        all_idents str.str_items @ more_idents rem
    | Tstr_module _ -> more_idents rem
    | Tstr_attribute _ -> []

and all_idents = function
    [] -> []
  | item :: rem ->
    match item.str_desc with
    | Tstr_eval (expr, _attrs) -> all_idents rem
    | Tstr_value(rec_flag, pat_expr_list) ->
      let_bound_idents pat_expr_list @ all_idents rem
    | Tstr_primitive _ -> all_idents rem
    | Tstr_type decls -> all_idents rem
    | Tstr_exception decl -> decl.cd_id :: all_idents rem
    | Tstr_exn_rebind(id, _, path, _, _) -> id :: all_idents rem
    | Tstr_recmodule decls ->
      List.map (fun mb -> mb.mb_id) decls @ all_idents rem
    | Tstr_modtype _ -> all_idents rem
    | Tstr_open _ -> all_idents rem
    | Tstr_class cl_list ->
      List.map (fun (ci, _, _) -> ci.ci_id_class) cl_list @ all_idents rem
    | Tstr_class_type cl_list -> all_idents rem
    | Tstr_include(modl, sg, _) -> bound_value_identifiers sg @ all_idents rem
    | Tstr_module {mb_id;mb_expr={mod_desc = Tmod_structure str}} ->
        mb_id :: all_idents str.str_items @ all_idents rem
    | Tstr_module mb -> mb.mb_id :: all_idents rem
    | Tstr_attribute _ -> []


(* A variant of transl_structure used to compile toplevel structure definitions
   for the native-code compiler. Store the defined values in the fields
   of the global as soon as they are defined, in order to reduce register
   pressure.  Also rewrites the defining expressions so that they
   refer to earlier fields of the structure through the fields of
   the global, not by their names.
   "map" is a table from defined idents to (pos in global block, coercion).
   "prim" is a list of (pos in global block, primitive declaration). *)

let transl_store_subst = ref Ident.empty
  (** In the native toplevel, this reference is threaded through successive
      calls of transl_store_structure *)

let nat_toplevel_name id =
  try match Ident.find_same id !transl_store_subst with
    | Lprim(Pfield pos, [Lprim(Pgetglobal glob, [])]) -> (glob,pos)
    | _ -> raise Not_found
  with Not_found ->
    fatal_error("Translmod.nat_toplevel_name: " ^ Ident.unique_name id)

let transl_store_structure glob map prims str =
  let rec transl_store rootpath subst = function
    [] ->
      transl_store_subst := subst;
        lambda_unit
    | item :: rem ->
        match item.str_desc with
  | Tstr_eval (expr, _attrs) ->
      Lsequence(subst_lambda subst (transl_exp expr),
                transl_store rootpath subst rem)
  | Tstr_value(rec_flag, pat_expr_list) ->
      let ids = let_bound_idents pat_expr_list in
      let lam = transl_let rec_flag pat_expr_list (store_idents ids) in
      Lsequence(subst_lambda subst lam,
                transl_store rootpath (add_idents false ids subst) rem)
  | Tstr_primitive descr ->
      record_primitive descr.val_val;
      transl_store rootpath subst rem
  | Tstr_type(decls) ->
      transl_store rootpath subst rem
  | Tstr_exception decl ->
      let id = decl.cd_id in
      let lam = transl_exception (field_path rootpath id) decl in
      Lsequence(Llet(Strict, id, lam, store_ident id),
                transl_store rootpath (add_ident false id subst) rem)
  | Tstr_exn_rebind( id, _, path, _, _) ->
      let lam = subst_lambda subst (transl_path path) in
      Lsequence(Llet(Strict, id, lam, store_ident id),
                transl_store rootpath (add_ident false id subst) rem)
  | Tstr_module{mb_id=id; mb_expr={mod_desc = Tmod_structure str}} ->
    let lam = transl_store (field_path rootpath id) subst str.str_items in
      (* Careful: see next case *)
    let subst = !transl_store_subst in
    Lsequence(lam,
              Llet(Strict, id,
                   subst_lambda subst
                   (Lprim(Pmakeblock(0, Immutable),
                          List.map (fun id -> Lvar id)
                                   (defined_idents str.str_items))),
                   Lsequence(store_ident id,
                             transl_store rootpath (add_ident true id subst)
                                          rem)))
  | Tstr_module{mb_id=id; mb_expr=modl} ->
      let lam =
        transl_module Tcoerce_none (field_path rootpath id) modl in
      (* Careful: the module value stored in the global may be different
         from the local module value, in case a coercion is applied.
         If so, keep using the local module value (id) in the remainder of
         the compilation unit (add_ident true returns subst unchanged).
         If not, we can use the value from the global
         (add_ident true adds id -> Pgetglobal... to subst). *)
      Llet(Strict, id, subst_lambda subst lam,
        Lsequence(store_ident id,
                  transl_store rootpath (add_ident true id subst) rem))
  | Tstr_recmodule bindings ->
      let ids = List.map (fun mb -> mb.mb_id) bindings in
      compile_recmodule
        (fun id modl ->
          subst_lambda subst
            (transl_module Tcoerce_none
                           (field_path rootpath id) modl))
        bindings
        (Lsequence(store_idents ids,
                   transl_store rootpath (add_idents true ids subst) rem))
  | Tstr_class cl_list ->
      let ids = List.map (fun (ci, _, _) -> ci.ci_id_class) cl_list in
      let lam =
        Lletrec(List.map
              (fun (ci, meths, vf) ->
                let id = ci.ci_id_class in
                let cl = ci.ci_expr in
                     (id, transl_class ids id meths cl vf))
                  cl_list,
                store_idents ids) in
      Lsequence(subst_lambda subst lam,
                transl_store rootpath (add_idents false ids subst) rem)
  | Tstr_include(modl, sg, _attrs) ->
      let ids = bound_value_identifiers sg in
      let mid = Ident.create "include" in
      let rec store_idents pos = function
        [] -> transl_store rootpath (add_idents true ids subst) rem
      | id :: idl ->
          Llet(Alias, id, Lprim(Pfield pos, [Lvar mid]),
               Lsequence(store_ident id, store_idents (pos + 1) idl)) in
      Llet(Strict, mid,
           subst_lambda subst (transl_module Tcoerce_none None modl),
           store_idents 0 ids)
  | Tstr_modtype _
  | Tstr_open _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      transl_store rootpath subst rem

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
                    [Lprim(Pgetglobal glob, []);
                     transl_primitive Location.none prim]),
              cont)

  in List.fold_right store_primitive prims
                     (transl_store (global_path glob) !transl_store_subst str)

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

let build_ident_map restr idlist more_ids =
  let rec natural_map pos map prims = function
    [] ->
      (map, prims, pos)
  | id :: rem ->
      natural_map (pos+1) (Ident.add id (pos, Tcoerce_none) map) prims rem in
  let (map, prims, pos) =
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
  in
  natural_map pos map prims more_ids

(* Compile an implementation using transl_store_structure
   (for the native-code compiler). *)

let transl_store_gen module_name ({ str_items = str }, restr) topl =
  reset_labels ();
  primitive_declarations := [];
  let module_id = Ident.create_persistent module_name in
  let (map, prims, size) =
    build_ident_map restr (defined_idents str) (more_idents str) in
  let f = function
    | [ { str_desc = Tstr_eval (expr, _attrs) } ] when topl ->
        assert (size = 0);
        subst_lambda !transl_store_subst (transl_exp expr)
    | str -> transl_store_structure module_id map prims str in
  transl_store_label_init module_id size f str
  (*size, transl_label_init (transl_store_structure module_id map prims str)*)

let transl_store_phrases module_name str =
  transl_store_gen module_name (str,Tcoerce_none) true

let transl_store_implementation module_name (str, restr) =
  let s = !transl_store_subst in
  transl_store_subst := Ident.empty;
  let r = transl_store_gen module_name (str, restr) false in
  transl_store_subst := s;
  r

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
         [Lconst(Const_base(Const_string (toplevel_name id, None)))],
         Location.none)

let toploop_setvalue id lam =
  Lapply(Lprim(Pfield toploop_setvalue_pos,
                 [Lprim(Pgetglobal toploop_ident, [])]),
         [Lconst(Const_base(Const_string (toplevel_name id, None))); lam],
         Location.none)

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let close_toplevel_term lam =
  IdentSet.fold (fun id l -> Llet(Strict, id, toploop_getvalue id, l))
                (free_variables lam) lam

let transl_toplevel_item item =
  match item.str_desc with
    Tstr_eval (expr, _attrs) ->
      transl_exp expr
  | Tstr_value(rec_flag, pat_expr_list) ->
      let idents = let_bound_idents pat_expr_list in
      transl_let rec_flag pat_expr_list
                 (make_sequence toploop_setvalue_id idents)
  | Tstr_exception decl ->
      toploop_setvalue decl.cd_id (transl_exception None decl)
  | Tstr_exn_rebind(id, _, path, _, _) ->
      toploop_setvalue id (transl_path path)
  | Tstr_module {mb_id=id; mb_expr=modl} ->
      (* we need to use the unique name for the module because of issues
         with "open" (PR#1672) *)
      set_toplevel_unique_name id;
      toploop_setvalue id
                        (transl_module Tcoerce_none (Some(Pident id)) modl)
  | Tstr_recmodule bindings ->
      let idents = List.map (fun mb -> mb.mb_id) bindings in
      compile_recmodule
        (fun id modl -> transl_module Tcoerce_none (Some(Pident id)) modl)
        bindings
        (make_sequence toploop_setvalue_id idents)
  | Tstr_class cl_list ->
      (* we need to use unique names for the classes because there might
         be a value named identically *)
      let ids = List.map (fun (ci, _, _) -> ci.ci_id_class) cl_list in
      List.iter set_toplevel_unique_name ids;
      Lletrec(List.map
          (fun (ci, meths, vf) ->
            let id = ci.ci_id_class in
            let cl = ci.ci_expr in
                   (id, transl_class ids id meths cl vf))
                cl_list,
              make_sequence
                (fun (ci, _, _) -> toploop_setvalue_id ci.ci_id_class)
                cl_list)
  | Tstr_include(modl, sg, _attrs) ->
      let ids = bound_value_identifiers sg in
      let mid = Ident.create "include" in
      let rec set_idents pos = function
        [] ->
          lambda_unit
      | id :: ids ->
          Lsequence(toploop_setvalue id (Lprim(Pfield pos, [Lvar mid])),
                    set_idents (pos + 1) ids) in
      Llet(Strict, mid, transl_module Tcoerce_none None modl, set_idents 0 ids)
  | Tstr_modtype _
  | Tstr_open _
  | Tstr_primitive _
  | Tstr_type _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      lambda_unit

let transl_toplevel_item_and_close itm =
  close_toplevel_term (transl_label_init (transl_toplevel_item itm))

let transl_toplevel_definition str =
  reset_labels ();
  make_sequence transl_toplevel_item_and_close str.str_items

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
        "@[Cannot safely evaluate the definition@ \
         of the recursively-defined module %a@]"
        Printtyp.ident id
