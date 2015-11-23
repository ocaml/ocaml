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

(* Keep track of the root path (from the root of the namespace to the
   currently compiled module expression).  Useful for naming extensions. *)

let global_path glob = Some(Pident glob)
let functor_path path param =
  match path with
    None -> None
  | Some p -> Some(Papply(p, Pident param))
let field_path path field =
  match path with
    None -> None
  | Some p -> Some(Pdot(p, Ident.name field, Path.nopos))

(* Compile type extensions *)

let prim_fresh_oo_id =
  Pccall (Primitive.simple ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

let transl_extension_constructor env path ext =
  let name =
    match path with
      None -> Ident.name ext.ext_id
    | Some p -> Path.name p
  in
  match ext.ext_kind with
    Text_decl(args, ret) ->
      Lprim (Pmakeblock (Obj.object_tag, Immutable),
        [Lconst (Const_base (Const_string (name, None)));
         Lprim (prim_fresh_oo_id, [Lconst (Const_base (Const_int 0))])])
  | Text_rebind(path, lid) ->
      transl_path ~loc:ext.ext_loc env path

let transl_type_extension env rootpath tyext body =
  List.fold_right
    (fun ext body ->
      let lam =
        transl_extension_constructor env (field_path rootpath ext.ext_id) ext
      in
      Llet(Strict, ext.ext_id, lam, body))
    tyext.tyext_constructors
    body

(* Compile a coercion *)

let rec apply_coercion strict restr arg =
  match restr with
    Tcoerce_none ->
      arg
  | Tcoerce_structure(pos_cc_list, id_pos_list) ->
      name_lambda strict arg (fun id ->
        let get_field pos = Lprim(Pfield pos,[Lvar id]) in
        let lam =
          Lprim(Pmakeblock(0, Immutable),
                List.map (apply_coercion_field get_field) pos_cc_list)
        in
        wrap_id_pos_list id_pos_list get_field lam)
  | Tcoerce_functor(cc_arg, cc_res) ->
      let param = Ident.create "funarg" in
      name_lambda strict arg (fun id ->
        Lfunction{kind = Curried; params = [param];
                  attr = default_function_attribute;
                  body = apply_coercion
                           Strict cc_res
                           (Lapply{ap_should_be_tailcall=false;
                                   ap_loc=Location.none;
                                   ap_func=Lvar id;
                                   ap_args=[apply_coercion Alias cc_arg (Lvar param)];
                                   ap_inlined=Default_inline})})
  | Tcoerce_primitive { pc_loc; pc_desc; pc_env; pc_type; } ->
      transl_primitive pc_loc pc_desc pc_env pc_type None
  | Tcoerce_alias (path, cc) ->
      name_lambda strict arg
        (fun id -> apply_coercion Alias cc (transl_normal_path path))

and apply_coercion_field get_field (pos, cc) =
  apply_coercion Alias cc (get_field pos)

and wrap_id_pos_list id_pos_list get_field lam =
  let fv = free_variables lam in
  (*Format.eprintf "%a@." Printlambda.lambda lam;
  IdentSet.iter (fun id -> Format.eprintf "%a " Ident.print id) fv;
  Format.eprintf "@.";*)
  let (lam,s) =
    List.fold_left (fun (lam,s) (id',pos,c) ->
      if IdentSet.mem id' fv then
        let id'' = Ident.create (Ident.name id') in
        (Llet(Alias,id'',
              apply_coercion Alias c (get_field pos),lam),
         Ident.add id' (Lvar id'') s)
      else (lam,s))
      (lam, Ident.empty) id_pos_list
  in
  if s == Ident.empty then lam else subst_lambda s lam


(* Compose two coercions
   apply_coercion c1 (apply_coercion c2 e) behaves like
   apply_coercion (compose_coercions c1 c2) e. *)

let rec compose_coercions c1 c2 =
  match (c1, c2) with
    (Tcoerce_none, c2) -> c2
  | (c1, Tcoerce_none) -> c1
  | (Tcoerce_structure (pc1, ids1), Tcoerce_structure (pc2, ids2)) ->
      let v2 = Array.of_list pc2 in
      let ids1 =
        List.map (fun (id,pos1,c1) ->
          let (pos2,c2) = v2.(pos1) in (id, pos2, compose_coercions c1 c2))
          ids1
      in
      Tcoerce_structure
        (List.map
          (function (p1, Tcoerce_primitive p) ->
                      (p1, Tcoerce_primitive p)
                  | (p1, c1) ->
                      let (p2, c2) = v2.(p1) in (p2, compose_coercions c1 c2))
             pc1,
         ids1 @ ids2)
  | (Tcoerce_functor(arg1, res1), Tcoerce_functor(arg2, res2)) ->
      Tcoerce_functor(compose_coercions arg2 arg1,
                      compose_coercions res1 res2)
  | (c1, Tcoerce_alias (path, c2)) ->
      Tcoerce_alias (path, compose_coercions c1 c2)
  | (_, _) ->
      fatal_error "Translmod.compose_coercions"

(*
let apply_coercion a b c =
  Format.eprintf "@[<2>apply_coercion@ %a@]@." Includemod.print_coercion b;
  apply_coercion a b c

let compose_coercions c1 c2 =
  let c3 = compose_coercions c1 c2 in
  let open Includemod in
  Format.eprintf "@[<2>compose_coercions@ (%a)@ (%a) =@ %a@]@."
    print_coercion c1 print_coercion c2 print_coercion c3;
  c3
*)

(* Record the primitive declarations occuring in the module compiled *)

let primitive_declarations = ref ([] : Primitive.description list)
let record_primitive = function
  | {val_kind=Val_prim p} ->
      primitive_declarations := p :: !primitive_declarations
  | _ -> ()

(* Utilities for compiling "module rec" definitions *)

let mod_prim name =
  try
    transl_normal_path
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
    | Mty_alias _ ->
        Const_block (1, [Const_pointer 0])
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
        init_shape_struct (Env.add_type ~check:false id tdecl env) rem
    | Sig_typext(id, ext, _) :: rem ->
        raise Not_found
    | Sig_module(id, md, _) :: rem ->
        init_shape_mod env md.md_type ::
        init_shape_struct (Env.add_module_declaration id md env) rem
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
  let status = Array.make num_bindings Undefined in
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
      Llet(Strict, id,
           Lapply{ap_should_be_tailcall=false;
                  ap_loc=Location.none;
                  ap_func=mod_prim "init_mod";
                  ap_args=[loc; shape];
                  ap_inlined=Default_inline},
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
      Lsequence(Lapply{ap_should_be_tailcall=false;
                       ap_loc=Location.none;
                       ap_func=mod_prim "update_mod";
                       ap_args=[shape; Lvar id; rhs];
                       ap_inlined=Default_inline},
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
   correspond to a run-time value: values, extensions, modules, classes.
   Note: manifest primitives do not correspond to a run-time value! *)

let rec bound_value_identifiers = function
    [] -> []
  | Sig_value(id, {val_kind = Val_reg}) :: rem ->
      id :: bound_value_identifiers rem
  | Sig_typext(id, ext, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_module(id, mty, _) :: rem -> id :: bound_value_identifiers rem
  | Sig_class(id, decl, _) :: rem -> id :: bound_value_identifiers rem
  | _ :: rem -> bound_value_identifiers rem


(* Code to translate class entries in a structure *)

let transl_class_bindings cl_list =
  let ids = List.map (fun (ci, _) -> ci.ci_id_class) cl_list in
  (ids,
   List.map
     (fun ({ci_id_class=id; ci_expr=cl; ci_virt=vf}, meths) ->
       (id, transl_class ids id meths cl vf))
     cl_list)

(* Compile a module expression *)

let rec transl_module cc rootpath mexp =
  match mexp.mod_type with
    Mty_alias _ -> apply_coercion Alias cc lambda_unit
  | _ ->
  match mexp.mod_desc with
    Tmod_ident (path,_) ->
      apply_coercion Strict cc
        (transl_path ~loc:mexp.mod_loc mexp.mod_env path)
  | Tmod_structure str ->
      fst (transl_struct [] cc rootpath str)
  | Tmod_functor( param, _, mty, body) ->
      let bodypath = functor_path rootpath param in
      oo_wrap mexp.mod_env true
        (function
        | Tcoerce_none ->
            Lfunction{kind = Curried; params = [param];
                      attr = default_function_attribute;
                      body = transl_module Tcoerce_none bodypath body}
        | Tcoerce_functor(ccarg, ccres) ->
            let param' = Ident.create "funarg" in
            Lfunction{kind = Curried; params = [param'];
                      attr = { default_function_attribute with
                               is_a_functor = true };
                      body = Llet(Alias, param,
                                  apply_coercion Alias ccarg (Lvar param'),
                                  transl_module ccres bodypath body)}
        | _ ->
            fatal_error "Translmod.transl_module")
        cc
  | Tmod_apply(funct, arg, ccarg) ->
      oo_wrap mexp.mod_env true
        (apply_coercion Strict cc)
        (Lapply{ap_should_be_tailcall=false;
                ap_loc=mexp.mod_loc;
                ap_func=transl_module Tcoerce_none None funct;
                ap_args=[transl_module ccarg None arg];
                ap_inlined=Default_inline})
  | Tmod_constraint(arg, mty, _, ccarg) ->
      transl_module (compose_coercions cc ccarg) rootpath arg
  | Tmod_unpack(arg, _) ->
      apply_coercion Strict cc (Translcore.transl_exp arg)

and transl_struct fields cc rootpath str =
  transl_structure fields cc rootpath str.str_items

and transl_structure fields cc rootpath = function
    [] ->
      begin match cc with
        Tcoerce_none ->
          Lprim(Pmakeblock(0, Immutable),
                List.map (fun id -> Lvar id) (List.rev fields)),
            List.length fields
      | Tcoerce_structure(pos_cc_list, id_pos_list) ->
              (* Do not ignore id_pos_list ! *)
          (*Format.eprintf "%a@.@[" Includemod.print_coercion cc;
          List.iter (fun l -> Format.eprintf "%a@ " Ident.print l)
            fields;
          Format.eprintf "@]@.";*)
          let v = Array.of_list (List.rev fields) in
          let get_field pos = Lvar v.(pos)
          and ids = List.fold_right IdentSet.add fields IdentSet.empty in
          let lam =
            (Lprim(Pmakeblock(0, Immutable),
                List.map
                  (fun (pos, cc) ->
                    match cc with
                      Tcoerce_primitive p ->
                        transl_primitive p.pc_loc
                          p.pc_desc p.pc_env p.pc_type None
                    | _ -> apply_coercion Strict cc (get_field pos))
                  pos_cc_list))
          and id_pos_list =
            List.filter (fun (id,_,_) -> not (IdentSet.mem id ids)) id_pos_list
          in
          wrap_id_pos_list id_pos_list get_field lam,
            List.length pos_cc_list
      | _ ->
          fatal_error "Translmod.transl_structure"
      end
  | item :: rem ->
      match item.str_desc with
      | Tstr_eval (expr, _) ->
        let body, size = transl_structure fields cc rootpath rem in
        Lsequence(transl_exp expr, body), size
  | Tstr_value(rec_flag, pat_expr_list) ->
      let ext_fields = rev_let_bound_idents pat_expr_list @ fields in
      let body, size = transl_structure ext_fields cc rootpath rem in
      transl_let rec_flag pat_expr_list body, size
  | Tstr_primitive descr ->
      record_primitive descr.val_val;
      transl_structure fields cc rootpath rem
  | Tstr_type(_, decls) ->
      transl_structure fields cc rootpath rem
  | Tstr_typext(tyext) ->
      let ids = List.map (fun ext -> ext.ext_id) tyext.tyext_constructors in
      let body, size =
        transl_structure (List.rev_append ids fields) cc rootpath rem
      in
      transl_type_extension item.str_env rootpath tyext body, size
  | Tstr_exception ext ->
      let id = ext.ext_id in
      let path = field_path rootpath id in
      let body, size = transl_structure (id :: fields) cc rootpath rem in
      Llet(Strict, id, transl_extension_constructor item.str_env path ext,
           body), size
  | Tstr_module mb ->
      let id = mb.mb_id in
      let body, size = transl_structure (id :: fields) cc rootpath rem in
      Llet(pure_module mb.mb_expr, id,
           transl_module Tcoerce_none (field_path rootpath id) mb.mb_expr,
           body), size
  | Tstr_recmodule bindings ->
      let ext_fields =
        List.rev_append (List.map (fun mb -> mb.mb_id) bindings) fields
      in
      let body, size = transl_structure ext_fields cc rootpath rem in
      let lam =
        compile_recmodule
          (fun id modl ->
            transl_module Tcoerce_none (field_path rootpath id) modl)
          bindings
          body
      in
      lam, size
  | Tstr_class cl_list ->
      let (ids, class_bindings) = transl_class_bindings cl_list in
      let body, size =
        transl_structure (List.rev_append ids fields) cc rootpath rem
      in
      Lletrec(class_bindings, body), size
  | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let modl = incl.incl_mod in
      let mid = Ident.create "include" in
      let rec rebind_idents pos newfields = function
        [] ->
          transl_structure newfields cc rootpath rem
      | id :: ids ->
          let body, size = rebind_idents (pos + 1) (id :: newfields) ids in
          Llet(Alias, id, Lprim(Pfield pos, [Lvar mid]), body), size
      in
      let body, size = rebind_idents 0 fields ids in
      Llet(pure_module modl, mid, transl_module Tcoerce_none None modl,
           body), size

  | Tstr_modtype _
  | Tstr_open _
  | Tstr_class_type _
  | Tstr_attribute _ ->
      transl_structure fields cc rootpath rem

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

(* Update forward declaration in Translcore *)
let _ =
  Translcore.transl_module := transl_module

(* Introduce dependencies on modules referenced only by "external". *)

let scan_used_globals lam =
  let globals = ref IdentSet.empty in
  let rec scan lam =
    Lambda.iter scan lam;
    match lam with
      Lprim ((Pgetglobal id | Psetglobal id), _) ->
        globals := IdentSet.add id !globals
    | _ -> ()
  in
  scan lam; !globals

let wrap_globals body =
  let globals = scan_used_globals body in
  let add_global id req =
    if IdentSet.mem id globals then req else IdentSet.add id req in
  let required =
    Hashtbl.fold (fun path loc -> add_global (Path.head path))
      used_primitives IdentSet.empty
  in
  let required =
    List.fold_right add_global (Env.get_required_globals ()) required
  in
  Env.reset_required_globals ();
  Hashtbl.clear used_primitives;
  IdentSet.fold
    (fun id expr -> Lsequence(Lprim(Pgetglobal id, []), expr))
    required body
  (* Location.prerr_warning loc
        (Warnings.Nonrequired_global (Ident.name (Path.head path),
                                      "uses the primitive " ^
                                      Printtyp.string_of_path path))) *)

(* Compile an implementation *)

let transl_implementation_native module_name (str, cc) =
  reset_labels ();
  primitive_declarations := [];
  Hashtbl.clear used_primitives;
  let module_id = Ident.create_persistent module_name in
  let body, size =
    transl_label_init
      (fun () -> transl_struct [] cc (global_path module_id) str)
  in
  module_id, (wrap_globals body, size)

let transl_implementation module_name (str, cc) =
  let module_id, (module_initializer, _size) =
    transl_implementation_native module_name (str, cc)
  in
  Lprim (Psetglobal module_id, [module_initializer])

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
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield toploop_getvalue_pos, [Lprim(Pgetglobal toploop_ident, [])]);
         ap_args=[Lconst(Const_base(Const_string (toplevel_name id, None)))];
         ap_inlined=Default_inline}

let toploop_setvalue id lam =
  Lapply{ap_should_be_tailcall=false;
         ap_loc=Location.none;
         ap_func=Lprim(Pfield toploop_setvalue_pos, [Lprim(Pgetglobal toploop_ident, [])]);
         ap_args=[Lconst(Const_base(Const_string (toplevel_name id, None))); lam];
         ap_inlined=Default_inline}

let toploop_setvalue_id id = toploop_setvalue id (Lvar id)

let close_toplevel_term (lam, ()) =
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
  | Tstr_typext(tyext) ->
      let idents =
        List.map (fun ext -> ext.ext_id) tyext.tyext_constructors
      in
      (* we need to use unique name in case of multiple
         definitions of the same extension constructor in the toplevel *)
      List.iter set_toplevel_unique_name idents;
        transl_type_extension item.str_env None tyext
          (make_sequence toploop_setvalue_id idents)
  | Tstr_exception ext ->
      set_toplevel_unique_name ext.ext_id;
      toploop_setvalue ext.ext_id
        (transl_extension_constructor item.str_env None ext)
  | Tstr_module {mb_id=id; mb_expr=modl} ->
      (* we need to use the unique name for the module because of issues
         with "open" (PR#1672) *)
      set_toplevel_unique_name id;
      let lam = transl_module Tcoerce_none (Some(Pident id)) modl in
      toploop_setvalue id lam
  | Tstr_recmodule bindings ->
      let idents = List.map (fun mb -> mb.mb_id) bindings in
      compile_recmodule
        (fun id modl -> transl_module Tcoerce_none (Some(Pident id)) modl)
        bindings
        (make_sequence toploop_setvalue_id idents)
  | Tstr_class cl_list ->
      (* we need to use unique names for the classes because there might
         be a value named identically *)
      let (ids, class_bindings) = transl_class_bindings cl_list in
      List.iter set_toplevel_unique_name ids;
      Lletrec(class_bindings, make_sequence toploop_setvalue_id ids)
  | Tstr_include incl ->
      let ids = bound_value_identifiers incl.incl_type in
      let modl = incl.incl_mod in
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
  close_toplevel_term
    (transl_label_init (fun () -> transl_toplevel_item itm, ()))

let transl_toplevel_definition str =
  reset_labels ();
  Hashtbl.clear used_primitives;
  make_sequence transl_toplevel_item_and_close str.str_items

(* Compile the initialization code for a packed library *)

let get_component = function
    None -> Lconst const_unit
  | Some id -> Lprim(Pgetglobal id, [])

let transl_package_native component_names target_name coercion =
  let size =
    match coercion with
    | Tcoerce_none -> List.length component_names
    | Tcoerce_structure (l, _) -> List.length l
    | Tcoerce_functor _
    | Tcoerce_primitive _
    | Tcoerce_alias _ -> assert false
  in
  size,
  apply_coercion Strict coercion
    (Lprim(Pmakeblock(0, Immutable), List.map get_component component_names))

let transl_package component_names target_name coercion =
  let components =
    Lprim(Pmakeblock(0, Immutable), List.map get_component component_names) in
  Lprim(Psetglobal target_name, [apply_coercion Strict coercion components])

(* Error report *)

open Format

let report_error ppf = function
    Circular_dependency id ->
      fprintf ppf
        "@[Cannot safely evaluate the definition@ \
         of the recursively-defined module %a@]"
        Printtyp.ident id

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )

let reset () =
  primitive_declarations := [];
  toploop_ident.Ident.flags <- 0;
  aliased_idents := Ident.empty;
  Env.reset_required_globals ();
  Hashtbl.clear used_primitives
