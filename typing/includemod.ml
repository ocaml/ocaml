(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Inclusion checks for the module language *)

open Misc
open Typedtree
open Types

type symptom =
    Missing_field of Ident.t * Location.t * string (* kind *)
  | Value_descriptions of Ident.t * value_description * value_description
  | Type_declarations of Ident.t * type_declaration
        * type_declaration * Includecore.type_mismatch
  | Extension_constructors of Ident.t * extension_constructor
        * extension_constructor * Includecore.extension_constructor_mismatch
  | Module_types of module_type * module_type
  | Modtype_infos of Ident.t * modtype_declaration * modtype_declaration
  | Modtype_permutation of Types.module_type * Typedtree.module_coercion
  | Interface_mismatch of string * string
  | Class_type_declarations of
      Ident.t * class_type_declaration * class_type_declaration *
      Ctype.class_match_failure list
  | Class_declarations of
      Ident.t * class_declaration * class_declaration *
      Ctype.class_match_failure list
  | Unbound_modtype_path of Path.t
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of functor_parameter
  | Body of functor_parameter
type error = pos list * Env.t * symptom

exception Error of error list
exception Apply_error of Location.t * Path.t * Path.t * error list

type mark =
  | Mark_both
  | Mark_positive
  | Mark_negative
  | Mark_neither

let negate_mark = function
  | Mark_both -> Mark_both
  | Mark_positive -> Mark_negative
  | Mark_negative -> Mark_positive
  | Mark_neither -> Mark_neither

let mark_positive = function
  | Mark_both | Mark_positive -> true
  | Mark_negative | Mark_neither -> false

(* All functions "blah env x1 x2" check that x1 is included in x2,
   i.e. that x1 is the type of an implementation that fulfills the
   specification x2. If not, Error is raised with a backtrace of the error. *)

(* Inclusion between value descriptions *)

let value_descriptions ~loc env ~mark cxt subst id vd1 vd2 =
  Cmt_format.record_value_dependency vd1 vd2;
  if mark_positive mark then
    Env.mark_value_used vd1.val_uid;
  let vd2 = Subst.value_description subst vd2 in
  try
    Includecore.value_descriptions ~loc env (Ident.name id) vd1 vd2
  with Includecore.Dont_match ->
    raise(Error[cxt, env, Value_descriptions(id, vd1, vd2)])

(* Inclusion between type declarations *)

let type_declarations ~loc env ~mark ?old_env:_ cxt subst id decl1 decl2 =
  let mark = mark_positive mark in
  if mark then
    Env.mark_type_used decl1.type_uid;
  let decl2 = Subst.type_declaration subst decl2 in
  match
    Includecore.type_declarations ~loc env ~mark
      (Ident.name id) decl1 (Path.Pident id) decl2
  with
  | None -> ()
  | Some err ->
      raise(Error[cxt, env, Type_declarations(id, decl1, decl2, err)])

(* Inclusion between extension constructors *)

let extension_constructors ~loc env ~mark cxt subst id ext1 ext2 =
  let mark = mark_positive mark in
  let ext2 = Subst.extension_constructor subst ext2 in
  match Includecore.extension_constructors ~loc env ~mark id ext1 ext2 with
  | None -> ()
  | Some err ->
      raise(Error[cxt, env, Extension_constructors(id, ext1, ext2, err)])

(* Inclusion between class declarations *)

let class_type_declarations ~loc ~old_env:_ env cxt subst id decl1 decl2 =
  let decl2 = Subst.cltype_declaration subst decl2 in
  match Includeclass.class_type_declarations ~loc env decl1 decl2 with
    []     -> ()
  | reason ->
      raise(Error[cxt, env,
                  Class_type_declarations(id, decl1, decl2, reason)])

let class_declarations ~old_env:_ env cxt subst id decl1 decl2 =
  let decl2 = Subst.class_declaration subst decl2 in
  match Includeclass.class_declarations env decl1 decl2 with
    []     -> ()
  | reason ->
      raise(Error[cxt, env, Class_declarations(id, decl1, decl2, reason)])

(* Expand a module type identifier when possible *)

exception Dont_match

let try_expand_modtype_path env path =
  try
    Env.find_modtype_expansion path env
  with Not_found -> raise Dont_match

let expand_module_alias env cxt path =
  try (Env.find_module path env).md_type
  with Not_found ->
    raise(Error[cxt, env, Unbound_module_path path])

(* Extract name, kind and ident from a signature item *)

type field_desc =
    Field_value of string
  | Field_type of string
  | Field_exception of string
  | Field_typext of string
  | Field_module of string
  | Field_modtype of string
  | Field_class of string
  | Field_classtype of string

let kind_of_field_desc = function
  | Field_value _ -> "value"
  | Field_type _ -> "type"
  | Field_exception _ -> "exception"
  | Field_typext _ -> "extension constructor"
  | Field_module _ -> "module"
  | Field_modtype _ -> "module type"
  | Field_class _ -> "class"
  | Field_classtype _ -> "class type"

(** Map indexed by both field types and names.
    This avoids name clashes between different sorts of fields
    such as values and types. *)
module FieldMap = Map.Make(struct
    type t = field_desc
    let compare = Stdlib.compare
  end)

let item_ident_name = function
    Sig_value(id, d, _) -> (id, d.val_loc, Field_value(Ident.name id))
  | Sig_type(id, d, _, _) -> (id, d.type_loc, Field_type(Ident.name id))
  | Sig_typext(id, d, _, _) ->
     let kind =
       if Path.same d.ext_type_path Predef.path_exn
       then Field_exception(Ident.name id)
       else Field_typext(Ident.name id)
     in
     (id, d.ext_loc, kind)
  | Sig_module(id, _, d, _, _) -> (id, d.md_loc, Field_module(Ident.name id))
  | Sig_modtype(id, d, _) -> (id, d.mtd_loc, Field_modtype(Ident.name id))
  | Sig_class(id, d, _, _) -> (id, d.cty_loc, Field_class(Ident.name id))
  | Sig_class_type(id, d, _, _) ->
      (id, d.clty_loc, Field_classtype(Ident.name id))

let is_runtime_component = function
  | Sig_value(_,{val_kind = Val_prim _}, _)
  | Sig_type(_,_,_,_)
  | Sig_module(_,Mp_absent,_,_,_)
  | Sig_modtype(_,_,_)
  | Sig_class_type(_,_,_,_) -> false
  | Sig_value(_,_,_)
  | Sig_typext(_,_,_,_)
  | Sig_module(_,Mp_present,_,_,_)
  | Sig_class(_,_,_,_) -> true

(* Print a coercion *)

let rec print_list pr ppf = function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; Format.fprintf ppf ";@ "; print_list pr ppf l
let print_list pr ppf l =
  Format.fprintf ppf "[@[%a@]]" (print_list pr) l

let rec print_coercion ppf c =
  let pr fmt = Format.fprintf ppf fmt in
  match c with
    Tcoerce_none -> pr "id"
  | Tcoerce_structure (fl, nl) ->
      pr "@[<2>struct@ %a@ %a@]"
        (print_list print_coercion2) fl
        (print_list print_coercion3) nl
  | Tcoerce_functor (inp, out) ->
      pr "@[<2>functor@ (%a)@ (%a)@]"
        print_coercion inp
        print_coercion out
  | Tcoerce_primitive {pc_desc; pc_env = _; pc_type}  ->
      pr "prim %s@ (%a)" pc_desc.Primitive.prim_name
        Printtyp.raw_type_expr pc_type
  | Tcoerce_alias (_, p, c) ->
      pr "@[<2>alias %a@ (%a)@]"
        Printtyp.path p
        print_coercion c
and print_coercion2 ppf (n, c) =
  Format.fprintf ppf "@[%d,@ %a@]" n print_coercion c
and print_coercion3 ppf (i, n, c) =
  Format.fprintf ppf "@[%s, %d,@ %a@]"
    (Ident.unique_name i) n print_coercion c

(* Simplify a structure coercion *)

let equal_module_paths env p1 subst p2 =
  Path.same p1 p2
  || Path.same (Env.normalize_module_path None env p1)
       (Env.normalize_module_path None env
          (Subst.module_path subst p2))

let equal_modtype_paths env p1 subst p2 =
  Path.same p1 p2
  || Path.same (Env.normalize_modtype_path env p1)
       (Env.normalize_modtype_path env
          (Subst.modtype_path subst p2))

let simplify_structure_coercion cc id_pos_list =
  let rec is_identity_coercion pos = function
  | [] ->
      true
  | (n, c) :: rem ->
      n = pos && c = Tcoerce_none && is_identity_coercion (pos + 1) rem in
  if is_identity_coercion 0 cc
  then Tcoerce_none
  else Tcoerce_structure (cc, id_pos_list)

(* Inclusion between module types.
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

let rec modtypes ~loc env ~mark cxt subst mty1 mty2 =
  try
    try_modtypes ~loc env ~mark cxt subst mty1 mty2
  with
    Dont_match ->
      raise(Error[cxt, env,
                  Module_types(mty1, Subst.modtype Make_local subst mty2)])
  | Error reasons as err ->
      match mty1, mty2 with
        Mty_alias _, _
      | _, Mty_alias _ -> raise err
      | _ ->
          raise(Error((cxt, env,
                       Module_types(mty1, Subst.modtype Make_local subst mty2))
                      :: reasons))

and try_modtypes ~loc env ~mark cxt subst mty1 mty2 =
  match mty1, mty2 with
  | (Mty_alias p1, Mty_alias p2) ->
      if Env.is_functor_arg p2 env then
        raise (Error[cxt, env, Invalid_module_alias p2]);
      if not (equal_module_paths env p1 subst p2) then
        raise Dont_match;
      Tcoerce_none
  | (Mty_alias p1, _) ->
      let p1 = try
        Env.normalize_module_path (Some Location.none) env p1
      with Env.Error (Env.Missing_module (_, _, path)) ->
        raise (Error[cxt, env, Unbound_module_path path])
      in
      let mty1 = expand_module_alias env cxt p1 in
      strengthened_modtypes ~loc ~aliasable:true env ~mark cxt
        subst mty1 p1 mty2
  | (Mty_ident p1, Mty_ident p2) ->
      let p1 = Env.normalize_modtype_path env p1 in
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      if Path.same p1 p2 then Tcoerce_none
      else
        try_modtypes ~loc env ~mark cxt subst
          (try_expand_modtype_path env p1)
          (try_expand_modtype_path env p2)
  | (Mty_ident p1, _) ->
      let p1 = Env.normalize_modtype_path env p1 in
      try_modtypes ~loc env ~mark cxt subst
        (try_expand_modtype_path env p1) mty2
  | (_, Mty_ident p2) ->
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      try_modtypes ~loc env ~mark cxt subst mty1
        (try_expand_modtype_path env p2)
  | (Mty_signature sig1, Mty_signature sig2) ->
      signatures ~loc env ~mark cxt subst sig1 sig2
  | (Mty_functor(Unit, res1), Mty_functor(Unit, res2)) ->
    begin
      match modtypes ~loc env ~mark (Body Unit::cxt) subst res1 res2 with
      | Tcoerce_none -> Tcoerce_none
      | cc -> Tcoerce_functor (Tcoerce_none, cc)
    end
  | (Mty_functor(Named (param1, arg1) as arg, res1),
     Mty_functor(Named (param2, arg2), res2)) ->
      let arg2' = Subst.modtype Keep subst arg2 in
      let cc_arg =
        modtypes ~loc env ~mark:(negate_mark mark)
          (Arg arg::cxt) Subst.identity arg2' arg1
      in
      let env, subst =
        match param1, param2 with
        | Some p1, Some p2 ->
            Env.add_module p1 Mp_present arg2' env,
            Subst.add_module p2 (Path.Pident p1) subst
        | None, Some p2 ->
            Env.add_module p2 Mp_present arg2' env, subst
        | Some p1, None ->
            Env.add_module p1 Mp_present arg2' env, subst
        | None, None ->
            env, subst
      in
      let cc_res = modtypes ~loc env ~mark (Body arg::cxt) subst res1 res2 in
      begin match (cc_arg, cc_res) with
          (Tcoerce_none, Tcoerce_none) -> Tcoerce_none
        | _ -> Tcoerce_functor(cc_arg, cc_res)
      end
  | (_, _) ->
      raise Dont_match

and strengthened_modtypes ~loc ~aliasable env ~mark cxt subst mty1 path1 mty2 =
  match mty1, mty2 with
  | Mty_ident p1, Mty_ident p2 when equal_modtype_paths env p1 subst p2 ->
      Tcoerce_none
  | _, _ ->
      let mty1 = Mtype.strengthen ~aliasable env mty1 path1 in
      modtypes ~loc env ~mark cxt subst mty1 mty2

and strengthened_module_decl ~loc ~aliasable env ~mark cxt subst md1 path1 md2 =
  match md1.md_type, md2.md_type with
  | Mty_ident p1, Mty_ident p2 when equal_modtype_paths env p1 subst p2 ->
      Tcoerce_none
  | _, _ ->
      let md1 = Mtype.strengthen_decl ~aliasable env md1 path1 in
      modtypes ~loc env ~mark cxt subst md1.md_type md2.md_type

(* Inclusion between signatures *)

and signatures ~loc env ~mark cxt subst sig1 sig2 =
  (* Environment used to check inclusion of components *)
  let new_env =
    Env.add_signature sig1 (Env.in_signature true env) in
  (* Keep ids for module aliases *)
  let (id_pos_list,_) =
    List.fold_left
      (fun (l,pos) -> function
          Sig_module (id, Mp_present, _, _, _) ->
            ((id,pos,Tcoerce_none)::l , pos+1)
        | item -> (l, if is_runtime_component item then pos+1 else pos))
      ([], 0) sig1 in
  (* Build a table of the components of sig1, along with their positions.
     The table is indexed by kind and name of component *)
  let rec build_component_table pos tbl = function
      [] -> pos, tbl
    | (Sig_value (_, _, Hidden)
      |Sig_type (_, _, _, Hidden)
      |Sig_typext (_, _, _, Hidden)
      |Sig_module (_, _, _, _, Hidden)
      |Sig_modtype (_, _, Hidden)
      |Sig_class (_, _, _, Hidden)
      |Sig_class_type (_, _, _, Hidden)
      ) as item :: rem ->
        let pos = if is_runtime_component item then pos + 1 else pos in
        build_component_table pos tbl rem (* do not pair private items. *)
    | item :: rem ->
        let (id, _loc, name) = item_ident_name item in
        let pos, nextpos =
          if is_runtime_component item then pos, pos + 1
          else -1, pos
        in
        build_component_table nextpos
                              (FieldMap.add name (id, item, pos) tbl) rem in
  let len1, comps1 =
    build_component_table 0 FieldMap.empty sig1 in
  let len2 =
    List.fold_left
      (fun n i -> if is_runtime_component i then n + 1 else n)
      0
      sig2
  in
  (* Pair each component of sig2 with a component of sig1,
     identifying the names along the way.
     Return a coercion list indicating, for all run-time components
     of sig2, the position of the matching run-time components of sig1
     and the coercion to be applied to it. *)
  let rec pair_components subst paired unpaired = function
      [] ->
        begin match unpaired with
            [] ->
              let cc =
                signature_components ~loc env ~mark new_env cxt subst
                  (List.rev paired)
              in
              if len1 = len2 then (* see PR#5098 *)
                simplify_structure_coercion cc id_pos_list
              else
                Tcoerce_structure (cc, id_pos_list)
          | _  -> raise(Error unpaired)
        end
    | item2 :: rem ->
        let (id2, loc, name2) = item_ident_name item2 in
        let name2, report =
          match item2, name2 with
            Sig_type (_, {type_manifest=None}, _, _), Field_type s
            when Btype.is_row_name s ->
              (* Do not report in case of failure,
                 as the main type will generate an error *)
              Field_type (String.sub s 0 (String.length s - 4)), false
          | _ -> name2, true
        in
        begin try
          let (id1, item1, pos1) = FieldMap.find name2 comps1 in
          let new_subst =
            match item2 with
              Sig_type _ ->
                Subst.add_type id2 (Path.Pident id1) subst
            | Sig_module _ ->
                Subst.add_module id2 (Path.Pident id1) subst
            | Sig_modtype _ ->
                Subst.add_modtype id2 (Mty_ident (Path.Pident id1)) subst
            | Sig_value _ | Sig_typext _
            | Sig_class _ | Sig_class_type _ ->
                subst
          in
          pair_components new_subst
            ((item1, item2, pos1) :: paired) unpaired rem
        with Not_found ->
          let unpaired =
            if report then
              (cxt, env, Missing_field (id2, loc, kind_of_field_desc name2)) ::
              unpaired
            else unpaired in
          pair_components subst paired unpaired rem
        end in
  (* Do the pairing and checking, and return the final coercion *)
  pair_components subst [] [] sig2

(* Inclusion between signature components *)

and signature_components ~loc old_env ~mark env cxt subst paired =
  let comps_rec rem =
    signature_components ~loc old_env ~mark env cxt subst rem
  in
  match paired with
    [] -> []
  | (Sig_value(id1, valdecl1, _), Sig_value(_id2, valdecl2, _), pos) :: rem ->
      let cc =
        value_descriptions ~loc env ~mark cxt subst id1 valdecl1 valdecl2
      in
      begin match valdecl2.val_kind with
        Val_prim _ -> comps_rec rem
      | _ -> (pos, cc) :: comps_rec rem
      end
  | (Sig_type(id1, tydecl1, _, _), Sig_type(_id2, tydecl2, _, _), _pos) :: rem
    ->
      type_declarations ~loc ~old_env env ~mark cxt subst id1 tydecl1 tydecl2;
      comps_rec rem
  | (Sig_typext(id1, ext1, _, _), Sig_typext(_id2, ext2, _, _), pos)
    :: rem ->
      extension_constructors ~loc env ~mark cxt subst id1 ext1 ext2;
      (pos, Tcoerce_none) :: comps_rec rem
  | (Sig_module(id1, pres1, mty1, _, _),
     Sig_module(_id2, pres2, mty2, _, _), pos) :: rem -> begin
      let cc = module_declarations ~loc env ~mark cxt subst id1 mty1 mty2 in
      let rem = comps_rec rem in
      match pres1, pres2, mty1.md_type with
      | Mp_present, Mp_present, _ -> (pos, cc) :: rem
      | _, Mp_absent, _ -> rem
      | Mp_absent, Mp_present, Mty_alias p1 ->
          (pos, Tcoerce_alias (env, p1, cc)) :: rem
      | Mp_absent, Mp_present, _ -> assert false
    end
  | (Sig_modtype(id1, info1, _), Sig_modtype(_id2, info2, _), _pos) :: rem ->
      modtype_infos ~loc env ~mark cxt subst id1 info1 info2;
      comps_rec rem
  | (Sig_class(id1, decl1, _, _), Sig_class(_id2, decl2, _, _), pos) :: rem ->
      class_declarations ~old_env env cxt subst id1 decl1 decl2;
      (pos, Tcoerce_none) :: comps_rec rem
  | (Sig_class_type(id1, info1, _, _),
     Sig_class_type(_id2, info2, _, _), _pos) :: rem ->
      class_type_declarations ~loc ~old_env env cxt subst id1 info1 info2;
      comps_rec rem
  | _ ->
      assert false

and module_declarations ~loc env ~mark cxt subst id1 md1 md2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:md1.md_loc
    ~use:md2.md_loc
    loc
    md1.md_attributes md2.md_attributes
    (Ident.name id1);
  let p1 = Path.Pident id1 in
  if mark_positive mark then
    Env.mark_module_used md1.md_uid;
  strengthened_modtypes ~loc ~aliasable:true env ~mark (Module id1::cxt) subst
    md1.md_type p1 md2.md_type

(* Inclusion between module type specifications *)

and modtype_infos ~loc env ~mark cxt subst id info1 info2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:info1.mtd_loc
    ~use:info2.mtd_loc
    loc
    info1.mtd_attributes info2.mtd_attributes
    (Ident.name id);
  let info2 = Subst.modtype_declaration Keep subst info2 in
  let cxt' = Modtype id :: cxt in
  try
    match (info1.mtd_type, info2.mtd_type) with
      (None, None) -> ()
    | (Some _, None) -> ()
    | (Some mty1, Some mty2) ->
        check_modtype_equiv ~loc env ~mark cxt' mty1 mty2
    | (None, Some mty2) ->
        check_modtype_equiv ~loc env ~mark cxt' (Mty_ident(Path.Pident id)) mty2
  with Error reasons ->
    raise(Error((cxt, env, Modtype_infos(id, info1, info2)) :: reasons))

and check_modtype_equiv ~loc env ~mark cxt mty1 mty2 =
  match
    (modtypes ~loc env ~mark cxt Subst.identity mty1 mty2,
     modtypes ~loc env ~mark:(negate_mark mark) cxt Subst.identity mty2 mty1)
  with
    (Tcoerce_none, Tcoerce_none) -> ()
  | (c1, _c2) ->
      (* Format.eprintf "@[c1 = %a@ c2 = %a@]@."
        print_coercion _c1 print_coercion _c2; *)
      raise(Error [cxt, env, Modtype_permutation (mty1, c1)])

(* Simplified inclusion check between module types (for Env) *)

let can_alias env path =
  let rec no_apply = function
    | Path.Pident _ -> true
    | Path.Pdot(p, _) -> no_apply p
    | Path.Papply _ -> false
  in
  no_apply path && not (Env.is_functor_arg path env)

let check_modtype_inclusion ~loc env mty1 path1 mty2 =
  let aliasable = can_alias env path1 in
  ignore
    (strengthened_modtypes ~loc ~aliasable env ~mark:Mark_both []
       Subst.identity mty1 path1 mty2)

let check_modtype_equiv ~loc env mty1 mty2 =
  check_modtype_equiv ~loc env ~mark:Mark_both [] mty1 mty2

let () =
  Env.check_functor_application :=
    (fun ~errors ~loc env mty1 path1 mty2 path2 ->
       try
         check_modtype_inclusion ~loc env mty1 path1 mty2
       with Error errs ->
         if errors then
           raise (Apply_error(loc, path1, path2, errs))
         else
           raise Not_found)

(* Check that an implementation of a compilation unit meets its
   interface. *)

let compunit env ~mark impl_name impl_sig intf_name intf_sig =
  try
    signatures ~loc:(Location.in_file impl_name) env ~mark []
      Subst.identity impl_sig intf_sig
  with Error reasons ->
    raise(Error(([], Env.empty,Interface_mismatch(impl_name, intf_name))
                :: reasons))

(* Hide the context and substitution parameters to the outside world *)

let modtypes ~loc env ~mark mty1 mty2 =
  modtypes ~loc env ~mark [] Subst.identity mty1 mty2
let signatures env ~mark sig1 sig2 =
  signatures ~loc:Location.none env ~mark [] Subst.identity sig1 sig2
let type_declarations ~loc env ~mark id decl1 decl2 =
  type_declarations ~loc env ~mark [] Subst.identity id decl1 decl2
let strengthened_module_decl ~loc ~aliasable env ~mark
      md1 path1 md2 =
  strengthened_module_decl ~loc ~aliasable env ~mark [] Subst.identity
    md1 path1 md2

(*
let modtypes env m1 m2 =
  let c = modtypes env m1 m2 in
  Format.eprintf "@[<2>modtypes@ %a@ %a =@ %a@]@."
    Printtyp.modtype m1 Printtyp.modtype m2
    print_coercion c;
  c
*)

(* Error report *)

module Illegal_permutation = struct
  (** Extraction of information in case of illegal permutation
      in a module type *)

  (** When examining coercions, we only have runtime component indices,
      we use thus a limited version of {!pos}. *)
  type coerce_pos =
    | Item of int
    | InArg
    | InBody

  let either f x g y = match f x with
    | None -> g y
    | Some _ as v -> v

  (** We extract a lone transposition from a full tree of permutations. *)
  let rec transposition_under path = function
    | Tcoerce_structure(c,_) ->
        either
          (not_fixpoint path 0) c
          (first_non_id path 0) c
    | Tcoerce_functor(arg,res) ->
        either
          (transposition_under (InArg::path)) arg
          (transposition_under (InBody::path)) res
    | Tcoerce_none -> None
    | Tcoerce_alias _ | Tcoerce_primitive _ ->
        (* these coercions are not inversible, and raise an error earlier when
           checking for module type equivalence *)
        assert false
  (* we search the first point which is not invariant at the current level *)
  and not_fixpoint path pos = function
    | [] -> None
    | (n, _) :: q ->
        if n = pos then
          not_fixpoint path (pos+1) q
        else
          Some(List.rev path, pos, n)
  (* we search the first item with a non-identity inner coercion *)
  and first_non_id path pos = function
    | [] -> None
    | (_,Tcoerce_none) :: q -> first_non_id path (pos + 1) q
    | (_,c) :: q ->
        either
          (transposition_under (Item pos :: path)) c
          (first_non_id path (pos + 1)) q

  let transposition c =
    match transposition_under [] c with
    | None -> raise Not_found
    | Some x -> x

  let rec runtime_item k = function
    | [] -> raise Not_found
    | item :: q ->
        if not(is_runtime_component item) then
          runtime_item k q
        else if k = 0 then
          item
        else
          runtime_item (k-1) q

  (* Find module type at position [path] and convert the [coerce_pos] path to
     a [pos] path *)
  let rec find env ctx path mt = match mt, path with
    | Mty_ident p, _ ->
        begin match (Env.find_modtype p env).mtd_type with
        | None -> raise Not_found
        | Some mt -> find env ctx path mt
        end
    | Mty_alias _, _ -> assert false
    | Mty_signature s , [] -> List.rev ctx, s
    | Mty_signature s, Item k :: q ->
        begin match runtime_item k s with
        | Sig_module (id, _, md,_,_) -> find env (Module id :: ctx) q md.md_type
        | _ -> raise Not_found
        end
    | Mty_functor(Named (_,mt) as arg,_), InArg :: q ->
        find env (Arg arg :: ctx) q mt
    | Mty_functor(arg, mt), InBody :: q ->
        find env (Body arg :: ctx) q mt
    | _ -> raise Not_found

  let find env path mt = find env [] path mt
  let item mt k = item_ident_name (runtime_item k mt)

  let pp_item ppf (id,_,kind) =
    Format.fprintf ppf "%s %S" (kind_of_field_desc kind) (Ident.name id)

  let pp ctx_printer env ppf (mty,c) =
    try
      let p, k, l = transposition c in
      let ctx, mt = find env p mty in
      Format.fprintf ppf
        "@[<hv 2>Illegal permutation of runtime components in a module type.@ \
         @[For example,@ %a@[the %a@ and the %a are not in the same order@ \
         in the expected and actual module types.@]@]"
        ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
    with Not_found -> (* this should not happen *)
      Format.fprintf ppf
        "Illegal permutation of runtime components in a module type."

end

open Format

let show_loc msg ppf loc =
  let pos = loc.Location.loc_start in
  if List.mem pos.Lexing.pos_fname [""; "_none_"; "//toplevel//"] then ()
  else fprintf ppf "@\n@[<2>%a:@ %s@]" Location.print_loc loc msg

let show_locs ppf (loc1, loc2) =
  show_loc "Expected declaration" ppf loc2;
  show_loc "Actual declaration" ppf loc1

let path_of_context = function
    Module id :: rem ->
      let rec subm path = function
        | [] -> path
        | Module id :: rem -> subm (Path.Pdot (path, Ident.name id)) rem
        | _ -> assert false
      in subm (Path.Pident id) rem
  | _ -> assert false


let rec context ppf = function
    Module id :: rem ->
      fprintf ppf "@[<2>module %a%a@]" Printtyp.ident id args rem
  | Modtype id :: rem ->
      fprintf ppf "@[<2>module type %a =@ %a@]"
        Printtyp.ident id context_mty rem
  | Body x :: rem ->
      fprintf ppf "functor (%s) ->@ %a" (argname x) context_mty rem
  | Arg x :: rem ->
      fprintf ppf "functor (%s : %a) -> ..." (argname x) context_mty rem
  | [] ->
      fprintf ppf "<here>"
and context_mty ppf = function
    (Module _ | Modtype _) :: _ as rem ->
      fprintf ppf "@[<2>sig@ %a@;<1 -2>end@]" context rem
  | cxt -> context ppf cxt
and args ppf = function
    Body x :: rem ->
      fprintf ppf "(%s)%a" (argname x) args rem
  | Arg x :: rem ->
      fprintf ppf "(%s :@ %a) : ..." (argname  x) context_mty rem
  | cxt ->
      fprintf ppf " :@ %a" context_mty cxt
and argname = function
  | Unit -> ""
  | Named (None, _) -> "_"
  | Named (Some id, _) -> Ident.name id

let alt_context ppf cxt =
  if cxt = [] then () else
  if List.for_all (function Module _ -> true | _ -> false) cxt then
    fprintf ppf "in module %a,@ " Printtyp.path (path_of_context cxt)
  else
    fprintf ppf "@[<hv 2>at position@ %a,@]@ " context cxt

let context ppf cxt =
  if cxt = [] then () else
  if List.for_all (function Module _ -> true | _ -> false) cxt then
    fprintf ppf "In module %a:@ " Printtyp.path (path_of_context cxt)
  else
    fprintf ppf "@[<hv 2>At position@ %a@]@ " context cxt

let include_err env ppf = function
  | Missing_field (id, loc, kind) ->
      fprintf ppf "The %s `%a' is required but not provided"
        kind Printtyp.ident id;
      show_loc "Expected declaration" ppf loc
  | Value_descriptions(id, d1, d2) ->
      fprintf ppf
        "@[<hv 2>Values do not match:@ %a@;<1 -2>is not included in@ %a@]"
        !Oprint.out_sig_item (Printtyp.tree_of_value_description id d1)
        !Oprint.out_sig_item (Printtyp.tree_of_value_description id d2);
      show_locs ppf (d1.val_loc, d2.val_loc)
  | Type_declarations(id, d1, d2, err) ->
      fprintf ppf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a@]"
        "Type declarations do not match"
        !Oprint.out_sig_item
        (Printtyp.tree_of_type_declaration id d1 Trec_first)
        "is not included in"
        !Oprint.out_sig_item
        (Printtyp.tree_of_type_declaration id d2 Trec_first)
        (Includecore.report_type_mismatch
           "the first" "the second" "declaration") err
        show_locs (d1.type_loc, d2.type_loc)
  | Extension_constructors(id, x1, x2, err) ->
      fprintf ppf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@ %a%a@]"
        "Extension declarations do not match"
        !Oprint.out_sig_item
        (Printtyp.tree_of_extension_constructor id x1 Text_first)
        "is not included in"
        !Oprint.out_sig_item
        (Printtyp.tree_of_extension_constructor id x2 Text_first)
        (Includecore.report_extension_constructor_mismatch
           "the first" "the second" "declaration") err
        show_locs (x1.ext_loc, x2.ext_loc)
  | Module_types(mty1, mty2)->
      fprintf ppf
       "@[<hv 2>Modules do not match:@ \
        %a@;<1 -2>is not included in@ %a@]"
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)
  | Modtype_infos(id, d1, d2) ->
      fprintf ppf
       "@[<hv 2>Module type declarations do not match:@ \
        %a@;<1 -2>does not match@ %a@]"
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d1)
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d2)
  | Modtype_permutation (mty,c) ->
      Illegal_permutation.pp alt_context env ppf (mty,c)
  | Interface_mismatch(impl_name, intf_name) ->
      fprintf ppf "@[The implementation %s@ does not match the interface %s:"
       impl_name intf_name
  | Class_type_declarations(id, d1, d2, reason) ->
      fprintf ppf
       "@[<hv 2>Class type declarations do not match:@ \
        %a@;<1 -2>does not match@ %a@]@ %a"
       !Oprint.out_sig_item
       (Printtyp.tree_of_cltype_declaration id d1 Trec_first)
       !Oprint.out_sig_item
       (Printtyp.tree_of_cltype_declaration id d2 Trec_first)
      Includeclass.report_error reason
  | Class_declarations(id, d1, d2, reason) ->
      fprintf ppf
       "@[<hv 2>Class declarations do not match:@ \
        %a@;<1 -2>does not match@ %a@]@ %a"
      !Oprint.out_sig_item (Printtyp.tree_of_class_declaration id d1 Trec_first)
      !Oprint.out_sig_item (Printtyp.tree_of_class_declaration id d2 Trec_first)
      Includeclass.report_error reason
  | Unbound_modtype_path path ->
      fprintf ppf "Unbound module type %a" Printtyp.path path
  | Unbound_module_path path ->
      fprintf ppf "Unbound module %a" Printtyp.path path
  | Invalid_module_alias path ->
      fprintf ppf "Module %a cannot be aliased" Printtyp.path path

let include_err ppf (cxt, env, err) =
  Printtyp.wrap_printing_env ~error:true env (fun () ->
    fprintf ppf "@[<v>%a%a@]" context (List.rev cxt) (include_err env) err)

let buffer = ref Bytes.empty
let is_big obj =
  let size = !Clflags.error_size in
  size > 0 &&
  begin
    if Bytes.length !buffer < size then buffer := Bytes.create size;
    try ignore (Marshal.to_buffer !buffer 0 size obj []); false
    with _ -> true
  end

let report_error ppf errs =
  if errs = [] then () else
  let (errs , err) = split_last errs in
  let pe = ref true in
  let include_err' ppf (_,_,obj as err) =
    if not (is_big obj) then fprintf ppf "%a@ " include_err err
    else if !pe then (fprintf ppf "...@ "; pe := false)
  in
  let print_errs ppf = List.iter (include_err' ppf) in
  Printtyp.Conflicts.reset();
  fprintf ppf "@[<v>%a%a%t@]" print_errs errs include_err err
    Printtyp.Conflicts.print_explanations

let report_apply_error p1 p2 ppf errs =
  fprintf ppf "@[The type of %a does not match %a's parameter@ %a@]"
    Printtyp.path p1 Printtyp.path p2 report_error errs

(* We could do a better job to split the individual error items
   as sub-messages of the main interface mismatch on the whole unit. *)
let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | Apply_error(loc, p1, p2, err) ->
          Some (Location.error_of_printer ~loc (report_apply_error p1 p2) err)
      | _ -> None
    )
