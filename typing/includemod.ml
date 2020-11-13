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
  | Unbound_module_path of Path.t
  | Invalid_module_alias of Path.t

type pos =
  | Module of Ident.t
  | Modtype of Ident.t
  | Arg of functor_parameter
  | Body of functor_parameter

module Error = struct
  type ('a,'b) diff = {got:'a; expected:'a; symptom:'b}
  type 'a core_diff =('a,unit) diff
  let diff x y s = {got=x;expected=y; symptom=s}
  let sdiff x y = {got=x; expected=y; symptom=()}

  type core_sigitem_symptom =
    | Value_descriptions of value_description core_diff
    | Type_declarations of (type_declaration, Includecore.type_mismatch) diff
    | Extension_constructors of
        (extension_constructor, Includecore.extension_constructor_mismatch) diff
    | Class_type_declarations of
        (class_type_declaration, Ctype.class_match_failure list) diff
    | Class_declarations of
        (class_declaration, Ctype.class_match_failure list) diff

  type core_module_type_symptom =
    | Not_an_alias
    | Not_an_identifier
    | Incompatible_aliases
    | Abstract_module_type
    | Unbound_module_path of Path.t
    | Invalid_module_alias of Path.t

  type module_type_symptom =
    | Mt_core of core_module_type_symptom
    | Signature of signature_symptom
    | Functor of functor_syndrom

  and module_type_diff = (module_type, module_type_symptom) diff

  and functor_syndrom =
    | Params of functor_params_diff
    | Result of module_type_diff

  and ('arg,'path) functor_param_syndrom =
    | Incompatible_params of 'arg * functor_parameter
    | Mismatch of 'path option * Ident.t option * module_type_diff

  and arg_functor_param_syndrom =
    (functor_parameter, Ident.t) functor_param_syndrom

  and functor_params_diff = (functor_parameter list * module_type) core_diff

  and signature_symptom = {
    env: Env.t;
    missings: signature_item list;
    incompatibles: (Ident.t * sigitem_symptom) list;
    oks: (int * module_coercion) list;
  }
  and sigitem_symptom =
    | Core of core_sigitem_symptom
    | Module_type_declaration of
        (modtype_declaration, module_type_declaration_symptom) diff
    | Module_type of module_type_diff

  and module_type_declaration_symptom =
    | Illegal_permutation of Typedtree.module_coercion
    | Not_greater_than of module_type_diff
    | Not_less_than of module_type_diff
    | Incomparable of
        {less_than:module_type_diff; greater_than: module_type_diff}


  type all =
    | In_Compilation_unit of (string, signature_symptom) diff
    | In_Signature of signature_symptom
    | In_Module_type of module_type_diff
    | In_Module_type_substitution of
        Ident.t * (module_type,module_type_declaration_symptom) diff
    | In_Type_declaration of Ident.t * core_sigitem_symptom
    | In_Expansion of core_module_type_symptom

end

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

let value_descriptions ~loc env ~mark subst id vd1 vd2 =
  Cmt_format.record_value_dependency vd1 vd2;
  if mark_positive mark then
    Env.mark_value_used vd1.val_uid;
  let vd2 = Subst.value_description subst vd2 in
  try
    Ok (Includecore.value_descriptions ~loc env (Ident.name id) vd1 vd2)
  with Includecore.Dont_match ->
    Error Error.(Core (Value_descriptions (sdiff vd1 vd2)))

(* Inclusion between type declarations *)

let type_declarations ~loc env ~mark ?old_env:_ subst id decl1 decl2 =
  let mark = mark_positive mark in
  if mark then
    Env.mark_type_used decl1.type_uid;
  let decl2 = Subst.type_declaration subst decl2 in
  match
    Includecore.type_declarations ~loc env ~mark
      (Ident.name id) decl1 (Path.Pident id) decl2
  with
  | None -> Ok Tcoerce_none
  | Some err ->
      Error Error.(Core(Type_declarations (diff decl1 decl2 err)))

(* Inclusion between extension constructors *)

let extension_constructors ~loc env ~mark  subst id ext1 ext2 =
  let mark = mark_positive mark in
  let ext2 = Subst.extension_constructor subst ext2 in
  match Includecore.extension_constructors ~loc env ~mark id ext1 ext2 with
  | None -> Ok Tcoerce_none
  | Some err ->
      Error Error.(Core(Extension_constructors(diff ext1 ext2 err)))

(* Inclusion between class declarations *)

let class_type_declarations ~loc ~old_env:_ env  subst _id decl1 decl2 =
  let decl2 = Subst.cltype_declaration subst decl2 in
  match Includeclass.class_type_declarations ~loc env decl1 decl2 with
    []     -> Ok Tcoerce_none
  | reason ->
      Error Error.(Core(Class_type_declarations(diff decl1 decl2 reason)))

let class_declarations ~old_env:_ env  subst _id decl1 decl2 =
  let decl2 = Subst.class_declaration subst decl2 in
  match Includeclass.class_declarations env decl1 decl2 with
    []     -> Ok Tcoerce_none
  | reason ->
     Error Error.(Core(Class_declarations(diff decl1 decl2 reason)))

(* Expand a module type identifier when possible *)

let expand_modtype_path env path =
   match Env.(find_modtype path env).mtd_type with
     | None | exception Not_found -> None
     | Some x -> Some x

let expand_module_alias env path =
  match (Env.find_module path env).md_type with
  | x -> Ok x
  | exception Not_found -> Error (Error.Unbound_module_path path)

(* Extract name, kind and ident from a signature item *)

type field_kind =
  | Field_value
  | Field_type
  | Field_exception
  | Field_typext
  | Field_module
  | Field_modtype
  | Field_class
  | Field_classtype



type field_desc = { name: string; kind: field_kind }

let kind_of_field_desc fd = match fd.kind with
  | Field_value -> "value"
  | Field_type -> "type"
  | Field_exception -> "exception"
  | Field_typext -> "extension constructor"
  | Field_module -> "module"
  | Field_modtype -> "module type"
  | Field_class -> "class"
  | Field_classtype -> "class type"

let field_desc kind id = { kind; name = Ident.name id }

(** Map indexed by both field types and names.
    This avoids name clashes between different sorts of fields
    such as values and types. *)
module FieldMap = Map.Make(struct
    type t = field_desc
    let compare = Stdlib.compare
  end)

let item_ident_name = function
    Sig_value(id, d, _) -> (id, d.val_loc, field_desc Field_value id)
  | Sig_type(id, d, _, _) -> (id, d.type_loc, field_desc Field_type  id )
  | Sig_typext(id, d, _, _) ->
      let kind =
        if Path.same d.ext_type_path Predef.path_exn
        then Field_exception
        else Field_typext
      in
      (id, d.ext_loc, field_desc kind id)
  | Sig_module(id, _, d, _, _) -> (id, d.md_loc, field_desc Field_module id)
  | Sig_modtype(id, d, _) -> (id, d.mtd_loc, field_desc Field_modtype id)
  | Sig_class(id, d, _, _) -> (id, d.cty_loc, field_desc Field_class id)
  | Sig_class_type(id, d, _, _) ->
      (id, d.clty_loc, field_desc Field_classtype id)

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

let retrieve_functor_params env mty =
  let rec retrieve_functor_params before env =
    function
    | Mty_ident p as res ->
        begin match expand_modtype_path env p with
        | Some mty -> retrieve_functor_params before env mty
        | None -> List.rev before, res
        end
    | Mty_alias p as res ->
        begin match expand_module_alias env p with
        | Ok mty ->  retrieve_functor_params before env mty
        | Error _ -> List.rev before, res
        end
    | Mty_functor (p, res) -> retrieve_functor_params (p :: before) env res
    | Mty_signature _ as res -> List.rev before, res
  in
  retrieve_functor_params [] env mty

(* Inclusion between module types.
   Return the restriction that transforms a value of the smaller type
   into a value of the bigger type. *)

let rec modtypes ~loc env ~mark subst mty1 mty2 =
  let dont_match reason =
    let mty2 = Subst.modtype Make_local subst mty2 in
    Error Error.(diff mty1 mty2 reason) in
  try_modtypes ~loc env ~mark dont_match subst mty1 mty2


and try_modtypes ~loc env ~mark dont_match subst mty1 mty2 =
  match mty1, mty2 with
  | (Mty_alias p1, Mty_alias p2) ->
      if Env.is_functor_arg p2 env then
        Error Error.(diff mty1 mty2 @@ Mt_core(Invalid_module_alias p2))
      else if not (equal_module_paths env p1 subst p2) then
          dont_match Error.(Mt_core Incompatible_aliases)
      else Ok Tcoerce_none
  | (Mty_alias p1, _) -> begin
      match
        Env.normalize_module_path (Some Location.none) env p1
      with
      | exception Env.Error (Env.Missing_module (_, _, path)) ->
          dont_match Error.(Mt_core(Unbound_module_path path))
      | p1 ->
          begin match expand_module_alias env  p1 with
          | Ok mty1 ->
              strengthened_modtypes ~loc ~aliasable:true env ~mark
                subst mty1 p1 mty2
          | Error e -> dont_match (Error.Mt_core e)
          end
    end
  | (Mty_ident p1, Mty_ident p2) ->
      let p1 = Env.normalize_modtype_path env p1 in
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      if Path.same p1 p2 then Ok Tcoerce_none
      else
        begin match expand_modtype_path env p1, expand_modtype_path env p2 with
        | Some p1, Some p2 ->
            try_modtypes ~loc env ~mark dont_match subst p1 p2
        | None, _  | _, None -> dont_match (Error.Mt_core Abstract_module_type)
        end
  | (Mty_ident p1, _) ->
      let p1 = Env.normalize_modtype_path env p1 in
      begin match expand_modtype_path env p1 with
      | Some p1 ->
          try_modtypes ~loc env ~mark dont_match subst p1 mty2
      | None -> dont_match (Error.Mt_core Abstract_module_type)
      end
  | (_, Mty_ident p2) ->
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      begin match expand_modtype_path env p2 with
      | Some p2 -> try_modtypes ~loc env ~mark dont_match subst mty1 p2
      | None ->
          begin match mty1 with
          | Mty_functor _ ->
              let params1 = retrieve_functor_params env mty1 in
              let d = Error.sdiff params1 ([],mty2) in
              dont_match Error.(Functor (Params d))
          | _ -> dont_match Error.(Mt_core Not_an_identifier)
          end
      end
  | (Mty_signature sig1, Mty_signature sig2) ->
      begin match signatures ~loc env ~mark subst sig1 sig2 with
      | Ok _ as ok -> ok
      | Error e -> dont_match (Error.Signature e)
      end
  | Mty_functor (param1, res1), Mty_functor (param2, res2) ->
      let cc_arg, env, subst =
        functor_param ~loc env ~mark:(negate_mark mark) subst param1 param2
      in
      let cc_res = modtypes ~loc env ~mark subst res1 res2 in
      begin match cc_arg, cc_res with
      | Ok Tcoerce_none, Ok Tcoerce_none -> Ok Tcoerce_none
      | Ok cc_arg, Ok cc_res -> Ok (Tcoerce_functor(cc_arg, cc_res))
      | _, Error {Error.symptom = Error.Functor Error.Params res; _} ->
          let got_params, got_res = res.got in
          let expected_params, expected_res = res.expected in
          let d = Error.sdiff
              (param1::got_params, got_res)
              (param2::expected_params, expected_res) in
          dont_match Error.(Functor (Params d))
      | Error _, _ ->
          let params1, res1 = retrieve_functor_params env res1 in
          let params2, res2 = retrieve_functor_params env res2 in
          let d = Error.sdiff (param1::params1, res1) (param2::params2, res2) in
          dont_match Error.(Functor (Params d))
      | Ok _, Error res ->
          dont_match Error.(Functor (Result res))
      end
  | Mty_functor _, _
  | _, Mty_functor _ ->
      let params1 = retrieve_functor_params env mty1 in
      let params2 = retrieve_functor_params env mty2 in
      let d = Error.sdiff params1 params2 in
      dont_match Error.(Functor (Params d))
  | _, Mty_alias _ ->
      dont_match (Error.Mt_core Error.Not_an_alias)

(* Functor parameters *)

and functor_param ~loc env ~mark subst param1 param2 = match param1, param2 with
  | Unit, Unit ->
      Ok Tcoerce_none, env, subst
  | Named (name1, arg1), Named (name2, arg2) ->
      let arg2' = Subst.modtype Keep subst arg2 in
      let cc_arg =
        match modtypes ~loc env ~mark Subst.identity arg2' arg1 with
        | Ok cc -> Ok cc
        | Error err -> Error (Error.Mismatch (name1, name2, err))
      in
      let env, subst =
        match name1, name2 with
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
      cc_arg, env, subst
  | _, _ ->
      Error (Error.Incompatible_params (param1, param2)), env, subst

and strengthened_modtypes ~loc ~aliasable env ~mark subst mty1 path1 mty2 =
  match mty1, mty2 with
  | Mty_ident p1, Mty_ident p2 when equal_modtype_paths env p1 subst p2 ->
      Ok Tcoerce_none
  | _, _ ->
      let mty1 = Mtype.strengthen ~aliasable env mty1 path1 in
      modtypes ~loc env ~mark subst mty1 mty2

and strengthened_module_decl ~loc ~aliasable env ~mark subst md1 path1 md2 =
  match md1.md_type, md2.md_type with
  | Mty_ident p1, Mty_ident p2 when equal_modtype_paths env p1 subst p2 ->
      Ok Tcoerce_none
  | _, _ ->
      let md1 = Mtype.strengthen_decl ~aliasable env md1 path1 in
      modtypes ~loc env ~mark subst md1.md_type md2.md_type

(* Inclusion between signatures *)

and signatures ~loc env ~mark subst sig1 sig2 =
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
        let oks, errors =
          signature_components ~loc env ~mark new_env subst (List.rev paired) in
        begin match unpaired, errors, oks with
            | [], [], cc ->
                if len1 = len2 then (* see PR#5098 *)
                  Ok (simplify_structure_coercion cc id_pos_list)
                else
                  Ok (Tcoerce_structure (cc, id_pos_list))
            | missings, incompatibles, cc ->
                Error { env=new_env; Error.missings; incompatibles; oks=cc }
        end
    | item2 :: rem ->
        let (id2, _loc, name2) = item_ident_name item2 in
        let name2, report =
          match item2, name2 with
            Sig_type (_, {type_manifest=None}, _, _), {name=s; kind=Field_type}
            when Btype.is_row_name s ->
              (* Do not report in case of failure,
                 as the main type will generate an error *)
              { kind=Field_type; name=String.sub s 0 (String.length s - 4) },
              false
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
              item2 :: unpaired
            else unpaired in
          pair_components subst paired unpaired rem
        end in
  (* Do the pairing and checking, and return the final coercion *)
  pair_components subst [] [] sig2

(* Inclusion between signature components *)

and signature_components ~loc old_env ~mark env subst paired =
  match paired with
  | [] -> [], []
  | (sigi1, sigi2, pos) :: rem ->
      let id, item, present_at_runtime = match sigi1, sigi2 with
        | Sig_value(id1, valdecl1, _) ,Sig_value(_id2, valdecl2, _) ->
            let item =
              value_descriptions ~loc env ~mark subst id1 valdecl1 valdecl2
            in
            let present_at_runtime = match valdecl2.val_kind with
              | Val_prim _ -> false
              | _ -> true
            in
            id1, item, present_at_runtime
        | Sig_type(id1, tydec1, _, _), Sig_type(_id2, tydec2, _, _) ->
            let item =
              type_declarations ~loc ~old_env env ~mark subst id1 tydec1 tydec2
            in
            id1, item, false
        | Sig_typext(id1, ext1, _, _), Sig_typext(_id2, ext2, _, _) ->
            let item =
              extension_constructors ~loc env ~mark  subst id1 ext1 ext2
            in
            id1, item, true
        | Sig_module(id1, pres1, mty1, _, _), Sig_module(_, pres2, mty2, _, _)
          -> begin
              let item =
                module_declarations ~loc env ~mark subst id1 mty1 mty2
              in
              let item =
                Result.map_error (fun diff -> Error.Module_type diff) item
              in
              let present_at_runtime, item =
                match pres1, pres2, mty1.md_type with
                | Mp_present, Mp_present, _ -> true, item
                | _, Mp_absent, _ -> false, item
                | Mp_absent, Mp_present, Mty_alias p1 ->
                    true, Result.map (fun i -> Tcoerce_alias (env, p1, i)) item
                | Mp_absent, Mp_present, _ -> assert false
              in
              id1, item, present_at_runtime
            end
        | Sig_modtype(id1, info1, _), Sig_modtype(_id2, info2, _) ->
            let item =
              modtype_infos ~loc env ~mark  subst id1 info1 info2
            in
            id1, item, false
        | Sig_class(id1, decl1, _, _), Sig_class(_id2, decl2, _, _) ->
            let item =
              class_declarations ~old_env env subst id1 decl1 decl2
            in
            id1, item, true
        | Sig_class_type(id1, info1, _, _), Sig_class_type(_id2, info2, _, _) ->
            let item =
              class_type_declarations ~loc ~old_env env subst id1 info1 info2
            in
            id1, item, false
        | _ ->
            assert false
      in
      let oks, errors =
        signature_components ~loc old_env ~mark env subst rem
      in
      match item with
      | Ok x when present_at_runtime -> (pos,x) :: oks, errors
      | Ok _ -> oks, errors
      | Error y -> oks , (id,y) :: errors

and module_declarations ~loc env ~mark  subst id1 md1 md2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:md1.md_loc
    ~use:md2.md_loc
    loc
    md1.md_attributes md2.md_attributes
    (Ident.name id1);
  let p1 = Path.Pident id1 in
  if mark_positive mark then
    Env.mark_module_used md1.md_uid;
  strengthened_modtypes ~loc ~aliasable:true env ~mark subst
    md1.md_type p1 md2.md_type

(* Inclusion between module type specifications *)

and modtype_infos ~loc env ~mark subst id info1 info2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:info1.mtd_loc
    ~use:info2.mtd_loc
    loc
    info1.mtd_attributes info2.mtd_attributes
    (Ident.name id);
  let info2 = Subst.modtype_declaration Keep subst info2 in
  let r =
    match (info1.mtd_type, info2.mtd_type) with
      (None, None) -> Ok Tcoerce_none
    | (Some _, None) -> Ok Tcoerce_none
    | (Some mty1, Some mty2) ->
        check_modtype_equiv ~loc env ~mark mty1 mty2
    | (None, Some mty2) ->
        check_modtype_equiv ~loc env ~mark (Mty_ident(Path.Pident id)) mty2 in
  match r with
  | Ok _ as ok -> ok
  | Error e -> Error Error.(Module_type_declaration (diff info1 info2 e))

and check_modtype_equiv ~loc env ~mark mty1 mty2 =
  match
    (modtypes ~loc env ~mark Subst.identity mty1 mty2,
     modtypes ~loc env ~mark:(negate_mark mark) Subst.identity mty2 mty1)
  with
    (Ok Tcoerce_none, Ok Tcoerce_none) -> Ok Tcoerce_none
  | (Ok c1, Ok _c2) ->
      (* Format.eprintf "@[c1 = %a@ c2 = %a@]@."
        print_coercion _c1 print_coercion _c2; *)
      Error Error.(Illegal_permutation c1)
  | Ok _, Error e -> Error Error.(Not_greater_than e)
  | Error e, Ok _ -> Error Error.(Not_less_than e)
  | Error less_than, Error greater_than ->
      Error Error.(Incomparable {less_than; greater_than})


(* Simplified inclusion check between module types (for Env) *)

let can_alias env path =
  let rec no_apply = function
    | Path.Pident _ -> true
    | Path.Pdot(p, _) -> no_apply p
    | Path.Papply _ -> false
  in
  no_apply path && not (Env.is_functor_arg path env)



type explanation = Env.t * Error.all
exception Error of explanation
exception Apply_error of {
    loc : Location.t ;
    env : Env.t ;
    lid_app : Longident.t option ;
    mty_f : module_type ;
    args : (Path.t option * Parsetree.module_expr option * module_type) list ;
  }

let check_modtype_inclusion_raw ~loc env mty1 path1 mty2 =
  let aliasable = can_alias env path1 in
  strengthened_modtypes ~loc ~aliasable env ~mark:Mark_both
    Subst.identity mty1 path1 mty2

let check_modtype_inclusion ~loc env mty1 path1 mty2 =
  match check_modtype_inclusion_raw ~loc env mty1 path1 mty2 with
  | Ok _ -> None
  | Error e -> Some (env, Error.In_Module_type e)

let check_functor_application_in_path
    ~errors ~loc ~lid_app ~f_path ~arg ~arg_path ~arg_mty ~param_mty env =
  match check_modtype_inclusion_raw ~loc env arg_mty arg_path param_mty with
  | Ok _ -> ()
  | Error _errs ->
      if errors then
        let mty_arg arg =
          let path, md = Env.find_module_by_name arg env in
          let aliasable = can_alias env path in
          let smd = Mtype.strengthen ~aliasable env md.md_type path in
          (Some path, None, smd)
        in
        let args = List.map mty_arg arg in
        let mty_f = (Env.find_module f_path env).md_type in
        let lid_app = Some lid_app in
        raise (Apply_error {loc; env; lid_app; mty_f; args})
      else
        raise Not_found

let () =
  Env.check_functor_application := check_functor_application_in_path


(* Check that an implementation of a compilation unit meets its
   interface. *)

let compunit env ~mark impl_name impl_sig intf_name intf_sig =
  match
    signatures ~loc:(Location.in_file impl_name) env ~mark Subst.identity
      impl_sig intf_sig
  with Result.Error reasons ->
    let cdiff = Error.In_Compilation_unit(Error.diff impl_name intf_name reasons) in
    raise(Error(env, cdiff))
  | Ok x -> x

(* Error report *)

module Short_name = struct

  type 'a item = {
    item: 'a;
    name : string;
    from: Ident.t option;
  }

  type 'a t =
    | Original of 'a
    | Synthetic of string * 'a

  type functor_param =
    | Unit
    | Named of (Ident.t option * module_type t)

  let modtype (r : _ item) = match r.item with
    | Mty_ident _
    | Mty_alias _
    | Mty_signature []
      -> Original r.item
    | Mty_signature _ | Mty_functor _
      -> Synthetic (r.name, r.item)

  let functor_param (ua : _ item) = match ua.item with
    | Types.Unit -> Unit
    | Types.Named (from, mty) ->
        Named (from, modtype { ua with item = mty ; from })

  let modexpr (r : _ item) = match r.item.Parsetree.pmod_desc with
    | Pmod_ident _
    | Pmod_structure []
      -> Original r.item
    | _
      -> Synthetic (r.name, r.item)

  let argument ua =
    let (path, md, mty, param) = ua.item in
    let md = match md with
      | None -> None
      | Some md -> Some (modexpr {ua with item = md})
    in
    let mty = modtype { ua with item = mty } in
    let param = functor_param { ua with item = param } in
    (path, md, mty, param)

  let pp ppx = function
    | Original x -> ppx x
    | Synthetic (s,_) -> Format.dprintf "%s" s

  let pp_orig ppx = function
    | Original x | Synthetic (_, x) -> ppx x

end

module FunctorDiff = struct
  open Diffing


  type ('a,'b) data = { data:'a; metadata:'b}
  type functor_arg = {path: Path.t option; mty: Types.module_type }

  let data = function
    | Insert x -> Insert x.data
    | Delete x -> Delete x.data
    | Keep (x,y,d) -> Keep(x.data,y.data,d)
    | Change(x,y,d) -> Change(x.data,y.data,d)

  let weight = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) -> begin
        match param1.metadata, param2.metadata with
        | None, None
          -> 0
        | Some n1, Some n2
          when String.equal (Ident.name n1) (Ident.name n2)
          -> 0
        | Some _, Some _ -> 1
        | Some _,  None | None, Some _ -> 1
      end

  type state = {
    res: module_type option;
    env: Env.t;
    subst: Subst.t;
  }

  let keep_expansible_param = function
    | Mty_ident _ | Mty_alias _ as mty -> Some mty
    | Mty_signature _ | Mty_functor _ -> None

  let param_preprocess data =
    let metadata =
      match data with
      | Named(x,_) -> x
      | Unit -> None in
    { data; metadata }

  let need_expansion { env ; res ; _ } = match res with
    | None -> None
    | Some res ->
        match retrieve_functor_params env res with
        | [], _ -> None
        | params, res ->
            let more = Array.of_list @@ List.map param_preprocess @@ params  in
            Some (keep_expansible_param res, more)

  let expand_arg_params state  =
    match need_expansion state with
    | None -> state, [||]
    | Some (res, expansion) -> { state with res }, expansion

  let arg_update d st = match data d with
    | Insert (Unit | Named (None,_))
    | Delete (Unit | Named (None,_))
    | Keep (Unit,_,_)
    | Keep (_,Unit,_)
    | Change (_,(Unit | Named (None,_)), _) ->
        st, [||]
    | Insert (Named (Some p, arg))
    | Delete (Named (Some p, arg))
    | Change (Unit, Named (Some p, arg), _) ->
        let arg' = Subst.modtype Keep st.subst arg in
        let env = Env.add_module p Mp_present arg' st.env in
        expand_arg_params { st with env }
    | Keep (Named (name1, _), Named (name2, arg2), _)
    | Change (Named (name1, _), Named (name2, arg2), _) -> begin
        let arg' = Subst.modtype Keep st.subst arg2 in
        match name1, name2 with
        | Some p1, Some p2 ->
            let env = Env.add_module p1 Mp_present arg' st.env in
            let subst = Subst.add_module p2 (Path.Pident p1) st.subst in
            expand_arg_params { st with env; subst }
        | None, Some p2 ->
            let env = Env.add_module p2 Mp_present arg' st.env in
            { st with env }, [||]
        | Some p1, None ->
            let env = Env.add_module p1 Mp_present arg' st.env in
            expand_arg_params { st with env }
        | None, None ->
            st, [||]
      end

  let arg_diff env _ctxt (l1,res1) (l2,_res2) =
    let update = Diffing.Left arg_update in
    let test st mty1 mty2 =
      let loc = Location.none in
      let snap = Btype.snapshot () in
      let res, _, _ =
        functor_param ~loc st.env ~mark:Mark_neither st.subst mty1.data
          mty2.data
      in
      Btype.backtrack snap;
      res
    in
    let param1 = Array.map param_preprocess @@ Array.of_list l1 in
    let param2 = Array.map param_preprocess @@ Array.of_list l2 in
    let state =
      { env; subst = Subst.identity; res = keep_expansible_param res1}
    in
    Diffing.variadic_diff ~weight ~test ~update state param1 param2

  let data_preprocess (parg,_,_,fn) =
    match fn with
    | Unit -> None
    | Named(_,mty) -> Some {path=parg; mty}

  let arg_preprocess (_,_,_,fn as data) =
    let metadata =
      match fn with
      | Unit -> None
      | Named(x,_) -> x
    in
    { data; metadata }

  let expand_app_params state =
    match need_expansion state with
    | None -> state, [||]
    | Some (res, expansion) -> { state with res }, expansion

  let app_update d st =
    match Diffing.map data_preprocess Fun.id (data d) with
    | Insert _
    | Delete _
    | Keep (None,_,_)
    | Keep (_,Unit,_)
    | Change (_,(Unit | Named (None,_)), _ )
    | Change (None, Named (Some _, _), _) ->
        st, [||]
    | Keep (Some arg, Named (param_name, _param), _)
    | Change (Some arg, Named (param_name, _param), _) -> begin
        let arg' = Subst.modtype Keep st.subst arg.mty in
        match arg.path, param_name with
        | Some arg, Some param ->
            let res = Option.map (fun res ->
              let scope = Ctype.create_scope () in
              let subst = Subst.add_module param arg Subst.identity in
              Subst.modtype (Rescope scope) subst res) st.res in
            let subst = Subst.add_module param arg st.subst in
            expand_app_params { st with subst; res }
        | None, Some param ->
            let env =
              Env.add_module ~arg:true param Mp_present arg' st.env in
            let res =
              Option.map (Mtype.nondep_supertype env [param]) st.res in
            expand_app_params { st with env; res}
        | _, None ->
            st, [||]
      end

  let app_diff env ~f ~args =
    let params, res = retrieve_functor_params env f in
    let update = Diffing.Right app_update in
    let test state x y =
      let arg = data_preprocess x.data and param = y.data in
      let loc = Location.none in
      let snap = Btype.snapshot () in
      let res = match arg, param with
        | None, Unit -> Ok Tcoerce_none
        | None, Named _ | Some _, Unit ->
            Result.Error (Error.Incompatible_params(arg,param))
        | Some arg, Named (name, param) ->
            match
              modtypes ~loc state.env ~mark:Mark_neither state.subst
                arg.mty param
            with
            | Error mty -> Result.Error (Error.Mismatch(arg.path, name, mty))
            | Ok _ as x -> x
      in
      Btype.backtrack snap;
      res
    in
    let args = Array.map arg_preprocess @@ Array.of_list args in
    let params = Array.map param_preprocess @@ Array.of_list params in
    
    let state =
      { env; subst = Subst.identity; res = keep_expansible_param res }
    in
    Diffing.variadic_diff ~weight ~test ~update state args params

  (* Simplication for printing *)

  let shortname side pos =
    match side with
    | `Got -> Format.sprintf "$S%d" pos
    | `Expected -> Format.sprintf "$T%d" pos
    | `Unneeded -> "..."

  let to_shortnames ctx patch =
    let to_shortname side pos mty =
      {Short_name. name = (shortname side pos); item = mty; from=None }
    in
    let elide_if_app s = match ctx with
      | `App -> `Unneeded
      | `Sig -> s
    in
    let aux i d =
      let pos = i + 1 in
      let d = match d with
        | Diffing.Insert mty ->
            Diffing.Insert (to_shortname `Expected pos mty)
        | Diffing.Delete mty ->
            Diffing.Delete (to_shortname (elide_if_app `Got) pos mty)
        | Diffing.Change (g, e, p) ->
            Diffing.Change
              (to_shortname `Got pos g,
               to_shortname `Expected pos e, p)
        | Diffing.Keep (g, e, p) ->
            Diffing.Keep (to_shortname `Got pos g,
                       to_shortname (elide_if_app `Expected) pos e, p)
      in
      pos, d
    in
    List.mapi aux patch

  let drop_inserted_suffix patch =
    let rec drop = function
      | Diffing.Insert _ :: q -> drop q
      | rest -> List.rev rest in
    drop (List.rev patch)

  let prepare_patch ~drop ~ctx patch =
    let drop_suffix x = if drop then drop_inserted_suffix x else x in
    patch |> List.map data |> drop_suffix |> to_shortnames ctx

end


(* Hide the context and substitution parameters to the outside world *)

let modtypes ~loc env ~mark mty1 mty2 =
  match modtypes ~loc env ~mark Subst.identity mty1 mty2 with
  | Ok x -> x
  | Error reason -> raise (Error (env, Error.(In_Module_type reason)))
let signatures env ~mark sig1 sig2 =
  match signatures ~loc:Location.none env ~mark Subst.identity sig1 sig2 with
  | Ok x -> x
  | Error reason -> raise (Error(env,Error.(In_Signature reason)))

let type_declarations ~loc env ~mark id decl1 decl2 =
  match type_declarations ~loc env ~mark Subst.identity id decl1 decl2 with
  | Ok _ -> ()
  | Error (Error.Core reason) ->
      raise (Error(env,Error.(In_Type_declaration(id,reason))))
  | Error _ -> assert false

let strengthened_module_decl ~loc ~aliasable env ~mark md1 path1 md2 =
  match strengthened_module_decl ~loc ~aliasable env ~mark Subst.identity
    md1 path1 md2 with
  | Ok x -> x
  | Error mdiff ->
      raise (Error(env,Error.(In_Module_type mdiff)))




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
         @[For example,@ %a@]@ @[the %a@ and the %a are not in the same order@ \
         in the expected and actual module types.@]@]"
        ctx_printer ctx pp_item (item mt k) pp_item (item mt l)
    with Not_found -> (* this should not happen *)
      Format.fprintf ppf
        "Illegal permutation of runtime components in a module type."

end



let show_loc msg ppf loc =
  let pos = loc.Location.loc_start in
  if List.mem pos.Lexing.pos_fname [""; "_none_"; "//toplevel//"] then ()
  else Format.fprintf ppf "@\n@[<2>%a:@ %s@]" Location.print_loc loc msg

let show_locs ppf (loc1, loc2) =
  show_loc "Expected declaration" ppf loc2;
  show_loc "Actual declaration" ppf loc1


open Format


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
    fprintf ppf "in module %a," Printtyp.path (path_of_context cxt)
  else
    fprintf ppf "@[<hv 2>at position@ %a,@]" context cxt

let context ppf cxt =
  if cxt = [] then () else
  if List.for_all (function Module _ -> true | _ -> false) cxt then
    fprintf ppf "In module %a:@ " Printtyp.path (path_of_context cxt)
  else
    fprintf ppf "@[<hv 2>At position@ %a@]@ " context cxt



let buffer = ref Bytes.empty
let is_big obj =
  let size = !Clflags.error_size in
  size > 0 &&
  begin
    if Bytes.length !buffer < size then buffer := Bytes.create size;
    try ignore (Marshal.to_buffer !buffer 0 size obj []); false
    with _ -> true
  end

module Pp = struct
  open Error

  let core id x =
    match x with
    | Value_descriptions diff ->
        let t1 = Printtyp.tree_of_value_description id diff.got in
        let t2 = Printtyp.tree_of_value_description id diff.expected in
        Format.dprintf
          "@[<hv 2>Values do not match:@ %a@;<1 -2>is not included in@ %a@]%a%t"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
        show_locs (diff.got.val_loc, diff.expected.val_loc)
        Printtyp.Conflicts.print_explanations
    | Type_declarations diff ->
        Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]%a%a%t@]"
          "Type declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.got Trec_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_type_declaration id diff.expected Trec_first)
          (Includecore.report_type_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.type_loc, diff.expected.type_loc)
          Printtyp.Conflicts.print_explanations
    | Extension_constructors diff ->
        Format.dprintf "@[<v>@[<hv>%s:@;<1 2>%a@ %s@;<1 2>%a@]@ %a%a%t@]"
          "Extension declarations do not match"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.got Text_first)
          "is not included in"
          !Oprint.out_sig_item
          (Printtyp.tree_of_extension_constructor id diff.expected Text_first)
          (Includecore.report_extension_constructor_mismatch
             "the first" "the second" "declaration") diff.symptom
          show_locs (diff.got.ext_loc, diff.expected.ext_loc)
          Printtyp.Conflicts.print_explanations
    | Class_type_declarations diff ->
        Format.dprintf
          "@[<hv 2>Class type declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a%t"
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.got Trec_first)
          !Oprint.out_sig_item
          (Printtyp.tree_of_cltype_declaration id diff.expected Trec_first)
          Includeclass.report_error diff.symptom
          Printtyp.Conflicts.print_explanations
    | Class_declarations {got;expected;symptom} ->
        let t1 = Printtyp.tree_of_class_declaration id got Trec_first in
        let t2 = Printtyp.tree_of_class_declaration id expected Trec_first in
        Format.dprintf
          "@[<hv 2>Class declarations do not match:@ \
           %a@;<1 -2>does not match@ %a@]@ %a%t"
          !Oprint.out_sig_item t1
          !Oprint.out_sig_item t2
          Includeclass.report_error symptom
          Printtyp.Conflicts.print_explanations

  let missing_field ppf item =
    let id, loc, kind = item_ident_name item in
    Format.fprintf ppf "The %s `%a' is required but not provided%a"
      (kind_of_field_desc kind) Printtyp.ident id
    (show_loc "Expected declaration") loc

  let module_types {got=mty1; expected=mty2} =
    Format.dprintf
      "@[<hv 2>Modules do not match:@ \
       %a@;<1 -2>is not included in@ %a@]"
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)

  let eq_module_types {got=mty1; expected=mty2} =
    Format.dprintf
      "@[<hv 2>Module types do not match:@ \
       %a@;<1 -2>is not equal to@ %a@]"
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty1)
      !Oprint.out_module_type (Printtyp.tree_of_modtype mty2)


  let module_type_declarations id {got=d1 ; expected=d2} =
    Format.dprintf
      "@[<hv 2>Module type declarations do not match:@ \
       %a@;<1 -2>does not match@ %a@]"
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d1)
      !Oprint.out_sig_item (Printtyp.tree_of_modtype_declaration id d2)

  let interface_mismatch ppf diff =
    Format.fprintf ppf
      "The implementation %s@ does not match the interface %s:@ "
      diff.got diff.expected

  let core_module_type_symptom x  =
    match x with
    | Not_an_alias | Not_an_identifier | Abstract_module_type
    | Incompatible_aliases ->
        if Printtyp.Conflicts.exists () then
          Some (Printtyp.Conflicts.print_explanations)
        else None
    | Unbound_module_path path ->
        Some(Format.dprintf "Unbound module %a" Printtyp.path path)
    | Invalid_module_alias path ->
        Some(Format.dprintf "Module %a cannot be aliased" Printtyp.path path)

  let dmodtype mty =
    let tmty = Printtyp.tree_of_modtype mty in
    Format.dprintf "%a" !Oprint.out_module_type tmty

  let definition_of_functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named(_,short_mty) ->
        match short_mty with
        | Original mty -> dmodtype mty
        | Synthetic (name, mty) ->
            Format.dprintf
              "%s@ =@ %t" name (dmodtype mty)

  let short_functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named (_, short_mty) ->
        Short_name.pp dmodtype short_mty

  let functor_param x = match Short_name.functor_param x with
    | Short_name.Unit -> Format.dprintf "()"
    | Short_name.Named (None, short_mty) ->
        Short_name.pp dmodtype short_mty
    | Short_name.Named (Some p, short_mty) ->
        Format.dprintf "(%s : %t)"
          (Ident.name p) (Short_name.pp dmodtype short_mty)

  let definition_of_argument arg =
    match Short_name.argument arg with
    | _, _, _, Short_name.Unit -> Format.dprintf "()"
    | Some p, _, mty, _ ->
        Format.dprintf
          "%a@ :@ %t"
          Printtyp.path p
          (Short_name.pp_orig dmodtype mty)
    | _, Some short_md, _, _ ->
        begin match short_md with
        | Original md -> fun ppf -> Pprintast.module_expr ppf md
        | Synthetic (name, md) -> fun ppf ->
            Format.fprintf ppf
              "%s@ =@ %a" name Pprintast.module_expr md
        end
    | None, None, _, _ -> assert false

  let short_argument arg =
    match Short_name.argument arg with
    | _, _, _, Short_name.Unit -> Format.dprintf "()"
    | Some p, _, _, _ -> fun ppf -> Printtyp.path ppf p
    | _, Some short_md, _, _ ->
        Short_name.pp (fun x ppf -> Pprintast.module_expr ppf x) short_md
    | None, None, _, _ -> assert false

  let style = function
    | Diffing.Keep _ -> Misc.Color.[ FG Green ]
    | Diffing.Delete _ -> Misc.Color.[ FG Red; Bold]
    | Diffing.Insert _ -> Misc.Color.[ FG Red; Bold]
    | Diffing.Change _ -> Misc.Color.[ FG Magenta; Bold]

  let decorate preprinter x =
    let sty = style x in
    let printer = preprinter x in
    fun ppf ->
      Format.pp_open_stag ppf (Misc.Color.Style sty);
      printer ppf;
      Format.pp_close_stag ppf ()

  let prefix ppf (pos, p) =
    let sty = style p in
    Format.pp_open_stag ppf (Misc.Color.Style sty);
    Format.fprintf ppf "%i." pos;
    Format.pp_close_stag ppf ()

  let got f = function
    | Diffing.Delete mty
    | Diffing.Keep (mty,_,_)
    | Diffing.Change (mty,_,_)
      -> f mty
    | Diffing.Insert _ -> ignore
  let expected f = function
    | Diffing.Insert mty
    | Diffing.Keep (_,mty,_)
    | Diffing.Change (_,mty,_)
      -> f mty
    | Diffing.Delete _ -> ignore

  let space ppf () = Format.fprintf ppf "@ "
  let dlist ?(sep=space) f l  =
    let l' = List.map f l in
    fun ppf -> Format.pp_print_list ~pp_sep:sep (|>) ppf l'
  let params_diff sep f patch =
    let elt (_,x) = decorate f x in
    dlist ~sep elt patch

end



module Linearize = struct
  (** Construct a linear presentation of the error tree *)

  open Error
  let with_context ?loc ctx printer diff =
    Location.msg ?loc "%a%a" context (List.rev ctx)
      printer diff

  let dwith_context ?loc ctx printer =
    Location.msg ?loc "%a%t" context (List.rev ctx) printer

  let dwith_context_and_elision ?loc ctx printer diff =
    if is_big (diff.Error.got,diff.Error.expected) then
      Location.msg ?loc "..."
    else
      dwith_context ?loc ctx (printer diff)

  type ('a,'b) patch =
    ( 'a Short_name.item, 'b Short_name.item,
      Typedtree.module_coercion, Error.arg_functor_param_syndrom
    ) Diffing.change
  type ('a,'b) t = {
    msgs: Location.msg list;
    post:
      (Env.t * (int * ('a, 'b) patch) list) option
  }

  let rec module_type ~expansion_token ~eqmode ~env ~before ~ctx diff =
    match diff.symptom with
    | Mt_core (Invalid_module_alias _ as s) ->
        begin match Pp.core_module_type_symptom s with
        | None -> { msgs = before; post = None }
        | Some main ->
            let more = with_context ctx (fun ppf () -> main ppf) () in
            { msgs = more :: before; post = None }
        end
    | _ ->
        let inner = if eqmode then Pp.eq_module_types else Pp.module_types in
        let before = match diff.symptom with
          | Functor Params _ -> before
          | _ ->
              let next = dwith_context_and_elision ctx inner diff in
              next :: before in
        module_type_symptom ~expansion_token ~env ~before ~ctx diff.symptom

  and module_type_symptom ~expansion_token ~env ~before ~ctx = function
    | Mt_core core ->
        begin match Pp.core_module_type_symptom core with
        | None -> { msgs = before; post = None }
        | Some msg ->
            { msgs = Location.msg "%t" msg :: before; post = None }
        end
    | Signature s -> signature ~expansion_token ~env ~before ~ctx s
    | Functor f -> functor_symptom ~expansion_token ~env ~before ~ctx f

  and functor_symptom ~expansion_token ~env ~before ~ctx = function
    | Result res ->
        module_type ~expansion_token ~eqmode:false ~env ~before ~ctx res
    | Params Error.{got; expected; symptom=()} ->
        let d =
          FunctorDiff.arg_diff env ctx got expected
          |> FunctorDiff.prepare_patch ~drop:false ~ctx:`Sig
        in
        let got = Pp.(params_diff space (got functor_param) d) in
        let expected = Pp.(params_diff space (expected functor_param) d) in
        let main =
          Format.dprintf
            "@[<hv 2>Modules do not match:@ \
             @[functor@ %t@ -> ...@]@;<1 -2>is not included in@ \
             @[functor@ %t@ -> ...@]@]"
            got expected
        in
        let post = if expansion_token then Some (env,d) else None in
        { msgs = dwith_context ctx main :: before; post }

  and signature ~expansion_token ~env:_ ~before ~ctx sgs =
    Printtyp.wrap_printing_env ~error:true sgs.env (fun () ->
    match sgs.missings, sgs.incompatibles with
    | a :: l , _ ->
        let more = List.map (Location.msg "%a" Pp.missing_field) l in
        let msgs = with_context ctx Pp.missing_field a :: more @ before in
        { msgs; post = None }
    | [], a :: _ -> sigitem ~expansion_token ~env:sgs.env ~before ~ctx a
    | [], [] -> assert false
      )
  and sigitem ~expansion_token ~env ~before ~ctx (name,s) = match s with
    | Core c ->
        { msgs = dwith_context ctx (Pp.core name c):: before; post = None }
    | Module_type diff ->
        module_type ~expansion_token ~eqmode:false ~env ~before
          ~ctx:(Module name :: ctx) diff
    | Module_type_declaration diff ->
        module_type_decl ~expansion_token ~env ~before ~ctx name diff
  and module_type_decl ~expansion_token ~env ~before ~ctx id diff =
    let next =
      dwith_context_and_elision ctx (Pp.module_type_declarations id) diff in
    let before = next :: before in
    match diff.symptom with
    | Not_less_than mts ->
        let before =
          Location.msg "The first module type is not included in the second"
          :: before in
        module_type ~expansion_token ~eqmode:true ~before ~env
          ~ctx:(Modtype id :: ctx) mts
    | Not_greater_than mts ->
        let before =
          Location.msg "The second module type is not included in the first"
          :: before in
        module_type ~expansion_token ~eqmode:true ~before ~env
          ~ctx:(Modtype id :: ctx) mts
    | Incomparable mts ->
        module_type ~expansion_token ~eqmode:true ~env ~before
          ~ctx:(Modtype id :: ctx) mts.less_than
    | Illegal_permutation c ->
        begin match diff.got.Types.mtd_type with
        | None -> assert false
        | Some mty ->
            let main =
            with_context (Modtype id::ctx)
              (Illegal_permutation.pp alt_context env) (mty,c) in
            { msgs = main :: before; post = None }
        end

  let module_type_subst ~env id diff =
    match diff.symptom with
    | Not_less_than mts ->
        module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
          ~ctx:[Modtype id] mts
    | Not_greater_than mts ->
        module_type ~expansion_token:true ~eqmode:true ~before:[] ~env
          ~ctx:[Modtype id] mts
    | Incomparable mts ->
        module_type ~expansion_token:true ~eqmode:true ~env ~before:[]
          ~ctx:[Modtype id] mts.less_than
    | Illegal_permutation c ->
        let mty = diff.got in
        let main =
          with_context [Modtype id]
            (Illegal_permutation.pp alt_context env) (mty,c) in
        { msgs = [main]; post = None }


  let insert_suberror mty =
    Format.dprintf
      "An argument appears to be missing with module type@;<1 2>@[%t@]"
      (Pp.definition_of_functor_param mty)

  let delete_suberror mty =
    Format.dprintf
      "An extra argument is provided of module type@;<1 2>@[%t@]"
      (Pp.definition_of_functor_param mty)

  let delete_suberror_app mty =
    Format.dprintf
      "The following extra argument is provided@;<1 2>@[%t@]"
      (Pp.definition_of_argument mty)

  let ok_suberror x y =
    Format.dprintf
      "Module types %t and %t match"
      (Pp.short_functor_param x)
      (Pp.short_functor_param y)

  let ok_suberror_app x y =
    let pp_orig_name = match Short_name.functor_param y with
      | Short_name.Named (_, Original mty) ->
          Format.dprintf " %t" (Pp.dmodtype mty)
      | _ -> ignore
    in
    Format.dprintf
      "Module %t matches the expected module type%t"
      (Pp.short_argument x)
      pp_orig_name

  let diff_arg g e more =
    let g = Pp.definition_of_functor_param g in
    let e = Pp.definition_of_functor_param e in
    Format.dprintf
      "Module types do not match:@ @[%t@]@;<1 -2>does not include@ \
       @[%t@]@;<1 -2>@[%t@]"
      g e (more ())

  let diff_app g e more =
    let g = Pp.definition_of_argument g in
    let e = Pp.definition_of_functor_param e in
    Format.dprintf
      "Modules do not match:@ @[%t@]@;<1 -2>\
       is not included in@ @[%t@]@;<1 -2>@[%t@]"
      g e (more ())

  let param_subcase sub ~expansion_token env (pos, diff) =
    Location.msg "%a @[<hv 2>%t@]" Pp.prefix (pos, diff)
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )
  let param_onlycase sub ~expansion_token env (_, diff) =
    Location.msg "   @[<hv 2>%t@]"
      (Printtyp.wrap_printing_env env ~error:true
         (fun () -> sub ~expansion_token env diff)
      )

  let param_suberrors sub ~expansion_token env l =
    let rec aux = function
      | [] -> []
      | (_, Diffing.Keep _) as a :: q ->
          param_subcase sub ~expansion_token env a
          :: aux q
      | a :: q ->
          param_subcase sub ~expansion_token env a
          :: List.map (param_subcase sub ~expansion_token:false env) q
    in
    match l with
    | [a] -> [param_onlycase sub ~expansion_token env a]
    | l -> aux l


  let arg_incompatible = function
    | Unit ->
        Format.dprintf
          "The functor was expected to be applicative at this position"
    | Named _ ->
        Format.dprintf
          "The functor was expected to be generative at this position"

  let app_incompatible = function
    | None ->
        Format.dprintf
          "The functor was expected to be applicative at this position"
    | Some _ ->
        Format.dprintf
          "The functor was expected to be generative at this position"


  let rec diff_suberror:
    'a 'b 'c 'd. ('c -> _) -> ('a -> 'b -> _) -> expansion_token:_ -> _ ->
    'a -> 'b -> ('c,'d) Error.functor_param_syndrom -> _
    = fun incompatible msg ~expansion_token env g e diff -> match diff with
    | Error.Incompatible_params (i,_) -> incompatible i
    | Error.Mismatch(_,_,mty_diff) ->
        let more () =
          let r =
            module_type_symptom ~expansion_token ~env ~before:[] ~ctx:[]
              mty_diff.symptom in
          let list l ppf =
            Format.pp_print_list ~pp_sep:Pp.space
              (fun ppf f -> f.Location.txt ppf)
              ppf l in
          let post = match r.post with
            | None -> []
            | Some (env, patch) ->
                param_suberrors arg ~expansion_token env patch in
          list (List.rev_append r.msgs post) in
        msg g e more

  and arg ~expansion_token env = function
    | Diffing.Insert mty -> insert_suberror mty
    | Diffing.Delete mty -> delete_suberror mty
    | Diffing.Change (g, e, d) ->
        diff_suberror arg_incompatible diff_arg ~expansion_token env g e d
    | Diffing.Keep (x, y, _) -> ok_suberror x y

  let app ~expansion_token env = function
    | Diffing.Insert mty -> insert_suberror mty
    | Diffing.Delete mty -> delete_suberror_app mty
    | Diffing.Change (g, e, d) ->
        diff_suberror app_incompatible diff_app ~expansion_token env g e d
    | Diffing.Keep (x, y, _) -> ok_suberror_app x y

  let all env = function
    | In_Compilation_unit diff ->
      let first = Location.msg "%a" Pp.interface_mismatch diff in
      signature ~expansion_token:true ~env ~before:[first] ~ctx:[] diff.symptom
    | In_Type_declaration (id,reason) ->
        let main = Location.msg "%t" (Pp.core id reason) in
        { msgs = [main]; post = None }
    | In_Module_type diff ->
        module_type ~expansion_token:true ~eqmode:false ~before:[] ~env ~ctx:[]
          diff
    | In_Module_type_substitution (id,diff) ->
        module_type_subst ~env id diff
    | In_Signature diff ->
        signature ~expansion_token:true ~before:[] ~env ~ctx:[] diff
    | In_Expansion cmts ->
        match Pp.core_module_type_symptom cmts with
        | None -> assert false
        | Some main ->
            { msgs = [Location.msg "%t" main]; post = None }

  let coalesce { msgs; _ } =
    match List.rev msgs with
    | [] -> ignore
    | before ->
        let ctx ppf =
          Format.pp_print_list ~pp_sep:Pp.space
            (fun ppf x -> x.Location.txt ppf)
            ppf before in
        ctx
end

let err_msgs (env, err) =
  Printtyp.Conflicts.reset();
  Printtyp.wrap_printing_env ~error:true env (fun () ->
      let l = Linearize.all env err in
      let main = Linearize.coalesce l in
      let sub = match l.Linearize.post with
        | None -> []
        | Some (env,post) ->
            Linearize.(param_suberrors arg) ~expansion_token:true env post in
      sub, main
    )

let report_error err =
  let sub, main = err_msgs err in
  Location.errorf ~loc:Location.(in_file !input_name) ~sub "%t" main

let report_apply_error ~loc env (lid_app, mty_f, args) =
  let may_print_app ppf = match lid_app with
    | None -> ()
    | Some lid -> Format.fprintf ppf "%a " Printtyp.longident lid
  in
  let d =
    FunctorDiff.app_diff env ~f:mty_f ~args
    |> FunctorDiff.prepare_patch ~drop:true ~ctx:`App
  in
  Location.errorf ~loc
    ~sub:(Linearize.(param_suberrors app) env ~expansion_token:true d)
    "@[<hv>The functor application %tis ill-typed.@ \
     These arguments:@;<1 2>\
     @[%t@]@ do not match these parameters:@;<1 2>@[functor@ %t@ -> ...@]@]"
    may_print_app
    Pp.(params_diff space (got short_argument) d)
    Pp.(params_diff space (expected functor_param) d)


(* We could do a better job to split the individual error items
   as sub-messages of the main interface mismatch on the whole unit. *)
let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (report_error err)
      | Apply_error {loc; env; lid_app; mty_f; args} ->
          let prepare_arg (path_arg, md_arg, mty_arg) =
            let param = match path_arg, md_arg with
              | _, Some {Parsetree.pmod_desc = Pmod_structure []} -> Types.Unit
              | Some(Path.Pident p), _ -> Types.Named(Some p,mty_arg)
              | _, _ -> Types.Named(None,mty_arg)
            in
            (path_arg, md_arg, mty_arg, param)
          in
          let args = List.map prepare_arg args in
          Some (Printtyp.wrap_printing_env env ~error:true (fun () ->
              report_apply_error ~loc env (lid_app, mty_f, args))
            )
      | _ -> None
    )


let expand_module_alias env path =
  match expand_module_alias env path with
  | Ok x -> x
  | Result.Error _ ->
      raise (Error(env,In_Expansion(Error.Unbound_module_path path)))

let check_modtype_equiv ~loc env id mty1 mty2 =
  match check_modtype_equiv ~loc env ~mark:Mark_both mty1 mty2 with
  | Ok _ -> ()
  | Error e ->
      raise (Error(env,
                   Error.(In_Module_type_substitution (id,diff mty1 mty2 e)))
            )
