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

  type functor_arg_descr =
    | Anonymous
    | Named of Path.t
    | Unit

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

  type module_type_symptom =
    | Mt_core of core_module_type_symptom
    | Signature of signature_symptom
    | Functor of functor_symptom
    | Invalid_module_alias of Path.t
    | After_alias_expansion of module_type_diff


  and module_type_diff = (module_type, module_type_symptom) diff

  and functor_symptom =
    | Params of functor_params_diff
    | Result of module_type_diff

  and ('arg,'path) functor_param_symptom =
    | Incompatible_params of 'arg * functor_parameter
    | Mismatch of module_type_diff

  and arg_functor_param_symptom =
    (functor_parameter, Ident.t) functor_param_symptom

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
        Ident.t * (Types.module_type,module_type_declaration_symptom) diff
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

let class_type_declarations ~loc ~old_env:_ env  subst decl1 decl2 =
  let decl2 = Subst.cltype_declaration subst decl2 in
  match Includeclass.class_type_declarations ~loc env decl1 decl2 with
    []     -> Ok Tcoerce_none
  | reason ->
      Error Error.(Core(Class_type_declarations(diff decl1 decl2 reason)))

let class_declarations ~old_env:_ env  subst decl1 decl2 =
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
  match try_modtypes ~loc env ~mark subst mty1 mty2 with
  | Ok _ as ok -> ok
  | Error reason ->
    let mty2 = Subst.modtype Make_local subst mty2 in
    Error Error.(diff mty1 mty2 reason)

and try_modtypes ~loc env ~mark subst mty1 mty2 =
  match mty1, mty2 with
  | (Mty_alias p1, Mty_alias p2) ->
      if Env.is_functor_arg p2 env then
        Error (Error.Invalid_module_alias p2)
      else if not (equal_module_paths env p1 subst p2) then
          Error Error.(Mt_core Incompatible_aliases)
      else Ok Tcoerce_none
  | (Mty_alias p1, _) -> begin
      match
        Env.normalize_module_path (Some Location.none) env p1
      with
      | exception Env.Error (Env.Missing_module (_, _, path)) ->
          Error Error.(Mt_core(Unbound_module_path path))
      | p1 ->
          begin match expand_module_alias env  p1 with
          | Error e -> Error (Error.Mt_core e)
          | Ok mty1 ->
              match strengthened_modtypes ~loc ~aliasable:true env ~mark
                      subst mty1 p1 mty2
              with
              | Ok _ as x -> x
              | Error reason -> Error (Error.After_alias_expansion reason)
          end
    end
  | (Mty_ident p1, Mty_ident p2) ->
      let p1 = Env.normalize_modtype_path env p1 in
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      if Path.same p1 p2 then Ok Tcoerce_none
      else
        begin match expand_modtype_path env p1, expand_modtype_path env p2 with
        | Some mty1, Some mty2 ->
            try_modtypes ~loc env ~mark subst mty1 mty2
        | None, _  | _, None -> Error (Error.Mt_core Abstract_module_type)
        end
  | (Mty_ident p1, _) ->
      let p1 = Env.normalize_modtype_path env p1 in
      begin match expand_modtype_path env p1 with
      | Some p1 ->
          try_modtypes ~loc env ~mark subst p1 mty2
      | None -> Error (Error.Mt_core Abstract_module_type)
      end
  | (_, Mty_ident p2) ->
      let p2 = Env.normalize_modtype_path env (Subst.modtype_path subst p2) in
      begin match expand_modtype_path env p2 with
      | Some p2 -> try_modtypes ~loc env ~mark subst mty1 p2
      | None ->
          begin match mty1 with
          | Mty_functor _ ->
              let params1 = retrieve_functor_params env mty1 in
              let d = Error.sdiff params1 ([],mty2) in
              Error Error.(Functor (Params d))
          | _ -> Error Error.(Mt_core Not_an_identifier)
          end
      end
  | (Mty_signature sig1, Mty_signature sig2) ->
      begin match signatures ~loc env ~mark subst sig1 sig2 with
      | Ok _ as ok -> ok
      | Error e -> Error (Error.Signature e)
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
          Error Error.(Functor (Params d))
      | Error _, _ ->
          let params1, res1 = retrieve_functor_params env res1 in
          let params2, res2 = retrieve_functor_params env res2 in
          let d = Error.sdiff (param1::params1, res1) (param2::params2, res2) in
          Error Error.(Functor (Params d))
      | Ok _, Error res ->
          Error Error.(Functor (Result res))
      end
  | Mty_functor _, _
  | _, Mty_functor _ ->
      let params1 = retrieve_functor_params env mty1 in
      let params2 = retrieve_functor_params env mty2 in
      let d = Error.sdiff params1 params2 in
      Error Error.(Functor (Params d))
  | _, Mty_alias _ ->
      Error (Error.Mt_core Error.Not_an_alias)

(* Functor parameters *)

and functor_param ~loc env ~mark subst param1 param2 = match param1, param2 with
  | Unit, Unit ->
      Ok Tcoerce_none, env, subst
  | Named (name1, arg1), Named (name2, arg2) ->
      let arg2' = Subst.modtype Keep subst arg2 in
      let cc_arg =
        match modtypes ~loc env ~mark Subst.identity arg2' arg1 with
        | Ok cc -> Ok cc
        | Error err -> Error (Error.Mismatch err)
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
      let id, item, present_at_runtime =
        match sigi1, sigi2 with
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
              class_declarations ~old_env env subst decl1 decl2
            in
            id1, item, true
        | Sig_class_type(id1, info1, _, _), Sig_class_type(_id2, info2, _, _) ->
            let item =
              class_type_declarations ~loc ~old_env env subst info1 info2
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
    args : (Error.functor_arg_descr * module_type) list ;
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
    ~errors ~loc ~lid_whole_app ~f0_path ~args
    ~arg_path ~arg_mty ~param_mty env =
  match check_modtype_inclusion_raw ~loc env arg_mty arg_path param_mty with
  | Ok _ -> ()
  | Error _errs ->
      if errors then
        let prepare_arg (arg_path, arg_mty) =
          let aliasable = can_alias env arg_path in
          let smd = Mtype.strengthen ~aliasable env arg_mty arg_path in
          (Error.Named arg_path, smd)
        in
        let mty_f = (Env.find_module f0_path env).md_type in
        let args = List.map prepare_arg args in
        let lid_app = Some lid_whole_app in
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
    let cdiff =
      Error.In_Compilation_unit(Error.diff impl_name intf_name reasons) in
    raise(Error(env, cdiff))
  | Ok x -> x

(* Functor diffing computation:
   The diffing computation uses the internal typing function
 *)

module Functor_inclusion_diff = struct
  open Diffing

  let param_name = function
      | Named(x,_) -> x
      | Unit -> None

  let weight = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) -> begin
        match param_name param1, param_name param2 with
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

  let lookup_expansion { env ; res ; _ } = match res with
    | None -> None
    | Some res ->
        match retrieve_functor_params env res with
        | [], _ -> None
        | params, res ->
            let more = Array.of_list params  in
            Some (keep_expansible_param res, more)

  let expand_params state  =
    match lookup_expansion state with
    | None -> state, [||]
    | Some (res, expansion) -> { state with res }, expansion

  let update d st = match d with
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
        expand_params { st with env }
    | Keep (Named (name1, _), Named (name2, arg2), _)
    | Change (Named (name1, _), Named (name2, arg2), _) -> begin
        let arg' = Subst.modtype Keep st.subst arg2 in
        match name1, name2 with
        | Some p1, Some p2 ->
            let env = Env.add_module p1 Mp_present arg' st.env in
            let subst = Subst.add_module p2 (Path.Pident p1) st.subst in
            expand_params { st with env; subst }
        | None, Some p2 ->
            let env = Env.add_module p2 Mp_present arg' st.env in
            { st with env }, [||]
        | Some p1, None ->
            let env = Env.add_module p1 Mp_present arg' st.env in
            expand_params { st with env }
        | None, None ->
            st, [||]
      end

  let diff env (l1,res1) (l2,_) =
    let update = Diffing.With_left_extensions update in
    let test st mty1 mty2 =
      let loc = Location.none in
      let res, _, _ =
        functor_param ~loc st.env ~mark:Mark_neither st.subst mty1 mty2
      in
      res
    in
    let param1 = Array.of_list l1 in
    let param2 = Array.of_list l2 in
    let state =
      { env; subst = Subst.identity; res = keep_expansible_param res1}
    in
    Diffing.variadic_diff ~weight ~test ~update state param1 param2

end

module Functor_app_diff = struct
  module I = Functor_inclusion_diff
  open Diffing

  let weight = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Change _ -> 10
    | Keep (param1, param2, _) ->
        (* We assign a small penalty to named arguments with
           non-matching names *)
        begin
          let desc1 : Error.functor_arg_descr = fst param1 in
          match desc1, I.param_name param2 with
          | (Unit | Anonymous) , None
            -> 0
          | Named (Path.Pident n1), Some n2
            when String.equal (Ident.name n1) (Ident.name n2)
            -> 0
          | Named _, Some _ -> 1
          | Named _,  None | (Unit | Anonymous), Some _ -> 1
        end

  let update (d: (_,Types.functor_parameter,_,_) change) (st:I.state) =
    let open Error in
    match d with
    | Insert _
    | Delete _
    | Keep ((Unit,_),_,_)
    | Keep (_,Unit,_)
    | Change (_,(Unit | Named (None,_)), _ )
    | Change ((Unit,_), Named (Some _, _), _) ->
        st, [||]
    | Keep ((Named arg,  _mty) , Named (param_name, _param), _)
    | Change ((Named arg, _mty), Named (param_name, _param), _) ->
        begin match param_name with
        | Some param ->
            let res =
              Option.map (fun res ->
                  let scope = Ctype.create_scope () in
                  let subst = Subst.add_module param arg Subst.identity in
                  Subst.modtype (Rescope scope) subst res
                )
                st.res
            in
            let subst = Subst.add_module param arg st.subst in
            I.expand_params { st with subst; res }
        | None ->
            st, [||]
        end
    | Keep ((Anonymous, mty) , Named (param_name, _param), _)
    | Change ((Anonymous, mty), Named (param_name, _param), _) -> begin
        begin match param_name with
        | Some param ->
            let mty' = Subst.modtype Keep st.subst mty in
            let env =
              Env.add_module ~arg:true param Mp_present mty' st.env in
            let res =
              Option.map (Mtype.nondep_supertype env [param]) st.res in
            I.expand_params { st with env; res}
        | None ->
            st, [||]
        end
      end

  let diff env ~f ~args =
    let params, res = retrieve_functor_params env f in
    let update = Diffing.With_right_extensions update in
    let test (state:I.state) (arg,arg_mty) param =
      let loc = Location.none in
      let res = match (arg:Error.functor_arg_descr), param with
        | Unit, Unit -> Ok Tcoerce_none
        | Unit, Named _ | (Anonymous | Named _), Unit ->
            Result.Error (Error.Incompatible_params(arg,param))
        | ( Anonymous | Named _ ) , Named (_, param) ->
            match
              modtypes ~loc state.env ~mark:Mark_neither state.subst
                arg_mty param
            with
            | Error mty -> Result.Error (Error.Mismatch mty)
            | Ok _ as x -> x
      in
      res
    in
    let args = Array.of_list args in
    let params = Array.of_list params in
    let state : I.state =
      { env; subst = Subst.identity; res = I.keep_expansible_param res }
    in
    Diffing.variadic_diff ~weight ~test ~update state args params

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
