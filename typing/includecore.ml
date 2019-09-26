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

(* Inclusion checks for the core language *)

open Asttypes
open Path
open Types
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions ~loc env name
    (vd1 : Types.value_description)
    (vd2 : Types.value_description) =
  Builtin_attributes.check_alerts_inclusion
    ~def:vd1.val_loc
    ~use:vd2.val_loc
    loc
    vd1.val_attributes vd2.val_attributes
    name;
  if Ctype.moregeneral env true vd1.val_type vd2.val_type then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim p1, Val_prim p2) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) ->
          let pc = {pc_desc = p; pc_type = vd2.Types.val_type;
                  pc_env = env; pc_loc = vd1.Types.val_loc; } in
          Tcoerce_primitive pc
      | (_, Val_prim _) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

(* Inclusion between "private" annotations *)

let private_flags decl1 decl2 =
  match decl1.type_private, decl2.type_private with
  | Private, Public ->
      decl2.type_kind = Type_abstract &&
      (decl2.type_manifest = None || decl1.type_kind <> Type_abstract)
  | _, _ -> true

(* Inclusion between manifest types (particularly for private row types) *)

let is_absrow env ty =
  match ty.desc with
    Tconstr(Pident _, _, _) ->
      begin match Ctype.expand_head env ty with
        {desc=Tobject _|Tvariant _} -> true
      | _ -> false
      end
  | _ -> false

let type_manifest env ty1 params1 ty2 params2 priv2 =
  let ty1' = Ctype.expand_head env ty1 and ty2' = Ctype.expand_head env ty2 in
  match ty1'.desc, ty2'.desc with
    Tvariant row1, Tvariant row2 when is_absrow env (Btype.row_more row2) ->
      let row1 = Btype.row_repr row1 and row2 = Btype.row_repr row2 in
      Ctype.equal env true (ty1::params1) (row2.row_more::params2) &&
      begin match row1.row_more with
        {desc=Tvar _|Tconstr _|Tnil} -> true
      | _ -> false
      end &&
      let r1, r2, pairs =
        Ctype.merge_row_fields row1.row_fields row2.row_fields in
      (not row2.row_closed ||
       row1.row_closed && Ctype.filter_row_fields false r1 = []) &&
      List.for_all
        (fun (_,f) -> match Btype.row_field_repr f with
          Rabsent | Reither _ -> true | Rpresent _ -> false)
        r2 &&
      let to_equal = ref (List.combine params1 params2) in
      List.for_all
        (fun (_, f1, f2) ->
          match Btype.row_field_repr f1, Btype.row_field_repr f2 with
            Rpresent(Some t1),
            (Rpresent(Some t2) | Reither(false, [t2], _, _)) ->
              to_equal := (t1,t2) :: !to_equal; true
          | Rpresent None, (Rpresent None | Reither(true, [], _, _)) -> true
          | Reither(c1,tl1,_,_), Reither(c2,tl2,_,_)
            when List.length tl1 = List.length tl2 && c1 = c2 ->
              to_equal := List.combine tl1 tl2 @ !to_equal; true
          | Rabsent, (Reither _ | Rabsent) -> true
          | _ -> false)
        pairs &&
      let tl1, tl2 = List.split !to_equal in
      Ctype.equal env true tl1 tl2
  | Tobject (fi1, _), Tobject (fi2, _)
    when is_absrow env (snd(Ctype.flatten_fields fi2)) ->
      let (fields2,rest2) = Ctype.flatten_fields fi2 in
      Ctype.equal env true (ty1::params1) (rest2::params2) &&
      let (fields1,rest1) = Ctype.flatten_fields fi1 in
      (match rest1 with {desc=Tnil|Tvar _|Tconstr _} -> true | _ -> false) &&
      let pairs, _miss1, miss2 = Ctype.associate_fields fields1 fields2 in
      miss2 = [] &&
      let tl1, tl2 =
        List.split (List.map (fun (_,_,t1,_,t2) -> t1, t2) pairs) in
      Ctype.equal env true (params1 @ tl1) (params2 @ tl2)
  | _ ->
      let rec check_super ty1 =
        Ctype.equal env true (ty1 :: params1) (ty2 :: params2) ||
        priv2 = Private &&
        try check_super
              (Ctype.try_expand_once_opt env (Ctype.expand_head env ty1))
        with Ctype.Cannot_expand -> false
      in check_super ty1

(* Inclusion between type declarations *)

type position = Ctype.Unification_trace.position = First | Second

let choose ord first second =
  match ord with
  | First -> first
  | Second -> second

let choose_other ord first second =
  match ord with
  | First -> choose Second first second
  | Second -> choose First first second

type label_mismatch =
  | Type
  | Mutability of position

type record_mismatch =
  | Label_mismatch of Types.label_declaration
                      * Types.label_declaration
                      * label_mismatch
  | Label_names of int * Ident.t * Ident.t
  | Label_missing of position * Ident.t
  | Unboxed_float_representation of position

type constructor_mismatch =
  | Type
  | Arity
  | Inline_record of record_mismatch
  | Kind of position
  | Explicit_return_type of position

type variant_mismatch =
  | Constructor_mismatch of Types.constructor_declaration
                            * Types.constructor_declaration
                            * constructor_mismatch
  | Constructor_names of int * Ident.t * Ident.t
  | Constructor_missing of position * Ident.t

type extension_constructor_mismatch =
  | Constructor_privacy
  | Constructor_mismatch of Ident.t
                            * Types.extension_constructor
                            * Types.extension_constructor
                            * constructor_mismatch

type type_mismatch =
  | Arity
  | Privacy
  | Kind
  | Constraint
  | Manifest
  | Variance
  | Record_mismatch of record_mismatch
  | Variant_mismatch of variant_mismatch
  | Unboxed_representation of position
  | Immediate of Type_immediacy.Violation.t

let report_label_mismatch first second ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : label_mismatch) with
  | Type -> pr "The types are not equal."
  | Mutability ord ->
      pr "%s is mutable and %s is not."
        (String.capitalize_ascii  (choose ord first second))
        (choose_other ord first second)

let report_record_mismatch first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
  | Label_mismatch (l1, l2, err) ->
      pr
        "@[<hv>Fields do not match:@;<1 2>%a@ is not compatible with:\
         @;<1 2>%a@ %a"
        Printtyp.label l1
        Printtyp.label l2
        (report_label_mismatch first second) err
  | Label_names (n, name1, name2) ->
      pr "@[<hv>Fields number %i have different names, %s and %s.@]"
        n (Ident.name name1) (Ident.name name2)
  | Label_missing (ord, s) ->
      pr "@[<hv>The field %s is only present in %s %s.@]"
        (Ident.name s) (choose ord first second) decl
  | Unboxed_float_representation ord ->
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "uses unboxed float representation"

let report_constructor_mismatch first second decl ppf err =
  let pr fmt  = Format.fprintf ppf fmt in
  match (err : constructor_mismatch) with
  | Type -> pr "The types are not equal."
  | Arity -> pr "They have different arities."
  | Inline_record err -> report_record_mismatch first second decl ppf err
  | Kind ord ->
      pr "%s uses inline records and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Explicit_return_type ord ->
      pr "%s has explicit return type and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)

let report_variant_mismatch first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : variant_mismatch) with
  | Constructor_mismatch (c1, c2, err) ->
      pr
        "@[<hv>Constructors do not match:@;<1 2>%a@ is not compatible with:\
         @;<1 2>%a@ %a"
        Printtyp.constructor c1
        Printtyp.constructor c2
        (report_constructor_mismatch first second decl) err
  | Constructor_names (n, name1, name2) ->
      pr "Constructors number %i have different names, %s and %s."
        n (Ident.name name1) (Ident.name name2)
  | Constructor_missing (ord, s) ->
      pr "The constructor %s is only present in %s %s."
        (Ident.name s) (choose ord first second) decl

let report_extension_constructor_mismatch first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : extension_constructor_mismatch) with
  | Constructor_privacy -> pr "A private type would be revealed."
  | Constructor_mismatch (id, ext1, ext2, err) ->
      pr "@[<hv>Constructors do not match:@;<1 2>%a@ is not compatible with:\
          @;<1 2>%a@ %a@]"
        (Printtyp.extension_only_constructor id) ext1
        (Printtyp.extension_only_constructor id) ext2
        (report_constructor_mismatch first second decl) err

let report_type_mismatch0 first second decl ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
  | Arity -> pr "They have different arities."
  | Privacy -> pr "A private type would be revealed."
  | Kind -> pr "Their kinds differ."
  | Constraint -> pr "Their constraints differ."
  | Manifest -> ()
  | Variance -> pr "Their variances do not agree."
  | Record_mismatch err -> report_record_mismatch first second decl ppf err
  | Variant_mismatch err -> report_variant_mismatch first second decl ppf err
  | Unboxed_representation ord ->
      pr "Their internal representations differ:@ %s %s %s."
         (choose ord first second) decl
         "uses unboxed representation"
  | Immediate violation ->
      let first = StringLabels.capitalize_ascii first in
      match violation with
      | Type_immediacy.Violation.Not_always_immediate ->
          pr "%s is not an immediate type." first
      | Type_immediacy.Violation.Not_always_immediate_on_64bits ->
          pr "%s is not a type that is always immediate on 64 bit platforms."
            first

let report_type_mismatch first second decl ppf err =
  if err = Manifest then () else
  Format.fprintf ppf "@ %a" (report_type_mismatch0 first second decl) err

let rec compare_constructor_arguments ~loc env params1 params2 arg1 arg2 =
  match arg1, arg2 with
  | Types.Cstr_tuple arg1, Types.Cstr_tuple arg2 ->
      if List.length arg1 <> List.length arg2 then
        Some (Arity : constructor_mismatch)
      else if
        (* Ctype.equal must be called on all arguments at once, cf. PR#7378 *)
        Ctype.equal env true (params1 @ arg1) (params2 @ arg2)
      then None else Some Type
  | Types.Cstr_record l1, Types.Cstr_record l2 ->
      Option.map
        (fun rec_err -> Inline_record rec_err)
        (compare_records env ~loc params1 params2 0 l1 l2)
  | Types.Cstr_record _, _ -> Some (Kind First : constructor_mismatch)
  | _, Types.Cstr_record _ -> Some (Kind Second : constructor_mismatch)

and compare_constructors ~loc env params1 params2 res1 res2 args1 args2 =
  match res1, res2 with
  | Some r1, Some r2 ->
      if Ctype.equal env true [r1] [r2] then
        compare_constructor_arguments ~loc env [r1] [r2] args1 args2
      else Some Type
  | Some _, None -> Some (Explicit_return_type First)
  | None, Some _ -> Some (Explicit_return_type Second)
  | None, None ->
      compare_constructor_arguments ~loc env params1 params2 args1 args2

and compare_variants ~loc env params1 params2 n
    (cstrs1 : Types.constructor_declaration list)
    (cstrs2 : Types.constructor_declaration list) =
  match cstrs1, cstrs2 with
  | [], []   -> None
  | [], c::_ -> Some (Constructor_missing (Second, c.Types.cd_id))
  | c::_, [] -> Some (Constructor_missing (First, c.Types.cd_id))
  | cd1::rem1, cd2::rem2 ->
      if Ident.name cd1.cd_id <> Ident.name cd2.cd_id then
        Some (Constructor_names (n, cd1.cd_id, cd2.cd_id))
      else begin
        Builtin_attributes.check_alerts_inclusion
          ~def:cd1.cd_loc
          ~use:cd2.cd_loc
          loc
          cd1.cd_attributes cd2.cd_attributes
          (Ident.name cd1.cd_id);
        match compare_constructors ~loc env params1 params2
                cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
        | Some r ->
            Some ((Constructor_mismatch (cd1, cd2, r)) : variant_mismatch)
        | None -> compare_variants ~loc env params1 params2 (n+1) rem1 rem2
      end

and compare_labels env params1 params2
      (ld1 : Types.label_declaration)
      (ld2 : Types.label_declaration) =
      if ld1.ld_mutable <> ld2.ld_mutable
      then
        let ord = if ld1.ld_mutable = Asttypes.Mutable then First else Second in
        Some (Mutability  ord)
      else
        if Ctype.equal env true (ld1.ld_type::params1) (ld2.ld_type::params2)
        then None
        else Some (Type : label_mismatch)

and compare_records ~loc env params1 params2 n
    (labels1 : Types.label_declaration list)
    (labels2 : Types.label_declaration list) =
  match labels1, labels2 with
  | [], []           -> None
  | [], l::_ -> Some (Label_missing (Second, l.Types.ld_id))
  | l::_, [] -> Some (Label_missing (First, l.Types.ld_id))
  | ld1::rem1, ld2::rem2 ->
      if Ident.name ld1.ld_id <> Ident.name ld2.ld_id
      then Some (Label_names (n, ld1.ld_id, ld2.ld_id))
      else begin
        Builtin_attributes.check_deprecated_mutable_inclusion
          ~def:ld1.ld_loc
          ~use:ld2.ld_loc
          loc
          ld1.ld_attributes ld2.ld_attributes
          (Ident.name ld1.ld_id);
        match compare_labels env params1 params2 ld1 ld2 with
        | Some r -> Some (Label_mismatch (ld1, ld2, r))
        (* add arguments to the parameters, cf. PR#7378 *)
        | None -> compare_records ~loc env
                    (ld1.ld_type::params1) (ld2.ld_type::params2)
                    (n+1)
                    rem1 rem2
      end

let compare_records_with_representation ~loc env params1 params2 n
      labels1 labels2 rep1 rep2
  =
  match compare_records ~loc env params1 params2 n labels1 labels2 with
  | None when rep1 <> rep2 ->
      let pos = if rep2 = Record_float then Second else First in
      Some (Unboxed_float_representation pos)
  | err -> err

let type_declarations ?(equality = false) ~loc env ~mark name
      decl1 path decl2 =
  Builtin_attributes.check_alerts_inclusion
    ~def:decl1.type_loc
    ~use:decl2.type_loc
    loc
    decl1.type_attributes decl2.type_attributes
    name;
  if decl1.type_arity <> decl2.type_arity then Some Arity else
  if not (private_flags decl1 decl2) then Some Privacy else
  let err = match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        if Ctype.equal env true decl1.type_params decl2.type_params
        then None else Some Constraint
    | (Some ty1, Some ty2) ->
        if type_manifest env ty1 decl1.type_params ty2 decl2.type_params
            decl2.type_private
        then None else Some Manifest
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(path, decl2.type_params, ref Mnil))
        in
        if Ctype.equal env true decl1.type_params decl2.type_params then
          if Ctype.equal env false [ty1] [ty2] then None
          else Some Manifest
        else Some Constraint
  in
  if err <> None then err else
  let err =
    match (decl2.type_kind, decl1.type_unboxed.unboxed,
           decl2.type_unboxed.unboxed) with
    | Type_abstract, _, _ -> None
    | _, true, false -> Some (Unboxed_representation First)
    | _, false, true -> Some (Unboxed_representation Second)
    | _ -> None
  in
  if err <> None then err else
  let err = match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> None
    | (Type_variant cstrs1, Type_variant cstrs2) ->
        if mark then begin
          let mark usage name cstrs =
            List.iter
              (fun cstr ->
                 Env.mark_constructor_used usage name cstr)
              cstrs
          in
          let usage =
            if decl2.type_private = Public then Env.Positive
            else Env.Privatize
          in
          mark usage name cstrs1;
          if equality then mark Env.Positive (Path.name path) cstrs2
        end;
        Option.map
          (fun var_err -> Variant_mismatch var_err)
          (compare_variants ~loc env decl1.type_params decl2.type_params 1
             cstrs1 cstrs2)
    | (Type_record(labels1,rep1), Type_record(labels2,rep2)) ->
        Option.map (fun rec_err -> Record_mismatch rec_err)
          (compare_records_with_representation ~loc env
             decl1.type_params decl2.type_params 1
             labels1 labels2
             rep1 rep2)
    | (Type_open, Type_open) -> None
    | (_, _) -> Some Kind
  in
  if err <> None then err else
  let abstr = decl2.type_kind = Type_abstract && decl2.type_manifest = None in
  (* If attempt to assign a non-immediate type (e.g. string) to a type that
   * must be immediate, then we error *)
  let err =
    if not abstr then
      None
    else
      match
        Type_immediacy.coerce decl1.type_immediate ~as_:decl2.type_immediate
      with
      | Ok () -> None
      | Error violation -> Some (Immediate violation)
  in
  if err <> None then err else
  let need_variance =
    abstr || decl1.type_private = Private || decl1.type_kind = Type_open in
  if not need_variance then None else
  let abstr = abstr || decl2.type_private = Private in
  let opn = decl2.type_kind = Type_open && decl2.type_manifest = None in
  let constrained ty = not (Btype.(is_Tvar (repr ty))) in
  if List.for_all2
      (fun ty (v1,v2) ->
        let open Variance in
        let imp a b = not a || b in
        let (co1,cn1) = get_upper v1 and (co2,cn2) = get_upper v2 in
        (if abstr then (imp co1 co2 && imp cn1 cn2)
         else if opn || constrained ty then (co1 = co2 && cn1 = cn2)
         else true) &&
        let (p1,n1,i1,j1) = get_lower v1 and (p2,n2,i2,j2) = get_lower v2 in
        imp abstr (imp p2 p1 && imp n2 n1 && imp i2 i1 && imp j2 j1))
      decl2.type_params (List.combine decl1.type_variance decl2.type_variance)
  then None else Some Variance

(* Inclusion between extension constructors *)

let extension_constructors ~loc env ~mark id ext1 ext2 =
  if mark then begin
    let usage =
      if ext2.ext_private = Public then Env.Positive
      else Env.Privatize
    in
    Env.mark_extension_used usage (Ident.name id) ext1
  end;
  let ty1 =
    Btype.newgenty (Tconstr(ext1.ext_type_path, ext1.ext_type_params, ref Mnil))
  in
  let ty2 =
    Btype.newgenty (Tconstr(ext2.ext_type_path, ext2.ext_type_params, ref Mnil))
  in
  if not (Ctype.equal env true (ty1 :: ext1.ext_type_params)
                               (ty2 :: ext2.ext_type_params))
  then Some (Constructor_mismatch (id, ext1, ext2, Type))
  else
    let r =
      compare_constructors ~loc env ext1.ext_type_params ext2.ext_type_params
        ext1.ext_ret_type ext2.ext_ret_type
        ext1.ext_args ext2.ext_args
    in
    match r with
    | Some r -> Some (Constructor_mismatch (id, ext1, ext2, r))
    | None -> match ext1.ext_private, ext2.ext_private with
        Private, Public -> Some Constructor_privacy
      | _, _ -> None
