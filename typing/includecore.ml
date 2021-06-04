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

type position = Errortrace.position = First | Second

(* Inclusion between value descriptions *)

type primitive_mismatch =
  | Name
  | Arity
  | No_alloc of position
  | Native_name
  | Result_repr
  | Argument_repr of int

let native_repr_args nra1 nra2 =
  let rec loop i nra1 nra2 =
    match nra1, nra2 with
    | [], [] -> None
    | [], _ :: _ -> assert false
    | _ :: _, [] -> assert false
    | nr1 :: nra1, nr2 :: nra2 ->
      if not (Primitive.equal_native_repr nr1 nr2) then Some (Argument_repr i)
      else loop (i+1) nra1 nra2
  in
  loop 1 nra1 nra2

let primitive_descriptions pd1 pd2 =
  let open Primitive in
  if not (String.equal pd1.prim_name pd2.prim_name) then
    Some Name
  else if not (Int.equal pd1.prim_arity pd2.prim_arity) then
    Some Arity
  else if (not pd1.prim_alloc) && pd2.prim_alloc then
    Some (No_alloc First)
  else if pd1.prim_alloc && (not pd2.prim_alloc) then
    Some (No_alloc Second)
  else if not (String.equal pd1.prim_native_name pd2.prim_native_name) then
    Some Native_name
  else if not
    (Primitive.equal_native_repr
       pd1.prim_native_repr_res pd2.prim_native_repr_res) then
    Some Result_repr
  else
    native_repr_args pd1.prim_native_repr_args pd2.prim_native_repr_args

type value_mismatch =
  | Primitive_mismatch of primitive_mismatch
  | Not_a_primitive
  | Type of Env.t * Errortrace.comparison Errortrace.t

exception Dont_match of value_mismatch

let value_descriptions ~loc env name
    (vd1 : Types.value_description)
    (vd2 : Types.value_description) =
  Builtin_attributes.check_alerts_inclusion
    ~def:vd1.val_loc
    ~use:vd2.val_loc
    loc
    vd1.val_attributes vd2.val_attributes
    name;
  match Ctype.moregeneral env true vd1.val_type vd2.val_type with
  | exception Ctype.Moregen trace -> raise (Dont_match (Type (env, trace)))
  | () -> begin
      match (vd1.val_kind, vd2.val_kind) with
      | (Val_prim p1, Val_prim p2) -> begin
          match primitive_descriptions p1 p2 with
          | None -> Tcoerce_none
          | Some err -> raise (Dont_match (Primitive_mismatch err))
        end
      | (Val_prim p, _) ->
          let pc =
            { pc_desc = p; pc_type = vd2.Types.val_type;
              pc_env = env; pc_loc = vd1.Types.val_loc; }
          in
          Tcoerce_primitive pc
      | (_, Val_prim _) -> raise (Dont_match Not_a_primitive)
      | (_, _) -> Tcoerce_none
    end

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
  | Tconstr(Pident _, _, _) -> begin
      match Ctype.expand_head env ty with
      | {desc=Tobject _|Tvariant _} -> true
      | _ -> false
      end
  | _ -> false

(* Inclusion between type declarations *)

let choose ord first second =
  match ord with
  | First -> first
  | Second -> second

let choose_other ord first second =
  match ord with
  | First -> choose Second first second
  | Second -> choose First first second

type label_mismatch =
  | Type of Env.t * Errortrace.comparison Errortrace.t
  | Mutability of position

type record_mismatch =
  | Label_mismatch of Types.label_declaration
                      * Types.label_declaration
                      * label_mismatch
  | Label_names of int * Ident.t * Ident.t
  | Label_missing of position * Ident.t
  | Unboxed_float_representation of position

type constructor_mismatch =
  | Type of Env.t * Errortrace.comparison Errortrace.t
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

type private_variant_mismatch =
  | Openness
  | Missing of position * string
  | Presence of string
  | Incompatible_types_for of string
  | Types of Env.t * Errortrace.comparison Errortrace.t

type private_object_mismatch =
  | Missing of string
  | Types of Env.t * Errortrace.comparison Errortrace.t

type type_mismatch =
  | Arity
  | Privacy
  | Kind
  | Constraint of Env.t * Errortrace.comparison Errortrace.t
  | Manifest of Env.t * Errortrace.comparison Errortrace.t
  | Private_variant of type_expr * type_expr * private_variant_mismatch
  | Private_object of type_expr * type_expr * private_object_mismatch
  | Variance
  | Record_mismatch of record_mismatch
  | Variant_mismatch of variant_mismatch
  | Unboxed_representation of position
  | Immediate of Type_immediacy.Violation.t

let report_label_mismatch first second ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : label_mismatch) with
  | Type _ -> pr "The types are not equal."
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
         @;<1 2>%a@ %a@]"
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
  | Type _ -> pr "The types are not equal."
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
         @;<1 2>%a@ %a@]"
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
  | Constraint _ -> pr "Their constraints differ."
  | Manifest _ -> ()
  | Private_variant _ -> ()
  | Private_object _ -> ()
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
  match err with
  | Manifest _ -> ()
  | Private_variant _ -> ()
  | Private_object _ -> ()
  | _ -> Format.fprintf ppf "@ %a" (report_type_mismatch0 first second decl) err

let rec compare_constructor_arguments ~loc env params1 params2 arg1 arg2 =
  match arg1, arg2 with
  | Types.Cstr_tuple arg1, Types.Cstr_tuple arg2 ->
      if List.length arg1 <> List.length arg2 then
        Some (Arity : constructor_mismatch)
      else begin
        (* Ctype.equal must be called on all arguments at once, cf. PR#7378 *)
        match Ctype.equal env true (params1 @ arg1) (params2 @ arg2) with
        | exception Ctype.Equality trace -> Some (Type (env, trace))
        | () -> None
      end
  | Types.Cstr_record l1, Types.Cstr_record l2 ->
      Option.map
        (fun rec_err -> Inline_record rec_err)
        (compare_records env ~loc params1 params2 0 l1 l2)
  | Types.Cstr_record _, _ -> Some (Kind First : constructor_mismatch)
  | _, Types.Cstr_record _ -> Some (Kind Second : constructor_mismatch)

and compare_constructors ~loc env params1 params2 res1 res2 args1 args2 =
  match res1, res2 with
  | Some r1, Some r2 -> begin
      match Ctype.equal env true [r1] [r2] with
      | exception Ctype.Equality trace -> Some (Type (env, trace))
      | () -> compare_constructor_arguments ~loc env [r1] [r2] args1 args2
    end
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

and compare_variants_with_representation ~loc env params1 params2 n
      cstrs1 cstrs2 rep1 rep2
  =
  let err = compare_variants ~loc env params1 params2 n cstrs1 cstrs2 in
  match err, rep1, rep2 with
  | None, Variant_regular, Variant_regular
  | None, Variant_unboxed, Variant_unboxed ->
     None
  | Some err, _, _ ->
     Some (Variant_mismatch err)
  | None, Variant_unboxed, Variant_regular ->
     Some (Unboxed_representation First)
  | None, Variant_regular, Variant_unboxed ->
     Some (Unboxed_representation Second)

and compare_labels env params1 params2
      (ld1 : Types.label_declaration) (ld2 : Types.label_declaration) =
  if ld1.ld_mutable <> ld2.ld_mutable then begin
    let ord = if ld1.ld_mutable = Asttypes.Mutable then First else Second in
    Some (Mutability  ord)
  end else begin
    let tl1 = params1 @ [ld1.ld_type] in
    let tl2 = params2 @ [ld2.ld_type] in
    match Ctype.equal env true tl1 tl2 with
    | exception Ctype.Equality trace ->
        Some (Type (env, trace) : label_mismatch)
    | () -> None
  end

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
  | Some err -> Some (Record_mismatch err)
  | None ->
     match rep1, rep2 with
     | Record_unboxed _, Record_unboxed _ -> None
     | Record_unboxed _, _ -> Some (Unboxed_representation First)
     | _, Record_unboxed _ -> Some (Unboxed_representation Second)

     | Record_float, Record_float -> None
     | Record_float, _ ->
        Some (Record_mismatch (Unboxed_float_representation First))
     | _, Record_float ->
        Some (Record_mismatch (Unboxed_float_representation Second))

     | Record_regular, Record_regular
     | Record_inlined _, Record_inlined _
     | Record_extension _, Record_extension _ -> None
     | (Record_regular|Record_inlined _|Record_extension _),
       (Record_regular|Record_inlined _|Record_extension _) ->
        assert false

let private_variant env row1 params1 row2 params2 =
    let r1, r2, pairs =
      Ctype.merge_row_fields row1.row_fields row2.row_fields
    in
    let err =
      if row2.row_closed && not row1.row_closed then Some Openness
      else begin
        match row2.row_closed, Ctype.filter_row_fields false r1 with
        | true, (s, _) :: _ ->
            Some (Missing (Second, s) : private_variant_mismatch)
        | _, _ -> None
      end
    in
    if err <> None then err else
    let err =
      let missing =
        List.find_opt
          (fun (_,f) ->
             match Btype.row_field_repr f with
             | Rabsent | Reither _ -> false
             | Rpresent _ -> true)
          r2
      in
      match missing with
      | None -> None
      | Some (s, _) -> Some (Missing (First, s) : private_variant_mismatch)
    in
    if err <> None then err else
    let rec loop tl1 tl2 pairs =
      match pairs with
      | [] -> begin
          match Ctype.equal env true tl1 tl2 with
          | exception Ctype.Equality trace ->
              Some (Types (env, trace) : private_variant_mismatch)
          | () -> None
        end
      | (s, f1, f2) :: pairs -> begin
          match Btype.row_field_repr f1, Btype.row_field_repr f2 with
          | Rpresent to1, Rpresent to2 -> begin
              match to1, to2 with
              | Some t1, Some t2 ->
                  loop (t1 :: tl1) (t2 :: tl2) pairs
              | None, None ->
                  loop tl1 tl2 pairs
              | Some _, None | None, Some _ ->
                  Some (Incompatible_types_for s)
            end
          | Rpresent to1, Reither(const2, tl2, _, _) -> begin
              match to1, const2, tl2 with
              | Some t1, false, [t2] -> loop (t1 :: tl1) (t2 :: tl2) pairs
              | None, true, [] -> loop tl1 tl2 pairs
              | _, _, _ -> Some (Incompatible_types_for s)
            end
          | Rpresent _, Rabsent ->
              Some (Missing (Second, s) : private_variant_mismatch)
          | Reither(const1, ts1, _, _), Reither(const2, ts2, _, _) ->
              if const1 = const2 && List.length ts1 = List.length ts2 then
                loop (ts1 @ tl1) (ts2 @ tl2) pairs
              else
                Some (Incompatible_types_for s)
          | Reither _, Rpresent _ ->
              Some (Presence s)
          | Reither _, Rabsent ->
              Some (Missing (Second, s) : private_variant_mismatch)
          | Rabsent, (Reither _ | Rabsent) ->
              loop tl1 tl2 pairs
          | Rabsent, Rpresent _ ->
              Some (Missing (First, s) : private_variant_mismatch)
        end
    in
    loop params1 params2 pairs

let private_object env fields1 params1 fields2 params2 =
  let pairs, _miss1, miss2 = Ctype.associate_fields fields1 fields2 in
  let err =
    match miss2 with
    | [] -> None
    | (f, _, _) :: _ -> Some (Missing f)
  in
  if err <> None then err else
  let tl1, tl2 =
    List.split (List.map (fun (_,_,t1,_,t2) -> t1, t2) pairs)
  in
  begin
    match Ctype.equal env true (params1 @ tl1) (params2 @ tl2) with
    | exception Ctype.Equality trace -> Some (Types (env, trace))
    | () -> None
  end

let type_manifest env ty1 params1 ty2 params2 priv2 kind2 =
  let ty1' = Ctype.expand_head env ty1 and ty2' = Ctype.expand_head env ty2 in
  match ty1'.desc, ty2'.desc with
  | Tvariant row1, Tvariant row2
    when is_absrow env (Btype.row_more row2) -> begin
      let row1 = Btype.row_repr row1 and row2 = Btype.row_repr row2 in
      assert (Ctype.is_equal env true (ty1::params1) (row2.row_more::params2));
      match private_variant env row1 params1 row2 params2 with
      | None -> None
      | Some err -> Some (Private_variant(ty1, ty2, err))
    end
  | Tobject (fi1, _), Tobject (fi2, _)
    when is_absrow env (snd (Ctype.flatten_fields fi2)) -> begin
      let (fields2,rest2) = Ctype.flatten_fields fi2 in
      let (fields1,_) = Ctype.flatten_fields fi1 in
      assert (Ctype.is_equal env true (ty1::params1) (rest2::params2));
      match private_object env fields1 params1 fields2 params2 with
      | None -> None
      | Some err -> Some (Private_object(ty1, ty2, err))
    end
  | _ -> begin
      let is_private_abbrev_2 =
        match priv2, kind2 with
        | Private, Type_abstract -> begin
            (* Same checks as the [when] guards from above, inverted *)
            match ty2'.desc with
            | Tvariant row ->
                not (is_absrow env (Btype.row_more row))
            | Tobject (fi, _) ->
                not (is_absrow env (snd (Ctype.flatten_fields fi)))
            | _ -> true
          end
        | _, _ -> false
      in
      match
        if is_private_abbrev_2 then
          Ctype.equal_private env params1 ty1 params2 ty2
        else
          Ctype.equal env true (params1 @ [ty1]) (params2 @ [ty2])
      with
      | exception Ctype.Equality err -> Some (Manifest(env,err))
      | () -> None
    end

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
        begin
          match Ctype.equal env true decl1.type_params decl2.type_params with
          | exception Ctype.Equality trace -> Some (Constraint(env, trace))
          | () -> None
        end
    | (Some ty1, Some ty2) ->
         type_manifest env ty1 decl1.type_params ty2 decl2.type_params
           decl2.type_private decl2.type_kind
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(path, decl2.type_params, ref Mnil))
        in
        match Ctype.equal env true decl1.type_params decl2.type_params with
        | exception Ctype.Equality trace -> Some (Constraint(env, trace))
        | () ->
          match Ctype.equal env false [ty1] [ty2] with
          | exception Ctype.Equality trace -> Some (Manifest(env, trace))
          | () -> None
  in
  if err <> None then err else
  let err = match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> None
    | (Type_variant (cstrs1, rep1), Type_variant (cstrs2, rep2)) ->
        if mark then begin
          let mark usage cstrs =
            List.iter (Env.mark_constructor_used usage) cstrs
          in
          let usage : Env.constructor_usage =
            if decl2.type_private = Public then Env.Exported
            else Env.Exported_private
          in
          mark usage cstrs1;
          if equality then mark Env.Exported cstrs2
        end;
        compare_variants_with_representation ~loc env
          decl1.type_params decl2.type_params 1
          cstrs1 cstrs2
          rep1 rep2
    | (Type_record(labels1,rep1), Type_record(labels2,rep2)) ->
        if mark then begin
          let mark usage lbls =
            List.iter (Env.mark_label_used usage) lbls
          in
          let usage : Env.label_usage =
            if decl2.type_private = Public then Env.Exported
            else Env.Exported_private
          in
          mark usage labels1;
          if equality then mark Env.Exported labels2
        end;
        compare_records_with_representation ~loc env
          decl1.type_params decl2.type_params 1
          labels1 labels2
          rep1 rep2
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
    let usage : Env.constructor_usage =
      if ext2.ext_private = Public then Env.Exported
      else Env.Exported_private
    in
    Env.mark_extension_used usage ext1
  end;
  let ty1 =
    Btype.newgenty (Tconstr(ext1.ext_type_path, ext1.ext_type_params, ref Mnil))
  in
  let ty2 =
    Btype.newgenty (Tconstr(ext2.ext_type_path, ext2.ext_type_params, ref Mnil))
  in
  let tl1 = ty1 :: ext1.ext_type_params in
  let tl2 = ty2 :: ext2.ext_type_params in
  match Ctype.equal env true tl1 tl2 with
  | exception Ctype.Equality trace ->
      Some (Constructor_mismatch (id, ext1, ext2, Type(env, trace)))
  | () ->
    let r =
      compare_constructors ~loc env
        ext1.ext_type_params ext2.ext_type_params
        ext1.ext_ret_type ext2.ext_ret_type
        ext1.ext_args ext2.ext_args
    in
    match r with
    | Some r -> Some (Constructor_mismatch (id, ext1, ext2, r))
    | None ->
      match ext1.ext_private, ext2.ext_private with
      | Private, Public -> Some Constructor_privacy
      | _, _ -> None
