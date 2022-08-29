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
  | Type of Errortrace.moregen_error

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
  | exception Ctype.Moregen err -> raise (Dont_match (Type err))
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

(* Inclusion between manifest types (particularly for private row types) *)

let is_absrow env ty =
  match get_desc ty with
  | Tconstr(Pident _, _, _) ->
      (* This function is checking for an abstract row on the side that is being
         included into (usually numbered with "2" in this file).  In this case,
         the abstract row variable has been substituted for an object or variant
         type. *)
      begin match get_desc (Ctype.expand_head env ty) with
      | Tobject _|Tvariant _ -> true
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

(* Documents which kind of private thing would be revealed *)
type privacy_mismatch =
  | Private_type_abbreviation
  | Private_variant_type
  | Private_record_type
  | Private_extensible_variant
  | Private_row_type

type type_kind =
  | Kind_abstract
  | Kind_record
  | Kind_variant
  | Kind_open

let of_kind = function
  | Type_abstract -> Kind_abstract
  | Type_record (_, _) -> Kind_record
  | Type_variant (_, _) -> Kind_variant
  | Type_open -> Kind_open

type kind_mismatch = type_kind * type_kind

type label_mismatch =
  | Type of Errortrace.equality_error
  | Mutability of position

type record_change =
  (Types.label_declaration, Types.label_declaration, label_mismatch)
    Diffing_with_keys.change

type record_mismatch =
  | Label_mismatch of record_change list
  | Unboxed_float_representation of position

type constructor_mismatch =
  | Type of Errortrace.equality_error
  | Arity
  | Inline_record of record_change list
  | Kind of position
  | Explicit_return_type of position

type extension_constructor_mismatch =
  | Constructor_privacy
  | Constructor_mismatch of Ident.t
                            * Types.extension_constructor
                            * Types.extension_constructor
                            * constructor_mismatch

type private_variant_mismatch =
  | Only_outer_closed (* It's only dangerous in one direction *)
  | Missing of position * string
  | Presence of string
  | Incompatible_types_for of string
  | Types of Errortrace.equality_error

type private_object_mismatch =
  | Missing of string
  | Types of Errortrace.equality_error

type variant_change =
  (Types.constructor_declaration as 'l, 'l, constructor_mismatch)
    Diffing_with_keys.change

type type_mismatch =
  | Arity
  | Privacy of privacy_mismatch
  | Kind of kind_mismatch
  | Constraint of Errortrace.equality_error
  | Manifest of Errortrace.equality_error
  | Private_variant of type_expr * type_expr * private_variant_mismatch
  | Private_object of type_expr * type_expr * private_object_mismatch
  | Variance
  | Record_mismatch of record_mismatch
  | Variant_mismatch of variant_change list
  | Unboxed_representation of position
  | Immediate of Type_immediacy.Violation.t

let report_primitive_mismatch first second ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : primitive_mismatch) with
  | Name ->
      pr "The names of the primitives are not the same"
  | Arity ->
      pr "The syntactic arities of these primitives were not the same.@ \
          (They must have the same number of arrows present in the source.)"
  | No_alloc ord ->
      pr "%s primitive is [@@@@noalloc] but %s is not"
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Native_name ->
      pr "The native names of the primitives are not the same"
  | Result_repr ->
      pr "The two primitives' results have different representations"
  | Argument_repr n ->
      pr "The two primitives' %d%s arguments have different representations"
        n (Misc.ordinal_suffix n)

let report_value_mismatch first second env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  pr "@ ";
  match (err : value_mismatch) with
  | Primitive_mismatch pm ->
      report_primitive_mismatch first second ppf pm
  | Not_a_primitive ->
      pr "The implementation is not a primitive."
  | Type trace ->
      Printtyp.report_moregen_error ppf Type_scheme env trace
        (fun ppf -> Format.fprintf ppf "The type")
        (fun ppf -> Format.fprintf ppf "is not compatible with the type")

let report_type_inequality env ppf err =
  Printtyp.report_equality_error ppf Type_scheme env err
    (fun ppf -> Format.fprintf ppf "The type")
    (fun ppf -> Format.fprintf ppf "is not equal to the type")

let report_privacy_mismatch ppf err =
  let singular, item =
    match err with
    | Private_type_abbreviation  -> true,  "type abbreviation"
    | Private_variant_type       -> false, "variant constructor(s)"
    | Private_record_type        -> true,  "record constructor"
    | Private_extensible_variant -> true,  "extensible variant"
    | Private_row_type           -> true,  "row type"
  in Format.fprintf ppf "%s %s would be revealed."
       (if singular then "A private" else "Private")
       item

let report_label_mismatch first second env ppf err =
  match (err : label_mismatch) with
  | Type err ->
      report_type_inequality env ppf err
  | Mutability ord ->
      Format.fprintf ppf "%s is mutable and %s is not."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)

let pp_record_diff first second prefix decl env ppf (x : record_change) =
  match x with
  | Delete cd ->
      Format.fprintf ppf "%aAn extra field, %s, is provided in %s %s."
        prefix x (Ident.name cd.delete.ld_id) first decl
  | Insert cd ->
      Format.fprintf  ppf "%aA field, %s, is missing in %s %s."
        prefix x (Ident.name cd.insert.ld_id) first decl
  | Change Type {got=lbl1; expected=lbl2; reason} ->
      Format.fprintf ppf
        "@[<hv>%aFields do not match:@;<1 2>\
         %a@ is not the same as:\
         @;<1 2>%a@ %a@]"
        prefix x
        Printtyp.label lbl1
        Printtyp.label lbl2
        (report_label_mismatch first second env) reason
  | Change Name n ->
      Format.fprintf ppf "%aFields have different names, %s and %s."
        prefix x n.got n.expected
  | Swap sw ->
      Format.fprintf ppf "%aFields %s and %s have been swapped."
        prefix x sw.first sw.last
  | Move {name; got; expected } ->
      Format.fprintf ppf
        "@[<2>%aField %s has been moved@ from@ position %d@ to %d.@]"
        prefix x name expected got

let report_patch pr_diff first second decl env ppf patch =
  let nl ppf () = Format.fprintf ppf "@," in
  let no_prefix _ppf _ = () in
  match patch with
  | [ elt ] ->
      Format.fprintf ppf "@[<hv>%a@]"
        (pr_diff first second no_prefix decl env) elt
  | _ ->
      let pp_diff = pr_diff first second Diffing_with_keys.prefix decl env in
      Format.fprintf ppf "@[<hv>%a@]"
        (Format.pp_print_list ~pp_sep:nl pp_diff) patch

let report_record_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match err with
  | Label_mismatch patch ->
      report_patch pp_record_diff first second decl env ppf patch
  | Unboxed_float_representation ord ->
      pr "@[<hv>Their internal representations differ:@ %s %s %s.@]"
        (choose ord first second) decl
        "uses unboxed float representation"

let report_constructor_mismatch first second decl env ppf err =
  let pr fmt  = Format.fprintf ppf fmt in
  match (err : constructor_mismatch) with
  | Type err -> report_type_inequality env ppf err
  | Arity -> pr "They have different arities."
  | Inline_record err ->
      report_patch pp_record_diff first second decl env ppf err
  | Kind ord ->
      pr "%s uses inline records and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)
  | Explicit_return_type ord ->
      pr "%s has explicit return type and %s doesn't."
        (String.capitalize_ascii (choose ord first second))
        (choose_other ord first second)

let pp_variant_diff first second prefix decl env ppf (x : variant_change) =
  match x with
  | Delete cd ->
      Format.fprintf ppf  "%aAn extra constructor, %s, is provided in %s %s."
        prefix x (Ident.name cd.delete.cd_id) first decl
  | Insert cd ->
      Format.fprintf ppf "%aA constructor, %s, is missing in %s %s."
        prefix x (Ident.name cd.insert.cd_id) first decl
  | Change Type {got; expected; reason} ->
      Format.fprintf ppf
        "@[<hv>%aConstructors do not match:@;<1 2>\
         %a@ is not the same as:\
         @;<1 2>%a@ %a@]"
        prefix x
        Printtyp.constructor got
        Printtyp.constructor expected
        (report_constructor_mismatch first second decl env) reason
  | Change Name n ->
      Format.fprintf ppf
        "%aConstructors have different names, %s and %s."
        prefix x n.got n.expected
  | Swap sw ->
      Format.fprintf ppf
        "%aConstructors %s and %s have been swapped."
        prefix x sw.first sw.last
  | Move {name; got; expected} ->
      Format.fprintf ppf
        "@[<2>%aConstructor %s has been moved@ from@ position %d@ to %d.@]"
        prefix x name expected got

let report_extension_constructor_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : extension_constructor_mismatch) with
  | Constructor_privacy ->
      pr "Private extension constructor(s) would be revealed."
  | Constructor_mismatch (id, ext1, ext2, err) ->
      pr "@[<hv>Constructors do not match:@;<1 2>%a@ is not the same as:\
          @;<1 2>%a@ %a@]"
        (Printtyp.extension_only_constructor id) ext1
        (Printtyp.extension_only_constructor id) ext2
        (report_constructor_mismatch first second decl env) err

let report_private_variant_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : private_variant_mismatch) with
  | Only_outer_closed ->
      (* It's only dangerous in one direction, so we don't have a position *)
      pr "%s is private and closed, but %s is not closed"
        (String.capitalize_ascii second) first
  | Missing (ord, name) ->
      pr "The constructor %s is only present in %s %s."
        name (choose ord first second) decl
  | Presence s ->
      pr "The tag `%s is present in the %s %s,@ but might not be in the %s"
        s second decl first
  | Incompatible_types_for s -> pr "Types for tag `%s are incompatible" s
  | Types err ->
      report_type_inequality env ppf err

let report_private_object_mismatch env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  match (err : private_object_mismatch) with
  | Missing s -> pr "The implementation is missing the method %s" s
  | Types err -> report_type_inequality env ppf err

let report_kind_mismatch first second ppf (kind1, kind2) =
  let pr fmt = Format.fprintf ppf fmt in
  let kind_to_string = function
  | Kind_abstract -> "abstract"
  | Kind_record -> "a record"
  | Kind_variant -> "a variant"
  | Kind_open -> "an extensible variant" in
  pr "%s is %s, but %s is %s."
    (String.capitalize_ascii first)
    (kind_to_string kind1)
    second
    (kind_to_string kind2)

let report_type_mismatch first second decl env ppf err =
  let pr fmt = Format.fprintf ppf fmt in
  pr "@ ";
  match err with
  | Arity ->
      pr "They have different arities."
  | Privacy err ->
      report_privacy_mismatch ppf err
  | Kind err ->
      report_kind_mismatch first second ppf err
  | Constraint err ->
      (* This error can come from implicit parameter disagreement or from
         explicit `constraint`s.  Both affect the parameters, hence this choice
         of explanatory text *)
      pr "Their parameters differ@,";
      report_type_inequality env ppf err
  | Manifest err ->
      report_type_inequality env ppf err
  | Private_variant (_ty1, _ty2, mismatch) ->
      report_private_variant_mismatch first second decl env ppf mismatch
  | Private_object (_ty1, _ty2, mismatch) ->
      report_private_object_mismatch env ppf mismatch
  | Variance ->
      pr "Their variances do not agree."
  | Record_mismatch err ->
      report_record_mismatch first second decl env ppf err
  | Variant_mismatch err ->
      report_patch pp_variant_diff first second decl env ppf err
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

module Record_diffing = struct

  let compare_labels env params1 params2
      (ld1 : Types.label_declaration)
      (ld2 : Types.label_declaration) =
    if ld1.ld_mutable <> ld2.ld_mutable
    then
      let ord = if ld1.ld_mutable = Asttypes.Mutable then First else Second in
      Some (Mutability  ord)
    else
    let tl1 = params1 @ [ld1.ld_type] in
    let tl2 = params2 @ [ld2.ld_type] in
    match Ctype.equal env true tl1 tl2 with
    | exception Ctype.Equality err ->
        Some (Type err : label_mismatch)
    | () -> None

  let rec equal ~loc env params1 params2
      (labels1 : Types.label_declaration list)
      (labels2 : Types.label_declaration list) =
    match labels1, labels2 with
    | [], [] -> true
    | _ :: _ , [] | [], _ :: _ -> false
    | ld1 :: rem1, ld2 :: rem2 ->
        if Ident.name ld1.ld_id <> Ident.name ld2.ld_id
        then false
        else begin
          Builtin_attributes.check_deprecated_mutable_inclusion
            ~def:ld1.ld_loc
            ~use:ld2.ld_loc
            loc
            ld1.ld_attributes ld2.ld_attributes
            (Ident.name ld1.ld_id);
          match compare_labels env params1 params2 ld1 ld2 with
          | Some _ -> false
          (* add arguments to the parameters, cf. PR#7378 *)
          | None ->
              equal ~loc env
                (ld1.ld_type::params1) (ld2.ld_type::params2)
                rem1 rem2
        end

  module Defs = struct
    type left = Types.label_declaration
    type right = left
    type diff = label_mismatch
    type state = type_expr list * type_expr list
  end
  module Diff = Diffing_with_keys.Define(Defs)

  let update (d:Diff.change) (params1,params2 as st) =
    match d with
    | Insert _ | Change _ | Delete _ -> st
    | Keep (x,y,_) ->
        (* We need to add equality between existential type parameters
           (in inline records) *)
        x.data.ld_type::params1, y.data.ld_type::params2

  let test _loc env (params1,params2)
      ({pos; data=lbl1}: Diff.left)
      ({data=lbl2; _ }: Diff.right)
    =
    let name1, name2 = Ident.name lbl1.ld_id, Ident.name lbl2.ld_id in
    if  name1 <> name2 then
      let types_match =
        match compare_labels env params1 params2 lbl1 lbl2 with
        | Some _ -> false
        | None -> true
      in
      Error
        (Diffing_with_keys.Name {types_match; pos; got=name1; expected=name2})
    else
      match compare_labels env params1 params2 lbl1 lbl2 with
      | Some reason ->
          Error (
            Diffing_with_keys.Type {pos; got=lbl1; expected=lbl2; reason}
          )
      | None -> Ok ()

  let weight: Diff.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Keep _ -> 0
    | Change (_,_,Diffing_with_keys.Name t ) ->
        if t.types_match then 10 else 15
    | Change _ -> 10



  let key (x: Defs.left) = Ident.name x.ld_id
  let diffing loc env params1 params2 cstrs_1 cstrs_2 =
    let module Compute = Diff.Simple(struct
        let key_left = key
        let key_right = key
        let update = update
        let test = test loc env
        let weight = weight
      end)
    in
    Compute.diff (params1,params2) cstrs_1 cstrs_2

  let compare ~loc env params1 params2 l r =
    if equal ~loc env params1 params2 l r then
      None
    else
      Some (diffing loc env params1 params2 l r)


  let compare_with_representation ~loc env params1 params2 l r rep1 rep2 =
    if not (equal ~loc env params1 params2 l r) then
      let patch = diffing loc env params1 params2 l r in
      Some (Record_mismatch (Label_mismatch patch))
    else
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

end


module Variant_diffing = struct

  let compare_constructor_arguments ~loc env params1 params2 arg1 arg2 =
    match arg1, arg2 with
    | Types.Cstr_tuple arg1, Types.Cstr_tuple arg2 ->
        if List.length arg1 <> List.length arg2 then
          Some (Arity : constructor_mismatch)
        else begin
        (* Ctype.equal must be called on all arguments at once, cf. PR#7378 *)
        match Ctype.equal env true (params1 @ arg1) (params2 @ arg2) with
        | exception Ctype.Equality err -> Some (Type err)
        | () -> None
      end
    | Types.Cstr_record l1, Types.Cstr_record l2 ->
        Option.map
          (fun rec_err -> Inline_record rec_err)
          (Record_diffing.compare env ~loc params1 params2 l1 l2)
    | Types.Cstr_record _, _ -> Some (Kind First : constructor_mismatch)
    | _, Types.Cstr_record _ -> Some (Kind Second : constructor_mismatch)

  let compare_constructors ~loc env params1 params2 res1 res2 args1 args2 =
    match res1, res2 with
    | Some r1, Some r2 ->
        begin match Ctype.equal env true [r1] [r2] with
        | exception Ctype.Equality err -> Some (Type err)
        | () -> compare_constructor_arguments ~loc env [r1] [r2] args1 args2
        end
    | Some _, None -> Some (Explicit_return_type First)
    | None, Some _ -> Some (Explicit_return_type Second)
    | None, None ->
        compare_constructor_arguments ~loc env params1 params2 args1 args2

  let equal ~loc env params1 params2
      (cstrs1 : Types.constructor_declaration list)
      (cstrs2 : Types.constructor_declaration list) =
    List.length cstrs1 = List.length cstrs2 &&
    List.for_all2 (fun (cd1:Types.constructor_declaration)
                    (cd2:Types.constructor_declaration) ->
        Ident.name cd1.cd_id = Ident.name cd2.cd_id
        &&
        begin
          Builtin_attributes.check_alerts_inclusion
            ~def:cd1.cd_loc
            ~use:cd2.cd_loc
            loc
            cd1.cd_attributes cd2.cd_attributes
            (Ident.name cd1.cd_id)
          ;
        match compare_constructors ~loc env params1 params2
                cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
        | Some _ -> false
        | None -> true
      end) cstrs1 cstrs2

  module Defs = struct
    type left = Types.constructor_declaration
    type right = left
    type diff = constructor_mismatch
    type state = type_expr list * type_expr list
  end
  module D = Diffing_with_keys.Define(Defs)

  let update _ st = st

  let weight: D.change -> _ = function
    | Insert _ -> 10
    | Delete _ -> 10
    | Keep _ -> 0
    | Change (_,_,Diffing_with_keys.Name t) ->
        if t.types_match then 10 else 15
    | Change _ -> 10


  let test loc env (params1,params2)
      ({pos; data=cd1}: D.left)
      ({data=cd2; _}: D.right) =
    let name1, name2 = Ident.name cd1.cd_id, Ident.name cd2.cd_id in
    if  name1 <> name2 then
      let types_match =
        match compare_constructors ~loc env params1 params2
                cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
        | Some _ -> false
        | None -> true
      in
      Error
        (Diffing_with_keys.Name {types_match; pos; got=name1; expected=name2})
    else
      match compare_constructors ~loc env params1 params2
              cd1.cd_res cd2.cd_res cd1.cd_args cd2.cd_args with
      | Some reason ->
          Error (Diffing_with_keys.Type {pos; got=cd1; expected=cd2; reason})
      | None -> Ok ()

  let diffing loc env params1 params2 cstrs_1 cstrs_2 =
    let key (x:Defs.left) = Ident.name x.cd_id in
    let module Compute = D.Simple(struct
        let key_left = key
        let key_right = key
        let test = test loc env
        let update = update
        let weight = weight
      end)
    in
    Compute.diff (params1,params2) cstrs_1 cstrs_2

  let compare ~loc env params1 params2 l r =
    if equal ~loc env params1 params2 l r then
      None
    else
      Some (diffing loc env params1 params2 l r)

  let compare_with_representation ~loc env params1 params2
      cstrs1 cstrs2 rep1 rep2
    =
    let err = compare ~loc env params1 params2 cstrs1 cstrs2 in
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
end

(* Inclusion between "private" annotations *)
let privacy_mismatch env decl1 decl2 =
  match decl1.type_private, decl2.type_private with
  | Private, Public -> begin
      match decl1.type_kind, decl2.type_kind with
      | Type_record  _, Type_record  _ -> Some Private_record_type
      | Type_variant _, Type_variant _ -> Some Private_variant_type
      | Type_open,      Type_open      -> Some Private_extensible_variant
      | Type_abstract, Type_abstract
        when Option.is_some decl2.type_manifest -> begin
          match decl1.type_manifest with
          | Some ty1 -> begin
            let ty1 = Ctype.expand_head env ty1 in
            match get_desc ty1 with
            | Tvariant row when Btype.is_constr_row ~allow_ident:true
                                  (row_more row) ->
                Some Private_row_type
            | Tobject (fi, _) when Btype.is_constr_row ~allow_ident:true
                                     (snd (Ctype.flatten_fields fi)) ->
                Some Private_row_type
            | _ ->
                Some Private_type_abbreviation
            end
          | None ->
              None
        end
      | _, _ ->
          None
    end
  | _, _ ->
      None

let private_variant env row1 params1 row2 params2 =
    let r1, r2, pairs =
      Ctype.merge_row_fields (row_fields row1) (row_fields row2)
    in
    let row1_closed = row_closed row1 in
    let row2_closed = row_closed row2 in
    let err =
      if row2_closed && not row1_closed then Some Only_outer_closed
      else begin
        match row2_closed, Ctype.filter_row_fields false r1 with
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
             match row_field_repr f with
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
          | exception Ctype.Equality err ->
              Some (Types err : private_variant_mismatch)
          | () -> None
        end
      | (s, f1, f2) :: pairs -> begin
          match row_field_repr f1, row_field_repr f2 with
          | Rpresent to1, Rpresent to2 -> begin
              match to1, to2 with
              | Some t1, Some t2 ->
                  loop (t1 :: tl1) (t2 :: tl2) pairs
              | None, None ->
                  loop tl1 tl2 pairs
              | Some _, None | None, Some _ ->
                  Some (Incompatible_types_for s)
            end
          | Rpresent to1, Reither(const2, ts2, _) -> begin
              match to1, const2, ts2 with
              | Some t1, false, [t2] -> loop (t1 :: tl1) (t2 :: tl2) pairs
              | None, true, [] -> loop tl1 tl2 pairs
              | _, _, _ -> Some (Incompatible_types_for s)
            end
          | Rpresent _, Rabsent ->
              Some (Missing (Second, s) : private_variant_mismatch)
          | Reither(const1, ts1, _), Reither(const2, ts2, _) ->
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
    | exception Ctype.Equality err -> Some (Types err)
    | () -> None
  end

let type_manifest env ty1 params1 ty2 params2 priv2 kind2 =
  let ty1' = Ctype.expand_head env ty1 and ty2' = Ctype.expand_head env ty2 in
  match get_desc ty1', get_desc ty2' with
  | Tvariant row1, Tvariant row2
    when is_absrow env (row_more row2) -> begin
      assert (Ctype.is_equal env true (ty1::params1) (row_more row2::params2));
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
            match get_desc ty2' with
            | Tvariant row ->
                not (is_absrow env (row_more row))
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
      | exception Ctype.Equality err -> Some (Manifest err)
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
  let err =
    match privacy_mismatch env decl1 decl2 with
    | Some err -> Some (Privacy err)
    | None -> None
  in
  if err <> None then err else
  let err = match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        begin
          match Ctype.equal env true decl1.type_params decl2.type_params with
          | exception Ctype.Equality err -> Some (Constraint err)
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
        | exception Ctype.Equality err -> Some (Constraint err)
        | () ->
          match Ctype.equal env false [ty1] [ty2] with
          | exception Ctype.Equality err -> Some (Manifest err)
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
        Variant_diffing.compare_with_representation ~loc env
          decl1.type_params
          decl2.type_params
          cstrs1
          cstrs2
          rep1
          rep2
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
        Record_diffing.compare_with_representation ~loc env
          decl1.type_params decl2.type_params
          labels1 labels2
          rep1 rep2
    | (Type_open, Type_open) -> None
    | (_, _) -> Some (Kind (of_kind decl1.type_kind, of_kind decl2.type_kind))
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
  let constrained ty = not (Btype.is_Tvar ty) in
  if List.for_all2
      (fun ty (v1,v2) ->
        let open Variance in
        let imp a b = not a || b in
        let (co1,cn1) = get_upper v1 and (co2,cn2) = get_upper v2 in
        (if abstr then (imp co1 co2 && imp cn1 cn2)
         else if opn || constrained ty then (co1 = co2 && cn1 = cn2)
         else true) &&
        let (p1,n1,j1) = get_lower v1 and (p2,n2,j2) = get_lower v2 in
        imp abstr (imp p2 p1 && imp n2 n1 && imp j2 j1))
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
  | exception Ctype.Equality err ->
      Some (Constructor_mismatch (id, ext1, ext2, Type err))
  | () ->
    let r =
      Variant_diffing.compare_constructors ~loc env
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
