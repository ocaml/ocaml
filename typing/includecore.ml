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

(* Inclusion checks for the core language *)

open Misc
open Asttypes
open Path
open Types
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions env vd1 vd2 =
  if Ctype.moregeneral env true vd1.val_type vd2.val_type then begin
    match (vd1.val_kind, vd2.val_kind) with
        (Val_prim p1, Val_prim p2) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Val_prim p, _) -> Tcoerce_primitive p
      | (_, Val_prim p) -> raise Dont_match
      | (_, _) -> Tcoerce_none
  end else
    raise Dont_match

(* Inclusion between "private" annotations *)

let private_flags priv1 priv2 =
  match (priv1, priv2) with (Private, Public) -> false | (_, _) -> true

(* Inclusion between manifest types (particularly for private row types) *)

let is_absrow env ty =
  match ty.desc with
    Tconstr(Pident id, _, _) ->
      begin match Ctype.expand_head env ty with
        {desc=Tobject _|Tvariant _} -> true
      | _ -> false
      end
  | _ -> false

let type_manifest env ty1 params1 ty2 params2 =
  let ty1' = Ctype.expand_head env ty1 and ty2' = Ctype.expand_head env ty2 in
  match ty1'.desc, ty2'.desc with
    Tvariant row1, Tvariant row2 when is_absrow env (Btype.row_more row2) ->
      let row1 = Btype.row_repr row1 and row2 = Btype.row_repr row2 in
      Ctype.equal env true (ty1::params1) (row2.row_more::params2) &&
      (match row1.row_more with	{desc=Tvar|Tconstr _} -> true | _ -> false) &&
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
      (match rest1 with {desc=Tnil|Tvar|Tconstr _} -> true | _ -> false) &&
      let pairs, miss1, miss2 = Ctype.associate_fields fields1 fields2 in
      miss2 = [] &&
      let tl1, tl2 =
	List.split (List.map (fun (_,_,t1,_,t2) -> t1, t2) pairs) in
      Ctype.equal env true (params1 @ tl1) (params2 @ tl2)
  | _ -> 
      Ctype.equal env true (ty1 :: params1) (ty2 :: params2)

(* Inclusion between type declarations *)

let type_declarations env id decl1 decl2 =
  decl1.type_arity = decl2.type_arity &&
  begin match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> true
    | (Type_variant (cstrs1, priv1), Type_variant (cstrs2, priv2)) ->
        private_flags priv1 priv2 &&
        Misc.for_all2
          (fun (cstr1, arg1) (cstr2, arg2) ->
            cstr1 = cstr2 &&
            Misc.for_all2
              (fun ty1 ty2 ->
                Ctype.equal env true (ty1::decl1.type_params)
                                     (ty2::decl2.type_params))
              arg1 arg2)
          cstrs1 cstrs2
    | (Type_record(labels1,rep1,priv1), Type_record(labels2,rep2,priv2)) ->
        private_flags priv1 priv2 &&
        rep1 = rep2 &&
        Misc.for_all2
          (fun (lbl1, mut1, ty1) (lbl2, mut2, ty2) ->
            lbl1 = lbl2 && mut1 = mut2 &&
            Ctype.equal env true (ty1::decl1.type_params)
                                 (ty2::decl2.type_params))
          labels1 labels2
    | (_, _) -> false
  end &&
  begin match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        Ctype.equal env true decl1.type_params decl2.type_params
    | (Some ty1, Some ty2) ->
	type_manifest env ty1 decl1.type_params ty2 decl2.type_params
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(Pident id, decl2.type_params, ref Mnil))
        in
        Ctype.equal env true decl1.type_params decl2.type_params &&
        Ctype.equal env false [ty1] [ty2]
  end &&
  if match decl2.type_kind with
  | Type_record(_,_,priv) | Type_variant(_,priv) -> priv = Private
  | Type_abstract ->
      match decl2.type_manifest with None -> true
      | Some ty -> Btype.has_constr_row (Ctype.expand_head env ty)
  then
    List.for_all2
      (fun (co1,cn1,ct1) (co2,cn2,ct2) -> (not co1 || co2) && (not cn1 || cn2))
      decl1.type_variance decl2.type_variance
  else true

(* Inclusion between exception declarations *)

let exception_declarations env ed1 ed2 =
  Misc.for_all2 (fun ty1 ty2 -> Ctype.equal env false [ty1] [ty2]) ed1 ed2

(* Inclusion between class types *)
let encode_val (mut, ty) rem =
  begin match mut with
    Asttypes.Mutable   -> Predef.type_unit
  | Asttypes.Immutable -> Btype.newgenty Tvar
  end
  ::ty::rem

let meths meths1 meths2 =
  Meths.fold
    (fun nam t2 (ml1, ml2) ->
       (begin try
          Meths.find nam meths1 :: ml1
        with Not_found ->
          ml1
        end,
        t2 :: ml2))
    meths2 ([], [])

let vars vars1 vars2 =
  Vars.fold
    (fun lab v2 (vl1, vl2) ->
       (begin try
          encode_val (Vars.find lab vars1) vl1
        with Not_found ->
          vl1
        end,
        encode_val v2 vl2))
    vars2 ([], [])
