(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Inclusion checks for the core language *)

open Misc
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

(* Inclusion between type declarations *)

let type_declarations env id decl1 decl2 =
  decl1.type_arity = decl2.type_arity &
  begin match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) -> true
    | (Type_variant cstrs1, Type_variant cstrs2) ->
        for_all2
          (fun (cstr1, arg1) (cstr2, arg2) ->
            cstr1 = cstr2 &
            for_all2
              (fun ty1 ty2 ->
                Ctype.equal env true (ty1::decl1.type_params)
                                     (ty2::decl2.type_params))
              arg1 arg2)
          cstrs1 cstrs2
    | (Type_record labels1, Type_record labels2) ->
        for_all2
          (fun (lbl1, mut1, ty1) (lbl2, mut2, ty2) ->
            lbl1 = lbl2 & mut1 = mut2 &
            Ctype.equal env true (ty1::decl1.type_params)
                                 (ty2::decl2.type_params))
          labels1 labels2
    | (_, _) -> false
  end &
  begin match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) ->
        Ctype.equal env true decl1.type_params decl2.type_params
    | (Some ty1, Some ty2) ->
        Ctype.equal env true (ty1::decl1.type_params)
                             (ty2::decl2.type_params)
    | (None, Some ty2) ->
        let ty1 =
          Btype.newgenty (Tconstr(Pident id, decl2.type_params, ref Mnil))
        in
        Ctype.equal env true decl1.type_params decl2.type_params
          &
        Ctype.equal env false [ty1] [ty2]
  end

(* Inclusion between exception declarations *)

let exception_declarations env ed1 ed2 =
  for_all2 (fun ty1 ty2 -> Ctype.equal env false [ty1] [ty2]) ed1 ed2

(* Inclusion between class types *)
let encode_val (mut, ty) rem =
  begin match mut with
    Asttypes.Mutable   -> Predef.type_unit
  | Asttypes.Immutable -> Ctype.newgenty Tvar
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
