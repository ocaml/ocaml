(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Inclusion checks for the core language *)

open Misc
open Path
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions env vd1 vd2 =
  if Ctype.moregeneral env vd1.val_type vd2.val_type then begin
    match (vd1.val_prim, vd2.val_prim) with
        (Some p1, Some p2) ->
          if p1 = p2 then Tcoerce_none else raise Dont_match
      | (Some p, None) -> Tcoerce_primitive p
      | (None, Some p) -> raise Dont_match
      | (None, None) -> Tcoerce_none
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
                Ctype.equal env decl1.type_params ty1 decl2.type_params ty2)
              arg1 arg2)
          cstrs1 cstrs2
    | (Type_record labels1, Type_record labels2) ->
        for_all2
          (fun (lbl1, mut1, ty1) (lbl2, mut2, ty2) ->
            lbl1 = lbl2 & mut1 = mut2 &
            Ctype.equal env decl1.type_params ty1 decl2.type_params ty2)
          labels1 labels2
    | (_, _) -> false
  end &
  begin match (decl1.type_manifest, decl2.type_manifest) with
      (_, None) -> true
    | (Some ty1, Some ty2) ->
        Ctype.equal env decl1.type_params ty1 decl2.type_params ty2
    | (None, Some ty2) ->
        let ty1 = Tconstr(Pident id, decl2.type_params) in
        Ctype.equal env [] ty1 [] ty2
  end

(* Inclusion between exception declarations *)

let exception_declarations env ed1 ed2 =
  for_all2 (fun ty1 ty2 -> Ctype.equal env [] ty1 [] ty2) ed1 ed2

