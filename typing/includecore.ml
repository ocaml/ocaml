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
open Typedtree

(* Inclusion between value descriptions *)

exception Dont_match

let value_descriptions env vd1 vd2 =
  if Ctype.moregeneral env vd1.val_type vd2.val_type then begin
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
        let ty1 =
	  {desc = Tconstr(Pident id, decl2.type_params, ref []);
	   level = Ctype.generic_level }
	in
          Ctype.equal env [] ty1 [] ty2
  end

(* Inclusion between exception declarations *)

let exception_declarations env ed1 ed2 =
  for_all2 (fun ty1 ty2 -> Ctype.equal env [] ty1 [] ty2) ed1 ed2

(* Inclusion between class types *)
let vars desc =
  Ctype.newgenty (Tobject (
    Vars.fold
      (fun lab (mut, ty) rem ->
      	 let ty' = Ctype.newgenty
	   (Ttuple [if mut = Asttypes.Mutable then Predef.type_mutable
      	            else Ctype.newgenty Tvar;
		    ty])
	 in
      	   Ctype.newgenty (Tfield (lab, ty', rem)))
      desc.cty_vars (Ctype.newgenty Tnil),
    ref None))

let class_type env d1 d2 =
  (* Same abbreviations *)
  let (cstr1, _, _, self1) = Ctype.prune_class_type d1 in
  let (cstr2, _, _, self2) = Ctype.prune_class_type d2 in
  Ctype.equal env (List.map fst cstr1) self1 (List.map fst cstr2) self2
      &
  (* Same concretes methods *)
  for_all2 Label. (=) (Sort.list Label. (=) d1.cty_concr)
      	       	      (Sort.list Label. (=) d2.cty_concr)
      &
  (* If virtual, stays virtual *)
  (d1.cty_new <> None or d2.cty_new = None)
      &
  (* Less general *)
  let t1 = Ctype.newgenty (Ttuple (d1.cty_self::vars d1::d1.cty_args)) in
  let t2 = Ctype.newgenty (Ttuple (d2.cty_self::vars d2::d2.cty_args)) in
  Ctype.moregeneral env t1 t2
