(* Inclusion checks for the core language *)

open Misc
open Path
open Typedtree


(* Inclusion between val descriptions *)

let value_descriptions env vd1 vd2 =
  Ctype.moregeneral env vd1.val_type vd2.val_type &
  begin match (vd1.val_prim, vd2.val_prim) with
      (Primitive(p1, ar1), Primitive(p2, ar2)) -> p1 = p2 & ar1 = ar2
    | (Not_prim, Primitive(p, ar)) -> false
    | _ -> true
  end

(* Inclusion between type declarations *)

let type_declarations env id decl1 decl2 =
  decl1.type_arity = decl2.type_arity &
  begin match (decl1.type_kind, decl2.type_kind) with
      (_, Type_abstract) ->
        true
    | (Type_manifest ty1, Type_manifest ty2) ->
        Ctype.equal env decl1.type_params ty1 decl2.type_params ty2
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
    | (_, Type_manifest ty2) ->
        let ty1 = Tconstr(Pident id, decl2.type_params) in
        Ctype.equal env [] ty1 [] ty2
    | (_, _) ->
        false
  end

(* Inclusion between exception declarations *)

let exception_declarations env ed1 ed2 =
  for_all2 (fun ty1 ty2 -> Ctype.equal env [] ty1 [] ty2) ed1 ed2

