(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Outcometree
let plain x = { highlighted=false; item=x }

let highlighting_on x = { x with highlighted=true }

let highlight_diff x y =
  if x.item = y.item then x, y else
    highlighting_on x, highlighting_on y

let rec highlight_map_diff ~key ~diff ~mismatch l r = match l, r with
  | [], r -> [], List.map mismatch r
  | l, [] -> List.map mismatch l, r
  | lh :: lq, rh :: rq ->
      if key lh < key rh then
        let l, r = highlight_map_diff ~key ~diff ~mismatch lq r in
        mismatch lh :: l, r
      else if key lh = key rh then
        let lh, rh = diff (key lh) lh rh in
        let lq, rq = highlight_map_diff ~key ~diff ~mismatch lq rq in
        lh :: lq, rh :: rq
      else
        let l, rq = highlight_map_diff ~key ~diff ~mismatch l rq in
        l, mismatch rh :: rq

let rec highlight_map2 ~mismatch f x y = match x, y with
  | [], [] -> [], []
  | [], r -> [], List.map mismatch r
  | l, [] -> List.map mismatch l, []
  | lh :: lq, rh :: rq ->
      let lh, rh = f lh rh in
      let lq, rq = highlight_map2 ~mismatch f lq rq in
      lh :: lq, rh :: rq

let hty ty = Otyp_highlight (Independent, ty)

let rec diff l r = match l, r with
  | Otyp_highlight (Independent, _) as x, y
  | x, (Otyp_highlight (Independent,_) as y) -> x, y
  | (Otyp_highlight (Paired, _) as x), (Otyp_highlight (Paired, _) as y) -> x, y
  | Otyp_highlight (Paired, x), y | x, Otyp_highlight (Paired, y) -> x, y
  | (Otyp_abstract | Otyp_open | Otyp_stuff _ | Otyp_manifest _
    | Otyp_record _ | Otyp_sum _ | Otyp_external _ ), _
  | _ , (Otyp_abstract | Otyp_open | Otyp_stuff _ | Otyp_manifest _
        | Otyp_record _ | Otyp_sum _ | Otyp_external _ ) -> l, r
  | (Otyp_var _ | Otyp_constr _ as l), r
  | l, (Otyp_var _ | Otyp_constr _ as r) -> l, r
  | Otyp_class _ , _ | _, Otyp_class _ -> l, r
  | Otyp_poly (b,ty), Otyp_poly (b',ty') ->
      let ty, ty' = diff ty ty' in
      Otyp_poly (b,ty), Otyp_poly (b',ty')
  | Otyp_poly (b,l), r ->
      let l, r = diff l r in
      Otyp_poly (b,l), r
  | l, Otyp_poly (b,r) ->
      let l, r = diff l r in
      l, Otyp_poly (b,r)
  | Otyp_alias l, Otyp_alias r ->
      let non_gen_l, non_gen_r = highlight_diff l.non_gen r.non_gen in
      let aliased_l, aliased_r = diff l.aliased r.aliased in
      Otyp_alias { non_gen=non_gen_l; aliased = aliased_l; alias = l.alias },
      Otyp_alias { non_gen=non_gen_r; aliased = aliased_r; alias = r.alias }
  | Otyp_alias l, r ->
      let aliased, r = diff l.aliased r in
      Otyp_alias { l with aliased }, r
  | l, Otyp_alias r ->
      let l, aliased = diff l r.aliased in
      l, Otyp_alias { r with aliased }

  | Otyp_arrow (label, arg, ret), Otyp_arrow (label', arg', ret') ->
     let label, label' = highlight_diff label label' in
     let arg, arg' = diff arg arg' in
     let ret, ret' = diff ret ret' in
     Otyp_arrow (label,arg,ret), Otyp_arrow (label',arg',ret')

  | Otyp_functor (label,name,package,ret),
    Otyp_functor (label',name',package', ret') ->
     let label, label' = highlight_diff label label' in
     let package, package' = package_highlight package package' in
     let ret, ret' = diff ret ret' in
     Otyp_functor (label,name,package,ret),
     Otyp_functor (label',name',package', ret')
  | Otyp_functor (label,name,package,ret), Otyp_arrow (label',arg', ret') ->
     let label, label' = highlight_diff label label' in
     let ret, ret' = diff ret ret' in
     Otyp_functor (label,name,package,ret), Otyp_arrow (label',arg', ret')
  | Otyp_arrow (label, arg, ret), Otyp_functor (label',name',package',ret') ->
     let label, label' = highlight_diff label label' in
     let ret, ret' = diff ret ret' in
     Otyp_arrow (label, arg, ret), Otyp_functor (label',name',package',ret')

  | Otyp_functor _, _ | _, Otyp_functor _ -> hty l, hty r
  | Otyp_arrow _, _ | _, Otyp_arrow _ -> hty l, hty r

  | Otyp_object l, Otyp_object r ->
      let (fields,row), (fields',row') =
        object_highlight (l.fields,l.row)  (r.fields, r.row)
      in
      Otyp_object { fields; row }, Otyp_object { fields=fields'; row=row' }
  | Otyp_object _, _ | _, Otyp_object _ -> hty l, hty r
  | Otyp_tuple l, Otyp_tuple r ->
      let elt (lbl,ty) (lbl',ty') =
        let lbl, lbl' = highlight_diff lbl lbl' in
        let ty, ty' = diff ty ty' in
        (lbl,ty), (lbl',ty')
      in
      let mismatch (lbl,ty) =
        if lbl.item <> None then (highlighting_on lbl, ty)
        else (lbl, hty ty)
      in
      let l, r = highlight_map2 ~mismatch elt l r in
      Otyp_tuple l, Otyp_tuple r
  | Otyp_tuple _, _ | _, Otyp_tuple _ -> hty l, hty r

  | Otyp_attribute (l,att), r ->
      let l, r = diff l r in
      Otyp_attribute (l,att), r
  | l, Otyp_attribute (r,att) ->
      let l, r = diff l r in
      l, Otyp_attribute (r,att)
  | Otyp_variant l, Otyp_variant r ->
      let fields, fields' = variant_diff l.fields r.fields in
      Otyp_variant { l with fields },
      Otyp_variant { r with fields = fields' }
  | Otyp_variant _, _ | _, Otyp_variant _ -> hty l, hty r
  | Otyp_module l, Otyp_module r ->
     let l, r = package_highlight l r in
     Otyp_module l, Otyp_module r

and variant_diff x y =
  match x, y with
  | Ovar_typ x, Ovar_typ y ->
    let x, y = diff x y in
    Ovar_typ x, Ovar_typ y
  | Ovar_typ _, _ | _, Ovar_typ _ -> x, y
  | Ovar_fields l, Ovar_fields r ->
      let key (x:out_field highlightable) = x.item.name.item in
      let mismatch x = x in
      let diff _name x y =
        let x = x.item and y = y.item in
        (* constant vs non constant can be decided syntactically *)
        let constant, constant' = highlight_diff x.constant y.constant in
        plain { x with constant }, plain { y with constant=constant' }
      in
      let l, r = highlight_map_diff ~key ~diff ~mismatch l r in
      Ovar_fields l, Ovar_fields r

and package_highlight l r =
  (* nothing syntactic here *)
  l, r

and object_highlight (fields,row) (fields',row') =
  let diff lbl (_,ty) (_,ty') =
    let ty, ty' = diff ty ty' in
    (plain lbl,ty), (plain lbl,ty')
  in
  (* no syntactic difference for subtyping *)
  let mismatch x = x in
  let key (lbl,_) = lbl.item in
  let fields, fields'=
    highlight_map_diff ~key ~diff ~mismatch fields fields'
  in
  (fields, row), (fields',row')

let diff l r = match l, r with
  | Otyp_highlight _ as x, y | x, (Otyp_highlight _ as y) -> x, y
  | x, y -> diff x y
