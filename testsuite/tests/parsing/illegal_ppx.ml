module H = Ast_helper
module M = Ast_mapper
open Parsetree
let empty_tuple loc = H.Exp.tuple ~loc []
let empty_record loc = H.Exp.record ~loc [] None
let empty_apply loc f =
  H.Exp.apply ~loc f []

let missing_rhs loc =
  let name = Location.mkloc "T" loc in
  let mtd = H.Mtd.mk ~loc name in
  H.Sig.modtype_subst ~loc mtd

let empty_let loc = H.Str.value ~loc Asttypes.Nonrecursive []
let empty_type loc = H.Str.type_ ~loc Asttypes.Nonrecursive []
let empty_poly_binder loc = H.Typ.(poly ~loc [] (any ~loc ()))
let functor_id loc = Location.mkloc
    (Longident.( Lapply (Lident "F", Lident "X"))) loc
let complex_record loc =
  H.Pat.record ~loc [functor_id loc, H.Pat.any ~loc () ] Asttypes.Closed


let empty_open_tuple_pat loc =
  let pat = H.Pat.mk Ppat_any in
  H.Pat.tuple ~loc [] Open

let short_closed_tuple_pat loc =
  let pat = H.Pat.mk Ppat_any in
  H.Pat.tuple ~loc [Some "baz", pat] Closed

let super = M.default_mapper
let expr mapper e =
  match e.pexp_desc with
  | Pexp_extension ({txt="tuple";loc},_) -> empty_tuple loc
  | Pexp_extension({txt="record";loc},_) -> empty_record loc
  | Pexp_extension({txt="no_args";loc},PStr[{pstr_desc= Pstr_eval (e,_);_}])
    -> empty_apply loc e
  | _ -> super.M.expr mapper e

let pat mapper p =
  match p.ppat_desc with
  | Ppat_extension ({txt="record_with_functor_fields";loc},_) ->
      complex_record loc
  | Ppat_extension ({txt="empty_open_tuple_pat";loc},_) ->
      empty_open_tuple_pat loc
  | Ppat_extension ({txt="short_closed_tuple_pat";loc},_) ->
      short_closed_tuple_pat loc
  | _ -> super.M.pat mapper p

let typ mapper ty =
  match ty.ptyp_desc with
  | Ptyp_extension ({txt="empty_poly_binder";loc},_) ->
      empty_poly_binder loc
  | _ -> super.M.typ mapper ty


let structure_item mapper stri = match stri.pstr_desc with
  | Pstr_extension (({Location.txt="empty_let";loc},_),_) -> empty_let loc
  | Pstr_extension (({Location.txt="empty_type";loc},_),_) -> empty_type loc
  | _ -> super.structure_item mapper stri

let signature_item mapper stri = match stri.psig_desc with
  | Psig_extension (({Location.txt="missing_rhs";loc},_),_) -> missing_rhs loc
  | _ -> super.signature_item mapper stri


let () = M.register "illegal ppx" (fun _ ->
    { super with expr; pat; structure_item; signature_item; typ }
  )
