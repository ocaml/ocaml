(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Jacques Garrigue, Graduate School of Mathematics, Nagoya University   *)
(*                                                                        *)
(*   Copyright 2003 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(* Print a raw type expression, with sharing *)

open Format
open Types
open Asttypes
let longident = Pprintast.longident

let raw_list pr ppf = function
    [] -> fprintf ppf "[]"
  | a :: l ->
      fprintf ppf "@[<1>[%a%t]@]" pr a
        (fun ppf -> List.iter (fun x -> fprintf ppf ";@,%a" pr x) l)

let raw_option pr ppf = function
    None -> fprintf ppf "None"
  | Some x -> fprintf ppf "@[<hov>Some(@,%a)@]" pr x

let kind_vars = ref []
let kind_count = ref 0

let string_of_field_kind v =
  match field_kind_repr v with
  | Fpublic -> "Fpublic"
  | Fabsent -> "Fabsent"
  | Fprivate -> "Fprivate"

let rec safe_repr v t =
  match Transient_expr.coerce t with
    {desc = Tlink t} when not (List.memq t v) ->
      safe_repr (t::v) t
  | t' -> t'

let rec list_of_memo = function
    Mnil -> []
  | Mcons { path; rem; _ } -> path :: list_of_memo rem
  | Mlink rem -> list_of_memo !rem

let print_name ppf = function
    None -> fprintf ppf "None"
  | Some name -> fprintf ppf "\"%s\"" name

let path = Format_doc.compat Path.print

let visited = ref []
let rec raw_type ppf ty =
  let ty = safe_repr [] ty in
  if List.memq ty !visited then fprintf ppf "{id=%d}" ty.id else begin
    visited := ty :: !visited;
    fprintf ppf "@[<1>{id=%d;level=%d;scope=%d;marks=%x;desc=@,%a}@]"
      ty.id ty.level
      (Transient_expr.get_scope ty) (Transient_expr.get_marks ty)
      raw_type_desc ty.desc
  end
and labeled_type ppf (label, ty) =
  begin match label with
  | Some s -> fprintf ppf "label=\"%s\" " s
  | None -> ()
  end;
  raw_type ppf ty
and raw_type_list tl = raw_list raw_type tl
and labeled_type_list tl = raw_list labeled_type tl
and raw_lid_type_list tl =
  raw_list (fun ppf (lid, typ) ->
             let lid = Longident.unflatten lid |> Option.get in
             fprintf ppf "(@,%a,@,%a)" longident lid raw_type typ)
    tl
and raw_type_option ot = raw_option raw_type ot
and raw_type_desc ppf = function
    Tvar name -> fprintf ppf "Tvar %a" print_name name
  | Tarrow(l,t1,t2,c) ->
      fprintf ppf "@[<hov1>Tarrow(\"%s\",@,%a,@,%a,@,%s)@]"
        (string_of_label l) raw_type t1 raw_type t2
        (if is_commu_ok c then "Cok" else "Cunknown")
  | Tfunctor (l, id, {pack_path; pack_constraints}, t2) ->
    fprintf ppf "@[<hov1>Tfunctor(\"%s\",@,%a,@,(%a,@,%a),@,%a)@]"
      (string_of_label l) Ident.Unscoped.print id
      path pack_path raw_lid_type_list pack_constraints raw_type t2
  | Ttuple tl ->
      fprintf ppf "@[<1>Ttuple@,%a@]" labeled_type_list tl
  | Tconstr (p, tl, abbrev) ->
      fprintf ppf "@[<hov1>Tconstr(@,%a,@,%a,@,%a)@]" path p
        raw_type_list tl
        (raw_list path) (list_of_memo !abbrev)
  | Tobject (t, nm) ->
      fprintf ppf "@[<hov1>Tobject(@,%a,@,@[<1>ref%t@])@]" raw_type t
        (fun ppf ->
          match !nm with None -> fprintf ppf " None"
          | Some(p,tl) ->
              fprintf ppf "(Some(@,%a,@,%a))" path p raw_type_list tl)
  | Tfield (f, k, t1, t2) ->
      fprintf ppf "@[<hov1>Tfield(@,%s,@,%s,@,%a,@;<0 -1>%a)@]" f
        (string_of_field_kind k)
        raw_type t1 raw_type t2
  | Tnil -> fprintf ppf "Tnil"
  | Tlink t -> fprintf ppf "@[<1>Tlink@,%a@]" raw_type t
  | Tsubst (t, None) -> fprintf ppf "@[<1>Tsubst@,(%a,None)@]" raw_type t
  | Tsubst (t, Some t') ->
      fprintf ppf "@[<1>Tsubst@,(%a,@ Some%a)@]" raw_type t raw_type t'
  | Texpand (t, p, args) ->
      fprintf ppf "@[<1>Texpand(@,%a,@,%a,@,%a)@]" raw_type t path p
        raw_type_list args
  | Tunivar name -> fprintf ppf "Tunivar %a" print_name name
  | Tpoly (t, tl) ->
      fprintf ppf "@[<hov1>Tpoly(@,%a,@,%a)@]"
        raw_type t
        raw_type_list tl
  | Tvariant row ->
      let Row {fields; more; name; fixed; closed} = row_repr row in
      fprintf ppf
        "@[<hov1>{@[%s@,%a;@]@ @[%s@,%a;@]@ %s%B;@ %s%a;@ @[<1>%s%t@]}@]"
        "row_fields="
        (raw_list (fun ppf (l, f) ->
          fprintf ppf "@[%s,@ %a@]" l raw_field f))
        fields
        "row_more=" raw_type more
        "row_closed=" closed
        "row_fixed=" raw_row_fixed fixed
        "row_name="
        (fun ppf ->
          match name with None -> fprintf ppf "None"
          | Some(p,tl) ->
              fprintf ppf "Some(@,%a,@,%a)" path p raw_type_list tl)
  | Tpackage pack ->
    fprintf ppf "@[<hov1>Tpackage(@,%a,@,%a)@]"
      path pack.pack_path
      raw_lid_type_list pack.pack_constraints
and raw_row_fixed ppf = function
| None -> fprintf ppf "None"
| Some Types.Fixed_private -> fprintf ppf "Some Fixed_private"
| Some Types.Rigid -> fprintf ppf "Some Rigid"
| Some Types.Univar t -> fprintf ppf "Some(Univar(%a))" raw_type t
| Some Types.Reified p -> fprintf ppf "Some(Reified(%a))" path p

and raw_field ppf rf =
  match_row_field
    ~absent:(fun _ -> fprintf ppf "RFabsent")
    ~present:(function
      | None ->
          fprintf ppf "RFpresent None"
      | Some t ->
          fprintf ppf  "@[<1>RFpresent(Some@,%a)@]" raw_type t)
    ~either:(fun c tl m (_,e) ->
      fprintf ppf "@[<hov1>RFeither(%B,@,%a,@,%B,@,@[<1>ref%t@])@]" c
        raw_type_list tl m
        (fun ppf ->
          match e with None -> fprintf ppf " RFnone"
          | Some f -> fprintf ppf "@,@[<1>(%a)@]" raw_field f))
    rf

let raw_wrap f ppf x =
  visited := []; kind_vars := []; kind_count := 0;
  f ppf x;
  visited := []; kind_vars := []

let type_expr ppf t =
  raw_wrap raw_type ppf t

let raw_label_decl ppf ld =
  fprintf ppf "@[<hov1>{ld_id=%a;@,ld_mutable=%s;@,ld_type=%a}@]"
    Ident.print ld.ld_id
    (match ld.ld_mutable with Immutable -> "Immutable" | Mutable -> "Mutable")
    raw_type ld.ld_type

let raw_cstr_args ppf = function
  | Cstr_tuple tl ->
      fprintf ppf "@[<hov>Cstr_tuple@,%a@]" raw_type_list tl
  | Cstr_record lbl ->
      fprintf ppf "@[<hov>Cstr_record@,%a@]"
        (raw_list raw_label_decl) lbl

let raw_cstr_decl ppf cd =
  fprintf ppf "@[<hov1>{cd_id=%a;@,cd_args=%a;@,cd_res=%a}@]"
    Ident.print cd.cd_id
    raw_cstr_args cd.cd_args
    raw_type_option cd.cd_res

let raw_type_kind ppf tk =
  match tk with
    Type_abstract _ -> fprintf ppf "Type_abstract"
  | Type_open -> fprintf ppf "Type_open"
  | Type_record (lbl,_) ->
      fprintf ppf "@[<hov>Type_record@,%a@]"
        (raw_list raw_label_decl) lbl
  | Type_variant (csl,_) ->
      fprintf ppf "@[<hov>Type_variant@,%a@]"
        (raw_list raw_cstr_decl) csl
  | Type_external s -> fprintf ppf "Type_external %s" s

let raw_type_decl ppf td =
  fprintf ppf
    "@[<hov1>{type_params=%a;@ type_kind=%a;@ type_private=%s;\
     @ type_manifest=%a}@]"
    raw_type_list td.type_params
    raw_type_kind td.type_kind
    (match td.type_private with Private -> "Private" | Public -> "Public")
    raw_type_option td.type_manifest

let type_declaration ppf td =
  raw_wrap raw_type_decl ppf td

let raw_value_desc ppf vd =
  fprintf ppf "@[<hov1>{val_type=%a;@,val_kind=%s}@]"
    raw_type vd.val_type
    (match vd.val_kind with
    | Val_reg -> "Val_reg"
    | Val_prim _ -> "Val_prim"
    | Val_ivar _ -> "Val_ivar"
    | Val_self _ -> "Val_self"
    | Val_anc _  -> "Val_anc")

let rec raw_sig_item ppf sg =
  match sg with
    Sig_value (id, vd, _vis) ->
      fprintf ppf "@[<hov>Sig_value(@,%a,@,%a)@]"
        Ident.print id
        (raw_wrap raw_value_desc) vd
  | Sig_type (id, td, _, _) ->
      fprintf ppf "@[<hov>Sig_type(@,%a,@,%a)@]"
        Ident.print id
        type_declaration td
  | Sig_typext (id, _, _, _) ->
      fprintf ppf "Sig_typext(%a,..)" Ident.print id
  | Sig_module (id, _, md, _, _) ->
      fprintf ppf "@[<hov>Sig_module(@,%a,@,%a,..)@]"
        Ident.print id
        modtype md.md_type
  | Sig_modtype (id, mtd, _) ->
      fprintf ppf "@[<hov>Sig_modtype(@,%a,@,%a,..)@]"
        Ident.print id
        (raw_option modtype) mtd.mtd_type
  | Sig_class (id, _, _, _) ->
      fprintf ppf "Sig_class(%a,..)" Ident.print id
  | Sig_class_type (id, _, _, _) ->
      fprintf ppf "Sig_class_type(%a,..)" Ident.print id

and signature ppf sg = raw_list raw_sig_item ppf sg

and modtype ppf mty =
  match mty with
    Mty_ident id -> fprintf ppf "Mty_ident(%a)" path id
  | Mty_alias id -> fprintf ppf "Mty_alias(%a)" path id
  | Mty_signature sg ->
      fprintf ppf "@[<hov>Mty_signature@,%a@]" signature sg
  | Mty_functor (fp, mty) ->
      fprintf ppf "@[<hov>Mty_functor(@,%a,@,%a)@]"
        raw_func_param fp
        modtype mty

and raw_func_param ppf = function
    Unit -> fprintf ppf "Unit"
  | Named (ido, mty) ->
      fprintf ppf "@[<hov>Named(@,%a,@,%a)@]"
        (raw_option Ident.print) ido
        modtype mty
