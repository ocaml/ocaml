(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Path
open Types
open Typedtree
open Lambda

let scrape env ty =
  (Ctype.repr (Ctype.expand_head_opt env (Ctype.correct_levels ty))).desc

let is_function_type env ty =
  match scrape env ty with
  | Tarrow (_, lhs, rhs, _) -> Some (lhs, rhs)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Tconstr(p, _, _) -> Path.same p base_ty_path
  | _ -> false

let has_base_type exp base_ty_path =
  is_base_type exp.exp_env exp.exp_type base_ty_path

let maybe_pointer_type env ty =
  if Ctype.maybe_pointer_type env ty then
    Pointer
  else
    Immediate

let maybe_pointer exp = maybe_pointer_type exp.exp_env exp.exp_type

let rec array_element_kind env ty =
  match scrape env ty with
  | Tvar _ | Tunivar _ ->
      Pgenarray
  | Tpoly(ty, _) -> array_element_kind env ty
  | Tconstr(p, _, _) ->
      if Path.same p Predef.path_float then
        Pfloatarray
      else if Path.same p Predef.path_int
           || Path.same p Predef.path_char
           || Path.same p Predef.path_string
           || Path.same p Predef.path_array
           || Path.same p Predef.path_nativeint
           || Path.same p Predef.path_int32
           || Path.same p Predef.path_int64 then
        Paddrarray
      else begin
        try
          match Env.find_type p env with
            {type_kind = Type_abstract} -> Pgenarray
          | {type_kind = _} -> Paddrarray
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Pgenarray
      end
  | _ -> Paddrarray

let array_type_kind env ty =
  match scrape env ty with
  | Tconstr(p, [elt_ty], _) | Tpoly({desc = Tconstr(p, [elt_ty], _)}, _)
    when Path.same p Predef.path_array ->
      array_element_kind env elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_type_pointer env ty =
  match scrape env ty with
  | Tconstr(p, [elt_ty], _) | Tpoly({desc = Tconstr(p, [elt_ty], _)}, _)
    when Path.same p Predef.path_array ->
      maybe_pointer_type env elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pointer

let array_label_kind env lbl ty =
  let repr =
    match lbl.lbl_kind with
    | Array k -> k
    | ArrayLength k -> k
    | Record _ -> assert false
  in
    match repr with
    | Array_regular -> Paddrarray
    | Array_dynamic -> array_type_kind env ty
    | Array_float -> Pfloatarray

let array_expression_kind lbl exp =
  array_label_kind exp.exp_env lbl exp.exp_type

let array_pattern_kind lbl pat =
  array_label_kind pat.pat_env lbl pat.pat_type

(* Access and set primitives for record labels *)
let access_label lbl arg =
  match lbl.lbl_kind with
  | Record (Record_regular | Record_inlined _) -> Pfield lbl.lbl_pos
  | Record Record_float -> Pfloatfield lbl.lbl_pos
  | Record Record_extension -> Pfield (lbl.lbl_pos + 1)
  | ArrayLength Array_regular -> Parraylength Paddrarray
  | ArrayLength Array_dynamic ->
      let kind = array_type_kind arg.exp_env arg.exp_type in
        Parraylength kind
  | ArrayLength Array_float -> Parraylength Pfloatarray
  | Array _ -> assert false

let set_record_label lbl newval init =
  match lbl.lbl_kind with
  | Record(Record_regular | Record_inlined _) ->
      Psetfield(lbl.lbl_pos, maybe_pointer newval, init)
  | Record Record_float ->
      Psetfloatfield(lbl.lbl_pos, init)
  | Record Record_extension ->
      Psetfield(lbl.lbl_pos + 1, maybe_pointer newval, init)
  | ArrayLength _ -> assert false
  | Array _ -> assert false

let access_record_label lbl =
  match lbl.lbl_kind with
  | Record (Record_regular | Record_inlined _) -> Pfield lbl.lbl_pos
  | Record Record_float -> Pfloatfield lbl.lbl_pos
  | Record Record_extension -> Pfield (lbl.lbl_pos + 1)
  | ArrayLength _ -> assert false
  | Array _ -> assert false

(* Access and set primitives for array labels *)
let access_array_label lbl res =
  let kind =
  match lbl.lbl_kind with
  | Array Array_regular -> Paddrarray
  | Array Array_dynamic ->
      array_element_kind res.exp_env res.exp_type
  | Array Array_float -> Pfloatarray
  | ArrayLength _ -> assert false
  | Record _ -> assert false
  in
    if !Clflags.fast then Parrayrefu kind
    else Parrayrefs kind

let set_array_label lbl newval =
  let kind =
  match lbl.lbl_kind with
  | Array Array_regular -> Paddrarray
  | Array Array_dynamic ->
      array_element_kind newval.exp_env newval.exp_type
  | Array Array_float -> Pfloatarray
  | ArrayLength _ -> assert false
  | Record _ -> assert false
  in
    if !Clflags.fast then Parraysetu (kind, maybe_pointer newval)
    else Parraysets (kind, maybe_pointer newval)

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Tconstr(Pdot(Pident mod_id, type_name, _), [], _)
    when Ident.name mod_id = "Bigarray" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float32_elt", Pbigarray_float32;
   "float64_elt", Pbigarray_float64;
   "int8_signed_elt", Pbigarray_sint8;
   "int8_unsigned_elt", Pbigarray_uint8;
   "int16_signed_elt", Pbigarray_sint16;
   "int16_unsigned_elt", Pbigarray_uint16;
   "int32_elt", Pbigarray_int32;
   "int64_elt", Pbigarray_int64;
   "int_elt", Pbigarray_caml_int;
   "nativeint_elt", Pbigarray_native_int;
   "complex32_elt", Pbigarray_complex32;
   "complex64_elt", Pbigarray_complex64]

let layout_table =
  ["c_layout", Pbigarray_c_layout;
   "fortran_layout", Pbigarray_fortran_layout]

let bigarray_type_kind_and_layout env typ =
  match scrape env typ with
  | Tconstr(_p, [_caml_type; elt_type; layout_type], _abbrev) ->
      (bigarray_decode_type env elt_type kind_table Pbigarray_unknown,
       bigarray_decode_type env layout_type layout_table
                            Pbigarray_unknown_layout)
  | _ ->
      (Pbigarray_unknown, Pbigarray_unknown_layout)

let value_kind env ty =
  match scrape env ty with
  | Tconstr(p, _, _) when Path.same p Predef.path_int ->
      Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_char ->
      Pintval
  | Tconstr(p, _, _) when Path.same p Predef.path_float ->
      Pfloatval
  | Tconstr(p, _, _) when Path.same p Predef.path_int32 ->
      Pboxedintval Pint32
  | Tconstr(p, _, _) when Path.same p Predef.path_int64 ->
      Pboxedintval Pint64
  | Tconstr(p, _, _) when Path.same p Predef.path_nativeint ->
      Pboxedintval Pnativeint
  | _ ->
      Pgenval
