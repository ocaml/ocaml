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
open Asttypes
open Types
open Typedtree
open Lambda

let scrape_ty env ty =
  let ty = Ctype.expand_head_opt env (Ctype.correct_levels ty) in
  match ty.desc with
  | Tconstr (p, _, _) ->
      begin match Env.find_type p env with
      | {type_unboxed = {unboxed = true; _}; _} ->
        begin match Typedecl.get_unboxed_type_representation env ty with
        | None -> ty
        | Some ty2 -> ty2
        end
      | _ -> ty
      | exception Not_found -> ty
      end
  | _ -> ty

let scrape env ty =
  (scrape_ty env ty).desc

let is_function_type env ty =
  match scrape env ty with
  | Tarrow (_, lhs, rhs, _) -> Some (lhs, rhs)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Tconstr(p, _, _) -> Path.same p base_ty_path
  | _ -> false

let expr_type_representation exp = Ctype.type_representation exp.exp_env exp.exp_type

let array_type_kind env ty =
  match scrape env ty with
  | Tconstr(p, [elt_ty], _) | Tpoly({desc = Tconstr(p, [elt_ty], _)}, _)
    when Path.same p Predef.path_array ->
      begin match Ctype.type_representation env elt_ty with
      | Generic -> Pgenarray
      | Float -> Pfloatarray
      | Non_float | Addr | Lazy -> Paddrarray
      | Immediate -> Pintarray
      end

  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_type_kind exp.exp_env exp.exp_type

let array_pattern_kind pat = array_type_kind pat.pat_env pat.pat_type

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Tconstr(Pdot(Pident mod_id, type_name, _), [], _)
    when Ident.name mod_id = "CamlinternalBigarray" ->
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


let lazy_val_requires_forward env ty =
  match Ctype.type_representation env ty with
  | Generic | Float | Lazy | Non_float -> true
  | Addr | Immediate -> false
