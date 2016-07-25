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
      | _ -> desc
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

let has_base_type exp base_ty_path =
  is_base_type exp.exp_env exp.exp_type base_ty_path

let maybe_pointer_type env ty =
  if Ctype.maybe_pointer_type env ty then
    Pointer
  else
    Immediate

let maybe_pointer exp = maybe_pointer_type exp.exp_env exp.exp_type

let array_element_kind env ty =
  let ty = scrape_ty env ty in
  if maybe_pointer_type env ty = Immediate then Pintarray
  else match ty.desc with
  | Tvar _ | Tunivar _ ->
      Pgenarray
  | Tconstr(p, _args, _abbrev) ->
      if Path.same p Predef.path_float then
        Pfloatarray
      else if Path.same p Predef.path_string
           || Path.same p Predef.path_array
           || Path.same p Predef.path_nativeint
           || Path.same p Predef.path_int32
           || Path.same p Predef.path_int64 then
        Paddrarray
      else begin
        try
          match (Env.find_type p env).type_kind with
          | Type_abstract ->
              Pgenarray
          | Type_record _ | Type_variant _ | Type_open ->
              Paddrarray
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Pgenarray
      end
  | _ ->
      Paddrarray

let array_type_kind env ty =
  match scrape env ty with
  | Tconstr(p, [elt_ty], _) | Tpoly({desc = Tconstr(p, [elt_ty], _)}, _)
    when Path.same p Predef.path_array ->
      array_element_kind env elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_type_kind exp.exp_env exp.exp_type

let array_pattern_kind pat = array_type_kind pat.pat_env pat.pat_type

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


let lazy_val_requires_forward env ty =
  let ty = Ctype.repr ty in
  match ty.desc with
  (* the following may represent a float/forward/lazy: need a
     forward_tag *)
  | Tvar _ | Tunivar _
  | Tpoly _ | Tfield _ ->
      true
  (* the following cannot be represented as float/forward/lazy:
     optimize *)
  | Tarrow _ | Ttuple _ | Tpackage _ | Tobject _ | Tnil | Tvariant _ ->
      false
  (* optimize predefined types (excepted float) *)
  | Tconstr _ when
      is_base_type env ty Predef.path_int
      || is_base_type env ty Predef.path_char
      || is_base_type env ty Predef.path_string
      || is_base_type env ty Predef.path_bool
      || is_base_type env ty Predef.path_unit
      || is_base_type env ty Predef.path_exn
      || is_base_type env ty Predef.path_array
      || is_base_type env ty Predef.path_list
      || is_base_type env ty Predef.path_option
      || is_base_type env ty Predef.path_nativeint
      || is_base_type env ty Predef.path_int32
      || is_base_type env ty Predef.path_int64 ->
      false
  | Tconstr _ ->
      true
  | Tlink _ | Tsubst _ ->
      assert false
