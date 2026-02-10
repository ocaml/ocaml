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
  let ty =
    match get_desc ty with
    | Tpoly(ty, _) -> ty
    | _ -> ty
  in
  match get_desc ty with
  | Tconstr _ ->
      let ty = Ctype.expand_head_opt env ty in
      begin match get_desc ty with
      | Tconstr (p, _, _) ->
          begin match Env.find_type p env with
          | {type_kind = ( Type_variant (_, Variant_unboxed)
          | Type_record (_, Record_unboxed _) ); _} ->
            Typedecl_unboxed.get_unboxed_type_representation env ty
          | _ -> Some ty
          | exception Not_found -> None
          end
      | _ ->
          Some ty
      end
  | _ -> Some ty

let scrape env ty =
  Option.map get_desc (scrape_ty env ty)

let scrape_poly env ty =
  let ty = scrape_ty env ty in
  Option.map (fun ty ->
      match get_desc ty with
      | Tpoly (ty, _) -> get_desc ty
      | d -> d)
    ty

let is_function_type env ty =
  match scrape env ty with
  | Some (Tarrow (_, lhs, rhs, _)) -> Some (lhs, rhs)
  | _ -> None

let is_base_type env ty base_ty_path =
  match scrape env ty with
  | Some (Tconstr(p, _, _)) -> Path.same p base_ty_path
  | _ -> false

let is_immediate = function
  | Type_immediacy.Unknown -> false
  | Type_immediacy.Always -> true
  | Type_immediacy.Always_on_64bits ->
      (* In bytecode, we don't know at compile time whether we are
         targeting 32 or 64 bits. *)
      !Clflags.native_code && Sys.word_size = 64

let maybe_pointer_type env ty =
  match scrape_ty env ty with
  | Some ty ->
    if is_immediate (Ctype.immediacy env ty) then Immediate
    else Pointer
  | None -> Pointer

let maybe_pointer exp = maybe_pointer_type exp.exp_env exp.exp_type

type classification =
  | Int
  | Float
  | Lazy
  | Addr  (* anything except a float or a lazy *)
  | Any

let classify env ty : classification =
  match scrape_ty env ty with
  | None -> Any
  | Some ty ->
  if maybe_pointer_type env ty = Immediate then Int
  else match get_desc ty with
  | Tvar _ | Tunivar _ ->
      Any
  | Tconstr (p, _args, _abbrev) ->
      begin match Predef.find_type_constr p with
      | Some `Float -> Float
      | Some `Lazy_t -> Lazy
      | Some (`Int | `Char) -> Int
      | Some (`String | `Bytes
             | `Int32 | `Int64 | `Nativeint
             | `Extension_constructor | `Continuation
             | `Array | `Floatarray | `Iarray
             | `Atomic_loc)
        -> Addr
      | Some #Predef.data_type_constr | None ->
        try
          match (Env.find_type p env).type_kind with
          | Type_abstract _ | Type_external _ ->
              Any
          | Type_record _ | Type_variant _ | Type_open ->
              Addr
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Any
      end
  | Tarrow _ | Ttuple _ | Tpackage _ | Tobject _
  | Tnil | Tvariant _ | Tfunctor _ -> Addr
  | Tlink _ | Tsubst _ | Tpoly _ | Tfield _ ->
      assert false

let array_type_kind env ty =
  match scrape_poly env ty with
  | Some (Tconstr(p, [elt_ty], _))
    when Path.same p Predef.path_array || Path.same p Predef.path_iarray ->
      begin match classify env elt_ty with
      | Any -> if Config.flat_float_array then Pgenarray else Paddrarray
      | Float -> if Config.flat_float_array then Pfloatarray else Paddrarray
      | Addr | Lazy -> Paddrarray
      | Int -> Pintarray
      end
  | Some (Tconstr(p, [], _)) when Path.same p Predef.path_floatarray ->
      Pfloatarray
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_type_kind exp.exp_env exp.exp_type

let array_pattern_kind pat = array_type_kind pat.pat_env pat.pat_type

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Some (Tconstr(Pdot(Pident mod_id, type_name), [], _))
    when Ident.name mod_id = "Stdlib__Bigarray" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float16_elt", Pbigarray_float16;
   "float32_elt", Pbigarray_float32;
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
  | Some (Tconstr(_p, [_caml_type; elt_type; layout_type], _abbrev)) ->
      (bigarray_decode_type env elt_type kind_table Pbigarray_unknown,
       bigarray_decode_type env layout_type layout_table
                            Pbigarray_unknown_layout)
  | _ ->
      (Pbigarray_unknown, Pbigarray_unknown_layout)

let value_kind env ty =
  match scrape_ty env ty with
  | None -> Pgenval
  | Some ty ->
  if is_immediate (Ctype.immediacy env ty) then Pintval
  else begin
    match get_desc ty with
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
  end

(** The compilation of the expression [lazy e] depends on the form of e:
    in some cases we optimize it into [let x = e in lazy x], evaluating
    [e] right now (if it is equivalent) and avoiding creating a thunk.

    We can also avoid creating a forward block completely in some
    cases, depending on the value type.

    This optimization must be taken into account when determining
    whether a recursive binding is safe.
*)

type lazy_summary =
  | Lazy_thunk
  | Eager of forward_repr
and forward_repr =
  | Forward
  | Shortcut

let classify_lazy_argument e =
  (* We can compile [lazy e] into [let x = e in lazy x] whenever [e]
     is what we call "commutative", when we can safely evaluate it
     earlier. In terms of [middle_end/semantics_of_primitive], this is
     valid when [e] has no coeffects (so it does not depend on the
     evaluation of other expressions) and only generative effects
     (so it does not influence the evaluation of other expressions).

     If a commutative expression is very large, it may be
     a performance pessimization to force its evaluation earlier in
     the program -- maybe the user put it under a [lazy] because it is
     not needed most of the time. We implement a cutoff based on
     expression size.
  *)
  let size_cutoff = 42 in
  let size = ref 0 in
  let rec small_and_commutative e =
    incr size;
    !size <= size_cutoff && match e.exp_desc with
    | Texp_ident _ | Texp_constant _ | Texp_function _ | Texp_lazy _ -> true
    | Texp_variant (_, args) ->
        Option.for_all small_and_commutative args
    | Texp_construct (_, _, args) | Texp_array (_, args) ->
        List.for_all small_and_commutative args
    | Texp_tuple args ->
        List.for_all (fun (_lbl, arg) -> small_and_commutative arg) args
    | Texp_record { fields; extended_expression } ->
        Array.for_all (fun (_lbl, def) ->
          match def with
          | Overridden (_, e) -> small_and_commutative e
          | Kept _ -> incr size; true
        ) fields
        && Option.for_all small_and_commutative extended_expression
    | Texp_extension_constructor _ -> true
    (* under-approximations: these could be refined
       -- below we write [c] for any commutative expression. *)
    | Texp_let _ (* [let x = c in c] would be ok *)
    | Texp_pack _ (* commutative modules would be ok *)
    | Texp_field _ (* [v.x] for immutable fields would be ok *)
    | Texp_atomic_loc _ (* [%atomic.loc c.x] would be ok *)
    | Texp_object _ (* commutative objects would be ok *)
    | Texp_struct_item _ (* [let <structure item> in c] would be ok *)
      -> false
    (* (typically) not commutative *)
    | Texp_apply _
    | Texp_match _
    | Texp_try _
    | Texp_setfield _
    | Texp_ifthenelse _
    | Texp_sequence _
    | Texp_while _
    | Texp_for _
    | Texp_send _
    | Texp_new _
    | Texp_instvar _
    | Texp_setinstvar _
    | Texp_override _
    | Texp_assert _
    | Texp_letop _
    | Texp_unreachable
      -> false
  in
  (* In [let x = e in lazy x], [lazy x] sometimes need to be
     a [Forward] block, but this block can typically be shortcut into
     just [x]. The forward block is required for expressions that may
     have a lazy type themselves, or float when flat-float-array is
     enabled. *)
  if not (small_and_commutative e) then Lazy_thunk
  else Eager (match classify e.exp_env e.exp_type with
    | Addr | Int -> Shortcut
    | Any | Lazy -> Forward
    | Float ->
        if Config.flat_float_array
        then Forward
        else Shortcut
  )

let value_kind_union k1 k2 =
  if k1 = k2 then k1
  else Pgenval
