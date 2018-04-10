(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation of primitives *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error =
  | Unknown_builtin_primitive of string
  | Wrong_arity_builtin_primitive of string

exception Error of Location.t * error

(* Insertion of debugging events *)

let event_before exp lam = match lam with
| Lstaticraise (_,_) -> lam
| _ ->
  if !Clflags.debug && not !Clflags.native_code
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

let event_after exp lam =
  if !Clflags.debug && not !Clflags.native_code
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

type comparison =
  | Equal
  | Not_equal
  | Less_equal
  | Less_than
  | Greater_equal
  | Greater_than
  | Compare

type comparison_kind =
  | Compare_generic
  | Compare_ints
  | Compare_floats
  | Compare_strings
  | Compare_bytes
  | Compare_nativeints
  | Compare_int32s
  | Compare_int64s

type loc_kind =
  | Loc_FILE
  | Loc_LINE
  | Loc_MODULE
  | Loc_LOC
  | Loc_POS

type prim =
  | Primitive of Lambda.primitive
  | Comparison of comparison * comparison_kind
  | Raise of Lambda.raise_kind
  | Raise_with_backtrace
  | Lazy_force
  | Loc of loc_kind
  | Send
  | Send_self
  | Send_cache

let used_primitives = Hashtbl.create 7
let add_used_primitive loc env path =
  match path with
    Some (Path.Pdot _ as path) ->
      let path = Env.normalize_path (Some loc) env path in
      let unit = Path.head path in
      if Ident.global unit && not (Hashtbl.mem used_primitives path)
      then Hashtbl.add used_primitives path loc
  | _ -> ()

let clear_used_primitives () = Hashtbl.clear used_primitives
let get_used_primitives () =
  Hashtbl.fold (fun path _ acc -> path :: acc) used_primitives []

let gen_array_kind =
  if Config.flat_float_array then Pgenarray else Paddrarray

let primitives_table = create_hashtable 57 [
  "%identity", Primitive Pidentity;
  "%bytes_to_string", Primitive Pbytes_to_string;
  "%bytes_of_string", Primitive Pbytes_of_string;
  "%ignore", Primitive Pignore;
  "%revapply", Primitive Prevapply;
  "%apply", Primitive Pdirapply;
  "%loc_LOC", Loc Loc_LOC;
  "%loc_FILE", Loc Loc_FILE;
  "%loc_LINE", Loc Loc_LINE;
  "%loc_POS", Loc Loc_POS;
  "%loc_MODULE", Loc Loc_MODULE;
  "%field0", Primitive (Pfield 0);
  "%field1", Primitive (Pfield 1);
  "%setfield0", Primitive (Psetfield(0, Pointer, Assignment));
  "%makeblock", Primitive (Pmakeblock(0, Immutable, None));
  "%makemutable", Primitive (Pmakeblock(0, Mutable, None));
  "%raise", Raise Raise_regular;
  "%reraise", Raise Raise_reraise;
  "%raise_notrace", Raise Raise_notrace;
  "%raise_with_backtrace", Raise_with_backtrace;
  "%sequand", Primitive Psequand;
  "%sequor", Primitive Psequor;
  "%boolnot", Primitive Pnot;
  "%big_endian", Primitive (Pctconst Big_endian);
  "%backend_type", Primitive (Pctconst Backend_type);
  "%word_size", Primitive (Pctconst Word_size);
  "%int_size", Primitive (Pctconst Int_size);
  "%max_wosize", Primitive (Pctconst Max_wosize);
  "%ostype_unix", Primitive (Pctconst Ostype_unix);
  "%ostype_win32", Primitive (Pctconst Ostype_win32);
  "%ostype_cygwin", Primitive (Pctconst Ostype_cygwin);
  "%negint", Primitive Pnegint;
  "%succint", Primitive (Poffsetint 1);
  "%predint", Primitive (Poffsetint(-1));
  "%addint", Primitive Paddint;
  "%subint", Primitive Psubint;
  "%mulint", Primitive Pmulint;
  "%divint", Primitive (Pdivint Safe);
  "%modint", Primitive (Pmodint Safe);
  "%andint", Primitive Pandint;
  "%orint", Primitive Porint;
  "%xorint", Primitive Pxorint;
  "%lslint", Primitive Plslint;
  "%lsrint", Primitive Plsrint;
  "%asrint", Primitive Pasrint;
  "%eq", Primitive (Pintcomp Ceq);
  "%noteq", Primitive (Pintcomp Cne);
  "%ltint", Primitive (Pintcomp Clt);
  "%leint", Primitive (Pintcomp Cle);
  "%gtint", Primitive (Pintcomp Cgt);
  "%geint", Primitive (Pintcomp Cge);
  "%incr", Primitive (Poffsetref(1));
  "%decr", Primitive (Poffsetref(-1));
  "%intoffloat", Primitive Pintoffloat;
  "%floatofint", Primitive Pfloatofint;
  "%negfloat", Primitive Pnegfloat;
  "%absfloat", Primitive Pabsfloat;
  "%addfloat", Primitive Paddfloat;
  "%subfloat", Primitive Psubfloat;
  "%mulfloat", Primitive Pmulfloat;
  "%divfloat", Primitive Pdivfloat;
  "%eqfloat", Primitive (Pfloatcomp CFeq);
  "%noteqfloat", Primitive (Pfloatcomp CFneq);
  "%ltfloat", Primitive (Pfloatcomp CFlt);
  "%lefloat", Primitive (Pfloatcomp CFle);
  "%gtfloat", Primitive (Pfloatcomp CFgt);
  "%gefloat", Primitive (Pfloatcomp CFge);
  "%string_length", Primitive Pstringlength;
  "%string_safe_get", Primitive Pstringrefs;
  "%string_safe_set", Primitive Pbytessets;
  "%string_unsafe_get", Primitive Pstringrefu;
  "%string_unsafe_set", Primitive Pbytessetu;
  "%bytes_length", Primitive Pbyteslength;
  "%bytes_safe_get", Primitive Pbytesrefs;
  "%bytes_safe_set", Primitive Pbytessets;
  "%bytes_unsafe_get", Primitive Pbytesrefu;
  "%bytes_unsafe_set", Primitive Pbytessetu;
  "%array_length", Primitive (Parraylength gen_array_kind);
  "%array_safe_get", Primitive (Parrayrefs gen_array_kind);
  "%array_safe_set", Primitive (Parraysets gen_array_kind);
  "%array_unsafe_get", Primitive (Parrayrefu gen_array_kind);
  "%array_unsafe_set", Primitive (Parraysetu gen_array_kind);
  "%obj_size", Primitive (Parraylength gen_array_kind);
  "%obj_field", Primitive (Parrayrefu gen_array_kind);
  "%obj_set_field", Primitive (Parraysetu gen_array_kind);
  "%floatarray_length", Primitive (Parraylength Pfloatarray);
  "%floatarray_safe_get", Primitive (Parrayrefs Pfloatarray);
  "%floatarray_safe_set", Primitive (Parraysets Pfloatarray);
  "%floatarray_unsafe_get", Primitive (Parrayrefu Pfloatarray);
  "%floatarray_unsafe_set", Primitive (Parraysetu Pfloatarray);
  "%obj_is_int", Primitive Pisint;
  "%lazy_force", Lazy_force;
  "%nativeint_of_int", Primitive (Pbintofint Pnativeint);
  "%nativeint_to_int", Primitive (Pintofbint Pnativeint);
  "%nativeint_neg", Primitive (Pnegbint Pnativeint);
  "%nativeint_add", Primitive (Paddbint Pnativeint);
  "%nativeint_sub", Primitive (Psubbint Pnativeint);
  "%nativeint_mul", Primitive (Pmulbint Pnativeint);
  "%nativeint_div", Primitive (Pdivbint { size = Pnativeint; is_safe = Safe });
  "%nativeint_mod", Primitive (Pmodbint { size = Pnativeint; is_safe = Safe });
  "%nativeint_and", Primitive (Pandbint Pnativeint);
  "%nativeint_or", Primitive  (Porbint Pnativeint);
  "%nativeint_xor", Primitive (Pxorbint Pnativeint);
  "%nativeint_lsl", Primitive (Plslbint Pnativeint);
  "%nativeint_lsr", Primitive (Plsrbint Pnativeint);
  "%nativeint_asr", Primitive (Pasrbint Pnativeint);
  "%int32_of_int", Primitive (Pbintofint Pint32);
  "%int32_to_int", Primitive (Pintofbint Pint32);
  "%int32_neg", Primitive (Pnegbint Pint32);
  "%int32_add", Primitive (Paddbint Pint32);
  "%int32_sub", Primitive (Psubbint Pint32);
  "%int32_mul", Primitive (Pmulbint Pint32);
  "%int32_div", Primitive (Pdivbint { size = Pint32; is_safe = Safe });
  "%int32_mod", Primitive (Pmodbint { size = Pint32; is_safe = Safe });
  "%int32_and", Primitive (Pandbint Pint32);
  "%int32_or", Primitive  (Porbint Pint32);
  "%int32_xor", Primitive (Pxorbint Pint32);
  "%int32_lsl", Primitive (Plslbint Pint32);
  "%int32_lsr", Primitive (Plsrbint Pint32);
  "%int32_asr", Primitive (Pasrbint Pint32);
  "%int64_of_int", Primitive (Pbintofint Pint64);
  "%int64_to_int", Primitive (Pintofbint Pint64);
  "%int64_neg", Primitive (Pnegbint Pint64);
  "%int64_add", Primitive (Paddbint Pint64);
  "%int64_sub", Primitive (Psubbint Pint64);
  "%int64_mul", Primitive (Pmulbint Pint64);
  "%int64_div", Primitive (Pdivbint { size = Pint64; is_safe = Safe });
  "%int64_mod", Primitive (Pmodbint { size = Pint64; is_safe = Safe });
  "%int64_and", Primitive (Pandbint Pint64);
  "%int64_or", Primitive  (Porbint Pint64);
  "%int64_xor", Primitive (Pxorbint Pint64);
  "%int64_lsl", Primitive (Plslbint Pint64);
  "%int64_lsr", Primitive (Plsrbint Pint64);
  "%int64_asr", Primitive (Pasrbint Pint64);
  "%nativeint_of_int32", Primitive (Pcvtbint(Pint32, Pnativeint));
  "%nativeint_to_int32", Primitive (Pcvtbint(Pnativeint, Pint32));
  "%int64_of_int32", Primitive (Pcvtbint(Pint32, Pint64));
  "%int64_to_int32", Primitive (Pcvtbint(Pint64, Pint32));
  "%int64_of_nativeint", Primitive (Pcvtbint(Pnativeint, Pint64));
  "%int64_to_nativeint", Primitive (Pcvtbint(Pint64, Pnativeint));
  "%caml_ba_ref_1",
    Primitive
      (Pbigarrayref(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_ref_2",
    Primitive
      (Pbigarrayref(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_ref_3",
    Primitive
      (Pbigarrayref(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_set_1",
    Primitive
      (Pbigarrayset(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_set_2",
    Primitive
      (Pbigarrayset(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_set_3",
    Primitive
      (Pbigarrayset(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_unsafe_ref_1",
    Primitive
      (Pbigarrayref(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_unsafe_ref_2",
    Primitive
      (Pbigarrayref(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_unsafe_ref_3",
    Primitive
      (Pbigarrayref(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_unsafe_set_1",
    Primitive
      (Pbigarrayset(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_unsafe_set_2",
    Primitive
      (Pbigarrayset(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_unsafe_set_3",
    Primitive
      (Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout));
  "%caml_ba_dim_1", Primitive (Pbigarraydim(1));
  "%caml_ba_dim_2", Primitive (Pbigarraydim(2));
  "%caml_ba_dim_3", Primitive (Pbigarraydim(3));
  "%caml_string_get16", Primitive (Pstring_load_16(false));
  "%caml_string_get16u", Primitive (Pstring_load_16(true));
  "%caml_string_get32", Primitive (Pstring_load_32(false));
  "%caml_string_get32u", Primitive (Pstring_load_32(true));
  "%caml_string_get64", Primitive (Pstring_load_64(false));
  "%caml_string_get64u", Primitive (Pstring_load_64(true));
  "%caml_string_set16", Primitive (Pbytes_set_16(false));
  "%caml_string_set16u", Primitive (Pbytes_set_16(true));
  "%caml_string_set32", Primitive (Pbytes_set_32(false));
  "%caml_string_set32u", Primitive (Pbytes_set_32(true));
  "%caml_string_set64", Primitive (Pbytes_set_64(false));
  "%caml_string_set64u", Primitive (Pbytes_set_64(true));
  "%caml_bytes_get16", Primitive (Pbytes_load_16(false));
  "%caml_bytes_get16u", Primitive (Pbytes_load_16(true));
  "%caml_bytes_get32", Primitive (Pbytes_load_32(false));
  "%caml_bytes_get32u", Primitive (Pbytes_load_32(true));
  "%caml_bytes_get64", Primitive (Pbytes_load_64(false));
  "%caml_bytes_get64u", Primitive (Pbytes_load_64(true));
  "%caml_bytes_set16", Primitive (Pbytes_set_16(false));
  "%caml_bytes_set16u", Primitive (Pbytes_set_16(true));
  "%caml_bytes_set32", Primitive (Pbytes_set_32(false));
  "%caml_bytes_set32u", Primitive (Pbytes_set_32(true));
  "%caml_bytes_set64", Primitive (Pbytes_set_64(false));
  "%caml_bytes_set64u", Primitive (Pbytes_set_64(true));
  "%caml_bigstring_get16", Primitive (Pbigstring_load_16(false));
  "%caml_bigstring_get16u", Primitive (Pbigstring_load_16(true));
  "%caml_bigstring_get32", Primitive (Pbigstring_load_32(false));
  "%caml_bigstring_get32u", Primitive (Pbigstring_load_32(true));
  "%caml_bigstring_get64", Primitive (Pbigstring_load_64(false));
  "%caml_bigstring_get64u", Primitive (Pbigstring_load_64(true));
  "%caml_bigstring_set16", Primitive (Pbigstring_set_16(false));
  "%caml_bigstring_set16u", Primitive (Pbigstring_set_16(true));
  "%caml_bigstring_set32", Primitive (Pbigstring_set_32(false));
  "%caml_bigstring_set32u", Primitive (Pbigstring_set_32(true));
  "%caml_bigstring_set64", Primitive (Pbigstring_set_64(false));
  "%caml_bigstring_set64u", Primitive (Pbigstring_set_64(true));
  "%bswap16", Primitive Pbswap16;
  "%bswap_int32", Primitive (Pbbswap(Pint32));
  "%bswap_int64", Primitive (Pbbswap(Pint64));
  "%bswap_native", Primitive (Pbbswap(Pnativeint));
  "%int_as_pointer", Primitive Pint_as_pointer;
  "%opaque", Primitive Popaque;
  "%send", Send;
  "%sendself", Send_self;
  "%sendcache", Send_cache;
  "%equal", Comparison(Equal, Compare_generic);
  "%notequal", Comparison(Not_equal, Compare_generic);
  "%lessequal", Comparison(Less_equal, Compare_generic);
  "%lessthan", Comparison(Less_than, Compare_generic);
  "%greaterequal", Comparison(Greater_equal, Compare_generic);
  "%greaterthan", Comparison(Greater_than, Compare_generic);
  "%compare", Comparison(Compare, Compare_generic);
]

let lookup_primitive loc p env path =
  match Hashtbl.find primitives_table p.prim_name with
  | prim -> prim
  | exception Not_found ->
      if String.length p.prim_name > 0 && p.prim_name.[0] = '%' then
        raise(Error(loc, Unknown_builtin_primitive p.prim_name));
      add_used_primitive loc env path;
      Primitive (Pccall p)

let simplify_constant_constructor = function
  | Equal -> true
  | Not_equal -> true
  | Less_equal -> false
  | Less_than -> false
  | Greater_equal -> false
  | Greater_than -> false
  | Compare -> false

(* The following function computes the greatest lower bound in the
   semilattice of array kinds:
          gen
         /   \
      addr   float
       |
      int
   Note that the GLB is not guaranteed to exist, in which case we return
   our first argument instead of raising a fatal error because, although
   it cannot happen in a well-typed program, (ab)use of Obj.magic can
   probably trigger it.
*)
let glb_array_type t1 t2 =
  match t1, t2 with
  | Pfloatarray, (Paddrarray | Pintarray)
  | (Paddrarray | Pintarray), Pfloatarray -> t1

  | Pgenarray, x | x, Pgenarray -> x
  | Paddrarray, x | x, Paddrarray -> x
  | Pintarray, Pintarray -> Pintarray
  | Pfloatarray, Pfloatarray -> Pfloatarray

(* Specialize a primitive from available type information. *)

let specialize_primitive env ty ~has_constant_constructor prim =
  let param_tys =
    match is_function_type env ty with
    | None -> []
    | Some (p1, rhs) ->
      match is_function_type env rhs with
      | None -> [p1]
      | Some (p2, _) -> [p1;p2]
  in
  match prim, param_tys with
  | Primitive (Psetfield(n, Pointer, init)), [_; p2] -> begin
      match maybe_pointer_type env p2 with
      | Pointer -> None
      | Immediate -> Some (Primitive (Psetfield(n, Immediate, init)))
    end
  | Primitive (Parraylength t), [p] -> begin
      let array_type = glb_array_type t (array_type_kind env p) in
      if t = array_type then None
      else Some (Primitive (Parraylength array_type))
    end
  | Primitive (Parrayrefu t), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parrayrefu array_type))
    end
  | Primitive (Parraysetu t), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parraysetu array_type))
    end
  | Primitive (Parrayrefs t), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parrayrefs array_type))
    end
  | Primitive (Parraysets t), p1 :: _ -> begin
      let array_type = glb_array_type t (array_type_kind env p1) in
      if t = array_type then None
      else Some (Primitive (Parraysets array_type))
    end
  | Primitive (Pbigarrayref(unsafe, n, Pbigarray_unknown,
                            Pbigarray_unknown_layout)), p1 :: _ -> begin
      let (k, l) = bigarray_type_kind_and_layout env p1 in
      match k, l with
      | Pbigarray_unknown, Pbigarray_unknown_layout -> None
      | _, _ -> Some (Primitive (Pbigarrayref(unsafe, n, k, l)))
    end
  | Primitive (Pbigarrayset(unsafe, n, Pbigarray_unknown,
                            Pbigarray_unknown_layout)), p1 :: _ -> begin
      let (k, l) = bigarray_type_kind_and_layout env p1 in
      match k, l with
      | Pbigarray_unknown, Pbigarray_unknown_layout -> None
      | _, _ -> Some (Primitive (Pbigarrayset(unsafe, n, k, l)))
    end
  | Primitive (Pmakeblock(tag, mut, None)), fields -> begin
      let shape = List.map (Typeopt.value_kind env) fields in
      let useful = List.exists (fun knd -> knd <> Pgenval) shape in
      if useful then Some (Primitive (Pmakeblock(tag, mut, Some shape)))
      else None
    end
  | Comparison(comp, Compare_generic), p1 :: _ ->
    if (has_constant_constructor
        && simplify_constant_constructor comp) then begin
      Some (Comparison(comp, Compare_ints))
    end else if (is_base_type env p1 Predef.path_int
        || is_base_type env p1 Predef.path_char
        || (maybe_pointer_type env p1 = Immediate)) then begin
      Some (Comparison(comp, Compare_ints))
    end else if is_base_type env p1 Predef.path_float then begin
      Some (Comparison(comp, Compare_floats))
    end else if is_base_type env p1 Predef.path_string then begin
      Some (Comparison(comp, Compare_strings))
    end else if is_base_type env p1 Predef.path_bytes then begin
      Some (Comparison(comp, Compare_bytes))
    end else if is_base_type env p1 Predef.path_nativeint then begin
      Some (Comparison(comp, Compare_nativeints))
    end else if is_base_type env p1 Predef.path_int32 then begin
      Some (Comparison(comp, Compare_int32s))
    end else if is_base_type env p1 Predef.path_int64 then begin
      Some (Comparison(comp, Compare_int64s))
    end else begin
      None
    end
  | _ -> None

let unboxed_compare name native_repr =
  Primitive.make ~name ~alloc:false ~native_name:(name^"_unboxed")
    ~native_repr_args:[native_repr;native_repr] ~native_repr_res:Untagged_int

let caml_equal =
  Primitive.simple ~name:"caml_equal" ~arity:2 ~alloc:true
let caml_string_equal =
  Primitive.simple ~name:"caml_string_equal" ~arity:2 ~alloc:false
let caml_bytes_equal =
  Primitive.simple ~name:"caml_bytes_equal" ~arity:2 ~alloc:false
let caml_notequal =
  Primitive.simple ~name:"caml_notequal" ~arity:2 ~alloc:true
let caml_string_notequal =
  Primitive.simple ~name:"caml_string_notequal" ~arity:2 ~alloc:false
let caml_bytes_notequal =
  Primitive.simple ~name:"caml_bytes_notequal" ~arity:2 ~alloc:false
let caml_lessequal =
  Primitive.simple ~name:"caml_lessequal" ~arity:2 ~alloc:true
let caml_string_lessequal =
  Primitive.simple ~name:"caml_string_lessequal" ~arity:2 ~alloc:false
let caml_bytes_lessequal =
  Primitive.simple ~name:"caml_bytes_lessequal" ~arity:2 ~alloc:false
let caml_lessthan =
  Primitive.simple ~name:"caml_lessthan" ~arity:2 ~alloc:true
let caml_string_lessthan =
  Primitive.simple ~name:"caml_string_lessthan" ~arity:2 ~alloc:false
let caml_bytes_lessthan =
  Primitive.simple ~name:"caml_bytes_lessthan" ~arity:2 ~alloc:false
let caml_greaterequal =
  Primitive.simple ~name:"caml_greaterequal" ~arity:2 ~alloc:true
let caml_string_greaterequal =
  Primitive.simple ~name:"caml_string_greaterequal" ~arity:2 ~alloc:false
let caml_bytes_greaterequal =
  Primitive.simple ~name:"caml_bytes_greaterequal" ~arity:2 ~alloc:false
let caml_greaterthan =
  Primitive.simple ~name:"caml_greaterthan" ~arity:2 ~alloc:true
let caml_string_greaterthan =
  Primitive.simple ~name:"caml_string_greaterthan" ~arity:2 ~alloc: false
let caml_bytes_greaterthan =
  Primitive.simple ~name:"caml_bytes_greaterthan" ~arity:2 ~alloc: false
let caml_compare =
  Primitive.simple ~name:"caml_compare" ~arity:2 ~alloc:true
let caml_int_compare =
  (* Not unboxed since the comparison is done directly on tagged int *)
  Primitive.simple ~name:"caml_int_compare" ~arity:2 ~alloc:false
let caml_float_compare =
  unboxed_compare "caml_float_compare" Unboxed_float
let caml_string_compare =
  Primitive.simple ~name:"caml_string_compare" ~arity:2 ~alloc:false
let caml_bytes_compare =
  Primitive.simple ~name:"caml_bytes_compare" ~arity:2 ~alloc:false
let caml_nativeint_compare =
  unboxed_compare "caml_nativeint_compare" (Unboxed_integer Pnativeint)
let caml_int32_compare =
  unboxed_compare "caml_int32_compare" (Unboxed_integer Pint32)
let caml_int64_compare =
  unboxed_compare "caml_int64_compare" (Unboxed_integer Pint64)

let comparison_primitive comparison comparison_kind =
  match comparison, comparison_kind with
  | Equal, Compare_generic -> Pccall caml_equal
  | Equal, Compare_ints -> Pintcomp Ceq
  | Equal, Compare_floats -> Pfloatcomp CFeq
  | Equal, Compare_strings -> Pccall caml_string_equal
  | Equal, Compare_bytes -> Pccall caml_bytes_equal
  | Equal, Compare_nativeints -> Pbintcomp(Pnativeint, Ceq)
  | Equal, Compare_int32s -> Pbintcomp(Pint32, Ceq)
  | Equal, Compare_int64s -> Pbintcomp(Pint64, Ceq)
  | Not_equal, Compare_generic -> Pccall caml_notequal
  | Not_equal, Compare_ints -> Pintcomp Cne
  | Not_equal, Compare_floats -> Pfloatcomp CFneq
  | Not_equal, Compare_strings -> Pccall caml_string_notequal
  | Not_equal, Compare_bytes -> Pccall caml_bytes_notequal
  | Not_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cne)
  | Not_equal, Compare_int32s -> Pbintcomp(Pint32, Cne)
  | Not_equal, Compare_int64s -> Pbintcomp(Pint64, Cne)
  | Less_equal, Compare_generic -> Pccall caml_lessequal
  | Less_equal, Compare_ints -> Pintcomp Cle
  | Less_equal, Compare_floats -> Pfloatcomp CFle
  | Less_equal, Compare_strings -> Pccall caml_string_lessequal
  | Less_equal, Compare_bytes -> Pccall caml_bytes_lessequal
  | Less_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cle)
  | Less_equal, Compare_int32s -> Pbintcomp(Pint32, Cle)
  | Less_equal, Compare_int64s -> Pbintcomp(Pint64, Cle)
  | Less_than, Compare_generic -> Pccall caml_lessthan
  | Less_than, Compare_ints -> Pintcomp Clt
  | Less_than, Compare_floats -> Pfloatcomp CFlt
  | Less_than, Compare_strings -> Pccall caml_string_lessthan
  | Less_than, Compare_bytes -> Pccall caml_bytes_lessthan
  | Less_than, Compare_nativeints -> Pbintcomp(Pnativeint, Clt)
  | Less_than, Compare_int32s -> Pbintcomp(Pint32, Clt)
  | Less_than, Compare_int64s -> Pbintcomp(Pint64, Clt)
  | Greater_equal, Compare_generic -> Pccall caml_greaterequal
  | Greater_equal, Compare_ints -> Pintcomp Cge
  | Greater_equal, Compare_floats -> Pfloatcomp CFge
  | Greater_equal, Compare_strings -> Pccall caml_string_greaterequal
  | Greater_equal, Compare_bytes -> Pccall caml_bytes_greaterequal
  | Greater_equal, Compare_nativeints -> Pbintcomp(Pnativeint, Cge)
  | Greater_equal, Compare_int32s -> Pbintcomp(Pint32, Cge)
  | Greater_equal, Compare_int64s -> Pbintcomp(Pint64, Cge)
  | Greater_than, Compare_generic -> Pccall caml_greaterthan
  | Greater_than, Compare_ints -> Pintcomp Cgt
  | Greater_than, Compare_floats -> Pfloatcomp CFgt
  | Greater_than, Compare_strings -> Pccall caml_string_greaterthan
  | Greater_than, Compare_bytes -> Pccall caml_bytes_greaterthan
  | Greater_than, Compare_nativeints -> Pbintcomp(Pnativeint, Cgt)
  | Greater_than, Compare_int32s -> Pbintcomp(Pint32, Cgt)
  | Greater_than, Compare_int64s -> Pbintcomp(Pint64, Cgt)
  | Compare, Compare_generic -> Pccall caml_compare
  | Compare, Compare_ints -> Pccall caml_int_compare
  | Compare, Compare_floats -> Pccall caml_float_compare
  | Compare, Compare_strings -> Pccall caml_string_compare
  | Compare, Compare_bytes -> Pccall caml_bytes_compare
  | Compare, Compare_nativeints -> Pccall caml_nativeint_compare
  | Compare, Compare_int32s -> Pccall caml_int32_compare
  | Compare, Compare_int64s -> Pccall caml_int64_compare

let lambda_of_loc kind loc =
  let loc_start = loc.Location.loc_start in
  let (file, lnum, cnum) = Location.get_pos_info loc_start in
  let enum = loc.Location.loc_end.Lexing.pos_cnum -
      loc_start.Lexing.pos_cnum + cnum in
  match kind with
  | Loc_POS ->
    Lconst (Const_block (0, [
          Const_immstring file;
          Const_base (Const_int lnum);
          Const_base (Const_int cnum);
          Const_base (Const_int enum);
        ]))
  | Loc_FILE -> Lconst (Const_immstring file)
  | Loc_MODULE ->
    let filename = Filename.basename file in
    let name = Env.get_unit_name () in
    let module_name = if name = "" then "//"^filename^"//" else name in
    Lconst (Const_immstring module_name)
  | Loc_LOC ->
    let loc = Printf.sprintf "File %S, line %d, characters %d-%d"
        file lnum cnum enum in
    Lconst (Const_immstring loc)
  | Loc_LINE -> Lconst (Const_base (Const_int lnum))

let caml_restore_raw_backtrace =
  Primitive.simple ~name:"caml_restore_raw_backtrace" ~arity:2 ~alloc:false

let try_ids = Hashtbl.create 8

let add_exception_ident id =
  Hashtbl.replace try_ids id ()

let remove_exception_ident id =
  Hashtbl.remove try_ids id

let lambda_of_prim prim_name prim loc args arg_exps =
  match prim, args with
  | Primitive prim, args ->
      Lprim(prim, args, loc)
  | Comparison(comp, knd), args ->
      let prim = comparison_primitive comp knd in
      Lprim(prim, args, loc)
  | Raise kind, [arg] ->
      let kind =
        match kind, arg with
        | Raise_regular, Lvar argv when Hashtbl.mem try_ids argv ->
            Raise_reraise
        | _, _ ->
            kind
      in
      let arg =
        match arg_exps with
        | None -> arg
        | Some [arg_exp] -> event_after arg_exp arg
        | Some _ -> assert false
      in
      Lprim(Praise kind, [arg], loc)
  | Raise_with_backtrace, [exn; bt] ->
      let vexn = Ident.create "exn" in
      let raise_arg =
        match arg_exps with
        | None -> Lvar vexn
        | Some [exn_exp; _] -> event_after exn_exp (Lvar vexn)
        | Some _ -> assert false
      in
      Llet(Strict, Pgenval, vexn, exn,
           Lsequence(Lprim(Pccall caml_restore_raw_backtrace,
                           [Lvar vexn; bt],
                           loc),
                     Lprim(Praise Raise_reraise, [raise_arg], loc)))
  | Lazy_force, [arg] ->
      Matching.inline_lazy_force arg Location.none
  | Loc kind, [] ->
      lambda_of_loc kind loc
  | Loc kind, [arg] ->
      let lam = lambda_of_loc kind loc in
      Lprim(Pmakeblock(0, Immutable, None), [lam; arg], loc)
  | Send, [obj; meth] ->
      Lsend(Public, meth, obj, [], loc)
  | Send_self, [obj; meth] ->
      Lsend(Self, meth, obj, [], loc)
  | Send_cache, [obj; meth; cache; pos] ->
      Lsend(Cached, meth, obj, [cache; pos], loc)
  | (Raise _ | Raise_with_backtrace
    | Lazy_force | Loc _
    | Send | Send_self | Send_cache), _ ->
      raise(Error(loc, Wrong_arity_builtin_primitive prim_name))

(* Eta-expand a primitive *)

let transl_primitive loc p env ty path =
  let prim = lookup_primitive loc p env path in
  let has_constant_constructor = false in
  let prim =
    match specialize_primitive env ty ~has_constant_constructor prim with
    | None -> prim
    | Some prim -> prim
  in
  let rec make_params n =
    if n <= 0 then [] else Ident.create "prim" :: make_params (n-1)
  in
  let params = make_params p.prim_arity in
  let args = List.map (fun id -> Lvar id) params in
  let body = lambda_of_prim p.prim_name prim loc args None in
  match params with
  | [] -> body
  | _ ->
      Lfunction{ kind = Curried; params;
                 attr = default_stub_attribute;
                 loc = loc;
                 body = body; }

(* Determine if a primitive is a Pccall or will be turned later into
   a C function call that may raise an exception *)
let primitive_is_ccall = function
  | Pccall _ | Pstringrefs  | Pbytesrefs | Pbytessets | Parrayrefs _ |
    Parraysets _ | Pbigarrayref _ | Pbigarrayset _ | Pduprecord _ | Pdirapply |
    Prevapply -> true
  | _ -> false

(* Determine if a primitive should be surrounded by an "after" debug event *)
let primitive_needs_event_after = function
  | Primitive prim -> primitive_is_ccall prim
  | Comparison(comp, knd) ->
      primitive_is_ccall (comparison_primitive comp knd)
  | Lazy_force | Send | Send_self | Send_cache -> true
  | Raise _ | Raise_with_backtrace | Loc _ -> false

let transl_primitive_application loc p env ty path exp args arg_exps =
  let prim = lookup_primitive loc p env (Some path) in
  let has_constant_constructor =
    match arg_exps with
    | [_; {exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}]
    | [{exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}; _]
    | [_; {exp_desc = Texp_variant(_, None)}]
    | [{exp_desc = Texp_variant(_, None)}; _] -> true
    | _ -> false
  in
  let prim =
    match specialize_primitive env ty ~has_constant_constructor prim with
    | None -> prim
    | Some prim -> prim
  in
  let lam = lambda_of_prim p.prim_name prim loc args (Some arg_exps) in
  let lam =
    if primitive_needs_event_after prim then begin
      match exp with
      | None -> lam
      | Some exp -> event_after exp lam
    end else begin
      lam
    end
  in
  lam

(* Error report *)

open Format

let report_error ppf = function
  | Unknown_builtin_primitive prim_name ->
      fprintf ppf "Unknown builtin primitive \"%s\"" prim_name
  | Wrong_arity_builtin_primitive prim_name ->
      fprintf ppf "Wrong arity for builtin primitive \"%s\"" prim_name

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
