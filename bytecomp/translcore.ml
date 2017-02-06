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

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var
  | Unknown_builtin_primitive of string
  | Unreachable_reached

exception Error of Location.t * error

let use_dup_for_constant_arrays_bigger_than = 4

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun _cc _rootpath _modl -> assert false) :
      module_coercion -> Path.t option -> module_expr -> lambda)

let transl_object =
  ref (fun _id _s _cl -> assert false :
       Ident.t -> string list -> class_expr -> lambda)

(* Compile an exception/extension definition *)

let prim_fresh_oo_id =
  Pccall (Primitive.simple ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

let transl_extension_constructor env path ext =
  let name =
    match path, !Clflags.for_package with
      None, _ -> Ident.name ext.ext_id
    | Some p, None -> Path.name p
    | Some p, Some pack -> Printf.sprintf "%s.%s" pack (Path.name p)
  in
  let loc = ext.ext_loc in
  match ext.ext_kind with
    Text_decl _ ->
      Lprim (Pmakeblock (Obj.object_tag, Immutable, None),
        [Lconst (Const_base (Const_string (name, None)));
         Lprim (prim_fresh_oo_id, [Lconst (Const_base (Const_int 0))], loc)],
        loc)
  | Text_rebind(path, _lid) ->
      transl_path ~loc env path

(* Translation of primitives *)

let comparisons_table = create_hashtable 11 [
  "%equal",
      (Pccall(Primitive.simple ~name:"caml_equal" ~arity:2 ~alloc:true),
       Pintcomp Ceq,
       Pfloatcomp Ceq,
       Pccall(Primitive.simple ~name:"caml_string_equal" ~arity:2
                ~alloc:false),
       Pccall(Primitive.simple ~name:"caml_bytes_equal" ~arity:2
                ~alloc:false),
       Pbintcomp(Pnativeint, Ceq),
       Pbintcomp(Pint32, Ceq),
       Pbintcomp(Pint64, Ceq),
       true);
  "%notequal",
      (Pccall(Primitive.simple ~name:"caml_notequal" ~arity:2 ~alloc:true),
       Pintcomp Cneq,
       Pfloatcomp Cneq,
       Pccall(Primitive.simple ~name:"caml_string_notequal" ~arity:2
                ~alloc:false),
       Pccall(Primitive.simple ~name:"caml_bytes_notequal" ~arity:2
                ~alloc:false),
       Pbintcomp(Pnativeint, Cneq),
       Pbintcomp(Pint32, Cneq),
       Pbintcomp(Pint64, Cneq),
       true);
  "%lessthan",
      (Pccall(Primitive.simple ~name:"caml_lessthan" ~arity:2 ~alloc:true),
       Pintcomp Clt,
       Pfloatcomp Clt,
       Pccall(Primitive.simple ~name:"caml_string_lessthan" ~arity:2
                ~alloc:false),
       Pccall(Primitive.simple ~name:"caml_bytes_lessthan" ~arity:2
                ~alloc:false),
       Pbintcomp(Pnativeint, Clt),
       Pbintcomp(Pint32, Clt),
       Pbintcomp(Pint64, Clt),
       false);
  "%greaterthan",
      (Pccall(Primitive.simple ~name:"caml_greaterthan" ~arity:2 ~alloc:true),
       Pintcomp Cgt,
       Pfloatcomp Cgt,
       Pccall(Primitive.simple ~name:"caml_string_greaterthan" ~arity:2
                ~alloc: false),
       Pccall(Primitive.simple ~name:"caml_bytes_greaterthan" ~arity:2
                ~alloc: false),
       Pbintcomp(Pnativeint, Cgt),
       Pbintcomp(Pint32, Cgt),
       Pbintcomp(Pint64, Cgt),
       false);
  "%lessequal",
      (Pccall(Primitive.simple ~name:"caml_lessequal" ~arity:2 ~alloc:true),
       Pintcomp Cle,
       Pfloatcomp Cle,
       Pccall(Primitive.simple ~name:"caml_string_lessequal" ~arity:2
                ~alloc:false),
       Pccall(Primitive.simple ~name:"caml_bytes_lessequal" ~arity:2
                ~alloc:false),
       Pbintcomp(Pnativeint, Cle),
       Pbintcomp(Pint32, Cle),
       Pbintcomp(Pint64, Cle),
       false);
  "%greaterequal",
      (Pccall(Primitive.simple ~name:"caml_greaterequal" ~arity:2 ~alloc:true),
       Pintcomp Cge,
       Pfloatcomp Cge,
       Pccall(Primitive.simple ~name:"caml_string_greaterequal" ~arity:2
                ~alloc:false),
       Pccall(Primitive.simple ~name:"caml_bytes_greaterequal" ~arity:2
                ~alloc:false),
       Pbintcomp(Pnativeint, Cge),
       Pbintcomp(Pint32, Cge),
       Pbintcomp(Pint64, Cge),
       false);
  "%compare",
      let unboxed_compare name native_repr =
        Pccall( Primitive.make ~name ~alloc:false
                  ~native_name:(name^"_unboxed")
                  ~native_repr_args:[native_repr;native_repr]
                  ~native_repr_res:Untagged_int
              ) in
      (Pccall(Primitive.simple ~name:"caml_compare" ~arity:2 ~alloc:true),
       (* Not unboxed since the comparison is done directly on tagged int *)
       Pccall(Primitive.simple ~name:"caml_int_compare" ~arity:2 ~alloc:false),
       unboxed_compare "caml_float_compare" Unboxed_float,
       Pccall(Primitive.simple ~name:"caml_string_compare" ~arity:2
                ~alloc:false),
       Pccall(Primitive.simple ~name:"caml_bytes_compare" ~arity:2
                ~alloc:false),
       unboxed_compare "caml_nativeint_compare" (Unboxed_integer Pnativeint),
       unboxed_compare "caml_int32_compare" (Unboxed_integer Pint32),
       unboxed_compare "caml_int64_compare" (Unboxed_integer Pint64),
       false)
]

let primitives_table = create_hashtable 57 [
  "%identity", Pidentity;
  "%bytes_to_string", Pbytes_to_string;
  "%bytes_of_string", Pbytes_of_string;
  "%ignore", Pignore;
  "%revapply", Prevapply;
  "%apply", Pdirapply;
  "%loc_LOC", Ploc Loc_LOC;
  "%loc_FILE", Ploc Loc_FILE;
  "%loc_LINE", Ploc Loc_LINE;
  "%loc_POS", Ploc Loc_POS;
  "%loc_MODULE", Ploc Loc_MODULE;
  "%field0", Pfield 0;
  "%field1", Pfield 1;
  "%setfield0", Psetfield(0, Pointer, Assignment);
  "%makeblock", Pmakeblock(0, Immutable, None);
  "%makemutable", Pmakeblock(0, Mutable, None);
  "%raise", Praise Raise_regular;
  "%reraise", Praise Raise_reraise;
  "%raise_notrace", Praise Raise_notrace;
  "%sequand", Psequand;
  "%sequor", Psequor;
  "%boolnot", Pnot;
  "%big_endian", Pctconst Big_endian;
  "%backend_type", Pctconst Backend_type;
  "%word_size", Pctconst Word_size;
  "%int_size", Pctconst Int_size;
  "%max_wosize", Pctconst Max_wosize;
  "%ostype_unix", Pctconst Ostype_unix;
  "%ostype_win32", Pctconst Ostype_win32;
  "%ostype_cygwin", Pctconst Ostype_cygwin;
  "%negint", Pnegint;
  "%succint", Poffsetint 1;
  "%predint", Poffsetint(-1);
  "%addint", Paddint;
  "%subint", Psubint;
  "%mulint", Pmulint;
  "%divint", Pdivint Safe;
  "%modint", Pmodint Safe;
  "%andint", Pandint;
  "%orint", Porint;
  "%xorint", Pxorint;
  "%lslint", Plslint;
  "%lsrint", Plsrint;
  "%asrint", Pasrint;
  "%eq", Pintcomp Ceq;
  "%noteq", Pintcomp Cneq;
  "%ltint", Pintcomp Clt;
  "%leint", Pintcomp Cle;
  "%gtint", Pintcomp Cgt;
  "%geint", Pintcomp Cge;
  "%incr", Poffsetref(1);
  "%decr", Poffsetref(-1);
  "%intoffloat", Pintoffloat;
  "%floatofint", Pfloatofint;
  "%negfloat", Pnegfloat;
  "%absfloat", Pabsfloat;
  "%addfloat", Paddfloat;
  "%subfloat", Psubfloat;
  "%mulfloat", Pmulfloat;
  "%divfloat", Pdivfloat;
  "%eqfloat", Pfloatcomp Ceq;
  "%noteqfloat", Pfloatcomp Cneq;
  "%ltfloat", Pfloatcomp Clt;
  "%lefloat", Pfloatcomp Cle;
  "%gtfloat", Pfloatcomp Cgt;
  "%gefloat", Pfloatcomp Cge;
  "%string_length", Pstringlength;
  "%string_safe_get", Pstringrefs;
  "%string_unsafe_get", Pstringrefu;
  "%bytes_length", Pbyteslength;
  "%bytes_safe_get", Pbytesrefs;
  "%bytes_safe_set", Pbytessets;
  "%bytes_unsafe_get", Pbytesrefu;
  "%bytes_unsafe_set", Pbytessetu;
  "%array_length", Parraylength Pgenarray;
  "%array_safe_get", Parrayrefs Pgenarray;
  "%array_safe_set", Parraysets Pgenarray;
  "%array_unsafe_get", Parrayrefu Pgenarray;
  "%array_unsafe_set", Parraysetu Pgenarray;
  "%obj_size", Parraylength Pgenarray;
  "%obj_field", Parrayrefu Pgenarray;
  "%obj_set_field", Parraysetu Pgenarray;
  "%obj_is_int", Pisint;
  "%lazy_force", Plazyforce;
  "%nativeint_of_int", Pbintofint Pnativeint;
  "%nativeint_to_int", Pintofbint Pnativeint;
  "%nativeint_neg", Pnegbint Pnativeint;
  "%nativeint_add", Paddbint Pnativeint;
  "%nativeint_sub", Psubbint Pnativeint;
  "%nativeint_mul", Pmulbint Pnativeint;
  "%nativeint_div", Pdivbint { size = Pnativeint; is_safe = Safe };
  "%nativeint_mod", Pmodbint { size = Pnativeint; is_safe = Safe };
  "%nativeint_and", Pandbint Pnativeint;
  "%nativeint_or",  Porbint Pnativeint;
  "%nativeint_xor", Pxorbint Pnativeint;
  "%nativeint_lsl", Plslbint Pnativeint;
  "%nativeint_lsr", Plsrbint Pnativeint;
  "%nativeint_asr", Pasrbint Pnativeint;
  "%int32_of_int", Pbintofint Pint32;
  "%int32_to_int", Pintofbint Pint32;
  "%int32_neg", Pnegbint Pint32;
  "%int32_add", Paddbint Pint32;
  "%int32_sub", Psubbint Pint32;
  "%int32_mul", Pmulbint Pint32;
  "%int32_div", Pdivbint { size = Pint32; is_safe = Safe };
  "%int32_mod", Pmodbint { size = Pint32; is_safe = Safe };
  "%int32_and", Pandbint Pint32;
  "%int32_or",  Porbint Pint32;
  "%int32_xor", Pxorbint Pint32;
  "%int32_lsl", Plslbint Pint32;
  "%int32_lsr", Plsrbint Pint32;
  "%int32_asr", Pasrbint Pint32;
  "%int64_of_int", Pbintofint Pint64;
  "%int64_to_int", Pintofbint Pint64;
  "%int64_neg", Pnegbint Pint64;
  "%int64_add", Paddbint Pint64;
  "%int64_sub", Psubbint Pint64;
  "%int64_mul", Pmulbint Pint64;
  "%int64_div", Pdivbint { size = Pint64; is_safe = Safe };
  "%int64_mod", Pmodbint { size = Pint64; is_safe = Safe };
  "%int64_and", Pandbint Pint64;
  "%int64_or",  Porbint Pint64;
  "%int64_xor", Pxorbint Pint64;
  "%int64_lsl", Plslbint Pint64;
  "%int64_lsr", Plsrbint Pint64;
  "%int64_asr", Pasrbint Pint64;
  "%nativeint_of_int32", Pcvtbint(Pint32, Pnativeint);
  "%nativeint_to_int32", Pcvtbint(Pnativeint, Pint32);
  "%int64_of_int32", Pcvtbint(Pint32, Pint64);
  "%int64_to_int32", Pcvtbint(Pint64, Pint32);
  "%int64_of_nativeint", Pcvtbint(Pnativeint, Pint64);
  "%int64_to_nativeint", Pcvtbint(Pint64, Pnativeint);
  "%caml_ba_ref_1",
    Pbigarrayref(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_ref_2",
    Pbigarrayref(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_ref_3",
    Pbigarrayref(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_1",
    Pbigarrayset(false, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_2",
    Pbigarrayset(false, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_set_3",
    Pbigarrayset(false, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_1",
    Pbigarrayref(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_2",
    Pbigarrayref(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_ref_3",
    Pbigarrayref(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_1",
    Pbigarrayset(true, 1, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_2",
    Pbigarrayset(true, 2, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_unsafe_set_3",
    Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout);
  "%caml_ba_dim_1", Pbigarraydim(1);
  "%caml_ba_dim_2", Pbigarraydim(2);
  "%caml_ba_dim_3", Pbigarraydim(3);
  "%caml_string_get16", Pstring_load_16(false);
  "%caml_string_get16u", Pstring_load_16(true);
  "%caml_string_get32", Pstring_load_32(false);
  "%caml_string_get32u", Pstring_load_32(true);
  "%caml_string_get64", Pstring_load_64(false);
  "%caml_string_get64u", Pstring_load_64(true);
  "%caml_string_set16", Pstring_set_16(false);
  "%caml_string_set16u", Pstring_set_16(true);
  "%caml_string_set32", Pstring_set_32(false);
  "%caml_string_set32u", Pstring_set_32(true);
  "%caml_string_set64", Pstring_set_64(false);
  "%caml_string_set64u", Pstring_set_64(true);
  "%caml_bigstring_get16", Pbigstring_load_16(false);
  "%caml_bigstring_get16u", Pbigstring_load_16(true);
  "%caml_bigstring_get32", Pbigstring_load_32(false);
  "%caml_bigstring_get32u", Pbigstring_load_32(true);
  "%caml_bigstring_get64", Pbigstring_load_64(false);
  "%caml_bigstring_get64u", Pbigstring_load_64(true);
  "%caml_bigstring_set16", Pbigstring_set_16(false);
  "%caml_bigstring_set16u", Pbigstring_set_16(true);
  "%caml_bigstring_set32", Pbigstring_set_32(false);
  "%caml_bigstring_set32u", Pbigstring_set_32(true);
  "%caml_bigstring_set64", Pbigstring_set_64(false);
  "%caml_bigstring_set64u", Pbigstring_set_64(true);
  "%bswap16", Pbswap16;
  "%bswap_int32", Pbbswap(Pint32);
  "%bswap_int64", Pbbswap(Pint64);
  "%bswap_native", Pbbswap(Pnativeint);
  "%int_as_pointer", Pint_as_pointer;
  "%opaque", Popaque;
]

let find_primitive prim_name =
  Hashtbl.find primitives_table prim_name

let specialize_comparison table env ty =
  let (gencomp, intcomp, floatcomp, stringcomp, bytescomp,
           nativeintcomp, int32comp, int64comp, _) = table in
  match () with
  | () when is_base_type env ty Predef.path_int
         || is_base_type env ty Predef.path_char
         || (maybe_pointer_type env ty = Immediate)   -> intcomp
  | () when is_base_type env ty Predef.path_float     -> floatcomp
  | () when is_base_type env ty Predef.path_string    -> stringcomp
  | () when is_base_type env ty Predef.path_bytes     -> bytescomp
  | () when is_base_type env ty Predef.path_nativeint -> nativeintcomp
  | () when is_base_type env ty Predef.path_int32     -> int32comp
  | () when is_base_type env ty Predef.path_int64     -> int64comp
  | () -> gencomp

(* Specialize a primitive from available type information,
   raise Not_found if primitive is unknown  *)

let specialize_primitive p env ty ~has_constant_constructor =
  try
    let table = Hashtbl.find comparisons_table p.prim_name in
    let (gencomp, intcomp, _, _, _, _, _, _, simplify_constant_constructor) =
      table in
    if has_constant_constructor && simplify_constant_constructor then
      intcomp
    else
      match is_function_type env ty with
      | Some (lhs,_rhs) -> specialize_comparison table env lhs
      | None -> gencomp
  with Not_found ->
    let p = find_primitive p.prim_name in
    (* Try strength reduction based on the type of the argument *)
    let params = match is_function_type env ty with
      | None -> []
      | Some (p1, rhs) -> match is_function_type env rhs with
        | None -> [p1]
        | Some (p2, _) -> [p1;p2]
    in
    match (p, params) with
      (Psetfield(n, _, init), [_p1; p2]) ->
        Psetfield(n, maybe_pointer_type env p2, init)
    | (Parraylength Pgenarray, [p])   -> Parraylength(array_type_kind env p)
    | (Parrayrefu Pgenarray, p1 :: _) -> Parrayrefu(array_type_kind env p1)
    | (Parraysetu Pgenarray, p1 :: _) -> Parraysetu(array_type_kind env p1)
    | (Parrayrefs Pgenarray, p1 :: _) -> Parrayrefs(array_type_kind env p1)
    | (Parraysets Pgenarray, p1 :: _) -> Parraysets(array_type_kind env p1)
    | (Pbigarrayref(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
       p1 :: _) ->
        let (k, l) = bigarray_type_kind_and_layout env p1 in
        Pbigarrayref(unsafe, n, k, l)
    | (Pbigarrayset(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
       p1 :: _) ->
        let (k, l) = bigarray_type_kind_and_layout env p1 in
        Pbigarrayset(unsafe, n, k, l)
    | (Pmakeblock(tag, mut, None), fields) ->
        let shape = List.map (Typeopt.value_kind env) fields in
        Pmakeblock(tag, mut, Some shape)
    | _ -> p

(* Eta-expand a primitive *)

let used_primitives = Hashtbl.create 7
let add_used_primitive loc env path =
  match path with
    Some (Path.Pdot _ as path) ->
      let path = Env.normalize_path (Some loc) env path in
      let unit = Path.head path in
      if Ident.global unit && not (Hashtbl.mem used_primitives path)
      then Hashtbl.add used_primitives path loc
  | _ -> ()

let transl_primitive loc p env ty path =
  let prim =
    try specialize_primitive p env ty ~has_constant_constructor:false
    with Not_found ->
      add_used_primitive loc env path;
      Pccall p
  in
  match prim with
  | Plazyforce ->
      let parm = Ident.create "prim" in
      Lfunction{kind = Curried; params = [parm];
                body = Matching.inline_lazy_force (Lvar parm) Location.none;
                loc = loc;
                attr = default_stub_attribute }
  | Ploc kind ->
    let lam = lam_of_loc kind loc in
    begin match p.prim_arity with
      | 0 -> lam
      | 1 -> (* TODO: we should issue a warning ? *)
        let param = Ident.create "prim" in
        Lfunction{kind = Curried; params = [param];
                  attr = default_stub_attribute;
                  loc = loc;
                  body = Lprim(Pmakeblock(0, Immutable, None),
                               [lam; Lvar param], loc)}
      | _ -> assert false
    end
  | _ ->
      let rec make_params n =
        if n <= 0 then [] else Ident.create "prim" :: make_params (n-1) in
      let params = make_params p.prim_arity in
      Lfunction{ kind = Curried; params;
                 attr = default_stub_attribute;
                 loc = loc;
                 body = Lprim(prim, List.map (fun id -> Lvar id) params, loc) }

let transl_primitive_application loc prim env ty path args =
  let prim_name = prim.prim_name in
  try
    let has_constant_constructor = match args with
        [_; {exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}]
      | [{exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}; _]
      | [_; {exp_desc = Texp_variant(_, None)}]
      | [{exp_desc = Texp_variant(_, None)}; _] -> true
      | _ -> false
    in
    specialize_primitive prim env ty ~has_constant_constructor
  with Not_found ->
    if String.length prim_name > 0 && prim_name.[0] = '%' then
      raise(Error(loc, Unknown_builtin_primitive prim_name));
    add_used_primitive loc env path;
    Pccall prim


(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda idlist lam =
  let rec check_top idlist = function
    | Lvar v -> not (List.mem v idlist)
    | Llet _ as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(_str, _k, id, arg, body) ->
        check idlist arg && check_top (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (_id, arg) -> check idlist' arg) bindings &&
        check_top idlist' body
    | Lprim (Pmakearray (Pgenarray, _), _, _) -> false
    | Lprim (Pmakearray (Pfloatarray, _), args, _) ->
        List.for_all (check idlist) args
    | Lsequence (lam1, lam2) -> check idlist lam1 && check_top idlist lam2
    | Levent (lam, _) -> check_top idlist lam
    | lam -> check idlist lam

  and check idlist = function
    | Lvar _ -> true
    | Lfunction _ -> true
    | Llet _ as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(_str, _k, id, arg, body) ->
        check idlist arg && check (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (_id, arg) -> check idlist' arg) bindings &&
        check idlist' body
    | Lprim(Pmakeblock _, args, _) ->
        List.for_all (check idlist) args
    | Lprim (Pmakearray (Pfloatarray, _), _, _) -> false
    | Lprim (Pmakearray _, args, _) ->
        List.for_all (check idlist) args
    | Lsequence (lam1, lam2) -> check idlist lam1 && check idlist lam2
    | Levent (lam, _) -> check idlist lam
    | lam ->
        let fv = free_variables lam in
        not (List.exists (fun id -> IdentSet.mem id fv) idlist)

  and add_let id arg idlist =
    let fv = free_variables arg in
    if List.exists (fun id -> IdentSet.mem id fv) idlist
    then id :: idlist
    else idlist

  and add_letrec bindings idlist =
    List.fold_right (fun (id, arg) idl -> add_let id arg idl)
                    bindings idlist

  (* reverse-engineering the code generated by transl_record case 2 *)
  (* If you change this, you probably need to change Bytegen.size_of_lambda. *)
  and check_recursive_recordwith idlist = function
    | Llet (Strict, _k, id1, Lprim (Pduprecord _, [e1], _), body) ->
       check_top idlist e1
       && check_recordwith_updates idlist id1 body
    | _ -> false

  and check_recordwith_updates idlist id1 = function
    | Lsequence (Lprim ((Psetfield _ | Psetfloatfield _), [Lvar id2; e1], _),
                 cont)
        -> id2 = id1 && check idlist e1
           && check_recordwith_updates idlist id1 cont
    | Lvar id2 -> id2 = id1
    | _ -> false

  in check_top idlist lam

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

(* Push the default values under the functional abstractions *)
(* Also push bindings of module patterns, since this sound *)

type binding =
  | Bind_value of value_binding list
  | Bind_module of Ident.t * string loc * module_expr

let rec push_defaults loc bindings cases partial =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function { arg_label; param; cases; partial; } }
        as exp}] ->
      let cases = push_defaults exp.exp_loc bindings cases partial in
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp with exp_desc = Texp_function { arg_label; param; cases;
          partial; }}}]
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{txt="#default"},_];
             exp_desc = Texp_let
               (Nonrecursive, binds, ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (Bind_value binds :: bindings)
                   [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{txt="#modulepat"},_];
             exp_desc = Texp_letmodule
               (id, name, mexpr, ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (Bind_module (id, name, mexpr) :: bindings)
                   [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial
  | [case] ->
      let exp =
        List.fold_left
          (fun exp binds ->
            {exp with exp_desc =
             match binds with
             | Bind_value binds -> Texp_let(Nonrecursive, binds, exp)
             | Bind_module (id, name, mexpr) ->
                 Texp_letmodule (id, name, mexpr, exp)})
          case.c_rhs bindings
      in
      [{case with c_rhs=exp}]
  | {c_lhs=pat; c_rhs=exp; c_guard=_} :: _ when bindings <> [] ->
      let param = Typecore.name_pattern "param" cases in
      let name = Ident.name param in
      let exp =
        { exp with exp_loc = loc; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_desc =
              Texp_ident (Path.Pident param, mknoloc (Longident.Lident name),
                          {val_type = pat.pat_type; val_kind = Val_reg;
                           val_attributes = [];
                           Types.val_loc = Location.none;
                          })},
             cases, [], partial) }
      in
      push_defaults loc bindings
        [{c_lhs={pat with pat_desc = Tpat_var (param, mknoloc name)};
          c_guard=None; c_rhs=exp}]
        Total
  | _ ->
      cases

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

let event_function exp lam =
  if !Clflags.debug && not !Clflags.native_code then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = Env.summary exp.exp_env}))
  else
    lam None

let primitive_is_ccall = function
  (* Determine if a primitive is a Pccall or will be turned later into
     a C function call that may raise an exception *)
  | Pccall _ | Pstringrefs  | Pbytesrefs | Pbytessets | Parrayrefs _ |
    Parraysets _ | Pbigarrayref _ | Pbigarrayset _ | Pduprecord _ | Pdirapply |
    Prevapply -> true
  | _ -> false

(* Assertions *)

let assert_failed exp =
  let (fname, line, char) =
    Location.get_pos_info exp.exp_loc.Location.loc_start in
  Lprim(Praise Raise_regular, [event_after exp
    (Lprim(Pmakeblock(0, Immutable, None),
          [transl_normal_path Predef.path_assert_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string (fname, None));
               Const_base(Const_int line);
               Const_base(Const_int char)]))], exp.exp_loc))], exp.exp_loc)
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let try_ids = Hashtbl.create 8

let rec transl_exp e =
  List.iter (Translattribute.check_attribute e) e.exp_attributes;
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 e else
  Translobj.oo_wrap e.exp_env true transl_exp0 e

and transl_exp0 e =
  match e.exp_desc with
    Texp_ident(path, _, {val_kind = Val_prim p}) ->
      let public_send = p.prim_name = "%send" in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public else Self in
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        Lfunction{kind = Curried; params = [obj; meth];
                  attr = default_stub_attribute;
                  loc = e.exp_loc;
                  body = Lsend(kind, Lvar meth, Lvar obj, [], e.exp_loc)}
      else if p.prim_name = "%sendcache" then
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        let cache = Ident.create "cache" and pos = Ident.create "pos" in
        Lfunction{kind = Curried; params = [obj; meth; cache; pos];
                  attr = default_stub_attribute;
                  loc = e.exp_loc;
                  body = Lsend(Cached, Lvar meth, Lvar obj,
                               [Lvar cache; Lvar pos], e.exp_loc)}
      else
        transl_primitive e.exp_loc p e.exp_env e.exp_type (Some path)
  | Texp_ident(_, _, {val_kind = Val_anc _}) ->
      raise(Error(e.exp_loc, Free_super_var))
  | Texp_ident(path, _, {val_kind = Val_reg | Val_self _}) ->
      transl_path ~loc:e.exp_loc e.exp_env path
  | Texp_ident _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (event_before body (transl_exp body))
  | Texp_function { arg_label = _; param; cases; partial; } ->
      let ((kind, params), body) =
        event_function e
          (function repr ->
            let pl = push_defaults e.exp_loc [] cases partial in
            transl_function e.exp_loc !Clflags.native_code repr partial
              param pl)
      in
      let attr = {
        default_function_attribute with
        inline = Translattribute.get_inline_attribute e.exp_attributes;
        specialise = Translattribute.get_specialise_attribute e.exp_attributes;
      }
      in
      let loc = e.exp_loc in
      Lfunction{kind; params; body; attr; loc}
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p});
                exp_type = prim_type } as funct, oargs)
    when List.length oargs >= p.prim_arity
    && List.for_all (fun (_, arg) -> arg <> None) oargs ->
      let args, args' = cut p.prim_arity oargs in
      let wrap f =
        if args' = []
        then event_after e f
        else
          let should_be_tailcall, funct =
            Translattribute.get_tailcall_attribute funct
          in
          let inlined, funct =
            Translattribute.get_and_remove_inlined_attribute funct
          in
          let specialised, funct =
            Translattribute.get_and_remove_specialised_attribute funct
          in
          let e = { e with exp_desc = Texp_apply(funct, oargs) } in
          event_after e
            (transl_apply ~should_be_tailcall ~inlined ~specialised
               f args' e.exp_loc)
      in
      let wrap0 f =
        if args' = [] then f else wrap f in
      let args =
         List.map (function _, Some x -> x | _ -> assert false) args in
      let argl = transl_list args in
      let public_send = p.prim_name = "%send"
        || not !Clflags.native_code && p.prim_name = "%sendcache"in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public else Self in
        let obj = List.hd argl in
        wrap (Lsend (kind, List.nth argl 1, obj, [], e.exp_loc))
      else if p.prim_name = "%sendcache" then
        match argl with [obj; meth; cache; pos] ->
          wrap (Lsend(Cached, meth, obj, [cache; pos], e.exp_loc))
        | _ -> assert false
      else begin
        let prim = transl_primitive_application
            e.exp_loc p e.exp_env prim_type (Some path) args in
        match (prim, args) with
          (Praise k, [arg1]) ->
            let targ = List.hd argl in
            let k =
              match k, targ with
              | Raise_regular, Lvar id
                when Hashtbl.mem try_ids id ->
                  Raise_reraise
              | _ ->
                  k
            in
            wrap0 (Lprim(Praise k, [event_after arg1 targ], e.exp_loc))
        | (Ploc kind, []) ->
          lam_of_loc kind e.exp_loc
        | (Ploc kind, [arg1]) ->
          let lam = lam_of_loc kind arg1.exp_loc in
          Lprim(Pmakeblock(0, Immutable, None), lam :: argl, e.exp_loc)
        | (Ploc _, _) -> assert false
        | (_, _) ->
            begin match (prim, argl) with
            | (Plazyforce, [a]) ->
                wrap (Matching.inline_lazy_force a e.exp_loc)
            | (Plazyforce, _) -> assert false
            |_ -> let p = Lprim(prim, argl, e.exp_loc) in
               if primitive_is_ccall prim then wrap p else wrap0 p
            end
      end
  | Texp_apply(funct, oargs) ->
      let should_be_tailcall, funct =
        Translattribute.get_tailcall_attribute funct
      in
      let inlined, funct =
        Translattribute.get_and_remove_inlined_attribute funct
      in
      let specialised, funct =
        Translattribute.get_and_remove_specialised_attribute funct
      in
      let e = { e with exp_desc = Texp_apply(funct, oargs) } in
      event_after e
        (transl_apply ~should_be_tailcall ~inlined ~specialised
           (transl_exp funct) oargs e.exp_loc)
  | Texp_match(arg, pat_expr_list, exn_pat_expr_list, partial) ->
    transl_match e arg pat_expr_list exn_pat_expr_list partial
  | Texp_try(body, pat_expr_list) ->
      let id = Typecore.name_pattern "exn" pat_expr_list in
      Ltrywith(transl_exp body, id,
               Matching.for_trywith (Lvar id) (transl_cases_try pat_expr_list))
  | Texp_tuple el ->
      let ll, shape = transl_list_with_shape el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, Some shape), ll, e.exp_loc)
      end
  | Texp_construct(_, cstr, args) ->
      let ll, shape = transl_list_with_shape args in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(Const_pointer n)
      | Cstr_unboxed ->
          (match ll with [v] -> v | _ -> assert false)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable, Some shape), ll, e.exp_loc)
          end
      | Cstr_extension(path, is_const) ->
          if is_const then
            transl_path e.exp_env path
          else
            Lprim(Pmakeblock(0, Immutable, Some (Pgenval :: shape)),
                  transl_path e.exp_env path :: ll, e.exp_loc)
      end
  | Texp_extension_constructor (_, path) ->
      transl_path e.exp_env path
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(Const_pointer tag)
      | Some arg ->
          let lam = transl_exp arg in
          try
            Lconst(Const_block(0, [Const_base(Const_int tag);
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, Immutable, None),
                  [Lconst(Const_base(Const_int tag)); lam], e.exp_loc)
      end
  | Texp_record {fields; representation; extended_expression} ->
      transl_record e.exp_loc e.exp_env fields representation
        extended_expression
  | Texp_field(arg, _, lbl) ->
      let targ = transl_exp arg in
      begin match lbl.lbl_repres with
          Record_regular | Record_inlined _ ->
          Lprim (Pfield lbl.lbl_pos, [targ], e.exp_loc)
        | Record_unboxed _ -> targ
        | Record_float -> Lprim (Pfloatfield lbl.lbl_pos, [targ], e.exp_loc)
        | Record_extension ->
          Lprim (Pfield (lbl.lbl_pos + 1), [targ], e.exp_loc)
      end
  | Texp_setfield(arg, _, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular
        | Record_inlined _ ->
          Psetfield(lbl.lbl_pos, maybe_pointer newval, Assignment)
        | Record_unboxed _ -> assert false
        | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
        | Record_extension ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, Assignment)
      in
      Lprim(access, [transl_exp arg; transl_exp newval], e.exp_loc)
  | Texp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list expr_list in
      begin try
        (* For native code the decision as to which compilation strategy to
           use is made later.  This enables the Flambda passes to lift certain
           kinds of array definitions to symbols. *)
        (* Deactivate constant optimization if array is small enough *)
        if List.length ll <= use_dup_for_constant_arrays_bigger_than
        then begin
          raise Not_constant
        end;
        begin match List.map extract_constant ll with
        | exception Not_constant when kind = Pfloatarray ->
            (* We cannot currently lift [Pintarray] arrays safely in Flambda
               because [caml_modify] might be called upon them (e.g. from
               code operating on polymorphic arrays, or functions such as
               [caml_array_blit].
               To avoid having different Lambda code for
               bytecode/Closure vs.  Flambda, we always generate
               [Pduparray] here, and deal with it in [Bytegen] (or in
               the case of Closure, in [Cmmgen], which already has to
               handle [Pduparray Pmakearray Pfloatarray] in the case
               where the array turned out to be inconstant).
               When not [Pfloatarray], the exception propagates to the handler
               below. *)
            let imm_array =
              Lprim (Pmakearray (kind, Immutable), ll, e.exp_loc)
            in
            Lprim (Pduparray (kind, Mutable), [imm_array], e.exp_loc)
        | cl ->
            let imm_array =
              match kind with
              | Paddrarray | Pintarray ->
                  Lconst(Const_block(0, cl))
              | Pfloatarray ->
                  Lconst(Const_float_array(List.map extract_float cl))
              | Pgenarray ->
                  raise Not_constant    (* can this really happen? *)
            in
            Lprim (Pduparray (kind, Mutable), [imm_array], e.exp_loc)
        end
      with Not_constant ->
        Lprim(Pmakearray (kind, Mutable), ll, e.exp_loc)
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  event_before ifnot (transl_exp ifnot))
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp expr1, event_before expr2 (transl_exp expr2))
  | Texp_while(cond, body) ->
      Lwhile(transl_exp cond, event_before body (transl_exp body))
  | Texp_for(param, _, low, high, dir, body) ->
      Lfor(param, transl_exp low, transl_exp high, dir,
           event_before body (transl_exp body))
  | Texp_send(_, _, Some exp) -> transl_exp exp
  | Texp_send(expr, met, None) ->
      let obj = transl_exp expr in
      let lam =
        match met with
          Tmeth_val id -> Lsend (Self, Lvar id, obj, [], e.exp_loc)
        | Tmeth_name nm ->
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache, e.exp_loc)
      in
      event_after e lam
  | Texp_new (cl, {Location.loc=loc}, _) ->
      Lapply{ap_should_be_tailcall=false;
             ap_loc=loc;
             ap_func=Lprim(Pfield 0, [transl_path ~loc e.exp_env cl], loc);
             ap_args=[lambda_unit];
             ap_inlined=Default_inline;
             ap_specialised=Default_specialise}
  | Texp_instvar(path_self, path, _) ->
      Lprim(Parrayrefu Paddrarray,
            [transl_normal_path path_self; transl_normal_path path], e.exp_loc)
  | Texp_setinstvar(path_self, path, _, expr) ->
      transl_setinstvar e.exp_loc (transl_normal_path path_self) path expr
  | Texp_override(path_self, modifs) ->
      let cpy = Ident.create "copy" in
      Llet(Strict, Pgenval, cpy,
           Lapply{ap_should_be_tailcall=false;
                  ap_loc=Location.none;
                  ap_func=Translobj.oo_prim "copy";
                  ap_args=[transl_normal_path path_self];
                  ap_inlined=Default_inline;
                  ap_specialised=Default_specialise},
           List.fold_right
             (fun (path, _, expr) rem ->
                Lsequence(transl_setinstvar Location.none
                            (Lvar cpy) path expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(id, _, modl, body) ->
      Llet(Strict, Pgenval, id,
           !transl_module Tcoerce_none None modl,
           transl_exp body)
  | Texp_letexception(cd, body) ->
      Llet(Strict, Pgenval,
           cd.ext_id, transl_extension_constructor e.exp_env None cd,
           transl_exp body)
  | Texp_pack modl ->
      !transl_module Tcoerce_none None modl
  | Texp_assert {exp_desc=Texp_construct(_, {cstr_name="false"}, _)} ->
      assert_failed e
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e)
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match e.exp_desc with
        (* a constant expr of type <> float gets compiled as itself *)
      | Texp_constant
          ( Const_int _ | Const_char _ | Const_string _
          | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
      | Texp_function _
      | Texp_construct (_, {cstr_arity = 0}, _)
        -> transl_exp e
      | Texp_constant(Const_float _) ->
          (* We don't need to wrap with Popaque: this forward
             block will never be shortcutted since it points to a float. *)
          Lprim(Pmakeblock(Obj.forward_tag, Immutable, None),
                [transl_exp e], e.exp_loc)
      | Texp_ident _ ->
          (* CR-someday mshinwell: Consider adding a new primitive
             that expresses the construction of forward_tag blocks.
             We need to use [Popaque] here to prevent unsound
             optimisation in Flambda, but the concept of a mutable
             block doesn't really match what is going on here.  This
             value may subsequently turn into an immediate... *)
          if Typeopt.lazy_val_requires_forward e.exp_env e.exp_type
          then
            Lprim (Popaque,
                   [Lprim(Pmakeblock(Obj.forward_tag, Immutable, None),
                          [transl_exp e], e.exp_loc)],
                   e.exp_loc)
          else transl_exp e
      (* other cases compile to a lazy block holding a function *)
      | _ ->
         let fn = Lfunction {kind = Curried; params = [Ident.create "param"];
                             attr = default_function_attribute;
                             loc = e.exp_loc;
                             body = transl_exp e} in
          Lprim(Pmakeblock(Config.lazy_tag, Mutable, None), [fn], e.exp_loc)
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create "class" in
      !transl_object cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }
  | Texp_unreachable ->
      raise (Error (e.exp_loc, Unreachable_reached))

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_list_with_shape expr_list =
  let transl_with_shape e =
    let shape = Typeopt.value_kind e.exp_env e.exp_type in
    transl_exp e, shape
  in
  List.split (List.map transl_with_shape expr_list)

and transl_guard guard rhs =
  let expr = event_before rhs (transl_exp rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before cond (Lifthenelse(transl_exp cond, expr, staticfail))

and transl_case {c_lhs; c_guard; c_rhs} =
  c_lhs, transl_guard c_guard c_rhs

and transl_cases cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map transl_case cases

and transl_case_try {c_lhs; c_guard; c_rhs} =
  match c_lhs.pat_desc with
  | Tpat_var (id, _)
  | Tpat_alias (_, id, _) ->
      Hashtbl.replace try_ids id ();
      Misc.try_finally
        (fun () -> c_lhs, transl_guard c_guard c_rhs)
        (fun () -> Hashtbl.remove try_ids id)
  | _ ->
      c_lhs, transl_guard c_guard c_rhs

and transl_cases_try cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map transl_case_try cases

and transl_tupled_cases patl_expr_list =
  let patl_expr_list =
    List.filter (fun (_,_,e) -> e.exp_desc <> Texp_unreachable)
      patl_expr_list in
  List.map (fun (patl, guard, expr) -> (patl, transl_guard guard expr))
    patl_expr_list

and transl_apply ?(should_be_tailcall=false) ?(inlined = Default_inline)
      ?(specialised = Default_specialise) lam sargs loc =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs, loc) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Levent(Lsend(k, lmet, lobj, largs, loc), _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Lapply ap ->
        Lapply {ap with ap_args = ap.ap_args @ args; ap_loc = loc}
    | lexp ->
        Lapply {ap_should_be_tailcall=should_be_tailcall;
                ap_loc=loc;
                ap_func=lexp;
                ap_args=args;
                ap_inlined=inlined;
                ap_specialised=specialised;}
  in
  let rec build_apply lam args = function
      (None, optional) :: l ->
        let defs = ref [] in
        let protect name lam =
          match lam with
            Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        let args, args' =
          if List.for_all (fun (_,opt) -> opt) args then [], args
          else args, [] in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args) in
        let handle = protect "func" lam
        and l = List.map (fun (arg, opt) -> may_map (protect "arg") arg, opt) l
        and id_arg = Ident.create "param" in
        let body =
          match build_apply handle ((Lvar id_arg, optional)::args') l with
            Lfunction{kind = Curried; params = ids; body = lam; attr; loc} ->
              Lfunction{kind = Curried; params = id_arg::ids; body = lam; attr;
                        loc}
          | Levent(Lfunction{kind = Curried; params = ids;
                             body = lam; attr; loc}, _) ->
              Lfunction{kind = Curried; params = id_arg::ids; body = lam; attr;
                        loc}
          | lam ->
              Lfunction{kind = Curried; params = [id_arg]; body = lam;
                        attr = default_stub_attribute; loc = loc}
        in
        List.fold_left
          (fun body (id, lam) -> Llet(Strict, Pgenval, id, lam, body))
          body !defs
    | (Some arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) l
    | [] ->
        lapply lam (List.rev_map fst args)
  in
  (build_apply lam [] (List.map (fun (l, x) ->
                                   may_map transl_exp x, Btype.is_optional l)
                                sargs)
     : Lambda.lambda)

and transl_function loc untuplify_fn repr partial param cases =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function { arg_label = _; param = param'; cases;
        partial = partial'; }} as exp}]
    when Parmatch.fluid pat ->
      let ((_, params), body) =
        transl_function exp.exp_loc false repr partial' param' cases in
      ((Curried, param :: params),
       Matching.for_function loc None (Lvar param) [pat, body] partial)
  | {c_lhs={pat_desc = Tpat_tuple pl}} :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let params = List.map (fun _ -> Ident.create "param") pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
           (transl_tupled_cases pats_expr_list) partial)
      with Matching.Cannot_flatten ->
        ((Curried, [param]),
         Matching.for_function loc repr (Lvar param)
           (transl_cases cases) partial)
      end
  | _ ->
      ((Curried, [param]),
       Matching.for_function loc repr (Lvar param)
         (transl_cases cases) partial)

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          body
      | {vb_pat=pat; vb_expr=expr; vb_attributes=attr; vb_loc} :: rem ->
          let lam = transl_exp expr in
          let lam =
            Translattribute.add_inline_attribute lam vb_loc attr
          in
          let lam =
            Translattribute.add_specialise_attribute lam vb_loc attr
          in
          Matching.for_let pat.pat_loc lam pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_) -> id
            | Tpat_alias ({pat_desc=Tpat_any}, id,_) -> id
            | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)))
        pat_expr_list in
      let transl_case {vb_expr=expr; vb_attributes; vb_loc} id =
        let lam = transl_exp expr in
        let lam =
          Translattribute.add_inline_attribute lam vb_loc
            vb_attributes
        in
        let lam =
          Translattribute.add_specialise_attribute lam vb_loc
            vb_attributes
        in
        if not (check_recursive_lambda idlist lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      Lletrec(List.map2 transl_case pat_expr_list idlist, body)

and transl_setinstvar loc self var expr =
  let prim =
    match maybe_pointer expr with
    | Pointer -> Paddrarray
    | Immediate -> Pintarray
  in
  Lprim(Parraysetu prim, [self; transl_normal_path var; transl_exp expr], loc)

and transl_record loc env fields repres opt_init_expr =
  let size = Array.length fields in
  (* Determine if there are "enough" fields (only relevant if this is a
     functional-style record update *)
  let no_init = match opt_init_expr with None -> true | _ -> false in
  if no_init || size < Config.max_young_wosize
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let init_id = Ident.create "init" in
    let lv =
      Array.mapi
        (fun i (_, definition) ->
           match definition with
           | Kept typ ->
               let field_kind = value_kind env typ in
               let access =
                 match repres with
                   Record_regular | Record_inlined _ -> Pfield i
                 | Record_unboxed _ -> assert false
                 | Record_extension -> Pfield (i + 1)
                 | Record_float -> Pfloatfield i in
               Lprim(access, [Lvar init_id], loc), field_kind
           | Overridden (_lid, expr) ->
               let field_kind = value_kind expr.exp_env expr.exp_type in
               transl_exp expr, field_kind)
        fields
    in
    let ll, shape = List.split (Array.to_list lv) in
    let mut =
      if Array.exists (fun (lbl, _) -> lbl.lbl_mut = Mutable) fields
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
        | Record_regular -> Lconst(Const_block(0, cl))
        | Record_inlined tag -> Lconst(Const_block(tag, cl))
        | Record_unboxed _ -> Lconst(match cl with [v] -> v | _ -> assert false)
        | Record_float ->
            Lconst(Const_float_array(List.map extract_float cl))
        | Record_extension ->
            raise Not_constant
      with Not_constant ->
        match repres with
          Record_regular ->
            Lprim(Pmakeblock(0, mut, Some shape), ll, loc)
        | Record_inlined tag ->
            Lprim(Pmakeblock(tag, mut, Some shape), ll, loc)
        | Record_unboxed _ -> (match ll with [v] -> v | _ -> assert false)
        | Record_float ->
            Lprim(Pmakearray (Pfloatarray, mut), ll, loc)
        | Record_extension ->
            let path =
              let (label, _) = fields.(0) in
              match label.lbl_res.desc with
              | Tconstr(p, _, _) -> p
              | _ -> assert false
            in
            let slot = transl_path env path in
            Lprim(Pmakeblock(0, mut, Some (Pgenval :: shape)), slot :: ll, loc)
    in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, Pgenval, init_id,
                             transl_exp init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    (* If you change anything here, you will likely have to change
       [check_recursive_recordwith] in this file. *)
    let copy_id = Ident.create "newrecord" in
    let update_field cont (lbl, definition) =
      match definition with
      | Kept _type -> cont
      | Overridden (_lid, expr) ->
          let upd =
            match repres with
              Record_regular
            | Record_inlined _ ->
                Psetfield(lbl.lbl_pos, maybe_pointer expr, Assignment)
            | Record_unboxed _ -> assert false
            | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
            | Record_extension ->
                Psetfield(lbl.lbl_pos + 1, maybe_pointer expr, Assignment)
          in
          Lsequence(Lprim(upd, [Lvar copy_id; transl_exp expr], loc), cont)
    in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, Pgenval, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp init_expr], loc),
             Array.fold_left update_field (Lvar copy_id) fields)
    end
  end

and transl_match e arg pat_expr_list exn_pat_expr_list partial =
  let id = Typecore.name_pattern "exn" exn_pat_expr_list
  and cases = transl_cases pat_expr_list
  and exn_cases = transl_cases_try exn_pat_expr_list in
  let static_catch body val_ids handler =
    let static_exception_id = next_negative_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (static_exception_id, body), id,
                 Matching.for_trywith (Lvar id) exn_cases),
       (static_exception_id, val_ids),
       handler)
  in
  match arg, exn_cases with
  | {exp_desc = Texp_tuple argl}, [] ->
    Matching.for_multiple_match e.exp_loc (transl_list argl) cases partial
  | {exp_desc = Texp_tuple argl}, _ :: _ ->
    let val_ids = List.map (fun _ -> Typecore.name_pattern "val" []) argl in
    let lvars = List.map (fun id -> Lvar id) val_ids in
    static_catch (transl_list argl) val_ids
      (Matching.for_multiple_match e.exp_loc lvars cases partial)
  | arg, [] ->
    Matching.for_function e.exp_loc None (transl_exp arg) cases partial
  | arg, _ :: _ ->
    let val_id = Typecore.name_pattern "val" pat_expr_list in
    static_catch [transl_exp arg] [val_id]
      (Matching.for_function e.exp_loc None (Lvar val_id) cases partial)


(* Wrapper for class compilation *)

(*
let transl_exp = transl_exp_wrap

let transl_let rec_flag pat_expr_list body =
  match pat_expr_list with
    [] -> body
  | (_, expr) :: _ ->
      Translobj.oo_wrap expr.exp_env false
        (transl_let rec_flag pat_expr_list) body
*)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_letrec_pat ->
      fprintf ppf
        "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      fprintf ppf
        "This kind of expression is not allowed as right-hand side of `let rec'"
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
  | Unknown_builtin_primitive prim_name ->
      fprintf ppf "Unknown builtin primitive \"%s\"" prim_name
  | Unreachable_reached ->
      fprintf ppf "Unreachable expression was reached"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
