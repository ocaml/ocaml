(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Path
open Types
open Typedtree
open Typeopt
open Lambda
open Format

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var
  | Illegal_tuple_expr

exception Error of Location.t * error

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun cc rootpath modl -> assert false) :
      module_coercion -> Path.t option -> module_expr -> lambda)

let transl_object =
  ref (fun id s cl -> assert false :
       Ident.t -> string list -> class_expr -> lambda)

(* Translation of primitives *)

let comparisons_table = create_hashtable 11 [
  "%equal",
      (Pccall{prim_name = "caml_equal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Ceq,
       Pfloatcomp Ceq,
       Pccall{prim_name = "caml_string_equal"; prim_arity = 2;
              prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pbintcomp(Pnativeint, Ceq),
       Pbintcomp(Pint32, Ceq),
       Pbintcomp(Pint64, Ceq),
       true);
  "%notequal",
      (Pccall{prim_name = "caml_notequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cneq,
       Pfloatcomp Cneq,
       Pccall{prim_name = "caml_string_notequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cneq),
       Pbintcomp(Pint32, Cneq),
       Pbintcomp(Pint64, Cneq),
       true);
  "%lessthan",
      (Pccall{prim_name = "caml_lessthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Clt,
       Pfloatcomp Clt,
       Pccall{prim_name = "caml_string_lessthan"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Clt),
       Pbintcomp(Pint32, Clt),
       Pbintcomp(Pint64, Clt),
       false);
  "%greaterthan",
      (Pccall{prim_name = "caml_greaterthan"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cgt,
       Pfloatcomp Cgt,
       Pccall{prim_name = "caml_string_greaterthan"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cgt),
       Pbintcomp(Pint32, Cgt),
       Pbintcomp(Pint64, Cgt),
       false);
  "%lessequal",
      (Pccall{prim_name = "caml_lessequal"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cle,
       Pfloatcomp Cle,
       Pccall{prim_name = "caml_string_lessequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cle),
       Pbintcomp(Pint32, Cle),
       Pbintcomp(Pint64, Cle),
       false);
  "%greaterequal",
      (Pccall{prim_name = "caml_greaterequal"; prim_arity = 2;
              prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cge,
       Pfloatcomp Cge,
       Pccall{prim_name = "caml_string_greaterequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pbintcomp(Pnativeint, Cge),
       Pbintcomp(Pint32, Cge),
       Pbintcomp(Pint64, Cge),
       false);
  "%compare",
      (Pccall{prim_name = "caml_compare"; prim_arity = 2; prim_alloc = true;
              prim_native_name = ""; prim_native_float = false},
       Pccall{prim_name = "caml_int_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_float_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_string_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_nativeint_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_int32_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       Pccall{prim_name = "caml_int64_compare"; prim_arity = 2;
              prim_alloc = false; prim_native_name = "";
              prim_native_float = false},
       false)
]

let primitives_table = create_hashtable 57 [
  "%identity", Pidentity;
  "%ignore", Pignore;
  "%field0", Pfield 0;
  "%field1", Pfield 1;
  "%setfield0", Psetfield(0, true);
  "%makeblock", Pmakeblock(0, Immutable);
  "%makemutable", Pmakeblock(0, Mutable);
  "%raise", Praise;
  "%sequand", Psequand;
  "%sequor", Psequor;
  "%boolnot", Pnot;
  "%negint", Pnegint;
  "%succint", Poffsetint 1;
  "%predint", Poffsetint(-1);
  "%addint", Paddint;
  "%subint", Psubint;
  "%mulint", Pmulint;
  "%divint", Pdivint;
  "%modint", Pmodint;
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
  "%string_safe_set", Pstringsets;
  "%string_unsafe_get", Pstringrefu;
  "%string_unsafe_set", Pstringsetu;
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
  "%nativeint_div", Pdivbint Pnativeint;
  "%nativeint_mod", Pmodbint Pnativeint;
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
  "%int32_div", Pdivbint Pint32;
  "%int32_mod", Pmodbint Pint32;
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
  "%int64_div", Pdivbint Pint64;
  "%int64_mod", Pmodbint Pint64;
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
    Pbigarrayset(true, 3, Pbigarray_unknown, Pbigarray_unknown_layout)
]

let prim_makearray =
  { prim_name = "caml_make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let prim_obj_dup =
  { prim_name = "caml_obj_dup"; prim_arity = 1; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let transl_prim prim args =
  try
    let (gencomp, intcomp, floatcomp, stringcomp,
         nativeintcomp, int32comp, int64comp,
         simplify_constant_constructor) =
      Hashtbl.find comparisons_table prim.prim_name in
    begin match args with
      [arg1; {exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}]
      when simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Texp_construct(_, {cstr_tag = Cstr_constant _}, _)}; arg2]
      when simplify_constant_constructor ->
        intcomp
    | [arg1; {exp_desc = Texp_variant(_, None)}]
      when simplify_constant_constructor ->
        intcomp
    | [{exp_desc = Texp_variant(_, None)}; exp2]
      when simplify_constant_constructor ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int
                     || has_base_type arg1 Predef.path_char ->
        intcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_float ->
        floatcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_string ->
        stringcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_nativeint ->
        nativeintcomp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int32 ->
        int32comp
    | [arg1; arg2] when has_base_type arg1 Predef.path_int64 ->
        int64comp
    | _ ->
        gencomp
    end
  with Not_found ->
  try
    let p = Hashtbl.find primitives_table prim.prim_name in
    (* Try strength reduction based on the type of the argument *)
    begin match (p, args) with
        (Psetfield(n, _), [arg1; arg2]) -> Psetfield(n, maybe_pointer arg2)
      | (Parraylength Pgenarray, [arg])   -> Parraylength(array_kind arg)
      | (Parrayrefu Pgenarray, arg1 :: _) -> Parrayrefu(array_kind arg1)
      | (Parraysetu Pgenarray, arg1 :: _) -> Parraysetu(array_kind arg1)
      | (Parrayrefs Pgenarray, arg1 :: _) -> Parrayrefs(array_kind arg1)
      | (Parraysets Pgenarray, arg1 :: _) -> Parraysets(array_kind arg1)
      | (Pbigarrayref(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayref(unsafe, n, k, l)
      | (Pbigarrayset(unsafe, n, Pbigarray_unknown, Pbigarray_unknown_layout),
                      arg1 :: _) ->
            let (k, l) = bigarray_kind_and_layout arg1 in
            Pbigarrayset(unsafe, n, k, l)
      | _ -> p
    end
  with Not_found ->
    Pccall prim


(* Eta-expand a primitive without knowing the types of its arguments *)

let transl_primitive p =
  let prim =
    try
      let (gencomp, _, _, _, _, _, _, _) =
        Hashtbl.find comparisons_table p.prim_name in
      gencomp
    with Not_found ->
    try
      Hashtbl.find primitives_table p.prim_name
    with Not_found ->
      Pccall p in
  match prim with
    Plazyforce ->
      let parm = Ident.create "prim" in
      Lfunction(Curried, [parm], Matching.inline_lazy_force (Lvar parm) Location.none)
  | _ ->
      let rec make_params n =
        if n <= 0 then [] else Ident.create "prim" :: make_params (n-1) in
      let params = make_params p.prim_arity in
      Lfunction(Curried, params, Lprim(prim, List.map (fun id -> Lvar id) params))

(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda idlist lam =
  let rec check_top idlist = function
    | Lvar v -> not (List.mem v idlist)
    | Llet (_, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, arg, body) ->
        check idlist arg && check_top (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check_top idlist' body
    | Lsequence (lam1, lam2) -> check idlist lam1 && check_top idlist lam2
    | Levent (lam, _) -> check_top idlist lam
    | lam -> check idlist lam

  and check idlist = function
    | Lvar _ -> true
    | Lfunction(kind, params, body) -> true
    | Llet (_, _, _, _) as lam when check_recursive_recordwith idlist lam ->
        true
    | Llet(str, id, arg, body) ->
        check idlist arg && check (add_let id arg idlist) body
    | Lletrec(bindings, body) ->
        let idlist' = add_letrec bindings idlist in
        List.for_all (fun (id, arg) -> check idlist' arg) bindings &&
        check idlist' body
    | Lprim(Pmakeblock(tag, mut), args) ->
        List.for_all (check idlist) args
    | Lprim(Pmakearray(Paddrarray|Pintarray), args) ->
        List.for_all (check idlist) args
    | Lprim (Pmakearray (Pgenarray), args) -> false
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
    | Llet (Strict, id1, Lprim (Pduprecord _, [e1]), body) ->
       check_top idlist e1
       && check_recordwith_updates idlist id1 body
    | _ -> false

  and check_recordwith_updates idlist id1 = function
    | Lsequence (Lprim ((Psetfield _ | Psetfloatfield _), [Lvar id2; e1]), cont)
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

(* To find reasonable names for let-bound and lambda-bound idents *)

let rec name_pattern default = function
    [] -> Ident.create default
  | (p, e) :: rem ->
      match p.pat_desc with
        Tpat_var id -> id
      | Tpat_alias(p, id) -> id
      | _ -> name_pattern default rem

(* Push the default values under the functional abstractions *)

let rec push_defaults loc bindings pat_expr_list partial =
  match pat_expr_list with
    [pat, ({exp_desc = Texp_function(pl,partial)} as exp)] ->
      let pl = push_defaults exp.exp_loc bindings pl partial in
      [pat, {exp with exp_desc = Texp_function(pl, partial)}]
  | [pat, {exp_desc = Texp_let
             (Default, cases, ({exp_desc = Texp_function _} as e2))}] ->
      push_defaults loc (cases :: bindings) [pat, e2] partial
  | [pat, exp] ->
      let exp =
        List.fold_left
          (fun exp cases ->
            {exp with exp_desc = Texp_let(Nonrecursive, cases, exp)})
          exp bindings
      in
      [pat, exp]
  | (pat, exp) :: _ when bindings <> [] ->
      let param = name_pattern "param" pat_expr_list in
      let exp =
        { exp with exp_loc = loc; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_desc =
              Texp_ident (Path.Pident param,
                          {val_type = pat.pat_type; val_kind = Val_reg})},
             pat_expr_list, partial) }
      in
      push_defaults loc bindings
        [{pat with pat_desc = Tpat_var param}, exp] Total
  | _ ->
      pat_expr_list

(* Insertion of debugging events *)

let event_before exp lam = match lam with
| Lstaticraise (_,_) -> lam
| _ ->
  if !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_before;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

let event_after exp lam =
  if !Clflags.debug
  then Levent(lam, {lev_loc = exp.exp_loc;
                    lev_kind = Lev_after exp.exp_type;
                    lev_repr = None;
                    lev_env = Env.summary exp.exp_env})
  else lam

let event_function exp lam =
  if !Clflags.debug then
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
  | Pccall _ | Pstringrefs | Pstringsets | Parrayrefs _ | Parraysets _ |
    Pbigarrayref _ | Pbigarrayset _ | Pduprecord _ -> true
  | _ -> false

(* Contracts *)
let contract_failed loc pathop = 
  (* [Location.get_pos_info] is too expensive 
     Code below modifies assert_failed by adding one more field to blame func_name *)
  let filename = match loc.Location.loc_start.Lexing.pos_fname with
              | "" -> !Location.input_name
              | x -> x
  in
  let func_name = match pathop with
                  | Some path -> (Path.name path)      
                  | None ->  "_"
  in 
  let pos = loc.Location.loc_start in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Lprim(Praise, [Lprim(Pmakeblock(0, Immutable),
                      [transl_path Predef.path_contract_failure;
	    	       Lconst(Const_block(0,
			[Const_base(Const_string filename);
			 Const_base(Const_int line);
			 Const_base(Const_int char);
                         Const_base(Const_string ("Blame: " ^ func_name))]))])])
               

(* Assertions *)

let assert_failed loc =
  (* [Location.get_pos_info] is too expensive *)
  let fname = match loc.Location.loc_start.Lexing.pos_fname with
              | "" -> !Location.input_name
              | x -> x
  in
  let pos = loc.Location.loc_start in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  Lprim(Praise, [Lprim(Pmakeblock(0, Immutable),
          [transl_path Predef.path_assert_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string fname);
               Const_base(Const_int line);
               Const_base(Const_int char)]))])])
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let rec transl_exp e =
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
    Texp_ident(path, {val_kind = Val_prim p}) ->
      let public_send = p.prim_name = "%send" in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public else Self in
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        Lfunction(Curried, [obj; meth], Lsend(kind, Lvar meth, Lvar obj, []))
      else if p.prim_name = "%sendcache" then
        let obj = Ident.create "obj" and meth = Ident.create "meth" in
        let cache = Ident.create "cache" and pos = Ident.create "pos" in
        Lfunction(Curried, [obj; meth; cache; pos],
                  Lsend(Cached, Lvar meth, Lvar obj, [Lvar cache; Lvar pos]))
      else
        transl_primitive p
  | Texp_ident(path, {val_kind = Val_anc _}) ->
      raise(Error(e.exp_loc, Free_super_var))
  | Texp_ident(path, {val_kind = Val_reg | Val_self _}) ->
      transl_path path
  | Texp_ident _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (event_before body (transl_exp body))
  | Texp_function (pat_expr_list, partial) ->
      let ((kind, params), body) =
        event_function e
          (function repr ->
            let pl = push_defaults e.exp_loc [] pat_expr_list partial in
            transl_function e.exp_loc !Clflags.native_code repr partial pl)
      in
      Lfunction(kind, params, body)
  | Texp_apply({exp_desc = Texp_ident(path, {val_kind = Val_prim p})}, oargs)
    when List.length oargs >= p.prim_arity
    && List.for_all (fun (arg,_) -> arg <> None) oargs ->
      let args, args' = cut p.prim_arity oargs in
      let wrap f =
        if args' = []
        then event_after e f
        else event_after e (transl_apply f args' e.exp_loc)
      in
      let wrap0 f =
        if args' = [] then f else wrap f in
      let args = List.map (function Some x, _ -> x | _ -> assert false) args in
      let argl = transl_list args in
      let public_send = p.prim_name = "%send"
        || not !Clflags.native_code && p.prim_name = "%sendcache"in
      if public_send || p.prim_name = "%sendself" then
        let kind = if public_send then Public else Self in
        let obj = List.hd argl in
        wrap (Lsend (kind, List.nth argl 1, obj, []))
      else if p.prim_name = "%sendcache" then
        match argl with [obj; meth; cache; pos] ->
          wrap (Lsend(Cached, meth, obj, [cache; pos]))
        | _ -> assert false
      else begin
        let prim = transl_prim p args in
        match (prim, args) with
          (Praise, [arg1]) ->
            wrap0 (Lprim(Praise, [event_after arg1 (List.hd argl)]))
        | (_, _) ->
            begin match (prim, argl) with
            | (Plazyforce, [a]) ->
                wrap (Matching.inline_lazy_force a e.exp_loc)
            | (Plazyforce, _) -> assert false
            |_ -> let p = Lprim(prim, argl) in
               if primitive_is_ccall prim then wrap p else wrap0 p
            end
      end
  | Texp_apply(funct, oargs) ->
      event_after e (transl_apply (transl_exp funct) oargs e.exp_loc)
  | Texp_match({exp_desc = Texp_tuple argl}, pat_expr_list, partial) ->
      Matching.for_multiple_match e.exp_loc
        (transl_list argl) (transl_cases pat_expr_list) partial
  | Texp_match(arg, pat_expr_list, partial) ->
      Matching.for_function e.exp_loc None
        (transl_exp arg) (transl_cases pat_expr_list) partial
  | Texp_try(body, pat_expr_list) ->
      let id = name_pattern "exn" pat_expr_list in
      Ltrywith(transl_exp body, id,
               Matching.for_trywith (Lvar id) (transl_cases pat_expr_list))
  | Texp_tuple el ->
      let ll = transl_list el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable), ll)
      end
  | Texp_construct(_, cstr, args) ->
      let ll = transl_list args in
      begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(Const_pointer n)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable), ll)
          end
      | Cstr_exception path ->
          Lprim(Pmakeblock(0, Immutable), transl_path path :: ll)
      end
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
            Lprim(Pmakeblock(0, Immutable),
                  [Lconst(Const_base(Const_int tag)); lam])
      end
  | Texp_record ((lbl1, _) :: _ as lbl_expr_list, opt_init_expr) ->
      transl_record lbl1.lbl_all lbl1.lbl_repres lbl_expr_list opt_init_expr
  | Texp_record ([], _) ->
      fatal_error "Translcore.transl_exp: bad Texp_record"
  | Texp_field(arg, lbl) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Pfield lbl.lbl_pos
        | Record_float -> Pfloatfield lbl.lbl_pos in
      Lprim(access, [transl_exp arg])
  | Texp_setfield(arg, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer newval)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
      Lprim(access, [transl_exp arg; transl_exp newval])
  | Texp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list expr_list in
      begin try
        (* Deactivate constant optimization if array is small enough *)
        if List.length ll <= 4 then raise Not_constant;
        let cl = List.map extract_constant ll in
        let master =
          match kind with
          | Paddrarray | Pintarray ->
              Lconst(Const_block(0, cl))
          | Pfloatarray ->
              Lconst(Const_float_array(List.map extract_float cl))
          | Pgenarray ->
              raise Not_constant in             (* can this really happen? *)
        Lprim(Pccall prim_obj_dup, [master])
      with Not_constant ->
        Lprim(Pmakearray kind, ll)
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
  | Texp_for(param, low, high, dir, body) ->
      Lfor(param, transl_exp low, transl_exp high, dir,
           event_before body (transl_exp body))
  | Texp_when(cond, body) ->
      event_before cond
        (Lifthenelse(transl_exp cond, event_before body (transl_exp body),
                     staticfail))
  | Texp_send(expr, met) ->
      let obj = transl_exp expr in
      let lam =
        match met with
          Tmeth_val id -> Lsend (Self, Lvar id, obj, [])
        | Tmeth_name nm ->
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache)
      in
      event_after e lam
  | Texp_new (cl, _) ->
      Lapply(Lprim(Pfield 0, [transl_path cl]), [lambda_unit], Location.none)
  | Texp_instvar(path_self, path) ->
      Lprim(Parrayrefu Paddrarray, [transl_path path_self; transl_path path])
  | Texp_setinstvar(path_self, path, expr) ->
      transl_setinstvar (transl_path path_self) path expr
  | Texp_override(path_self, modifs) ->
      let cpy = Ident.create "copy" in
      Llet(Strict, cpy,
           Lapply(Translobj.oo_prim "copy", [transl_path path_self],
                  Location.none),
           List.fold_right
             (fun (path, expr) rem ->
                Lsequence(transl_setinstvar (Lvar cpy) path expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(id, modl, body) ->
      Llet(Strict, id, !transl_module Tcoerce_none None modl, transl_exp body)
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e.exp_loc)
  | Texp_assertfalse -> assert_failed e.exp_loc
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match e.exp_desc with
        (* a constant expr of type <> float gets compiled as itself *)
      | Texp_constant
          ( Const_int _ | Const_char _ | Const_string _
          | Const_int32 _ | Const_int64 _ | Const_nativeint _ )
      | Texp_function(_, _)
      | Texp_construct (_, {cstr_arity = 0}, _)
        -> transl_exp e
      | Texp_constant(Const_float _) ->
          Lprim(Pmakeblock(Obj.forward_tag, Immutable), [transl_exp e])
      | Texp_ident(_, _) -> (* according to the type *)
          begin match e.exp_type.desc with
          (* the following may represent a float/forward/lazy: need a
             forward_tag *)
          | Tvar | Tlink _ | Tsubst _ | Tunivar
          | Tpoly(_,_) | Tfield(_,_,_,_) ->
              Lprim(Pmakeblock(Obj.forward_tag, Immutable), [transl_exp e])
          (* the following cannot be represented as float/forward/lazy:
             optimize *)
          | Tarrow(_,_,_,_) | Ttuple _ | Tobject(_,_) | Tnil | Tvariant _
              -> transl_exp e
          (* optimize predefined types (excepted float) *)
          | Tconstr(_,_,_) ->
              if has_base_type e Predef.path_int
                || has_base_type e Predef.path_char
                || has_base_type e Predef.path_string
                || has_base_type e Predef.path_bool
                || has_base_type e Predef.path_unit
                || has_base_type e Predef.path_exn
                || has_base_type e Predef.path_array
                || has_base_type e Predef.path_list
                || has_base_type e Predef.path_format6
                || has_base_type e Predef.path_option
                || has_base_type e Predef.path_nativeint
                || has_base_type e Predef.path_int32
                || has_base_type e Predef.path_int64
              then transl_exp e
              else
                Lprim(Pmakeblock(Obj.forward_tag, Immutable), [transl_exp e])
          end
      (* other cases compile to a lazy block holding a function *)
      | _ ->
          let fn = Lfunction (Curried, [Ident.create "param"], transl_exp e) in
          Lprim(Pmakeblock(Config.lazy_tag, Immutable), [fn])
      end
  | Texp_object (cs, cty, meths) ->
      let cl = Ident.create "class" in
      !transl_object cl meths
        { cl_desc = Tclass_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Tcty_signature cty;
          cl_env = e.exp_env }
  | Texp_contract (c, e, callee, caller) -> 
      let lexp = transl_exp e in
      if !Clflags.nocontract 
      then lexp
      else let wrapped_expr = transl_contract c e callee caller in
           transl_exp wrapped_expr
  | Texp_bad bl -> transl_blame bl
  | Texp_unr bl -> transl_blame bl

and transl_blame bl = 
   match bl with
   | Blame (loc, pathop) -> contract_failed loc pathop
   | UnknownBlame -> lambda_unit

(* We expand the wrapped expression at typedtree level, then do overall
translation to lambda.
val transl_contract: Typedtree.expression_desc ->  Typedtree.expression
*)

and transl_contract cntr e callee caller =
  let cty  = e.exp_type in
  let mkpat id ty = { pat_desc = Tpat_var id; 
                    pat_loc  = e.exp_loc;
                    pat_type = ty;
                    pat_env  = e.exp_env } in
  let mkexp ed ty = {e with exp_desc = ed ; exp_type = ty} in
  let mkident i ty = Texp_ident (Pident i , { val_type = ty;
                                              val_kind = Val_reg}) in
  let ce:expression_desc = match cntr.contract_desc with
     | Tctr_pred (x, p) -> 
       (* e |>r1,r2<| {x | p} =  let x = e in if p then x else r1 
          This forces evaluation of e, that is, if e diverges, RHS diverges;
          if e crashes, RHS crashes 
       *) 
        let xe = Texp_ident (Pident x, {val_type = cty; val_kind = Val_reg}) in
        let cond = Texp_ifthenelse (p, mkexp xe cty, Some callee) in
	Texp_let (Nonrecursive, [(mkpat x cty, e)], mkexp cond cty) 
     | Tctr_arrow (xop, c1, c2) -> 
      (* picky version:
         <<x:c1 -> c2>> e = \x. (<<c2[(<<c1>> x)/x]>> (e (<<c1>> x)))
         lax version: 
         <<x:c1 -> c2>> e = \x. (<<c2>> (e (<<c1>> x))) 
         we are implementing the picky version. *)      
         let c1_type = c1.contract_type in
         let c2_type = c2.contract_type in
         let res = match xop with
         | Some x -> 
	     let xvar = mkexp (mkident x c1_type) c1_type in
             let c1x = transl_contract c1 xvar caller callee in
	     let resarg = Texp_apply (e, [(Some c1x, Required)]) in
              (* e.g. x:({a | a > 0} -> {b | true}) -> {c | x 0 > c} 
                 we want to blame the x in {c | x 0 > c} 
  	      *)
	     let blame_x = mkexp (Texp_bad (Blame (c2.contract_loc, None))) cty in
	     let c1x_picky = transl_contract c1 xvar caller blame_x in
	     let c2subst = subst_contract x c1x_picky c2 in 
	     let resfun = transl_contract c2subst (mkexp resarg c2_type) callee caller in
	     Texp_function ([(mkpat x c1_type, resfun)], Partial)
         | None -> 
	     let x = Ident.create "c" in
             let xvar = mkexp (mkident x c1_type) c1_type in
             let c1x = transl_contract c1 xvar caller callee in
   	     let resarg = Texp_apply (e, [(Some c1x, Required)]) in
             let resfun = transl_contract c2 (mkexp resarg c2_type) callee caller in
             Texp_function ([(mkpat x c1_type, resfun)], Partial)
         in res
     | Tctr_tuple cs -> 
      (* <<(c1, c2)>> e = match e with 
                           (x1, x2) -> (<<c1>> x1, <<c2>> x2)  *)
       begin 	
	  let new_ids =  (Ident.create_idents "x" (List.length cs))  in
          let typs = match (Ctype.repr cty).desc with
                         | Ttuple ts -> ts
                         | _ -> raise(Error(e.exp_loc,Illegal_tuple_expr)) in
          let (ps, es) = List.split (List.map (fun (i, t) -> 
                                let vd = {val_type = t; val_kind = Val_reg} in
				let exp = { exp_desc = Texp_ident (Pident i, vd);
					    exp_loc = e.exp_loc;
                                            exp_type = t;
					    exp_env = e.exp_env
					      } in
		   	        (mkpat i t, exp))
                            (List.combine new_ids typs)) in
          let ces = List.map (fun (c, ei) -> transl_contract c ei callee caller) 
			    (List.combine cs es) in
          let newpat = { pat_desc = Tpat_tuple ps;
                         pat_loc = e.exp_loc;
                         pat_type = cty;
                         pat_env = e.exp_env } in
          Texp_match (e, [(newpat, mkexp (Texp_tuple ces) cty)], Total)
       end
  in mkexp ce cty

(* Given x:t1 -> t2, the subst_contract computes t2[(v |><| t1)/x] *)
and subst_contract v e cntr = 
  let mkpat var = { pat_desc = Tpat_var var; 
                    pat_loc  = e.exp_loc;
                    pat_type = e.exp_type;
                    pat_env  = e.exp_env } in
  let sc = match cntr.contract_desc with
             Tctr_pred (x, p) -> 
               (* {x | p} [e/v]   is expressed as  {x | let v = e in p} *) 
               let sp = Texp_let (Nonrecursive, [(mkpat v, e)], p) in
               Tctr_pred (x, { e with exp_desc = sp }) 
           | Tctr_arrow (xop, c1, c2) -> 
               let sc1 = subst_contract v e c1 in
               let sc2 = subst_contract v e c2 in
               Tctr_arrow (xop, sc1, sc2)
           | Tctr_tuple cs -> Tctr_tuple (List.map (subst_contract v e) cs)
  in { cntr with contract_desc = sc }
   

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_cases pat_expr_list =
  List.map
    (fun (pat, expr) -> (pat, event_before expr (transl_exp expr)))
    pat_expr_list

and transl_tupled_cases patl_expr_list =
  List.map (fun (patl, expr) -> (patl, transl_exp expr)) patl_expr_list

and transl_apply lam sargs loc =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs) ->
        Lsend(k, lmet, lobj, largs @ args)
    | Levent(Lsend(k, lmet, lobj, largs), _) ->
        Lsend(k, lmet, lobj, largs @ args)
    | Lapply(lexp, largs, _) ->
        Lapply(lexp, largs @ args, loc)
    | lexp ->
        Lapply(lexp, args, loc)
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
          if List.for_all (fun (_,opt) -> opt = Optional) args then [], args
          else args, [] in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args) in
        let handle = protect "func" lam
        and l = List.map (fun (arg, opt) -> may_map (protect "arg") arg, opt) l
        and id_arg = Ident.create "param" in
        let body =
          match build_apply handle ((Lvar id_arg, optional)::args') l with
            Lfunction(Curried, ids, lam) ->
              Lfunction(Curried, id_arg::ids, lam)
          | Levent(Lfunction(Curried, ids, lam), _) ->
              Lfunction(Curried, id_arg::ids, lam)
          | lam ->
              Lfunction(Curried, [id_arg], lam)
        in
        List.fold_left
          (fun body (id, lam) -> Llet(Strict, id, lam, body))
          body !defs
    | (Some arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) l
    | [] ->
        lapply lam (List.rev_map fst args)
  in
  build_apply lam [] (List.map (fun (x,o) -> may_map transl_exp x, o) sargs)

and transl_function loc untuplify_fn repr partial pat_expr_list =
  match pat_expr_list with
    [pat, ({exp_desc = Texp_function(pl,partial')} as exp)]
    when Parmatch.fluid pat ->
      let param = name_pattern "param" pat_expr_list in
      let ((_, params), body) =
        transl_function exp.exp_loc false repr partial' pl in
      ((Curried, param :: params),
       Matching.for_function loc None (Lvar param) [pat, body] partial)
  | ({pat_desc = Tpat_tuple pl}, _) :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun (pat, expr) -> (Matching.flatten_pattern size pat, expr))
            pat_expr_list in
        let params = List.map (fun p -> Ident.create "param") pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
           (transl_tupled_cases pats_expr_list) partial)
      with Matching.Cannot_flatten ->
        let param = name_pattern "param" pat_expr_list in
        ((Curried, [param]),
         Matching.for_function loc repr (Lvar param)
           (transl_cases pat_expr_list) partial)
      end
  | _ ->
      let param = name_pattern "param" pat_expr_list in
      ((Curried, [param]),
       Matching.for_function loc repr (Lvar param)
         (transl_cases pat_expr_list) partial)

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive | Default ->
      let rec transl = function
        [] ->
          body
      | (pat, expr) :: rem ->
          Matching.for_let pat.pat_loc (transl_exp expr) pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun (pat, expr) ->
            match pat.pat_desc with
              Tpat_var id -> id
            | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)))
        pat_expr_list in
      let transl_case (pat, expr) id =
        let lam = transl_exp expr in
        if not (check_recursive_lambda idlist lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      Lletrec(List.map2 transl_case pat_expr_list idlist, body)

and transl_setinstvar self var expr =
  Lprim(Parraysetu (if maybe_pointer expr then Paddrarray else Pintarray),
                    [self; transl_path var; transl_exp expr])

and transl_record all_labels repres lbl_expr_list opt_init_expr =
  let size = Array.length all_labels in
  (* Determine if there are "enough" new fields *)
  if 3 + 2 * List.length lbl_expr_list >= size
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let lv = Array.create (Array.length all_labels) staticfail in
    let init_id = Ident.create "init" in
    begin match opt_init_expr with
      None -> ()
    | Some init_expr ->
        for i = 0 to Array.length all_labels - 1 do
          let access =
            match all_labels.(i).lbl_repres with
              Record_regular -> Pfield i
            | Record_float -> Pfloatfield i in
          lv.(i) <- Lprim(access, [Lvar init_id])
        done
    end;
    List.iter
      (fun (lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp expr)
      lbl_expr_list;
    let ll = Array.to_list lv in
    let mut =
      if List.exists (fun (lbl, expr) -> lbl.lbl_mut = Mutable) lbl_expr_list
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
          Record_regular -> Lconst(Const_block(0, cl))
        | Record_float ->
            Lconst(Const_float_array(List.map extract_float cl))
      with Not_constant ->
        match repres with
          Record_regular -> Lprim(Pmakeblock(0, mut), ll)
        | Record_float -> Lprim(Pmakearray Pfloatarray, ll) in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, init_id, transl_exp init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    (* If you change anything here, you will likely have to change
       [check_recursive_recordwith] in this file. *)
    let copy_id = Ident.create "newrecord" in
    let rec update_field (lbl, expr) cont =
      let upd =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer expr)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
      Lsequence(Lprim(upd, [Lvar copy_id; transl_exp expr]), cont) in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp init_expr]),
             List.fold_right update_field lbl_expr_list (Lvar copy_id))
    end
  end

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

(* Compile an exception definition *)

let transl_exception id path decl =
  let name =
    match path with
      None -> Ident.name id
    | Some p -> Path.name p in
  Lprim(Pmakeblock(0, Immutable), [Lconst(Const_base(Const_string name))])

(* Error report *)

(* open Format *)

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
  | Illegal_tuple_expr -> 
      fprintf ppf
        "This expression is not of type tuple"

