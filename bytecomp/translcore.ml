(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Path
open Typedtree
open Lambda
open Translobj

type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr
  | Free_super_var

exception Error of Location.t * error

(* Translation of primitives *)

let comparisons_table = create_hashtable 11 [
  "%equal",
      (Pccall{prim_name = "equal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Ceq,
       Pfloatcomp Ceq,
       Pccall{prim_name = "string_equal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%notequal",
      (Pccall{prim_name = "notequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cneq,
       Pfloatcomp Cneq,
       Pccall{prim_name = "string_notequal"; prim_arity = 2;
              prim_alloc = false; prim_native_name = ""; 
              prim_native_float = false});
  "%lessthan",
      (Pccall{prim_name = "lessthan"; prim_arity = 2; prim_alloc = false; 
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Clt,
       Pfloatcomp Clt,
       Pccall{prim_name = "lessthan"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%greaterthan",
      (Pccall{prim_name = "greaterthan"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cgt,
       Pfloatcomp Cgt,
       Pccall{prim_name = "greaterthan"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%lessequal",
      (Pccall{prim_name = "lessequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cle,
       Pfloatcomp Cle,
       Pccall{prim_name = "lessequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false});
  "%greaterequal",
      (Pccall{prim_name = "greaterequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false},
       Pintcomp Cge,
       Pfloatcomp Cge,
       Pccall{prim_name = "greaterequal"; prim_arity = 2; prim_alloc = false;
              prim_native_name = ""; prim_native_float = false})
]

let primitives_table = create_hashtable 31 [
  "%identity", Pidentity;
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
  "%obj_size", Parraylength Paddrarray;
  "%obj_field", Parrayrefu Paddrarray;
  "%obj_set_field", Parraysetu Paddrarray
]

let same_base_type ty1 ty2 =
  match ((Ctype.repr ty1).desc, (Ctype.repr ty2).desc) with
    (Tconstr(p1, [], _), Tconstr(p2, [], _)) -> Path.same p1 p2
  | (_, _) -> false

let maybe_pointer arg =
  not(same_base_type arg.exp_type Predef.type_int or
      same_base_type arg.exp_type Predef.type_char)

let array_kind arg =
  match (Ctype.repr arg.exp_type).desc with
    Tconstr(p, [ty], _) when Path.same p Predef.path_array ->
      begin match (Ctype.repr ty).desc with
        Tvar -> Pgenarray
      | Tconstr(p, _, _) ->
          if Path.same p Predef.path_int or Path.same p Predef.path_char then
            Pintarray
          else if Path.same p Predef.path_float then
            Pfloatarray
          else
            Paddrarray
      | _ -> Paddrarray
      end
  | _ -> Pgenarray (* This can happen with abbreviations that we can't expand
                      here because the typing environment is lost *)

let prim_makearray =
  { prim_name = "make_vect"; prim_arity = 2; prim_alloc = true;
    prim_native_name = ""; prim_native_float = false }

let transl_prim prim args =
  try
    let (gencomp, intcomp, floatcomp, stringcomp) =
      Hashtbl.find comparisons_table prim.prim_name in
    match args with
      [arg1; {exp_desc = Texp_construct(cstr, [])}] ->
        intcomp
    | [{exp_desc = Texp_construct(cstr, [])}; arg2] ->
        intcomp
    | [arg1; arg2] when same_base_type arg1.exp_type Predef.type_int
                     or same_base_type arg1.exp_type Predef.type_char ->
        intcomp
    | [arg1; arg2] when same_base_type arg1.exp_type Predef.type_float ->
        floatcomp
    | [arg1; arg2] when same_base_type arg1.exp_type Predef.type_string ->
        stringcomp
    | _ ->
        gencomp
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
      | _ -> p
    end
  with Not_found ->
    Pccall prim

(* Eta-expand a primitive without knowing the types of its arguments *)

let transl_primitive p =
  let prim =
    try
      let (gencomp, intcomp, floatcomp, stringcomp) =
        Hashtbl.find comparisons_table p.prim_name in
      gencomp
    with Not_found ->
    try
      Hashtbl.find primitives_table p.prim_name
    with Not_found ->
      Pccall p in
  let rec make_params n =
    if n <= 0 then [] else Ident.create "prim" :: make_params (n-1) in
  let params = make_params p.prim_arity in
  Lfunction(params, Lprim(prim, List.map (fun id -> Lvar id) params))

(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda id lam =
  let rec check_top = function
      Lfunction(params, body) as funct -> true
    | Lprim(Pmakeblock(tag, mut), args) -> List.for_all check args
    | Llet(str, id, arg, body) -> check arg & check_top body
    | _ -> false
  and check = function
      Lvar _ -> true
    | Lconst cst -> true
    | Lfunction(params, body) -> true
    | Llet(_, _, arg, body) -> check arg & check body
    | Lprim(Pmakeblock(tag, mut), args) -> List.for_all check args
    | lam -> not(IdentSet.mem id (free_variables lam))
  in check_top lam

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

(* Translation of expressions *)

let rec transl_exp e =
  match e.exp_desc with
    Texp_ident(path, {val_kind = Val_prim p}) ->
      transl_primitive p
  | Texp_ident(path, {val_kind = Val_anc _}) ->
      raise(Error(e.exp_loc, Free_super_var))
  | Texp_ident(path, desc) ->
      transl_path path
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (transl_exp body)
  | Texp_function pat_expr_list ->
      let (params, body) = transl_function e.exp_loc pat_expr_list in
      Lfunction(params, body)
  | Texp_apply({exp_desc = Texp_ident(path, {val_kind = Val_prim p})}, args)
    when List.length args = p.prim_arity ->
      Lprim(transl_prim p args, transl_list args)
  | Texp_apply(funct, args) ->
      begin match transl_exp funct with
        Lapply(lfunct, largs) ->
          Lapply(lfunct, largs @ transl_list args)
      | Lsend(lmet, lobj, largs) ->
          Lsend(lmet, lobj, largs @ transl_list args)
      | lexp ->
          Lapply(lexp, transl_list args)
      end
  | Texp_match({exp_desc = Texp_tuple argl} as arg, pat_expr_list) ->
      Matching.for_multiple_match e.exp_loc
        (transl_list argl) (transl_cases pat_expr_list)
  | Texp_match(arg, pat_expr_list) ->
      Matching.for_function e.exp_loc
        (transl_exp arg) (transl_cases pat_expr_list)
  | Texp_try(body, pat_expr_list) ->
      let id = Ident.create "exn" in
      Ltrywith(transl_exp body, id,
               Matching.for_trywith (Lvar id) (transl_cases pat_expr_list))
  | Texp_tuple el ->
      let ll = transl_list el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable), ll)
      end
  | Texp_construct(cstr, args) ->
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
  | Texp_record ((lbl1, _) :: _ as lbl_expr_list) ->
      let lv = Array.create (Array.length lbl1.lbl_all) Lstaticfail in
      List.iter
        (fun (lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp expr)
        lbl_expr_list;
      let ll = Array.to_list lv in
      if List.exists (fun (lbl, expr) -> lbl.lbl_mut = Mutable) lbl_expr_list
      then begin
        match lbl1.lbl_repres with
          Record_regular -> Lprim(Pmakeblock(0, Mutable), ll)
        | Record_float -> Lprim(Pmakearray Pfloatarray, ll)
      end else begin
        try
          let cl = List.map extract_constant ll in
          match lbl1.lbl_repres with
            Record_regular -> Lconst(Const_block(0, cl))
          | Record_float ->
              Lconst(Const_float_array(List.map extract_float cl))
        with Not_constant ->
          match lbl1.lbl_repres with
            Record_regular -> Lprim(Pmakeblock(0, Immutable), ll)
          | Record_float -> Lprim(Pmakearray Pfloatarray, ll)
      end
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
      let len = List.length expr_list in
      if len <= Config.max_young_wosize then
        Lprim(Pmakearray kind, transl_list expr_list)
      else begin
        let v = Ident.create "makearray" in
        let rec fill_fields pos = function
          [] ->
            Lvar v
        | arg :: rem ->
            Lsequence(Lprim(Parraysetu kind,
                            [Lvar v;
                             Lconst(Const_base(Const_int pos));
                             transl_exp arg]),
                      fill_fields (pos+1) rem) in
        Llet(Strict, v,
             Lprim(Pccall prim_makearray,
                   [Lconst(Const_base(Const_int len));
                    transl_exp (List.hd expr_list)]),
             fill_fields 1 (List.tl expr_list))
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp cond, transl_exp ifso, transl_exp ifnot)
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp cond, transl_exp ifso, lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp expr1, transl_exp expr2)
  | Texp_while(cond, body) ->
      Lwhile(transl_exp cond, transl_exp body)
  | Texp_for(param, low, high, dir, body) ->
      Lfor(param, transl_exp low, transl_exp high, dir, transl_exp body)
  | Texp_when(cond, body) ->
      Lifthenelse(transl_exp cond, transl_exp body, Lstaticfail)
  | Texp_send(expr, met) ->
      Lsend(Lvar (meth met), transl_exp expr, [])
  | Texp_new cl ->
      Lprim(Pfield 0, [transl_path cl])
  | Texp_instvar(path_self, path) ->
      Lprim(Parrayrefu Paddrarray , [transl_path path_self; transl_path path])
  | Texp_setinstvar(path_self, path, expr) ->
      transl_setinstvar (transl_path path_self) path expr
  | Texp_override(path_self, modifs) ->
      let cpy = Ident.create "copy" in
      Llet(Strict, cpy, Lapply(oo_prim "copy", [transl_path path_self]),
      List.fold_right
      	(fun (path, expr) rem ->
	   Lsequence(transl_setinstvar (Lvar cpy) path expr,
	             rem))
	modifs
	(Lvar cpy))
  | _ ->
      fatal_error "Translcore.transl"

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_cases pat_expr_list =
  List.map (fun (pat, expr) -> (pat, transl_exp expr)) pat_expr_list

and transl_function loc pat_expr_list =
  let param = name_pattern "param" pat_expr_list in
  match pat_expr_list with
    [pat, ({exp_desc = Texp_function pl} as exp)] ->
      let (params, body) = transl_function exp.exp_loc pl in
      (param :: params, Matching.for_function loc (Lvar param) [pat, body])
  | _ ->
      ([param], Matching.for_function loc (Lvar param)
                                          (transl_cases pat_expr_list))

and transl_let rec_flag pat_expr_list body =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          body
      | (pat, expr) :: rem ->
          Matching.for_let pat.pat_loc (transl_exp expr) pat (transl rem)
      in transl pat_expr_list
  | Recursive ->
      let transl_case (pat, expr) =
        let id = 
          match pat.pat_desc with
            Tpat_var id -> id
          | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)) in
        let lam = transl_exp expr in
        if not (check_recursive_lambda id lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      Lletrec(List.map transl_case pat_expr_list, body)

and transl_setinstvar self var expr =
  Lprim(Parraysetu (if maybe_pointer expr then Paddrarray else Pintarray),
                    [self; transl_path var; transl_exp expr])

(* Compile an exception definition *)

let transl_exception id decl =
    Lprim(Pmakeblock(0, Immutable),
          [Lconst(Const_base(Const_string(Ident.name id)))])

(* Error report *)

open Format

let report_error = function
    Illegal_letrec_pat ->
      print_string
      "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      print_string
      "This kind of expression is not allowed as right-hand side of `let rec'"
  | Free_super_var ->
      print_string
        "Ancestor names can only be used to select inherited methods"
