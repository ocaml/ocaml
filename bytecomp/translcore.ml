(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
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


type error =
    Illegal_letrec_pat
  | Illegal_letrec_expr

exception Error of Location.t * error

(* The translation environment maps identifiers bound by patterns
   to lambda-terms, e.g. access paths.
   Identifiers unbound in the environment List.map to themselves. *)

(* Compute the access paths to identifiers bound in patterns. *)

let identity_env e = e

let rec bind_pattern env pat arg mut =
  match pat.pat_desc with
    Tpat_var id ->
      begin match mut with
        Mutable   -> (env, fun e -> Llet(id, arg, e))
      | Immutable -> (add_env id arg env, identity_env)
      end
  | Tpat_alias(pat, id) ->
      let (ext_env, bind) = bind_pattern env pat arg mut in
      begin match mut with
        Mutable   -> (ext_env, fun e -> Llet(id, arg, bind e))
      | Immutable -> (add_env id arg ext_env, bind)
      end
  | Tpat_tuple patl ->
      begin match arg with
        Lprim(Pmakeblock _, argl) -> bind_patterns env patl argl
      | _ -> bind_pattern_list env patl arg mut 0
      end
  | Tpat_construct(cstr, patl) ->
      begin match cstr.cstr_tag with
        Cstr_constant _  -> (env, identity_env)
      | Cstr_block _     -> bind_pattern_list env patl arg mut 0
      | Cstr_exception _ -> bind_pattern_list env patl arg mut 1
      end
  | Tpat_record lbl_pat_list ->
      bind_label_pattern env lbl_pat_list arg mut
  | _ ->
      (env, identity_env)

and bind_pattern_list env patl arg mut pos =
  match patl with
    [] -> (env, identity_env)
  | pat :: rem ->
      let (env1, bind1) =
        bind_pattern env pat (Lprim(Pfield pos, [arg])) mut in
      let (env2, bind2) =
        bind_pattern_list env1 rem arg mut (pos+1) in
      (env2, fun e -> bind1(bind2 e))

and bind_label_pattern env patl arg mut =
  match patl with
    [] -> (env, identity_env)
  | (lbl, pat) :: rem ->
      let mut1 =
        match lbl.lbl_mut with Mutable -> Mutable | Immutable -> mut in
      let access =
        match lbl.lbl_repres with
          Record_regular -> Pfield lbl.lbl_pos
        | Record_float -> Pfloatfield lbl.lbl_pos in
      let (env1, bind1) =
        bind_pattern env pat (Lprim(access, [arg])) mut1 in
      let (env2, bind2) =
        bind_label_pattern env1 rem arg mut in
      (env2, fun e -> bind1(bind2 e))

and bind_patterns env patl argl =
  match (patl, argl) with
    ([], []) -> (env, identity_env)
  | (pat1::patl, arg1::argl) ->
      let (env1, bind1) = bind_pattern env pat1 arg1 Immutable in
      let (env2, bind2) = bind_patterns env1 patl argl in
      (env2, fun e -> bind1(bind2 e))
  | (_, _) ->
      fatal_error "Translcore.bind_patterns"

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
  "%makeblock", Pmakeblock 0;
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
  match (Ctype.repr ty1, Ctype.repr ty2) with
    (Tconstr(p1, []), Tconstr(p2, [])) -> Path.same p1 p2
  | (_, _) -> false

let maybe_pointer arg =
  not(same_base_type arg.exp_type Predef.type_int or
      same_base_type arg.exp_type Predef.type_char)

let array_kind arg =
  match Ctype.repr arg.exp_type with
    Tconstr(p, [ty]) when Path.same p Predef.path_array ->
      begin match Ctype.repr ty with
        Tvar v -> Pgenarray
      | Tconstr(p, _) ->
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

(* To check the well-formedness of r.h.s. of "let rec" definitions *)

let check_recursive_lambda id lam =
  let rec check_top = function
      Lfunction(param, body) as funct -> true
    | Lprim(Pmakeblock tag, args) -> List.for_all check args
    | Llet(id, arg, body) -> check arg & check_top body
    | _ -> false
  and check = function
      Lvar _ -> true
    | Lconst cst -> true
    | Lfunction(param, body) -> true
    | Llet(_, arg, body) -> check arg & check body
    | Lprim(Pmakeblock tag, args) -> List.for_all check args
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

let name_pattern default p =
  match p.pat_desc with
    Tpat_var id -> id
  | Tpat_alias(p, id) -> id
  | _ -> Ident.new default

let name_pattern_list default = function
    [] -> Ident.new default
  | (p, e) :: _ -> name_pattern default p

(* Translation of expressions *)

let rec transl_exp env e =
  match e.exp_desc with
    Texp_ident(path, desc) ->
      begin match path with
          Pident id -> transl_access env id
        | _ -> transl_path path
      end
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      let (ext_env, add_let) = transl_let env rec_flag pat_expr_list in
      add_let(transl_exp ext_env body)
  | Texp_function pat_expr_list ->
      let param = name_pattern_list "param" pat_expr_list in
      Lfunction(param, Matching.for_function e.exp_loc (Lvar param)
                         (transl_cases env (Lvar param) pat_expr_list))
  | Texp_apply({exp_desc = Texp_ident(path, {val_prim = Some p})},
               args) when List.length args = p.prim_arity ->
      Lprim(transl_prim p args, transl_list env args)
  | Texp_apply(funct, args) ->
      Lapply(transl_exp env funct, transl_list env args)
  | Texp_match({exp_desc = Texp_tuple argl} as arg, pat_expr_list) ->
      name_lambda_list (transl_list env argl) (fun paraml ->
        let param = Lprim(Pmakeblock 0, paraml) in
          Matching.for_function e.exp_loc param
              (transl_cases env param pat_expr_list))
  | Texp_match(arg, pat_expr_list) ->
      name_lambda (transl_exp env arg) (fun id ->
        Matching.for_function e.exp_loc (Lvar id)
                              (transl_cases env (Lvar id) pat_expr_list))
  | Texp_try(body, pat_expr_list) ->
      let id = Ident.new "exn" in
      Ltrywith(transl_exp env body, id,
               Matching.for_trywith id
                 (transl_cases env (Lvar id) pat_expr_list))
  | Texp_tuple el ->
      let ll = transl_list env el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock 0, ll)
      end
  | Texp_construct(cstr, args) ->
      let ll = transl_list env args in
      begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(Const_pointer n)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock n, ll)
          end
      | Cstr_exception path ->
          Lprim(Pmakeblock 0, transl_path path :: ll)
      end
  | Texp_record ((lbl1, _) :: _ as lbl_expr_list) ->
      let lv = Array.new (Array.length lbl1.lbl_all) Lstaticfail in
      List.iter
        (fun (lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp env expr)
        lbl_expr_list;
      let ll = Array.to_list lv in
      begin try
        List.iter
          (fun (lbl, expr) -> if lbl.lbl_mut = Mutable then raise Not_constant)
          lbl_expr_list;
        let cl = List.map extract_constant ll in
        match lbl1.lbl_repres with
          Record_regular -> Lconst(Const_block(0, cl))
        | Record_float -> Lconst(Const_float_array(List.map extract_float cl))
      with Not_constant ->
        match lbl1.lbl_repres with
          Record_regular -> Lprim(Pmakeblock 0, ll)
        | Record_float -> Lprim(Pmakearray Pfloatarray, ll)
      end
  | Texp_field(arg, lbl) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Pfield lbl.lbl_pos
        | Record_float -> Pfloatfield lbl.lbl_pos in
      Lprim(access, [transl_exp env arg])
  | Texp_setfield(arg, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular -> Psetfield(lbl.lbl_pos, maybe_pointer newval)
        | Record_float -> Psetfloatfield lbl.lbl_pos in
      Lprim(access, [transl_exp env arg; transl_exp env newval])
  | Texp_array expr_list ->
      Lprim(Pmakearray(array_kind e), transl_list env expr_list)
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp env cond, transl_exp env ifso,
                                       transl_exp env ifnot)
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp env cond, transl_exp env ifso, lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp env expr1, transl_exp env expr2)
  | Texp_while(cond, body) ->
      Lwhile(transl_exp env cond, transl_exp env body)
  | Texp_for(param, low, high, dir, body) ->
      Lfor(param, transl_exp env low, transl_exp env high, dir,
           transl_exp env body)
  | Texp_when(cond, body) ->
      Lifthenelse(transl_exp env cond, transl_exp env body, Lstaticfail)
  | _ ->
      fatal_error "Translcore.transl"

and transl_list env = function
    [] -> []
  | expr :: rem -> transl_exp env expr :: transl_list env rem

and transl_cases env param pat_expr_list =
  let transl_case (pat, expr) =
    let (ext_env, bind_fun) = bind_pattern env pat param Immutable in
    (pat, bind_fun(transl_exp ext_env expr)) in
  List.map transl_case pat_expr_list

and transl_let env rec_flag pat_expr_list =
  match rec_flag with
    Nonrecursive ->
      let rec transl body_env = function
        [] ->
          (body_env, identity_env)
      | (pat, expr) :: rem ->
          let id = name_pattern "let" pat in
          let (ext_env, bind_fun) =
            bind_pattern body_env pat (Lvar id) Immutable in
          let (final_env, add_let_fun) =
            transl ext_env rem in
          (final_env,
           fun e -> Llet(id, transl_exp env expr,
                         Matching.for_let pat.pat_loc id pat
                           (bind_fun(add_let_fun e)))) in
      transl env pat_expr_list
  | Recursive ->
      let transl_case (pat, expr) =
        let id = 
          match pat.pat_desc with
            Tpat_var id -> id
          | _ -> raise(Error(pat.pat_loc, Illegal_letrec_pat)) in
        let lam = transl_exp env expr in
        if not (check_recursive_lambda id lam) then
          raise(Error(expr.exp_loc, Illegal_letrec_expr));
        (id, lam) in
      let decls =
        List.map transl_case pat_expr_list in
      (env, fun e -> Lletrec(decls, e))

(* Compile a primitive definition *)

let transl_primitive = function
    None -> fatal_error "Translcore.transl_primitive"
  | Some p ->
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
      let rec add_params n params =
        if n >= p.prim_arity
        then Lprim(prim, List.rev params)
        else begin
          let id = Ident.new "prim" in
          Lfunction(id, add_params (n+1) (Lvar id :: params))
        end in
      add_params 0 []

(* Compile an exception definition *)

let transl_exception id decl =
    Lprim(Pmakeblock 0, [Lconst(Const_base(Const_string(Ident.name id)))])

(* Error report *)

open Format

let report_error = function
    Illegal_letrec_pat ->
      print_string
      "Only variables are allowed as left-hand side of `let rec'"
  | Illegal_letrec_expr ->
      print_string
      "This kind of expression is not allowed as right-hand side of `let rec'"
