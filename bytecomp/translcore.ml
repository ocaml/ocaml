(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
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

let rec bind_pattern env pat arg mut =
  match pat.pat_desc with
    Tpat_var id ->
      begin match mut with
        Mutable   -> (env, fun e -> Llet(id, arg, e))
      | Immutable -> (add_env id arg env, fun e -> e)
      end
  | Tpat_alias(pat, id) ->
      let (ext_env, bind) = bind_pattern env pat arg mut in
      begin match mut with
        Mutable   -> (ext_env, fun e -> Llet(id, arg, bind e))
      | Immutable -> (add_env id arg ext_env, bind)
      end
  | Tpat_tuple patl ->
      bind_pattern_list env patl arg mut 0
  | Tpat_construct(cstr, patl) ->
      begin match cstr.cstr_tag with
        Cstr_constant _  -> (env, fun e -> e)
      | Cstr_block _     -> bind_pattern_list env patl arg mut 0
      | Cstr_exception _ -> bind_pattern_list env patl arg mut 1
      end
  | Tpat_record lbl_pat_list ->
      bind_label_pattern env lbl_pat_list arg mut
  | _ ->
      (env, fun e -> e)

and bind_pattern_list env patl arg mut pos =
  match patl with
    [] -> (env, fun e -> e)
  | pat :: rem ->
      let (env1, bind1) =
        bind_pattern env pat (Lprim(Pfield pos, [arg])) mut in
      let (env2, bind2) =
        bind_pattern_list env1 rem arg mut (pos+1) in
      (env2, fun e -> bind1(bind2 e))

and bind_label_pattern env patl arg mut =
  match patl with
    [] -> (env, fun e -> e)
  | (lbl, pat) :: rem ->
      let mut1 =
        match lbl.lbl_mut with Mutable -> Mutable | Immutable -> mut in
      let (env1, bind1) =
        bind_pattern env pat (Lprim(Pfield lbl.lbl_pos, [arg])) mut1 in
      let (env2, bind2) =
        bind_label_pattern env1 rem arg mut in
      (env2, fun e -> bind1(bind2 e))

(* Translation of primitives *)

let comparisons_table = create_hashtable 11 [
  "%equal",
      (Pccall("equal", 2), Pintcomp Ceq, Pfloatcomp Ceq);
  "%notequal",
      (Pccall("notequal", 2), Pintcomp Cneq, Pfloatcomp Cneq);
  "%lessthan",
      (Pccall("lessthan", 2), Pintcomp Clt, Pfloatcomp Clt);
  "%greaterthan",
      (Pccall("greaterthan", 2), Pintcomp Cgt, Pfloatcomp Cgt);
  "%lessequal",
      (Pccall("lessequal", 2), Pintcomp Cle, Pfloatcomp Cle);
  "%greaterequal",
      (Pccall("greaterequal", 2), Pintcomp Cge, Pfloatcomp Cge)
]

let primitives_table = create_hashtable 31 [
  "%identity", Pidentity;
  "%field0", Pfield 0;
  "%field1", Pfield 1;
  "%setfield0", Psetfield 0;
  "%makeblock", Pmakeblock 0;
  "%update", Pupdate;
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
  "%string_unsafe_get", Pgetstringchar;
  "%string_unsafe_set", Psetstringchar;
  "%array_length", Pvectlength;
  "%array_unsafe_get", Pgetvectitem;
  "%array_unsafe_set", Psetvectitem
]

let same_base_type ty1 ty2 =
  match (Ctype.repr ty1, Ctype.repr ty2) with
    (Tconstr(p1, []), Tconstr(p2, [])) -> Path.same p1 p2
  | (_, _) -> false

let transl_prim prim arity args =
  try
    let (gencomp, intcomp, floatcomp) =
      Hashtbl.find comparisons_table prim in
    match args with
      [arg1; arg2] when same_base_type arg1.exp_type Predef.type_int
                     or same_base_type arg1.exp_type Predef.type_char ->
        intcomp
    | [arg1; arg2] when same_base_type arg1.exp_type Predef.type_float ->
        floatcomp
    | _ ->
        gencomp
  with Not_found ->
  try
    Hashtbl.find primitives_table prim
  with Not_found ->
    Pccall(prim, arity)

(* To compile "let rec" *)

exception Unknown

let size_of_lambda id lam =
  let rec size = function
      Lfunction(param, body) -> 2
    | Lprim(Pmakeblock tag, args) -> List.iter check args; List.length args
    | Llet(id, arg, body) -> check arg; size body
    | _ -> raise Unknown
  and check = function
      Lvar _ -> ()
    | Lconst cst -> ()
    | Lfunction(param, body) -> ()
    | Llet(_, arg, body) -> check arg; check body
    | Lprim(Pmakeblock tag, args) -> List.iter check args
    | lam -> if List.mem id (free_variables lam) then raise Unknown
  in size lam

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function Lconst sc -> sc | _ -> raise Not_constant

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
      let param = Ident.new "fun" in
      Lfunction(param, Matching.for_function e.exp_loc param
                         (transl_cases env param pat_expr_list))
  | Texp_apply({exp_desc = Texp_ident(path, {val_prim = Primitive(s, arity)})},
               args) when List.length args = arity ->
      Lprim(transl_prim s arity args, transl_list env args)
  | Texp_apply(funct, args) ->
      Lapply(transl_exp env funct, transl_list env args)
  | Texp_match(arg, pat_expr_list) ->
      name_lambda (transl_exp env arg)
        (fun id ->
          Matching.for_function e.exp_loc id
                                 (transl_cases env id pat_expr_list))
  | Texp_try(body, pat_expr_list) ->
      let id = Ident.new "exn" in
      Ltrywith(transl_exp env body, id,
               Matching.for_trywith id (transl_cases env id pat_expr_list))
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
          Lconst(Const_base(Const_int n))
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock n, ll)
          end
      | Cstr_exception path ->
          Lprim(Pmakeblock 0, transl_path path :: ll)
      end
  | Texp_record lbl_expr_list ->
      let lv = Array.new (List.length lbl_expr_list) Lstaticfail in
      List.iter
        (fun (lbl, expr) -> lv.(lbl.lbl_pos) <- transl_exp env expr)
        lbl_expr_list;
      let ll = Array.to_list lv in
      if List.for_all (fun (lbl, expr) -> lbl.lbl_mut = Immutable)
                      lbl_expr_list
      then begin
        try
          Lconst(Const_block(0, List.map extract_constant ll))
        with Not_constant ->
          Lprim(Pmakeblock 0, ll)
      end else
        Lprim(Pmakeblock 0, ll)
  | Texp_field(arg, lbl) ->
      Lprim(Pfield lbl.lbl_pos, [transl_exp env arg])
  | Texp_setfield(arg, lbl, newval) ->
      Lprim(Psetfield lbl.lbl_pos,
            [transl_exp env arg; transl_exp env newval])
  | Texp_array expr_list ->
      Lprim(Pmakeblock 0, transl_list env expr_list)
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

and transl_list env = function
    [] -> []
  | expr :: rem -> transl_exp env expr :: transl_list env rem

and transl_cases env param pat_expr_list =
  let transl_case (pat, expr) =
    let (ext_env, bind_fun) = bind_pattern env pat (Lvar param) Immutable in
    (pat, bind_fun(transl_exp ext_env expr)) in
  List.map transl_case pat_expr_list

and transl_let env rec_flag pat_expr_list =
  match rec_flag with
    Nonrecursive ->
      let rec transl body_env = function
        [] ->
          (body_env, fun e -> e)
      | (pat, expr) :: rem ->
          let id = Ident.new "let" in
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
        let size =
          try size_of_lambda id lam
          with Unknown -> raise(Error(expr.exp_loc, Illegal_letrec_expr)) in
        (id, lam, size) in
      let decls =
        List.map transl_case pat_expr_list in
      (env, fun e -> Lletrec(decls, e))

(* Compile a primitive definition *)

let transl_primitive = function
    Not_prim -> fatal_error "Translcore.transl_primitive"
  | Primitive(name, arity) ->
      let prim =
        try
          let (gencomp, intcomp, floatcomp) =
            Hashtbl.find comparisons_table name in
          gencomp
        with Not_found ->
        try
          Hashtbl.find primitives_table name
        with Not_found ->
          Pccall(name, arity) in
      let rec add_params n params =
        if n >= arity
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
