open Why_ptree
open Asttypes
open Types
open Typedtree
open Path
open EscSyn
open Format

type error = 
  | Nonsimple_expression
  | Partial_function 
  | Not_lambda
  | Pattern_not_ident
  | Pattern_not_shallow
  | Argument_is_none
  | Expression_not_ident
  | Structure_not_convertable
  | Constant_not_convertable
  | Pattern_not_convertable
  | Expression_not_convertable
  | Type_not_convertable
  | Not_toplevel_function
  | Pattern_to_lexpr


exception Error of Location.t * error

(* make lexpr simper:
   (1) remove redundant existential quantifiers, 
   e.g. exists x. x = 5 and fml becomes fml [5/x]
   (2) true = true becomes true
   (3) true and fml becomes fml, false and fml becomes false
   (4) false or fml beceoms fml, true or fml becomes true
   (5) true -> fml becomes fml, false -> fml becomes true
*)

type fvalue = VBottom | VValue of lexpr

let rec simpl_formula eenv lexpr = 
  let simpl_e = match lexpr.pp_desc with
  | PPvar(s) -> begin try let v = Tbl.find s eenv in
                    begin match v with
                    | VBottom -> PPvar(s)
                    | VValue e -> e.pp_desc 
                    end
                with _ -> PPvar(s)
                end
  | PPapp(s,es) -> PPapp(s,List.map (simpl_formula eenv) es)
  | PPconst(c) -> PPconst(c)
  | PPinfix(e1, op, e2) -> 
      let simpl_e1 = simpl_formula eenv e1 in
      let simpl_e2 = simpl_formula eenv e2 in
      begin match (simpl_e1.pp_desc, op, simpl_e2.pp_desc) with
    | (PPconst(ConstTrue), PPeq, PPconst(ConstTrue)) -> PPconst(ConstTrue)
    | (PPconst(ConstTrue), PPeq, PPconst(ConstFalse)) -> PPconst(ConstFalse)
    | (PPconst(ConstFalse), PPeq, PPconst(ConstTrue)) -> PPconst(ConstFalse)
    | (PPconst(ConstFalse), PPeq, PPconst(ConstFalse)) -> PPconst(ConstTrue)
    | (PPconst(ConstFalse), PPand, _) -> PPconst(ConstFalse)
    | (_, PPand, PPconst(ConstFalse)) -> PPconst(ConstFalse)
    | (PPconst(ConstTrue), PPor, _) -> PPconst(ConstTrue)
    | (_, PPor, PPconst(ConstTrue)) -> PPconst(ConstTrue)
    | (PPconst(ConstTrue), PPand, _) -> simpl_e2.pp_desc
    | (_, PPand, PPconst(ConstTrue)) -> simpl_e1.pp_desc
    | (PPconst(ConstFalse), PPor, _) -> simpl_e2.pp_desc
    | (_, PPor, PPconst(ConstFalse)) -> simpl_e1.pp_desc  
    | _ -> PPinfix(simpl_e1, op, simpl_e2)  
    end
  | PPprefix(op, e) -> PPprefix(op, simpl_formula eenv e)
  | PPget(e1, e2) -> PPget(simpl_formula eenv e1, simpl_formula eenv e2)
  | PPset(e1,e2,e3) -> PPset(simpl_formula eenv e1, simpl_formula eenv e2, simpl_formula eenv e3)
  | PPextract(e1,e2,e3) -> 
      PPextract(simpl_formula eenv e1, simpl_formula eenv e2, simpl_formula eenv e3)
  | PPconcat(e1, e2) -> PPconcat(simpl_formula eenv e1, simpl_formula eenv e2)
  | PPif(e1,e2,e3) -> PPif(simpl_formula eenv e1, simpl_formula eenv e2, simpl_formula eenv e3)
  | PPforall(xs,ty,es,e) -> PPforall(xs,ty,es,simpl_formula eenv e)
  | PPexists(x,ty,e) -> begin match e.pp_desc with
   | PPinfix(e1, PPand, e2) -> begin match e1.pp_desc with
     | PPinfix(e3, PPeq, e4) -> begin match e3.pp_desc with
       | PPvar(x2) -> 
         if x = x2 then 
          let simpl_e = simpl_formula (Tbl.add x (VValue e4) eenv) e2 in
          simpl_e.pp_desc
         else 
          let new_env = Tbl.add x VBottom eenv in
          let simpl_e2 = simpl_formula new_env e2 in
          let e1simple2 = {pp_loc = e.pp_loc; pp_desc = PPinfix(e1,PPand,simpl_e2)} in
          PPexists(x,ty,e1simple2)
       | others -> 
          let new_env = Tbl.add x VBottom eenv in
          let simpl_e2 = simpl_formula new_env e2 in
          let e1simple2 = {pp_loc = e.pp_loc; pp_desc = PPinfix(e1,PPand,simpl_e2)} in
          PPexists(x,ty,e1simple2)
       end
     | others -> 
          let new_env = Tbl.add x VBottom eenv in
          let simpl_e = simpl_formula new_env e in
          PPexists(x,ty,simpl_e)
     end 
   | others -> 
          let new_env = Tbl.add x VBottom eenv in
          let simpl_e = simpl_formula new_env e in
          PPexists(x,ty,simpl_e)
     end 
  | PPnamed(s,e) -> PPnamed(s,simpl_formula eenv e)
  | PPlet(x,e1,e2) -> PPlet(x,simpl_formula eenv e1,simpl_formula eenv e2) 
  in {pp_loc = lexpr.pp_loc; pp_desc = simpl_e}


(* auxilary functions for constructing lexpr *)

let create_var s = Ident.unique_name (Ident.create s)
let create_vars s n = List.map Ident.unique_name (Ident.create_idents s n)

let mklexpr lc e = { pp_loc = lc; pp_desc = e }

let etrue       = PPconst ConstTrue
let efalse      = PPconst ConstFalse
let evoid       = PPconst ConstVoid
let enil        = PPvar "nil"
let eint  i     = PPconst (ConstInt i)
let ebitv b     = PPconst (ConstBitv (b))
let ereal n     = PPconst (ConstReal (n))

let f_and e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPand, e2))
let rec f_ands es       = match es with
                      | [] -> mklexpr Location.none etrue
                      | [e] -> e
                      | e::l -> f_and e (f_ands l)
let f_or e1 e2      = mklexpr e1.pp_loc (PPinfix (e1, PPor, e2))
let f_implies e1 e2 = mklexpr e1.pp_loc (PPinfix (e1, PPimplies, e2))
let f_iff e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPiff, e2))
let f_if e0 e1 e2   = mklexpr e1.pp_loc (PPif (e0, e1, e2))
let f_lt e1 e2      = mklexpr e1.pp_loc (PPinfix (e1, PPlt, e2))
let f_le e1 e2      = mklexpr e1.pp_loc (PPinfix (e1, PPle, e2))
let f_gt e1 e2      = mklexpr e1.pp_loc (PPinfix (e1, PPgt, e2))
let f_ge e1 e2      = mklexpr e1.pp_loc (PPinfix (e1, PPge, e2))
let f_eq e1 e2      = mklexpr e1.pp_loc (PPinfix (e1, PPeq, e2))
let f_neq e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPneq, e2))
let f_add e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPadd, e2))
let f_sub e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPsub, e2))
let f_mul e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPmul, e2))
let f_div e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPdiv, e2))
let f_mod e1 e2     = mklexpr e1.pp_loc (PPinfix (e1, PPmod, e2))
let f_neg e1        = mklexpr e1.pp_loc (PPprefix (PPneg, e1))
let f_not e1        = mklexpr e1.pp_loc (PPprefix (PPnot, e1))
let f_forall xs ty triggers e = mklexpr e.pp_loc (PPforall (xs, ty, triggers, e))
let f_exists x ty e = mklexpr e.pp_loc (PPexists (x, ty, e))
let f_var loc str_name = mklexpr loc (PPvar str_name)
let f_app loc str_name es = mklexpr loc (PPapp(str_name,es))

let rec is_expression_ergoble exp = if exp.exp_type = Predef.type_exn then false
else match exp.exp_desc with
| Texp_ident _ -> true
| Texp_constant _ -> true
| Texp_apply (e1, expopt_optl_list) -> 
   is_expression_ergoble e1 && 
   List.for_all (fun ei -> match ei with
   | (Some a, _) -> is_expression_argable a
   | _ -> false) expopt_optl_list
| Texp_let (rflag, pat_exp_list, e2) -> 
   List.for_all (fun (p,e) -> is_expression_ergoble e) pat_exp_list &&
   is_expression_ergoble e2
| Texp_function (pat_exp_list, ptial) ->
   List.for_all (fun (p,e) -> is_expression_ergoble e) pat_exp_list
| Texp_construct (_, _, es) -> 
   List.for_all is_expression_ergoble es
| Texp_tuple (expr_list) ->
        List.for_all is_expression_ergoble expr_list
| Texp_match (e0, pat_expr_list, ptial) -> 
    is_expression_ergoble e0 &&
    List.for_all (fun (p,e) -> is_expression_ergoble e) pat_expr_list
| Texp_array (expr_list) -> 
        List.for_all is_expression_ergoble expr_list
| Texp_ifthenelse (expr1, then_expr, exprop) -> 
        is_expression_ergoble expr1 && is_expression_ergoble then_expr
        && (match exprop with
            | Some e -> is_expression_ergoble e
            | None -> true) 
| _ -> false

let is_scrutinee_ergoble exp = match exp.exp_desc with
| Texp_ident _ -> false
| Texp_match _ -> false
| _ -> is_expression_ergoble exp


(* HOL, we introduce 
  type ('a, 'b) arrow 
  logic apply : ('a , 'b) arrow , 'a -> 'b *)
let rec type_to_ppure_type loc t = match (Ctype.repr t).desc with
  | Tvar -> PPTvarid ("'a", loc)
  | Tarrow (lbl, t1, t2, commu) -> 
      PPTexternal ([type_to_ppure_type loc t1;
                    type_to_ppure_type loc t2], "arrow", loc) 
  | Ttuple (ts) -> 
      PPTexternal (List.map (type_to_ppure_type loc) ts, "tuple", loc)
  | Tconstr (path, ts, abbre) ->
      if path = Predef.path_int then PPTint
	  else if path = Predef.path_bool then PPTbool
	      else if path = Predef.path_unit then PPTunit
	      else PPTexternal (List.map (type_to_ppure_type loc) ts,
                   Path.name path, loc)
  | _ -> PPTvarid ("type_to_ppure_type : not handled", loc)

(* first order type representation, for example:
   int -> int -> int becomes
   int , int -> int

 let rec type_to_ppure_type loc t = match (Ctype.repr t).desc with
  | Tvar -> PPTvarid ("'a", loc)
  | Ttuple (ts) -> 
      PPTexternal (List.map (type_to_ppure_type loc) ts, "tuple", loc)
  | Tconstr (path, ts, abbre) ->
      if path = Predef.path_int then PPTint
	  else if path = Predef.path_bool then PPTbool
	      else if path = Predef.path_unit then PPTunit
	      else PPTexternal (List.map (type_to_ppure_type loc) ts,
                   Path.name path, loc)
  | _ -> PPTvarid ("type_to_ppure_type : not handled", loc)

let rec split_type ty = 
   match (Ctype.repr ty).desc with
   | Tarrow (lbl, t1, t2, commu) -> 
       let (args, res) = split_type t2 in 
       (t1::args, res)
   | _ -> ([], ty)

let rec type_to_logic_type loc t = 
  let (args, res) = split_type t in
  if res = Predef.type_bool then
  PPredicate (List.map (type_to_ppure_type loc) args)
  else
  PFunction (List.map (type_to_ppure_type loc) args, 
		    type_to_ppure_type loc res)
*)

let rec type_to_logic_type loc t = 
 PFunction ([], type_to_ppure_type loc t)

(* val type_to_typelogic : loc -> Ident.t * Types.type_declaration -> decl list *)

let type_expr_to_string t = match t.desc with
| Tvar -> Ident.name (Ident.create "'a")
| _ -> "not Tvar"

let type_to_typelogic loc (id, tdecl) = 
  let type_name = Ident.name id in
  let type_params = List.map type_expr_to_string tdecl.type_params in
  let t = TypeDecl (loc, type_params, type_name) in
  let ppure_types = List.map (fun x -> PPTvarid (x, loc)) type_params in 
  let constrs = match tdecl.type_kind with
  | Type_abstract -> []
  | Type_variant s_ts_list -> 
     List.map (fun (s,ts) -> 
               Logic (loc, true, [s], 
               PFunction (List.map (type_to_ppure_type loc) ts, 
               PPTexternal (ppure_types, type_name, loc)))) s_ts_list
  | _ -> raise(Error(loc, Type_not_convertable))
  in t::constrs


(* val expression_to_lexpr : Typedtree.expression 
                         -> Why_ptree.lexpr -> Why_ptree.lexpr *)

let constant_to_lconstant c = match c with
  | Const_int i -> eint (string_of_int i)
  | Const_float s -> ereal (Num.num_of_string s)
  | Const_char ch -> ebitv (String.make 1 ch)
  | Const_string s -> ebitv s
  | Const_int32 i -> eint (Int32.to_string i)
  | Const_int64 i -> eint (Int64.to_string i)
  | Const_nativeint i -> eint (Int64.to_string (Int64.of_nativeint i))

exception Not_primitive

let is_expression_ident e = match e.exp_desc with
| Texp_ident _ -> true
| _ -> false

let expressionVar_to_string e = match e.exp_desc with
  | Texp_ident (path, vd) -> Path.unique_name path
  | _ -> raise(Error(e.exp_loc, Expression_not_ident))

let patternVar_to_string p = match p.pat_desc with
  | Tpat_var (id) -> Ident.unique_name id
  | _ -> raise(Error(p.pat_loc, Pattern_not_ident))

let pattern_to_ppure_type p = type_to_ppure_type p.pat_loc (p.pat_type)

let rec pattern_to_lexpr p = 
  let loc = p.pat_loc in
  match p.pat_desc with
  | Tpat_var (id) -> mklexpr loc (PPvar (Ident.unique_name id))
  | Tpat_constant (c) -> mklexpr loc (constant_to_lconstant c)
  | Tpat_construct (path, cdesc, ps) -> 
  let loc = p.pat_loc in
     begin match cdesc.cstr_tag with
     | Cstr_constant(i) -> 
	 if i = 1 then begin if path = Predef.path_bool
                         then mklexpr loc etrue 
                         else mklexpr loc (PPvar (Path.unique_name path))
                       end
         else begin if path = Predef.path_list 
              then mklexpr loc enil (* PPapp ("nil", List.map pattern_to_lexpr ps) *) 
              else mklexpr loc efalse 
              end
     | Cstr_block(i) -> 
	 if path = Predef.path_list 
         then mklexpr loc (PPapp ("cons", List.map pattern_to_lexpr ps))
         else mklexpr loc (PPapp (Path.unique_name path, List.map pattern_to_lexpr ps))
     | Cstr_exception(p) ->
	 mklexpr loc (PPapp ("exception"^(Path.unique_name p), 
			     List.map pattern_to_lexpr ps))
    end
   | _ -> raise(Error(p.pat_loc, Pattern_to_lexpr))

let rec flat_app exp = match exp.exp_desc with
| Texp_apply (e1, es1) -> 
    let (f, args) = flat_app e1 in
    (f, args@es1) 
| _ -> (exp, []) 

    

(* This is the operator [[e]]_f where f denotes e and 
   f can be (apply x y) or (K x) *)

let rec expression_to_eqlexpr (f: lexpr) exp = 
 match (exp.exp_type).desc with
| Tconstr (path, t, abbr) when cmpPath_byname path Predef.path_exn = 0 -> 
    (mklexpr exp.exp_loc efalse)
| _ -> begin
 match exp.exp_desc with
  | Texp_ident (path, vd) -> 
      f_eq f (mklexpr exp.exp_loc (PPvar (Path.unique_name path))) 
  | Texp_constant (c) -> 
      f_eq f (mklexpr exp.exp_loc (constant_to_lconstant c))
    (*  | Texp_let (rflag, pat_expr_list, e2) -> 
    List.fold_right (fun (p, e) default -> 
          f_and (f_exists (patternVar_to_string p) 
                          (pattern_to_ppure_type p) 
                          (expression_to_eqlexpr (pattern_to_lexpr p) e)) 
                default) 
    pat_expr_list
    (expression_to_eqlexpr f e2) *)
  | Texp_let (rflag, pat_exp_list, e2) -> 
     (* [[ let x:ty = e1 in e2 ]]_f = exists x:ty. [[ e1 ]]_x /\ [[ e2 ]]_f *)
      List.fold_right (fun (p, e1) default -> 
	let x = patternVar_to_string p in
	let x1 = mklexpr p.pat_loc (PPvar x) in 
	f_exists x (type_to_ppure_type p.pat_loc p.pat_type) 
	  (f_and (expression_to_eqlexpr x1 e1) default))
	pat_exp_list (expression_to_eqlexpr f e2)
  | Texp_function (pat_exp_list, ptl) ->
     (* pre-processing may make it one element in the list *) 
     (* [[ \x:ty.e ]]_f = forall x:ty. [[ e ]]_(apply f x) *)
      f_ands (List.map (fun (p,e) -> 
      let x = patternVar_to_string p in
      let xvar = mklexpr p.pat_loc (PPvar x) in
      let apply_f_x = mklexpr e.exp_loc (PPapp("apply",[f;xvar])) in 
      f_forall [x] (type_to_ppure_type p.pat_loc p.pat_type) []
                          (expression_to_eqlexpr apply_f_x e))
      pat_exp_list)
  | Texp_apply (e0, expop_optl_list) ->  
      let (g, args) = flat_app exp in
      let l0     = g.exp_loc in 
      begin try primitive_to_eqlexpr f g args
      with Not_primitive -> 
      (* [[ e1 e2 ]]_f = exists x1. exists x2. 
                           ([[ e1 ]]_x1 /\ [[ e2 ]]_x2) =>
	                   f = apply x1 x2 
       if e2 is a function, we generate
	 exists x1. forall x2. 
         ([[ e1 ]]_x1 /\ [[ e2 ]]_x2) => f = apply x1 x2 
       Here, although e1 is function, it is a variable. So we can use exists
      *)     
      let rec form_app acc xs = match xs with
      | [] -> (acc, [])
      | (Some a, _)::l -> 
	  if is_expression_argable a 
          then 
 	   let lexpr_a = expression_to_lexpr a in
	   let app = mklexpr l0 (PPapp ("apply", [acc;lexpr_a])) in
	   form_app app l 
          else 
	    let a1 = create_var "a" in
	    let a1var = f_var a.exp_loc a1 in
            let app = mklexpr l0 (PPapp ("apply", [acc;a1var])) in
	    let (res, ls) =  form_app app l in
            (res, (a1, a)::ls)
      | (None, _)::l -> raise(Error(exp.exp_loc, Argument_is_none)) 
      in
      if is_expression_argable e0
       then let x0 = expression_to_lexpr e0 in
            let (app_res, ls) = form_app x0 expop_optl_list in
            List.fold_right (fun (x,e) acc -> 
     	    let xvar = f_var e.exp_loc x in
   	    f_and (f_exists x (type_to_ppure_type e.exp_loc e.exp_type) 
		          (expression_to_eqlexpr xvar e))
            acc
            ) ls (f_eq f app_res)
       else let g1 = create_var "g" in
            let g1var = f_var e0.exp_loc g1 in
	    let (app_res, ls) = form_app g1var expop_optl_list in
            List.fold_right (fun (x,e) acc -> 
     	    let xvar = f_var e.exp_loc x in
	    f_and (f_exists x (type_to_ppure_type e.exp_loc e.exp_type) 
		          (expression_to_eqlexpr xvar e))
            acc) ((g1,e0)::ls) (f_eq f app_res)
      end
      (*
      let apply_exp = List.fold_left (fun acc e -> 
           match e with
           | (Some a, _) -> 
	       let lexpr_a = expression_to_lexpr a in
	       mklexpr l0 (PPapp ("apply", [acc;lexpr_a]))
           | (None, _) -> raise(Error(exp.exp_loc, Argument_is_none))           
          ) x0 expop_optl_list in
      f_eq f apply_exp
      end
      
      if is_expression_ident g
      then let x1     = expressionVar_to_string g in
           let x2s    = create_vars "x" (List.length args) in
           let x2e2s  = List.combine x2s args in
           let x2vars = List.map (fun y -> match y with
                     | (x, (Some a, _)) -> f_var a.exp_loc x
                     | _ -> raise(Error(exp.exp_loc, Argument_is_none))) x2e2s in
           List.fold_right (fun (x2, a) default -> match a with 
	   | (Some e, _) -> 
	   let x2var = f_var x2 in 
           f_implies (f_forall [x2] (type_to_ppure_type e.exp_loc e.exp_type) []
                              (expression_to_eqlexpr x2var e)) default
           | _ -> raise(Error(exp.exp_loc, Argument_is_none))) 
           x2e2s (f_eq f (mklexpr l0 (PPapp (x1,x2vars))))
      else
	let x1     = create_var "x" in
        let x1var  = f_var l0 x1 in
        let e1     = f_forall [x1] (type_to_ppure_type g.exp_loc g.exp_type) []
                                 (expression_to_eqlexpr x1var g) in
        let x2s    = create_vars "x" (List.length args) in
        let x2e2s  = List.combine x2s args in
        let x2vars = List.map (fun y -> match y with
                     | (x, (Some a, _)) -> f_var a.exp_loc x
                     | _ -> raise(Error(exp.exp_loc, Argument_is_none))) x2e2s in
        f_and e1 (
        List.fold_right (fun (x2, a) default -> match a with 
	   | (Some e, _) -> 
	   let x2var = f_var e.exp_loc x2 in 
           f_implies (f_forall [x2] (type_to_ppure_type e.exp_loc e.exp_type) []
                              (expression_to_eqlexpr x2var e)) default
           | _ -> raise(Error(exp.exp_loc, Argument_is_none))) 
         x2e2s (f_eq f (mklexpr l0 (PPapp (x1,x2vars)))))
      end
    *)
  | Texp_match (e0, pat_exp_list, ptl) -> 
      (* pre-processing may make it one element in the list *)
     (* [[ match e0 with Ki xi -> ei ]]_f = 
            \exists x0. [[ e0 ]]_x0 /\
             /\_i=1..n. forall xi. x0 = Ki xi => [[ ei ]]_f *)
      if is_expression_prop e0 
      then begin
      f_ands (List.map (fun (p,e) -> match is_pattern_true p with
      | Ptrue -> f_implies (expression_to_lexpr e0) (expression_to_eqlexpr f e)
      | Pfalse -> f_implies (f_not (expression_to_lexpr e0)) 
                            (expression_to_eqlexpr f e)
      | Pothers -> raise(Error(p.pat_loc, Pattern_not_convertable))) pat_exp_list)
      end
      else 
      (f_ands (List.map (fun (p,e) -> match p.pat_desc with
          | Tpat_var (id) -> 
            let xi = Ident.unique_name id in
            f_forall [xi] (type_to_ppure_type p.pat_loc p.pat_type) [] 
                         (f_implies (f_eq (expression_to_lexpr e0) 
				          (pattern_to_lexpr p))
                                    (expression_to_eqlexpr f e))
          | Tpat_constant (c) -> 
            f_implies (f_eq (expression_to_lexpr e0) 
                            (mklexpr p.pat_loc (constant_to_lconstant c)))
                      (expression_to_eqlexpr f e)
          | Tpat_construct(path, cdesc, ps) -> 
            List.fold_right (fun p default -> match p.pat_desc with
            | Tpat_var (id) -> let x = Ident.unique_name id in
            f_forall [x] (type_to_ppure_type p.pat_loc p.pat_type) [] 
                       default
            | Tpat_constant (c) -> default
            | _ -> raise(Error(p.pat_loc, Pattern_not_shallow)))
            ps (f_implies (f_eq (expression_to_lexpr e0) (pattern_to_lexpr p))
                                   (expression_to_eqlexpr f e))
          | _ -> raise(Error(p.pat_loc, Pattern_not_shallow))           
           ) pat_exp_list))
  | Texp_construct (path, cdesc, es) ->  
  (* [[ K (e1,...,en) ]]_f = \forall x1...xn. ([[ e1 ]]_x1 /\...
                        [[ en ]]_xn) => f = K (x1,..., xn) *)
     let loc = exp.exp_loc in
     begin match cdesc.cstr_tag with
     | Cstr_constant(i) -> 
	 if i = 1 then begin if path = Predef.path_bool
                         then f_eq f (mklexpr loc etrue)
                         else f_eq f (f_var loc (Path.unique_name path))
                       end
         else begin if path = Predef.path_list 
              then f_eq f (mklexpr loc enil)
              else f_eq f (mklexpr loc efalse)
              end
     | Cstr_block(i) -> 
	let constructor_name = if path = Predef.path_list 
                                then "cons"
                                else Path.unique_name path
        in       
       let xs = create_vars "x" (List.length es) in
       let x_e = List.combine xs es in
       let xs_lexpr = List.map (fun (x,e) -> f_var e.exp_loc x) x_e in
       let args = List.map (fun (xi, ei) -> 
	            f_exists xi (type_to_ppure_type ei.exp_loc ei.exp_type)
                    (expression_to_eqlexpr (f_var ei.exp_loc xi) ei)) 
                    x_e               in
       let last = f_eq f (mklexpr loc (PPapp (constructor_name, xs_lexpr))) in
       List.fold_right (fun fml default -> f_and fml default) args last
     | Cstr_exception(p) ->
	mklexpr loc efalse
	(* mklexpr loc (PPapp ("exception"^(Path.unique_name p), 
			     List.map (expression_to_eqlexpr f) es)) *)
    end 
  | Texp_tuple (es) ->  
       f_eq f (f_app exp.exp_loc "tup" (List.map expression_to_lexpr es))
  | Texp_ifthenelse (e0, e1, e2op) -> 
     (* [[ if e0 then e1 else e2 ]]_f = 
        \exists x0. [[ e0 ]]_x0 /\ (x0 = true -> [[ e1 ]]_f)
                                /\ (x0 = false -> [[ e2 ]]_f) *)    
     if EscSyn.is_expression_prop e0 
     then begin
       (* print_string "prop eqexp:"; print_expression e0; *)
       (f_and (expression_to_lexpr e0) (expression_to_eqlexpr f e1))
       end
     else begin
       (* print_string "not prop eqexp:"; print_expression e0; *)
     let x0 = create_var "x" in
     let x0var = f_var e0.exp_loc x0 in        
      begin match e2op with
      | None -> f_exists x0 PPTbool
        (f_and (expression_to_eqlexpr (mklexpr e0.exp_loc etrue) e0)
               (expression_to_eqlexpr f e1))
      | Some e2 -> 
	  match e2.exp_desc with
	  | Texp_unr _ | Texp_bad _ ->        
		(f_and (expression_to_eqlexpr (mklexpr e0.exp_loc etrue) e0)
		   (expression_to_eqlexpr f e1))
	  | _ ->  f_exists x0 PPTbool
          (f_and (expression_to_eqlexpr x0var e0)
            (f_and (f_implies (f_eq x0var (mklexpr e1.exp_loc etrue)) 
                              (expression_to_eqlexpr f e1))
                   (f_implies (f_eq x0var (mklexpr e2.exp_loc efalse))
                              (expression_to_eqlexpr f e2))))
      end
     end
  | Texp_unr _ -> mklexpr exp.exp_loc efalse
  | Texp_bad _ -> mklexpr exp.exp_loc efalse
  | _ -> print_string "Expression not convertable: "; 
         print_expression exp;
         raise(Error(exp.exp_loc, Expression_not_convertable))
 end

(* [[ e ]] = logical formula *)
and expression_to_lexpr exp =  match exp.exp_desc with
  | Texp_ident (path, vd) -> 
      f_var exp.exp_loc (Path.unique_name path)
  | Texp_constant (c) -> 
      mklexpr exp.exp_loc (constant_to_lconstant c)
  | Texp_let (rflag, pat_expr_list, e2) -> 
    List.fold_right (fun (p, e) default -> 
          f_and (f_exists (patternVar_to_string p) 
                          (pattern_to_ppure_type p) 
                          (expression_to_eqlexpr (pattern_to_lexpr p) e)) 
                default) 
    pat_expr_list
    (expression_to_lexpr e2)          
  | Texp_function (pat_exp_list, ptl) ->
     (* pre-processing may make it one element in the list *) 
     (* [[ \x:ty.e ]] = forall x:ty. [[ e ]] *)
      f_ands (List.map (fun (p,e) -> 
      let x = patternVar_to_string p in
      f_forall [x] (type_to_ppure_type p.pat_loc p.pat_type) []
                          (expression_to_lexpr e))
      pat_exp_list)
  | Texp_apply (e0, expop_optl_list) ->  
      (* if we only call first order theorem prover, we need to flat application *)
      let (g, args) = flat_app exp in
      let l0     = g.exp_loc in
      begin try primitive_to_lexpr g args
      with Not_primitive -> 
      let rec form_app acc xs = match xs with
      | [] -> (acc, [])
      | (Some a, _)::l -> 
	  if is_expression_argable a 
          then 
 	   let lexpr_a = expression_to_lexpr a in
	   let app = mklexpr l0 (PPapp ("apply", [acc;lexpr_a])) in
	   form_app app l 
          else 
	    let a1 = create_var "a" in
	    let a1var = f_var a.exp_loc a1 in
            let app = mklexpr l0 (PPapp ("apply", [acc;a1var])) in
	    let (res, ls) =  form_app app l in
            (res, (a1, a)::ls)
      | (None, _)::l -> raise(Error(exp.exp_loc, Argument_is_none)) 
      in
      if is_expression_argable e0
       then let x0 = expression_to_lexpr e0 in
            let (app_res, ls) = form_app x0 expop_optl_list in
            List.fold_right (fun (x,e) acc -> 
     	    let xvar = f_var e.exp_loc x in
   	    f_and (f_exists x (type_to_ppure_type e.exp_loc e.exp_type)
		          (expression_to_eqlexpr xvar e))
            acc) ls app_res
       else let g1 = create_var "g" in
            let g1var = f_var e0.exp_loc g1 in
	    let (app_res, ls) = form_app g1var expop_optl_list in
            List.fold_right (fun (x,e) acc -> 
     	    let xvar = f_var e.exp_loc x in
   	    f_and (f_exists x (type_to_ppure_type e.exp_loc e.exp_type)
		          (expression_to_eqlexpr xvar e))
            acc) ((g1,e0)::ls) app_res
      end
      (*
      let apply_exp = List.fold_left (fun acc e -> 
	    match e with
           | (Some a, _) -> 
	       let lexpr_a = expression_to_lexpr a in
               mklexpr l0 (PPapp ("apply", [acc;lexpr_a]))
           | (None, _) -> raise(Error(exp.exp_loc, Argument_is_none)) 
       ) x0 expop_optl_list in
      apply_exp
      end

      if is_expression_ident g
      then let x1     = expressionVar_to_string g in
           let x2s    = create_vars "x" (List.length args) in
           let x2e2s  = List.combine x2s args in
           let x2vars = List.map (fun y -> match y with
                     | (x, (Some a, _)) -> f_var a.exp_loc x
                     | _ -> raise(Error(exp.exp_loc, Argument_is_none))) x2e2s in
           List.fold_right (fun (x2, a) default -> match a with 
	   | (Some e, _) -> 
	   let x2var = f_var e.exp_loc x2 in 
           f_implies (f_forall [x2] (type_to_ppure_type e.exp_loc e.exp_type) []
                              (expression_to_eqlexpr x2var e)) default
           | _ -> raise(Error(exp.exp_loc, Argument_is_none))) 
           x2e2s (mklexpr l0 (PPapp (x1,x2vars)))
      else
        (* [[ e1 e2 ]] = apply [[ e1 ]] [[ e2 ]] *)
	let x1     = create_var "x" in
        let x1var  = f_var l0 x1 in
        let e1     = f_forall [x1] (type_to_ppure_type g.exp_loc g.exp_type) []
                                 (expression_to_eqlexpr x1var g) in
        let x2s    = create_vars "x" (List.length args) in
        let x2e2s  = List.combine x2s args in
        let x2vars = List.map (fun y -> match y with
                     | (x, (Some a, _)) -> f_var a.exp_loc x
                     | _ -> raise(Error(exp.exp_loc, Argument_is_none))) x2e2s in
        f_and e1 (
        List.fold_right (fun (x2, a) default -> match a with 
	   | (Some e, _) -> 
	   let x2var = f_var e.exp_loc x2 in 
           f_implies (f_forall [x2] (type_to_ppure_type e.exp_loc e.exp_type) []
                              (expression_to_eqlexpr x2var e)) default
           | _ -> raise(Error(exp.exp_loc, Argument_is_none))) 
         x2e2s (mklexpr l0 (PPapp (x1,x2vars))))
      end
     *)
  | Texp_match (e0, pat_exp_list, ptl) -> 
     (* pre-processing may make it one element in the list *)
     (* [[ match e0 with Ki xi -> ei ]]_f = 
            \exists x0. [[ e0 ]]_x0 /\
             /\_i=1..n. forall xi. x0 = Ki xi => [[ ei ]]_f 
	For converting match-expressions to axioms, Alt-ergo does not handle 
	the above formula well due to instance mapping. 
	We have to convert Ki xi to triggers:
	\exists x0 [Ki xi]. [[ e0 ]]_x0 /\ [[ ei ]]_f 
      As we do expression slicing, there is only one branch when being 
	converted to an axiom.
      *)
      if is_expression_prop e0 
      then begin
      f_ands (List.map (fun (p,e) -> match is_pattern_true p with
      | Ptrue -> f_implies (expression_to_lexpr e0) (expression_to_lexpr e)
      | Pfalse -> f_implies (f_not (expression_to_lexpr e0)) 
                            (expression_to_lexpr e)
      | Pothers -> raise(Error(p.pat_loc, Pattern_not_convertable))) pat_exp_list)
      end
      else 
      (f_ands (List.map (fun (p,e) -> match p.pat_desc with
          | Tpat_var (id) -> 
            let xi = Ident.unique_name id in
            f_forall [xi] (type_to_ppure_type p.pat_loc p.pat_type) [] 
                         (f_implies (f_eq (expression_to_lexpr e0) 
				          (pattern_to_lexpr p))
                                    (expression_to_lexpr e))
          | Tpat_constant (c) -> 
            f_implies (f_eq (expression_to_lexpr e0) 
                            (mklexpr p.pat_loc (constant_to_lconstant c)))
                      (expression_to_lexpr e)
          | Tpat_construct(path, cdesc, ps) -> 
            List.fold_right (fun p default -> match p.pat_desc with
            | Tpat_var (id) -> let x = Ident.unique_name id in
            f_forall [x] (type_to_ppure_type p.pat_loc p.pat_type) [] 
                       default
            | Tpat_constant (c) -> default
            | _ -> raise(Error(p.pat_loc, Pattern_not_shallow)))
            ps (f_implies (f_eq (expression_to_lexpr e0) (pattern_to_lexpr p))
                                   (expression_to_lexpr e))
          | _ -> raise(Error(p.pat_loc, Pattern_not_shallow))
           ) pat_exp_list))
  | Texp_construct (path, cdesc, es) ->  
  (* [[ K (e1,...,en) ]] = \forall x1...xn [[ e1 ]] /\ ... /\ [[ en ]] 
                                => K (x1,..., xn) *)
     let loc = exp.exp_loc in
     begin match cdesc.cstr_tag with
     | Cstr_constant(i) -> 
	 if i = 1 then begin if path = Predef.path_bool
                         then mklexpr loc etrue 
                         else f_var loc (Path.unique_name path)
                       end
         else begin if path = Predef.path_list 
              then mklexpr loc enil 
              else mklexpr loc efalse 
              end
     | Cstr_block(i) -> 
	let constructor_name = if path = Predef.path_list 
                                then "cons"
                                else Path.unique_name path
        in
        (*
        let xs = create_vars "x" (List.length es) in
        let xes = List.combine xs es in
        let xsvars = List.map (fun (x,e) -> f_var e.exp_loc x) xes in
        List.fold_right (fun (x,e) default -> 
         f_forall [x] (type_to_ppure_type e.exp_loc e.exp_type) [] default)
         xes
         (f_implies (f_ands (List.map (fun (x, e) -> 
                   expression_to_lexpr e) xes))
         (mklexpr loc (PPapp (constructor_name, xsvars))))
        *)
       let xs = List.map expression_to_lexpr es in
       mklexpr loc (PPapp (constructor_name, xs))
     | Cstr_exception(p) ->
	 mklexpr loc (PPapp ("exception"^(Path.unique_name p), 
			     List.map expression_to_lexpr es))
    end 
  | Texp_tuple (es) -> f_app exp.exp_loc "tup" (List.map expression_to_lexpr es)
  | Texp_ifthenelse (e0, e1, e2op) -> 
     (* [[ if e0 then e1 else e2 ]]_f = 
        \forall x0. [[ e0 ]]_x0 -> (x0 = true -> [[ e1 ]]_f
                                /\ x0 = false -> [[ e2 ]]_f) *)    
      print_string "e0 exp:"; print_expression e0;
      begin match e2op with
      | None -> (f_implies (expression_to_lexpr e0)
                           (expression_to_lexpr e1))
      | Some e2 ->
        (f_and (f_implies (expression_to_lexpr e0)
                          (expression_to_lexpr e1))
               (f_implies (f_not (expression_to_lexpr e0))
                          (expression_to_lexpr e2)))
      end
  | Texp_unr _ -> mklexpr exp.exp_loc etrue
  | Texp_bad _ -> mklexpr exp.exp_loc efalse
  | _ -> print_string "Expression not convertable 2: "; 
         print_expression exp;
         raise(Error(exp.exp_loc, Expression_not_convertable))


and primitive_to_eqlexpr f e0 expop_optl_list = 
let l0 = e0.exp_loc in 
match e0.exp_desc with
| Texp_ident(path, vd) -> begin match getop path with
    | "&&" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let f_true = mklexpr e0.exp_loc etrue in
	  let e1 = expression_to_eqlexpr f_true x in
	  let e2 = expression_to_eqlexpr f_true y in
 	  (f_iff (f_and e1 e2) (f_eq f f_true))
      | _ -> raise Not_primitive
    end
    | "||" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_eqlexpr f x in
	  let e2 = expression_to_eqlexpr f y in
 	   (f_or e1 e2)
      | _ -> raise Not_primitive
    end
    | ">" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
	  f_iff (f_gt e1 e2)
                (f_eq f (mklexpr l0 etrue)) 
      | _ -> raise Not_primitive
    end
    | ">=" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
	  f_iff (f_ge e1 e2)
                (f_eq f (mklexpr l0 etrue))            
      | _ -> raise Not_primitive
    end
   | "<" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
	  f_iff (f_lt e1 e2)
                (f_eq f (mklexpr l0 etrue)) 
      | _ -> raise Not_primitive
    end
   | "<=" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_iff (f_le e1 e2)
                (f_eq f (mklexpr l0 etrue)) 
      | _ -> raise Not_primitive
    end
   | "=" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  if is_expression_argable x 
	     then if is_expression_argable y 
	          then let e1 = expression_to_lexpr x in
		       let e2 = expression_to_lexpr y in
            	       (f_eq e1 e2)
                  else let e1 = expression_to_lexpr x in
		       let yloc = y.exp_loc in
		       let x2 = create_var "x" in
		       let t2 = type_to_ppure_type yloc y.exp_type in
		       let yvar = f_var yloc x2 in
		       let e2 = expression_to_eqlexpr e1 y in
            	       f_exists x2 t2 (f_and e2 (f_eq e1 yvar))
             else let xloc = x.exp_loc in
	          let yloc = y.exp_loc in
	          let x1 = create_var "x" in
	          let t1 = type_to_ppure_type xloc  x.exp_type in
		  let x2 = create_var "x" in
	          let t2 = type_to_ppure_type y.exp_loc y.exp_type in
		  let xvar = f_var xloc x1 in
		  let yvar = f_var yloc x2 in
		  let e1 = expression_to_eqlexpr xvar x in
		  let e2 = expression_to_eqlexpr yvar y in
		  f_exists x1 t1 (f_exists x2 t2 (f_and (f_and e1 e2) (f_eq xvar yvar)))
	(*  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_iff (f_eq e1 e2)
                (f_eq f (mklexpr l0 etrue)) *)
       | _ -> raise Not_primitive
    end
   | "<>" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
         
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_eq f (f_neq e1 e2)
      | _ -> raise Not_primitive
    end
    | "+" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_eq f (f_add e1 e2)
      | _ -> raise Not_primitive
    end 
    | "-" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_eq f (f_sub e1 e2)
      | _ -> raise Not_primitive
    end 
    | "*" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_eq f (f_mul e1 e2)
      | _ -> raise Not_primitive
    end 
    | "/" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_eq f (f_div e1 e2)
      | _ -> raise Not_primitive
    end 
    | "mod" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	  f_eq f (f_mod e1 e2)
      | _ -> raise Not_primitive
    end 
    | "not" -> begin match expop_optl_list with 
      | (Some x, _)::l -> 
	  let e1 = expression_to_lexpr x in
 	  f_eq f (f_not e1)
      | _ -> raise Not_primitive
    end 
    | _ -> raise Not_primitive
    end
| _ -> raise Not_primitive

and primitive_to_lexpr e0 expop_optl_list = match e0.exp_desc with
| Texp_ident(path, vd) -> begin match getop path with
    | "&&" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_and e1 e2)
      | _ -> raise Not_primitive
    end
    | "||" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_or e1 e2)
      | _ -> raise Not_primitive
    end
    | ">" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
	  (f_gt e1 e2)
      | _ -> raise Not_primitive
    end
    | ">=" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_ge e1 e2)
      | _ -> raise Not_primitive
    end
   | "<" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_lt e1 e2)
      | _ -> raise Not_primitive
    end
   | "<=" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_le e1 e2)
      | _ -> raise Not_primitive
    end
   | "=" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
         if is_expression_argable x 
	     then if is_expression_argable y 
	          then let e1 = expression_to_lexpr x in
		       let e2 = expression_to_lexpr y in
            	       (f_eq e1 e2)
                  else let e1 = expression_to_lexpr x in
		       let yloc = y.exp_loc in
		       let x2 = create_var "x" in
		       let t2 = type_to_ppure_type yloc y.exp_type in
		       let yvar = f_var yloc x2 in
		       let e2 = expression_to_eqlexpr yvar y in
            	       f_exists x2 t2 (f_and e2 (f_eq e1 yvar))
             else let xloc = x.exp_loc in
	          let yloc = y.exp_loc in
	          let x1 = create_var "x" in
	          let t1 = type_to_ppure_type xloc  x.exp_type in
		  let x2 = create_var "x" in
	          let t2 = type_to_ppure_type y.exp_loc y.exp_type in
		  let xvar = f_var xloc x1 in
		  let yvar = f_var yloc x2 in
		  let e1 = expression_to_eqlexpr xvar x in
		  let e2 = expression_to_eqlexpr yvar y in
		  f_exists x1 t1 (f_exists x2 t2 (f_and (f_and e1 e2) (f_eq xvar yvar)))
      | _ -> raise Not_primitive
    end
   | "<>" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
         
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_neq e1 e2)
      | _ -> raise Not_primitive
    end
    | "+" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_add e1 e2)
      | _ -> raise Not_primitive
    end 
    | "-" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_sub e1 e2)
      | _ -> raise Not_primitive
    end 
    | "*" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_mul e1 e2)
      | _ -> raise Not_primitive
    end 
    | "/" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_div e1 e2)
      | _ -> raise Not_primitive
    end 
    | "mod" -> begin match expop_optl_list with 
      | (Some x, _)::(Some y, _)::l -> 
	  let e1 = expression_to_lexpr x in
	  let e2 = expression_to_lexpr y in
 	   (f_mod e1 e2)
      | _ -> raise Not_primitive
    end 
 (*   | "not" -> begin match expop_optl_list with 
      | (Some x, _)::l -> 
	  let e1 = expression_to_lexpr x in
 	   (f_not e1)
      | _ -> raise Not_primitive
    end *)
    | _ -> raise Not_primitive
end
| _ -> raise Not_primitive

(* val def_to_axioms : structure -> decl list *)

let rec getArgsRes e = 
   let ty = e.exp_type in
   match ((Ctype.repr ty).desc, e.exp_desc) with
   | (Tarrow (lbl, t1, t2, commu), Texp_function (p_e_list, Total)) -> 
      begin 
      match p_e_list with
      | [(p,e)] -> let (args, res) = getArgsRes e in 
                   begin match p.pat_desc with
                   | Tpat_var (id) -> ((id, p.pat_type)::args, res)
                   | _ -> raise(Error(e.exp_loc, Not_lambda))
                   end
      | _ -> raise(Error(e.exp_loc, Not_lambda))
      end
   | _ -> ([], e)

(* remove bad, unr branches because we want to convert whatever is true to axioms.
This is ad hoc at the moment. Will polish it later. *)

let rec remove_bad_unr exp = 
  let f e = match e.exp_desc with
  | Texp_ifthenelse (e0, e1, e2opt) -> 
      begin match e2opt with
      | Some e2 -> begin match e2.exp_desc with
 	| Texp_unr _ -> {e with exp_desc = Texp_ifthenelse (e0, e1, None)}
	| _ -> e
      end
      | None -> e
      end
  | Texp_match (e0, pat_exp_list, ptial) -> 
      let filtered_list = List.filter (fun (p,e) -> match e.exp_desc with
      | Texp_bad _ -> false
      | Texp_apply (e1, _) -> 
	  begin match e1.exp_desc with
	  | Texp_bad _ -> false
	  | _ -> true
	  end
      | _ -> true) pat_exp_list in
      { e with exp_desc = Texp_match (e0, filtered_list, ptial)}
  | _ -> e
  in map_expression f exp

 
(* We want to make each branch an axiom, so we split an expression into 
   a list of expressions. 
   val split_expression : expression -> expression list *)

let mkpat loc desc ty en = { pat_desc = desc; pat_loc = loc; 
                          pat_type = ty; pat_env = en}

let rec slice_expression exp =  match exp.exp_desc with
 | Texp_ident (path, vd) -> [exp]
 | Texp_constant (c) -> [exp] 
 | Texp_let (rflag, pat_exp_list, expr) ->
     let exps = slice_expression expr in 
     List.map (fun e2 -> List.fold_right (fun (p, e) body -> 
         (* let rhss = slice_expression e in
         let p_e_list = List.map (fun e -> (p,e)) rhss in *)
         {exp with exp_desc = Texp_let (rflag, [(p,e)], body)})
     pat_exp_list e2) exps
  | Texp_function (pat_exp_list, ptl) ->
     List.flatten (List.map (fun (p,e) -> 
         let rhss = slice_expression e in
         List.map (fun e -> 
         { exp with exp_desc = Texp_function ([(p,e)], ptl)}) 
            rhss) pat_exp_list)
  | Texp_apply (e0, expop_optl_list) -> 
      [exp]
  | Texp_match (e0, pat_exp_list, ptl) -> 
      (* slice_match filter out non-exception branches *)
      let rec slice_match xs = match xs with
      | [] -> []
      | (p,e)::l -> 
         begin match e.exp_desc with
         | Texp_bad _ -> slice_match l
         | Texp_unr _ -> slice_match l
         | _ -> 
	     let xs = slice_expression e in
             let sub_branches = List.map (fun ei -> 
	       {exp with exp_desc = 
		Texp_match (e0, [(p,ei)], ptl)}) xs in
             sub_branches @ (slice_match l)
	 end
      in slice_match pat_exp_list
  | Texp_tuple (es) ->  [exp]
  | Texp_construct (path, constr_desc, es) -> [exp]
  | Texp_array (es) -> [exp]
  | Texp_ifthenelse (e0, e1, e2op) -> 
      let l1 = if e1.exp_type = Predef.type_exn then []
               else let true_desc = {cstr_res = Predef.type_bool; 
                       cstr_args = [];
		       cstr_arity = 0;
		       cstr_tag = Cstr_constant 1;
		       cstr_consts = 2;
		       cstr_nonconsts = 0;
		       cstr_private = Public} in
	       let constr_true = mkpat e0.exp_loc (Tpat_construct (Predef.path_bool, true_desc, [])) e0.exp_type e0.exp_env in
	       [{exp with exp_desc = Texp_match (e0, [(constr_true, e1)], Total)}] in
      let l2 = match e2op with 
      | None -> []
      | Some e2 -> if e2.exp_type = Predef.type_exn then []
                   else let false_desc = {cstr_res = Predef.type_bool; 
					  cstr_args = [];
					  cstr_arity = 0;
					  cstr_tag = Cstr_constant 0;
					  cstr_consts = 2;
					  cstr_nonconsts = 0;
					  cstr_private = Public} in     
		   let constr_false = mkpat e0.exp_loc (Tpat_construct (Predef.path_bool, false_desc, [])) e0.exp_type e0.exp_env in
		   [{exp with exp_desc = Texp_match (e0, [(constr_false, e2)], Total)}] in     
      l1@l2
  | _ -> []
   
let let_binding_to_axioms (pat, exp) = match pat.pat_desc with
| Tpat_var (id) -> 
    let loc = pat.pat_loc in
    let f_name = Ident.unique_name id in
    (*  let (args, rest) = getArgsRes exp in *)
    (* pre-processing exp by making each expression a list of expressions
       one for each branch of the original expression. Moreover, 
       branches leading to BAD or UNR are dropped. *)
    let ok_exp = remove_bad_unr exp in
    let exps = slice_expression ok_exp in 
    let ergoble_exps = exps in (* List.filter is_expression_ergoble exps in *)
    List.map (fun ei -> 
    let body = expression_to_eqlexpr (f_var loc f_name) ei in
    Axiom (pat.pat_loc, create_var f_name, body)
    ) ergoble_exps                 
| _ -> raise(Error(pat.pat_loc, Not_toplevel_function))

let bound_vars_to_logic pat =  match pat.pat_desc with
| Tpat_var (id) -> 
    let loc = pat.pat_loc in 
    let f_name = Ident.unique_name id in
    [Logic (loc, true, [f_name], type_to_logic_type loc pat.pat_type)]	   
| Tpat_construct(path, cstr, patl) ->
    List.map (fun p -> match p.pat_desc with 
    | Tpat_var (id) ->     
	let loc = p.pat_loc in 
	let f_name = Ident.unique_name id in
	Logic (loc, true, [f_name], type_to_logic_type loc p.pat_type)
    | _ -> raise(Error(pat.pat_loc, Pattern_not_convertable))
     )
      patl
| _ -> raise(Error(pat.pat_loc, Not_toplevel_function))


(* For example, ML code
   f x = [x]     becomes alt-ergo code
   logic f : 'a -> 'a list  // type 'a list should have been in the tasks
   axiom f1 : forall x:'a. f(x) = cons(x,nil)
*)
let def_to_axioms pat_exp_list = 
  let logics = List.fold_right (fun (p,_) default -> 
                     (bound_vars_to_logic p)@default) pat_exp_list [] in 
  let axioms = List.fold_right (fun x default -> 
                     (let_binding_to_axioms x)@default) pat_exp_list [] in
  logics@axioms


(* convert scrutinee to temporary axioms *)

(* 
let toAxiom exp = 
  let axiom_name = create_var "a" in
  let loc = exp.exp_loc in
  let f = f_var loc (Ident.name id) in
  let p = pattern_to_lexpr pat in
  let body = f_and (f_eq f p) (expression_to_lexpr f exp) in
  let loc = exp.exp_loc in
  let final_exp = List.fold_right (fun (id, ty) e ->
           mklexpr loc (PPforall ([Ident.unique_name id], 
                              type_to_ppure_type loc ty, [], e)))
           vars body in
  Axiom (exp.exp_loc, axiom_name, final_exp)
*)

let toAxiom exp = 
  let axiom_name = create_var "a" in
  let body = expression_to_lexpr exp in
  Axiom (exp.exp_loc, axiom_name, body)

let toAxiom_neg exp = 
  let axiom_name = create_var "a" in
  let body = f_not (expression_to_lexpr exp) in
  Axiom (exp.exp_loc, axiom_name, body)

let toAxiom_peq exp pat = 
  let axiom_name = create_var "a" in
  let p = pattern_to_lexpr pat in
  let body = expression_to_eqlexpr p exp in
  Axiom (exp.exp_loc, axiom_name, body)

let toAxiom_pneq exp pat = 
  let axiom_name = create_var "a" in
  let p = pattern_to_lexpr pat in
  let body = f_not (expression_to_eqlexpr p exp) in
  Axiom (exp.exp_loc, axiom_name, body)

let toAxiom_beq exp = 
  let axiom_name = create_var "g" in
  let loc  = exp.exp_loc in
  let body = expression_to_eqlexpr (mklexpr loc etrue) exp in
  Axiom (loc, axiom_name, body)

let toAxiom_bneq exp = 
  let axiom_name = create_var "g" in
  let loc  = exp.exp_loc in
  let body = expression_to_eqlexpr (mklexpr loc efalse) exp in
  Axiom (loc, axiom_name, body)

(* convert query (i.e. boolean expression) to goal *)

let toGoal_peq exp pat = 
  let goal_name = create_var "g" in
  let p = pattern_to_lexpr pat in
  let body = expression_to_eqlexpr p exp in
  Goal (exp.exp_loc, goal_name, body)

let toGoal_pneq exp pat = 
  let goal_name = create_var "g" in
  let p = pattern_to_lexpr pat in
  let body = f_not (expression_to_eqlexpr p exp) in
  Goal (exp.exp_loc, goal_name, body)

let toGoal_beq exp = 
  let goal_name = create_var "g" in
  let loc  = exp.exp_loc in
  let body = expression_to_eqlexpr (mklexpr loc etrue) exp in
  Goal (loc, goal_name, body)

let toGoal_bneq exp = 
  let goal_name = create_var "g" in
  let loc  = exp.exp_loc in
  let body = expression_to_eqlexpr (mklexpr loc efalse) exp in
  Goal (loc, goal_name, body)

let toGoal exp = 
  let goal_name = create_var "g" in
  let body = expression_to_lexpr exp in
  Goal (exp.exp_loc, goal_name, body)

let toGoal_neg exp = 
  let goal_name = create_var "g" in
  let body = f_not (expression_to_lexpr exp) in
  Goal (exp.exp_loc, goal_name, body)

(* identically converting axiom declaration in .ml to Alt-ergo axiom *)
let rec logical_formula_to_smt fml = match fml.taxm_desc with
| Taxm_forall (ids, ty, f) -> 
    f_forall (List.map Ident.unique_name ids) 
      (type_to_ppure_type fml.taxm_loc ty) []
      (logical_formula_to_smt f)
| Taxm_exist (id, ty, f) -> 
    f_exists (Ident.unique_name id)
      (type_to_ppure_type fml.taxm_loc ty) (logical_formula_to_smt f)
| Taxm_iff (f1, f2) -> 
    f_iff (logical_formula_to_smt f1) (logical_formula_to_smt f2)
| Taxm_imply (f1, f2) -> 
    f_implies (logical_formula_to_smt f1) (logical_formula_to_smt f2)
| Taxm_and (f1, f2) -> 
    f_and (logical_formula_to_smt f1) (logical_formula_to_smt f2)
| Taxm_or (f1, f2) -> 
    f_or (logical_formula_to_smt f1) (logical_formula_to_smt f2)
| Taxm_atom (e) -> expression_to_eqlexpr (mklexpr e.exp_loc etrue) e

let mlaxiom_to_smtaxiom decl = 
  Axiom (decl.ttopaxm_loc, Path.unique_name decl.ttopaxm_id,
	 (logical_formula_to_smt decl.ttopaxm_desc))



(* reporting error *)


let report_error ppf = function
  | Nonsimple_expression -> fprintf ppf "toErgosrc: Not a simple expression"
  | Partial_function -> fprintf ppf "toErgosrc: No partial function"
  | Not_lambda -> fprintf ppf "toErgosrc: Not a lambda expression"
  | Pattern_not_ident -> fprintf ppf "toErgosrc: pattern is not an ident"
  | Pattern_not_shallow -> fprintf ppf "toErgosrc: pattern is not shallow"
  | Argument_is_none -> fprintf ppf "toErgosrc: argument is none"
  | Expression_not_ident -> 
      fprintf ppf "toErgosrc: expression is not an ident"
  | Constant_not_convertable -> 
      fprintf ppf "toErgosrc: Constant not convertable"
  | Pattern_not_convertable -> 
      fprintf ppf "toErgosrc: Pattern not convertable"
  | Expression_not_convertable -> 
      fprintf ppf "toErgosrc: Expression not convertable"
  | Structure_not_convertable -> 
      fprintf ppf "toErgosrc: Structure not convertable"
  | Type_not_convertable -> 
      fprintf ppf "toErgosrc: Type not convertable"
  | Not_toplevel_function -> 
      fprintf ppf "toErgosrc: Not_toplevel_function"
  | Pattern_to_lexpr ->
      fprintf ppf "toErgosrc: Pattern cannot be converted to logical level"


(* print ergo *)

open Printf

let rec commalist f ppf l = match l with
| [] -> fprintf ppf "%s" ""
| [a] -> fprintf ppf "%a" f a
| a::xs -> fprintf ppf "%a , %a" f a (commalist f) xs

let rec list f ppf l = match l with
| [] -> fprintf ppf "%s" ""
| a::xs -> fprintf ppf "%a %a" f a (list f) xs

let rec tlist f ppf l = match l with
| [] -> fprintf ppf "%s" ""
| a::xs -> fprintf ppf "%a \n | %a" f a (tlist f) xs

let rec branches f ppf l = match l with
| [] -> fprintf ppf "%s" ""
| a::xs -> fprintf ppf "| %a \n %a" f a (branches f) xs

let rec block f ppf l = match l with
| [] -> fprintf ppf "%s" ""
| a::xs -> fprintf ppf "%a \n%a" f a (block f) xs

let out_string ppf s = fprintf ppf "%s" s

let rec out_ppure_type ppf pty = match pty with
  | PPTint -> fprintf ppf "%s" "int"
  | PPTbool -> fprintf ppf "%s" "bool"
  | PPTreal -> fprintf ppf "%s" "real"
  | PPTunit -> fprintf ppf "%s" "unit"
  | PPTbitv (i) -> fprintf ppf "%d %s" i "bitv"
  | PPTvarid (str, loc) -> fprintf ppf "%s" str
  | PPTexternal (ptys, str, loc) -> 
      if List.length ptys = 1 
      then fprintf ppf "%a%s" (list out_ppure_type) ptys str
      else fprintf ppf "(%a) %s" (commalist out_ppure_type) ptys str
  | PPTfarray (pty) -> fprintf ppf "%a array" out_ppure_type pty

let out_logic_type ppf lty = match lty with
| PPredicate (ptys) -> fprintf ppf "%a" (commalist out_ppure_type) ptys
| PFunction (ptys, pty) -> 
    if List.length ptys = 0
    then fprintf ppf "%a" out_ppure_type pty
    else
    fprintf ppf "%a -> %a" (commalist out_ppure_type) ptys out_ppure_type pty

let out_typevar ppf tvar = fprintf ppf "%s" tvar

let out_param ppf (loc, str, pty) = 
  fprintf ppf "(%s : %a)" str out_ppure_type pty

let out_constant ppf c = match c with
| ConstBitv (s) ->  fprintf ppf "%s" s
| ConstInt (i) -> fprintf ppf "%s" i
| ConstReal (nm) ->  fprintf ppf "%s" (Num.string_of_num nm)
| ConstTrue ->  fprintf ppf "%s" "true"
| ConstFalse ->  fprintf ppf "%s" "false"
| ConstVoid ->  fprintf ppf "%s" "()"
       
let out_infix op = match op with
	  | PPand -> "and"
	  | PPor  -> "or"
	  | PPimplies -> "->"
	  | PPiff -> "<->"
	  | PPlt -> "<"
	  | PPle -> "<="
	  | PPgt -> ">"
	  | PPge -> ">="
	  | PPeq -> "="
	  | PPneq -> "<>"	  
	  | PPadd -> "+"
	  | PPsub -> "-"
	  | PPmul -> "*"
	  | PPdiv -> "/"
	  | PPmod -> "mod"

let out_prefix op = match op with
	  | PPneg -> "neg"
	  | PPnot -> "not"

let rec out_lexpr ppf lexpr = match lexpr.pp_desc with
| PPvar (s) -> fprintf ppf "%s" s
| PPapp (str, es) -> fprintf ppf "%s(%a)" str (commalist out_lexpr) es
| PPconst (c) -> fprintf ppf "%a" out_constant c
| PPinfix (e1, op, e2) -> 
	  fprintf ppf "(%a %s %a)" 
	  out_lexpr e1 (out_infix op) out_lexpr e2
| PPprefix (op, e) -> 
	  fprintf ppf "%s(%a)" (out_prefix op) out_lexpr e
| PPif (e0, e1, e2) -> 
	  fprintf ppf "if %a \n then %a \n else %a" 
	  out_lexpr e0 out_lexpr e1 out_lexpr e2
| PPforall (slist, pty, trigers, e) -> 
	  fprintf ppf "forall %a:%a. %a" 
	  (commalist out_string) slist out_ppure_type pty out_lexpr e
| PPexists (str, pty, e) -> 
	  fprintf ppf "exists %s: %a . %a" 
	  str out_ppure_type pty out_lexpr e
| PPlet (str, e1, e2) -> 
	  fprintf ppf "let %s = %a \n in %a" 
	  str out_lexpr e1 out_lexpr e2
| _ -> fprintf ppf "%s" "out_lexpr: not handled"

let out_decl ppf d = match d with
| Axiom (loc, name, lexpr) -> 
	  fprintf ppf "axiom %s : %a " name
	  out_lexpr lexpr
| Goal (loc, name, lexpr) -> 
	  fprintf ppf "goal %s : %a " name
	  out_lexpr lexpr
| Logic (loc, is_ac, names, lty) -> 
	  fprintf ppf "logic %a : %a " 
	  (commalist out_string) names out_logic_type lty
| Predicate_def (loc, name, loc_str_pty_list, lexpr) -> 
	  fprintf ppf "function %s %a = %a " name
          (list out_param) loc_str_pty_list out_lexpr lexpr
| Function_def (loc, name, loc_str_pty_list, pty, lexpr) -> 
	  fprintf ppf "function %s %a : %a = %a " name
          (list out_param) loc_str_pty_list
          out_ppure_type pty out_lexpr lexpr
| TypeDecl (loc, slist, ty_name) ->  
          if List.length slist = 1 
          then fprintf ppf "type %a %s " (list out_typevar) slist ty_name
	  else fprintf ppf "type (%a) %s " (commalist out_typevar) slist ty_name

let out_ergotasks ppf ds = 
  fprintf ppf "%a" (block out_decl) ds




