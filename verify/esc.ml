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

(* $Id: typedtree.ml 8906 2008-07-09 13:03:38Z mauny $ *)

(* Abstract syntax tree after typing *)

open Misc
open Asttypes
open Types
open Typedtree
open ThmEnv
open Format
open Path
open EscSyn
open ToErgosrc
open ThmProvers
(* open Frontend *)

type error = 
  | Simpl_expr_Texp_function
  | Simpl_expr_Texp_let
  | Simpl_expr_Path_not_handled
  | Simpl_args_not_function_type
  | Simpl_args_no_optional_arg
  | Simpl_args_continuation_not_matched
  | KnownCon_impossible_pattern
  | Rebuild_not_function_type
  | Rebuild_pattern_not_matched
  | ErrorExp_Unknownblame
  | ErrorExp_Texp_unr
  | ErrorExp_function_has_no_argument
  | ErrorExp_no_pattern_matching
  | ErrorExp_tuple_no_argument
  | ErrorExp_constructor_no_argument
  | ErrorExp_expression_not_handled
  | Check_expression_not_handled
  | Has_function_call_expression_not_handled

exception Error of error

(* Auxilary functions *) 

let is_expression_primitiveop exp = match exp.exp_desc with
| Texp_ident (path, vd) -> begin match vd.val_kind with
  | Val_prim _ -> true
  | _ -> false
 end
| _ -> false

let rec is_expression_tv exp = match exp.exp_desc with
| Texp_constant _ -> true
| Texp_ident _ -> true
| Texp_construct (_, _, es) -> List.for_all is_expression_tv es
| _ -> false

let rec is_expression_tvalue exp = is_expression_tv exp ||
 (match exp.exp_desc with
 | Texp_function _ -> true
 | _ -> false)

let constant_to_string c = match c with
| Const_int i -> string_of_int i
| Const_char ch -> String.make 1 ch
| Const_string s -> s
| Const_float s -> s
| Const_int32 i -> Int32.to_string i
| Const_int64 i -> Int64.to_string i
| Const_nativeint i -> Int64.to_string (Int64.of_nativeint i)

let rec pattern_to_boundvars p = match p.pat_desc with
| Tpat_var (id) -> [(id, p.pat_type)]
| Tpat_construct (path, cdesc, ps) -> 
   List.flatten (List.map pattern_to_boundvars ps)
| _ -> []

let pattern_to_string pat = match pat.pat_desc with
| Tpat_var (id) -> Ident.name id
| Tpat_constant c -> constant_to_string c
| Tpat_construct (path, cdesc, ps) -> Path.name path
| _ -> "pattern_to_string: other patterns"

let rec expression_to_string exp = match exp.exp_desc with
  | Texp_ident (path, vdesc) -> Path.name path
  | Texp_constant (c) -> constant_to_string c
  | Texp_apply (e1, eopt_optl_list) ->
      (expression_to_string e1)^"... "
  | others -> "expression_to_string: other expressions"


(*  check take a AExp and checks if there is any *BAD* in the expression
 result is either (Exp, True) or (counter-example, False)
 where the example includes all the paths that lead to *BAD*. 
 This is for error message reporting *)

let rec check_pat_exp_list xs = match xs with
| [] -> None
| (p,e)::l -> let (e1, b1) = check e in
              if b1 then Some (p,e1)
              else check_pat_exp_list l 

and check_exp_list xs = match xs with
| [] -> None
| e::l -> let (e1, b1) = check e in
              if b1 then Some e1
              else check_exp_list l 

and check exp = match exp.exp_desc with
| Texp_ident (id, vd) -> (exp, false)
| Texp_constant (c) -> (exp, false)
| Texp_bad (bl) -> (exp, true)
| Texp_unr (bl) -> (exp, false)
| Texp_let (rec_flag, pat_expr_list, expr) -> 
    begin match check_pat_exp_list pat_expr_list with
    | Some (p,e) -> (e, true)
    | None -> check expr 
    end
| Texp_function (pat_expr_list, partial) -> 
    begin match check_pat_exp_list pat_expr_list with
    | Some (p,e) -> (e, true)
    | None -> (exp, false)
    end
| Texp_match (expr1, pat_expr_list, partial) ->
   let (e1, b1) = check expr1 in
   if b1 then (e1, b1)
   else let (pe_list, b) = match check_pat_exp_list pat_expr_list with
   | Some (p,e) -> ([(p,e)], true) 
   | None -> (pat_expr_list, false)
   in
   let ptial = match partial with
   | Total -> if List.length pat_expr_list = List.length pe_list
              then Total else Partial
   | Partial -> Partial
   in ({exp with exp_desc = Texp_match (expr1, pe_list, ptial)}, b)   
| Texp_tuple (expr_list) -> 
   begin match check_exp_list expr_list with
    | Some e -> ({exp with exp_desc = Texp_tuple [e]}, true)
    | None -> (exp, false)
   end
| Texp_construct (path, constr_desc, expr_list) ->
   begin match check_exp_list expr_list with
    | Some e -> ({exp with exp_desc = Texp_construct (path, constr_desc, [e])}, true)
    | None -> (exp, false)
   end
| Texp_ifthenelse (expr1, then_expr, exprop) -> 
    begin match exprop with
    | None -> let truePat = { pat_desc = trueCon;
                              pat_loc = expr1.exp_loc;
			      pat_type = expr1.exp_type;
			      pat_env = expr1.exp_env } in 
      check ({exp with exp_desc = Texp_match (expr1, [(truePat, then_expr)], Partial)})
    | Some else_expr -> 
      check
     ({exp with exp_desc = Texp_match (expr1, [((truePat expr1), then_expr);
                                            ((falsePat expr1), else_expr)], Total)})
    end
| Texp_apply (e1, eopt_optl_list) -> 
   let (a1, b1) = check e1 in
   if b1 
    then (a1, b1)
    else let rec bad_arg xs = match xs with
     | [] -> (exp, false) 
     | (Some e, optl) :: l-> 
	 let (a, b) = check e in
	 if b then (a, b) 
	 else bad_arg l
     | (None, optl)::l -> bad_arg l
     in bad_arg eopt_optl_list 
| _ -> raise(Error (Check_expression_not_handled))


(* unsimplified *)

type inexp = expression
type inalt = (pattern * expression) 
type inname = Path.t

(* simplified *)

type outexp = expression
type outname = Path.t

(* Types for continuation *)

type cont =  (* e a1 HOLE an *)
    Cargs of ThmEnv.t (* same env for all args *) 
          * inexp (* function *)
          * (inexp option * optional) list  (* args done *)
          * optional  (* for current arg being simplified *)
          * (inexp option * optional) list  (*args not done *) 
          * type_expr (* result type *) 
  | Capp of ThmEnv.t
          * (inexp option * optional) list (* None args *)
          * (inexp option * optional) list (* first arg is not None *)
          * type_expr (* HOLE e *)
  | CLam of expression (* original expression *)
          * pattern *partial (* \x.HOLE *)
  | Cprim of (inexp option * optional) list * type_expr (* op HOLE e2 *)
  | Clet of rec_flag * (pattern * outexp) list (* let x = e1 in HOLE *)
  | Cmatch of (ThmEnv.t * expression (* original env and expression *)
            * inalt list * partial) (* match HOLE with x alts *)
  | Cif of (ThmEnv.t * expression * expression option) (* if HOLE then e1 else e2 *)
  | Ctry of inalt list

(* Symbolically simplify pure expression in Typedtree.expression *)

let is_bad e = match e.exp_desc with 
  | Texp_bad (bl) -> true
  | _ -> false 

let is_unr e = match e.exp_desc with 
  | Texp_unr (bl) -> true
  | _ -> false 

let is_contract_exception e = match e.exp_desc with 
  | Texp_bad (bl) -> true
  | Texp_unr (bl) -> true
  | _ -> false 

(* This is the SL machine:
   the senv contains both the mapping from variable to value and
   the logical store  *)
let rec simpl_expr (senv : ThmEnv.t) (exp : expression) (k: cont list) : expression =
  (* print_string "***esc.simpl_expr: \n"; print_expression exp; *)
  let mkexp desc = {exp with exp_desc = desc} in 
  match exp.exp_desc with  
  | Texp_constant (c) -> rebuild senv exp k
  | Texp_bad (bl) -> rebuild senv exp k
  | Texp_unr (bl) -> rebuild senv exp k
  | Texp_ident (path, vdesc) -> 
      begin match path with
	| Pident id -> 
	    let aval = lookup_senv id senv in
            rebuildVar senv (id, exp) aval k
	| _ -> rebuild senv exp k
      end
  | Texp_function (pat_expr_list, ptial) ->
        (* NEW: we generate logic v : <type of v> 
           In alt-ergo, logic v, axiom f, goal g is the same as
           forall v. f -> g *)
      let simpl_list = List.map (fun (p,e) ->           
                       let env1 = add_tasks senv (bound_vars_to_logic p) in
		       (p, simpl_expr env1 e []))
                       pat_expr_list in
      rebuild senv {exp with exp_desc = Texp_function (simpl_list, ptial)} k
  | Texp_apply (e1, eopt_optl_list) ->
      (* A-normalize the arguments *)
     let rec genLet xs = match xs with
     | [] -> ([],[])
     | (arg::l) -> begin match arg with 
       | (Some e, optl) -> 
	   let (args, lets) = genLet l in
	   if is_expression_argable e
           then ((Some e, optl)::args, lets)
           else let x = Ident.create "x" in
                let vd = { val_type = e.exp_type;
                           val_kind = Val_reg } in
                let ex = {e with exp_desc = Texp_ident (Pident x, vd)} in
                let p = { pat_desc = Tpat_var x;
                          pat_loc  = e.exp_loc;
                          pat_type = e.exp_type;
                          pat_env  = e.exp_env} in
                ((Some ex, optl)::args, (p, e)::lets)
       | (None, optl) -> 
	   let (args, lets) = genLet l in
           ((None, optl)::args, lets)
       end
     in
     let (args, p_e_list) = genLet eopt_optl_list in
     begin match p_e_list with
     | [] ->  simpl_expr senv e1 (Capp (senv, [], eopt_optl_list, exp.exp_type)::k) 
     | _ -> simpl_expr senv ({exp with exp_desc = Texp_let (Nonrecursive, p_e_list, 
				    {exp with exp_desc = Texp_apply(e1, args)})}) k
     end
  | Texp_match (expr, pat_expr_list, ptial) -> 
      begin match k with
      | (Capp (env, [], nds, res_type) :: k1) ->  (* [matchR] *)
          let p_e_list = List.map (fun (p,e) -> (p, 
                         { exp_desc = Texp_apply(e, nds);
                           exp_loc  = exp.exp_loc;
                           exp_env  = exp.exp_env;
			   exp_type = res_type;
                         })) 
                         pat_expr_list in
	  let expr1 = {exp with exp_desc = Texp_match (expr, p_e_list, ptial)} in
          simpl_expr senv expr1 k1
      | _ -> simpl_expr senv expr (Cmatch (senv, exp, pat_expr_list, ptial) :: k) 
      end
  | Texp_try (e, pat_expr_list) ->
     simpl_expr senv e ((Ctry pat_expr_list) :: k) 
  | Texp_tuple (es) -> 
     let new_es = List.map (fun e -> simpl_expr senv e []) es in
     rebuild senv (mkexp (Texp_tuple new_es)) k
  | Texp_construct (path, constr_desc, es) -> 
     let new_es = List.map (fun e -> simpl_expr senv e []) es in
     rebuild senv (mkexp (Texp_construct (path, constr_desc, new_es))) k
  | Texp_ifthenelse (e0, e1, e2opt) -> 
     (* if (if e0 then e1 else e2) then e11 else e22 
        ==> [if-if]
        if e0 then (if e1 then e11 else e22)
              else (if e2 then e11 else e22)
     *)
     begin match k with
     | (Cif (env, e11, e22opt) :: k1) -> 
	 simpl_expr env {e1 with exp_desc = Texp_ifthenelse (e0, 
	   { e1 with exp_desc = Texp_ifthenelse (e1, e11, e22opt) },
           match e2opt with
	   | None -> None
           | Some e2 -> Some { e1 with exp_desc = Texp_ifthenelse (e2, e11, e22opt)})} k1
     | _ -> simpl_expr senv e0 (Cif (senv, e1, e2opt) :: k) 
     end
  | Texp_let (rflag, pat_expr_list, expr) ->     
     begin match k with
     | (Cmatch (env, e_match, alts, ptial):: k1) -> 
       (* match (let x = e1 in e2) with alts
           ==> [match-let]
          let x = e1 in match e2 with alts
       *)
         let rhs = {e_match with exp_desc = Texp_let (rflag, pat_expr_list,
              {e_match with exp_desc = Texp_match (expr, alts, ptial)})}
         in simpl_expr env rhs k1
     | (Cif (env, then_e, else_eopt):: k1) -> 
       (* if (let x = e1 in e2) then .. else ..
           ==> [if-let]
          let x = e1 in if e2 then .. else ..
       *)
         let rhs = {then_e with exp_desc = Texp_let (rflag, pat_expr_list,
              {then_e with exp_desc = Texp_ifthenelse (expr, then_e, else_eopt)})}
         in simpl_expr env rhs k1     
     | _ -> 
       let rec do_let xs acc = match xs with
       | [] -> acc             
       | (p,e)::l -> 
         let (trans_e, ds, isExn, env) = acc in
	 let simpl_e = simpl_expr env e [] in
         (* let x = BAD in e  ==> BAD *)
         if is_bad simpl_e then (None, [(p,simpl_e)], true, env)
         else (* let x = UNR in e  ==> UNR *)
	 if is_unr simpl_e then (None, [(p,simpl_e)], true, env)
         else 
         begin match (p.pat_desc, simpl_e.exp_desc) with
	       | (Tpat_var (id1), Texp_ident (Pident id2, _)) 
                  when rflag = Nonrecursive && id1 = id2 -> do_let l acc
	       | (Tpat_var (id1), Texp_match(e0, alts, ptial))
		  when rflag = Nonrecursive -> 
		    (* let x = (match e0 with K x -> ei) in e2  ==>  [let-match]
		       match e0 with K x -> let x = ei in e2 *)
		    let new_e = simpl_expr senv ({exp with exp_desc = Texp_match(e0, 
				     List.map (fun (pi,ei) -> (pi, {expr with exp_desc = 
								 Texp_let (rflag, (p,ei)::l, expr)}))
				 	    alts, ptial)}) [] in
		    (Some new_e, ds, isExn, env)
	       | (Tpat_var (id1), Texp_ifthenelse(e0, then_e, else_eopt))
		  when rflag = Nonrecursive -> 
		    (* let x = (if e0 then e1 else e3) in e2  ==>  [let-if]
		       if e0 then let x = e1 in e2 else let x = e3 in e2 *)
		    let new_e = simpl_expr senv ({exp with exp_desc = Texp_ifthenelse(e0, 
				      {expr with exp_desc = Texp_let (rflag, (p,then_e)::l, expr)},
				       match else_eopt with 
				       | None -> None
				       | Some e3 -> Some {expr with exp_desc = Texp_let (rflag, (p,e3)::l, expr)})}) [] in
		    (Some new_e, ds, isExn, env)
               | (Tpat_var (id1), _) -> 
		   if rflag = Nonrecursive && is_expression_tvalue simpl_e
		   then let new_id   = Ident.rename id1 in
  		   let vd       = { val_type = e.exp_type; val_kind = Val_reg } in
		   let eid      = {e with exp_desc = Texp_ident(Pident new_id, vd)} in
		   let env2     = extend_senv env id1 eid in
		   let env3     = extend_denv env2 eid (Inline simpl_e) in 
 		   do_let l (trans_e, ds, isExn, env3)
		   else
		     let env2 = add_tasks env
			 ((bound_vars_to_logic p)@[toAxiom simpl_e p]) in
		     (* (def_to_axioms [(p,simpl_e)]) in *)
		     do_let l (trans_e, ds@[(p, simpl_e)], isExn, env2)
	       | _ -> do_let l (trans_e, ds@[(p, simpl_e)], isExn, env)
         end
     in
     let (new_e, simpl_list, is_exn, new_senv) = do_let pat_expr_list (None, [], false, senv) in
     match new_e with 
     | Some exp -> simpl_expr new_senv exp k
     | None -> 
	 let body = simpl_expr new_senv expr [] in
	 if List.length simpl_list = 0 then rebuild new_senv body k
	 else if is_exn then let (p, texp_exn) = List.hd simpl_list in texp_exn
	 else 
	   rebuild new_senv {exp with exp_desc = Texp_let(rflag, simpl_list, body)} k   
   end
  | Texp_sequence (e1, e2) -> 
     let new_e1 = simpl_expr senv e1 k in 
     let new_e2 = simpl_expr senv e2 k in 
     rebuild senv (mkexp (Texp_sequence (new_e1, new_e2))) k
  | Texp_array (es) -> 
     let new_es =  List.map (fun e -> simpl_expr senv e []) es in
     rebuild senv (mkexp (Texp_array new_es)) k
  | others -> rebuild senv exp k

(* This is for dealing with primitive binary operators such as > , <, etc *)
and simpl_primitive_app senv exp f args k = match args with
| (Some a1, optl1)::(Some a2, optl2)::l -> begin match a1.exp_desc with
  (* \x.e (let x = e1 in e2) ==> let x = e1 in \x.e e2 *)
  | Texp_let (rflag, pat_exp_list, e2) -> 
      begin 
      let rhs = Texp_apply (f, (Some e2, optl1)::(Some a2, optl2)::l) in
      simpl_expr senv ({exp with exp_desc = Texp_let (rflag, pat_exp_list,
	{exp with exp_desc = rhs})}) k
      end
  (* \x.e (match e0 with Ki xi -> ei) ==> match e0 with Ki xi -> \x.e ei 
     since all variables are fresh, no shadowing in \x.e ei *)
  | Texp_match (e0, pat_exp_list, ptial) -> 
      simpl_expr senv ({exp with exp_desc = Texp_match (e0, 
      List.map (fun (p,e) -> 
	let rhs = Texp_apply (f, (Some e, optl1)::(Some a2, optl2)::l) in
	(p, {exp with exp_desc = rhs})) pat_exp_list, ptial)}) k
  (* \x.e (if e0 then e1 else e2) ==> if e0 then \x.e e1 else \x.e e2 *)
  | Texp_ifthenelse (e0, e1, e2opt) -> begin match e2opt with
    | None -> let new_e1 = Texp_apply (f, (Some e1, optl1)::(Some a2, optl2)::l) in
      simpl_expr senv ({exp with exp_desc = Texp_ifthenelse (e0, 
	{exp with exp_desc = new_e1}, None)}) k
    | Some e2 -> 
	let new_e1 = Texp_apply (f, (Some e1, optl1)::(Some a2, optl2)::l) in
	let new_e2 = Texp_apply (f, (Some e2, optl1)::(Some a2, optl2)::l) in
      simpl_expr senv ({exp with exp_desc = Texp_ifthenelse (e0, 
	{exp with exp_desc = new_e1}, Some {exp with exp_desc = new_e2})}) k
    end
  | _ -> (* we repeat above for the 2nd argument *)
      match a2.exp_desc with
    | Texp_let (rflag, pat_exp_list, e2) ->
	let rhs = Texp_apply (f, (Some a1, optl1)::(Some e2, optl2)::l) in
	simpl_expr senv ({exp with exp_desc = Texp_let (rflag, pat_exp_list,
					      {exp with exp_desc = rhs})}) k
    | Texp_match (e0, pat_exp_list, ptial) -> 
	simpl_expr senv ({exp with exp_desc = Texp_match (e0, 
			List.map (fun (p,e) ->
	let rhs = Texp_apply (f, (Some a1, optl1)::(Some e, optl2)::l) in
	(p, {exp with exp_desc = rhs})) pat_exp_list, ptial)}) k
   | Texp_ifthenelse (e0, e1, e2opt) -> begin match e2opt with
    | None -> let new_e1 = Texp_apply (f, (Some a1, optl1)::(Some e1, optl2)::l) in
      simpl_expr senv ({exp with exp_desc = Texp_ifthenelse (e0, 
	{exp with exp_desc = new_e1}, None)}) k
    | Some e2 -> 
	let new_e1 = Texp_apply (f, (Some a1, optl1)::(Some e1, optl2)::l) in
	let new_e2 = Texp_apply (f, (Some a1, optl1)::(Some e2, optl2)::l) in
      simpl_expr senv ({exp with exp_desc = Texp_ifthenelse (e0, 
	{exp with exp_desc = new_e1}, Some {exp with exp_desc = new_e2})}) k
    end
 | _ ->  rebuild senv exp k
   end
| _ ->  rebuild senv exp k

and simpl_args senv f ds nds res_type k = match nds with
| [] -> (* this means all args are simplified *)
    let expr = { exp_desc = Texp_apply (f, ds);
                 exp_loc  = f.exp_loc;
		 exp_type = res_type;
		 exp_env  = f.exp_env} in
    begin match f.exp_desc with
    | Texp_function ([(p,e)], optl) -> begin match p.pat_desc with
      | Tpat_var (id) ->
	  let new_id   = Ident.rename id in
	  let vd       = { val_type = p.pat_type; val_kind = Val_reg } in
          let eid      = {exp_desc = Texp_ident(Pident new_id, vd);
                          exp_type = p.pat_type;
                          exp_loc  = p.pat_loc;
                          exp_env  = p.pat_env } in
          let env2     = extend_senv senv id eid in
          begin match ds with
          | [] -> rebuild senv f k 
          | [(None, optl)] -> 
	      simpl_expr senv e k
          | [(Some a, optl)] -> 
              if is_expression_tvalue a then
		let env3 = extend_denv env2 eid (Inline a) in 
                (* beta reduction: (\x.e1) val ==> e1[val/x]  *)
                simpl_expr env3 e k
	      else
		(* (\x.e1) e2 ==> let x = e2 in e1 *)
                simpl_expr senv {expr with exp_desc = 
			       Texp_let (Nonrecursive, [(p,a)], e)} k  
	  | (Some a, optl)::rest -> 
              if is_expression_tvalue a then
		let env3 = extend_denv env2 eid (Inline a) in 
                (* beta reduction: (\x.e1) val ==> e1[val/x]  *)
                simpl_args env3 e rest [] e.exp_type k
	      else
		(* (\x.e1) e2 ==> let x = e2 in e1 *)
		simpl_args senv {expr with exp_desc = 
			       Texp_let (Nonrecursive, [(p,a)], e)} rest [] e.exp_type k
          | (None, optl)::rest -> 
	      simpl_args senv e rest [] e.exp_type k
	  end
      | _ -> rebuild senv expr k
      end 
    | Texp_ident(path, vd) -> 
	begin match vd.val_kind with
	| Val_prim _ -> simpl_primitive_app senv expr f ds k 
	| _ -> rebuild senv expr k
	end
    | Texp_let(rflag, pat_exp_list, e2) -> 
	(* (let x = e1 in \y.e') a ==> let x = e1 in (\y.e') a *)
       let new_e2 = { expr with exp_desc = Texp_apply(e2, ds) } in
       simpl_expr senv ({expr with exp_desc = Texp_let(rflag, pat_exp_list, new_e2)}) k
    | Texp_ifthenelse(e0, then_e, else_eopt) -> 
	(* (if e0 then \y.e1 else \y.e2) a ==> if e0 then (\y.e1) a else (\y.e2) a *)
       let new_then = { expr with exp_desc = Texp_apply(then_e, ds) } in
       let new_else = match else_eopt with
                      | None -> None 
                      | Some else_e -> Some { expr with exp_desc = 
					           Texp_apply(else_e, ds) } in
       simpl_expr senv ({expr with exp_desc = Texp_ifthenelse(e0, new_then, new_else)}) k
    | _ -> rebuild senv expr k
    end    
| ((Some e, optl) :: l) -> 
    simpl_expr senv e (Cargs (senv, f, ds, optl, l, res_type) :: k)
| ((None, optl) :: l) -> 
    simpl_args senv f (ds@[(None, optl)]) l res_type k
          
and rebuildMatch senv scrut aval alts ptial k = 
   let env1 = extend_denv senv scrut aval in
   let (_,e) = List.hd alts in
   let new_alts = List.map (fun (p,e) -> 
     match scrut.exp_desc with
     | Texp_ident (Pident id1, vd) -> 
	begin match p.pat_desc with 
	| Tpat_construct (path, cdesc, []) -> 
	    (* if pattern is a constructor with no parameters, we inline this constructor *)
	 let new_id = Ident.rename id1 in
	 let eid    = {e with exp_desc = Texp_ident(Pident new_id, vd)} in
	 let senv2  = extend_senv senv id1 eid in
	 let senv3   = extend_denv senv2 eid (Inline ({scrut with exp_desc = 
						     Texp_construct(path,cdesc,[])})) in
         (p, simpl_expr senv3 e k)
        | _ ->
	    let env2 = add_tasks env1 ((bound_vars_to_logic p)@[toAxiom scrut p])
	    in (p,simpl_expr env2 e k)
        end
     | _ -> 
       let env2 = add_tasks env1 ((bound_vars_to_logic p)@[toAxiom scrut p])
       in (p,simpl_expr env2 e k)) alts in
   {e with exp_desc = Texp_match (scrut, new_alts, ptial)}

and rebuildIf senv scrut e1 e2opt k = 
    let mkexp desc = {e1 with exp_desc = desc} in
    let unr_exp = {e1 with exp_desc = Texp_unr (UnknownBlame)} in
    let then_senv = extend_denv senv scrut 
                                (Inline (trueExpression scrut)) in
    let then_senv2 = add_tasks then_senv 
		     [toAxiom scrut (truePat scrut)] in
    let else_senv = extend_denv senv scrut 
                                (Inline (falseExpression scrut)) in
    let else_senv2 = add_tasks else_senv 
		         [toAxiom_neg scrut (falsePat scrut)] in
    match is_expression_true scrut with
     | Ptrue -> simpl_expr then_senv e1 k
     | Pfalse -> begin match e2opt with
       | None -> rebuild senv (mkexp (Texp_ifthenelse (scrut, unr_exp, None))) k
       | Some e2 -> simpl_expr else_senv e2 k
       end 
     | Pothers -> begin match e2opt with
       | Some e2 -> begin match e2.exp_desc with
	 | Texp_bad _ when (is_scrutinee_ergoble scrut) -> 
             let goal_tasks = goalTasks senv in        
	     let senv1 = if is_expression_prop scrut
                         then add_tasks senv (goal_tasks@[toGoal scrut])
	                 else add_tasks senv (goal_tasks@[toGoal_eq scrut]) in
	     let ts1   = tasks senv1 in     
	     let filename = (Location.toString scrut.exp_loc)^".why" in
	     begin match askErgo "temp1.why" ts1 with 
	     | Valid -> (* scrutinee is Valid *)
		 write_tasks_to_file filename ts1;
		 simpl_expr then_senv2 e1 k
	     | _ -> (* we omit the negation check for the moment
		       let senv2 = add_tasks senv 
		       (goal_tasks@[toGoal_neg scrut]) in
		       let ts2 = tasks senv2 in
		       write_tasks_to_file "temp2.why" ts2;
		       match askErgo filename ts2 with
		       | Valid -> (* negation of the scrutinee is Valid *)
		       begin match e2opt with
		       | Some e2 -> 
                       let else_senv2 = add_tasks else_senv 
		       [toAxiom_neg [] scrut (falsePat scrut)] in
                       simpl_expr else_senv2 e2 k
		       | None -> rebuild senv (mkexp (Texp_ifthenelse 
		       ((falseExpression scrut), unr_exp, None))) k
		       end 
		       | _ -> *) (* no validity info about scrutinee *)
		 let new_e1 = simpl_expr then_senv2 e1 [] in
		 let new_e2 = Some (simpl_expr else_senv2 e2 []) in
		 rebuild senv (mkexp (Texp_ifthenelse 
					   (scrut, new_e1, new_e2))) k
             end
	 | _ -> 
	     let new_e1 = simpl_expr then_senv2 e1 [] in
             let new_e2 = Some (simpl_expr else_senv e2 []) in
	     rebuild senv (mkexp (Texp_ifthenelse (scrut, new_e1, new_e2))) k
        end
       | None -> 	     
	   let new_e1 = simpl_expr then_senv2 e1 [] in
	   rebuild senv (mkexp (Texp_ifthenelse (scrut, new_e1, None))) k
       end
   
(* 
match Ki es with
   | Ki ps -> ei     ==>    let ps = es in ei
   | Kj ps -> ej

If we only do static contract checking, we slice program by removing safe
branches. So if Ki doesn't match any constructor, then LHS ==> UNR

If we do hybrid contract checking, we don't do slicing. So Ki must
match one of the branches.
*)

and knownCon senv (path, cdesc) es alts k =  
    let xs = List.filter (fun (p,e) -> match p.pat_desc with
    | Tpat_construct (path2, cdesc2, ps) -> cdesc.cstr_tag = cdesc2.cstr_tag
    | _ -> false) alts in
    let (p,e) = List.hd xs in
    match p.pat_desc with
    | Tpat_construct (path2, cdesc2, ps) -> 
       let rhs = {e with exp_desc = Texp_let (Nonrecursive, List.combine ps es, e)} in
       simpl_expr senv rhs k
    | _ -> raise(Error(KnownCon_impossible_pattern))   
   

and rebuild senv a k = 
  match (a.exp_desc, k) with
  | (_, []) -> a
  | (Texp_bad (bl), Cmatch _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Cif _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Capp _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Cargs _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cmatch _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cif _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Capp _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cargs _ :: k1) -> rebuild senv a k1
  | (Texp_construct (path, cdesc, es), 
     Cmatch (matchenv, e_match, alts, ptial) :: k1) -> 
      knownCon matchenv (path, cdesc) es alts k1
  | (_, CLam (e, p, ptial) :: k1) -> 
      rebuild senv {e with exp_desc = Texp_function ([(p,a)], ptial)} k1
  | (_, Cargs (env, f, ds, optl, nds, res_type) :: k1) -> 
      simpl_args env f (ds@[(Some a, optl)]) nds res_type k1
  | (_, Capp (env, ds, nds, res_type) :: k1) -> 
      simpl_args env a ds nds res_type k1
  | (_, Clet (rflag, es) :: k1) -> 
      rebuild senv ({a with exp_desc = Texp_let (rflag, es, a)}) k1
  | (_, Cmatch (matchenv, e_match, alts, ptial) :: k1) -> 
       begin match lookup_denv a senv with
       | Inline e -> (* if scrutinee reappears, replace it by its aval *)
                     simpl_expr senv e k1
       | _ -> (* [[ a ]]_(K x) *)
	   if a.exp_type = Predef.type_bool 
	   then begin
             let goal_tasks = goalTasks senv in
             let ts1  = tasks (add_tasks senv (goal_tasks@[toGoal a])) in
             let filename = (Location.toString a.exp_loc)^".why" in 
             match askErgo "temp_match1.why" ts1 with
             | Valid -> (* scrutinee is Valid *)
	       write_tasks_to_file filename ts1;
	       rebuildMatch matchenv a (Inline {a with exp_desc = trueExp}) 
                            alts ptial k1
             | _ -> (* we omit the negation check for the moment
		    let g = goal_tasks@[toGoal_neg a] in
                    let ts2 = tasks (add_tasks senv g) in
               match askErgo filename ts2 with
               | Valid -> (* negation of the scrutinee is Valid *)
 	         rebuildMatch matchenv a (Inline {a with exp_desc = falseExp}) alts ptial k1
               | _ -> *)
                rebuildMatch matchenv a NoVal alts ptial k1 end
	   else rebuildMatch matchenv a NoVal alts ptial k1
       end
  | (_, Cif (ifenv, e1, e2opt) :: k1) ->       
      begin match lookup_denv a senv with
      | Inline e -> (* if scrutinee reappears, replace it by its aval *)
                    simpl_expr senv e k
      | _ -> rebuildIf ifenv a e1 e2opt k1
      end
  | _ -> raise(Error(Rebuild_pattern_not_matched))
  

and rebuildVar senv ((n, e):(Ident.t * expression)) 
               (v:aval) (k:cont list) : outexp = 
(* e is the expression form of the ident n *)
    match v with
    | Inline a -> simpl_expr senv a k
    | NotCon (cs) -> begin match k with
      | Cmatch (senv, e_match, alts, ptial) :: k1 -> 
	  rebuildMatch senv e (NotCon cs) alts ptial k1
      | _ -> rebuild senv e k
      end
    | _ -> rebuild senv e k

(* report static contract checking error message *)

let pprPred exp pat = match is_pattern_true pat with
  | Ptrue -> (expression_to_string exp)^" holds"
  | Pfalse ->  (expression_to_string exp)^" does not hold"
  | Pothers -> (expression_to_string exp)^" pattern matches "^(pattern_to_string pat)

let rec pprPreds xs = match xs with
| [] -> ""
| [(a,c)] -> pprPred a c 
| (a,c)::l -> (pprPred a c)^" and \n"^(pprPreds l)

                         
let rec errorExp id exp preds = 
(* print_string "esc.errorExp: \n"; print_expression exp; *)
match exp.exp_desc with
| Texp_bad (bl) -> begin match bl with
  | Caller (loc, pathop, path) -> 
      begin match pathop with
      | Some caller -> 
	  ("\t"^(Location.toString loc)^": "^(Path.name caller)^" fails "^(Path.name path)^"'s precondition!", [])
      | None -> 
	  ("\t"^(Location.toString loc)^": "^"fails "^(Path.name path)^"'s precondition!", [])
      end
  | Callee (loc, path) -> 
      ((Ident.name id)^" may not satisfy its postcondition!", [])
  | UnknownBlame -> raise(Error(ErrorExp_Unknownblame))
  end
| Texp_unr (bl) -> raise(Error(ErrorExp_Texp_unr))
| Texp_let (rec_flag, pat_expr_list, expr1) -> errorExp id expr1 preds
| Texp_function (pat_expr_list, partial) -> 
    begin match pat_expr_list with
    | [] -> raise(Error(ErrorExp_function_has_no_argument))  
    | _ -> let (p, e) = List.hd pat_expr_list in 
           errorExp id e preds
    end
| Texp_match (expr1, pat_expr_list, partial) ->
    begin match pat_expr_list with
    | [] -> raise(Error(ErrorExp_no_pattern_matching))  
    | _ -> let (p, e) = List.hd pat_expr_list in 
           errorExp id e (preds@[(expr1,is_pattern_true p)])
    end
| Texp_try (expr1, pat_expr_list) -> 
    errorExp id expr1 preds
| Texp_tuple (expr_list) ->
    begin match expr_list with
    | [] -> raise(Error(ErrorExp_tuple_no_argument))  
    | _ -> errorExp id (List.hd expr_list) preds
    end
| Texp_construct (path, constr_desc, expr_list) ->
    begin match expr_list with
    | [] -> raise(Error(ErrorExp_constructor_no_argument)) 
    | _ -> errorExp id (List.hd expr_list) preds
    end
| Texp_ifthenelse (e0, e1, e2op) -> 
    errorExp id e1 (preds@[(e0, Ptrue)])
| others -> raise(Error(ErrorExp_expression_not_handled))

let static_report (id, exp) validity = 
  match validity with
  | EscSyn.Valid -> (Ident.name id)^" satisfies its contract!\n" 
  | EscSyn.Invalid ->  let (a, preds) = errorExp id exp [] in
    begin match preds with
    | [] -> (Ident.name id)^" does not satisfy its contract,\n"^a^"\n"
    | _ -> (Ident.name id)^" will crash when \n"^(pprPreds preds)^"\n"^a^"\n"
    end
  | EscSyn.Unknown ->  let (a, preds) = errorExp id exp [] in
    begin match preds with
    | [] -> (Ident.name id)^" does not satisfy its contract,\n"^a^"\n"
    | _ -> (Ident.name id)^" may crash when \n"^(pprPreds preds)^"\n"^a^"\n"
    end
  | _ -> "other validity: time out or highfailure"



let rec has_function_call gamma exp = match exp.exp_desc with
| Texp_ident (path, vd) -> if Tbl.mem path gamma then true else false
| Texp_bad (bl) -> false
| Texp_unr (bl) -> false
| Texp_let (rec_flag, pat_expr_list, expr1) -> 
  if List.exists (fun (p,e) -> has_function_call gamma e) pat_expr_list
  then true 
  else let rec add_fnames xs tbl = begin match xs with
  | [] -> tbl
  | (p,e)::l -> begin match p.pat_desc with
    | Tpat_var (id) -> let new_tbl = Tbl.add (Path.Pident id) rec_flag tbl in
                       add_fnames l new_tbl
    | _ -> add_fnames l tbl
    end end in
       has_function_call (add_fnames pat_expr_list gamma) expr1
| Texp_function (pat_expr_list, partial) -> 
  List.exists (fun (p,e) -> has_function_call gamma e) pat_expr_list
| Texp_match (expr1, pat_expr_list, partial) ->
  if has_function_call gamma expr1 then true
  else List.exists (fun (p,e) -> has_function_call gamma e) pat_expr_list
| Texp_tuple (expr_list) -> 
  List.exists (fun e -> has_function_call gamma e) expr_list
| Texp_construct (path, constr_desc, expr_list) ->
 List.exists (fun e -> has_function_call gamma e) expr_list
| Texp_ifthenelse (expr1, then_expr, exprop) -> 
  if has_function_call gamma expr1 then true
  else if has_function_call gamma then_expr then true
       else begin match exprop with
       | None -> false
       | Some else_expr -> has_function_call gamma else_expr
       end
| _ -> raise(Error(Has_function_call_expression_not_handled))

  

let print_validity v = match v with
| EscSyn.Valid -> print_string "valid \n"
| EscSyn.Invalid -> print_string "invalid \n"
| EscSyn.Unknown -> print_string "unknown \n"
| _ -> print_string "other validity\n"

let report_error ppf = function
  | Simpl_expr_Texp_function -> 
      fprintf ppf "Simpl_expr_Texp_function"
  | Simpl_expr_Texp_let ->
      fprintf ppf "Simpl_expr_Texp_let"
  | Simpl_expr_Path_not_handled ->
      fprintf ppf "Simpl_expr_Path_not_handled"
  | Simpl_args_not_function_type ->
      fprintf ppf "Simpl_args_not_function_type"
  | Simpl_args_no_optional_arg ->
      fprintf ppf "Simpl_args_no_optional_arg"
  | Simpl_args_continuation_not_matched ->
      fprintf ppf "Simpl_args_continuation_not_matched"
  | KnownCon_impossible_pattern ->
      fprintf ppf "KnownCon_impossible_pattern"
  | Rebuild_not_function_type ->
      fprintf ppf "Rebuild_not_function_type"
  | Rebuild_pattern_not_matched ->
      fprintf ppf "Rebuild_pattern_not_matched"
  | ErrorExp_Unknownblame ->
      fprintf ppf "ErrorExp_Unknownblame"
  | ErrorExp_Texp_unr ->
      fprintf ppf "ErrorExp_Texp_unr"
  | ErrorExp_function_has_no_argument ->
      fprintf ppf "ErrorExp_function_has_no_argument"
  | ErrorExp_no_pattern_matching ->
      fprintf ppf "ErrorExp_no_pattern_matching"
  | ErrorExp_tuple_no_argument ->
      fprintf ppf " ErrorExp_tuple_no_argument"
  | ErrorExp_constructor_no_argument ->
      fprintf ppf "ErrorExp_constructor_no_argument"
  | ErrorExp_expression_not_handled ->
      fprintf ppf "ErrorExp_expression_not_handled"
  | Check_expression_not_handled ->
      fprintf ppf "Check_expression_not_handled"
  | Has_function_call_expression_not_handled ->
      fprintf ppf "Has_function_call_expression_not_handled"

let rec rmUNR exp =
  let removeUNR expr =  match expr.exp_desc with
  | Texp_ifthenelse (e0, then_e, else_eopt) -> 
    begin match else_eopt with
    | Some e2 -> begin match e2.exp_desc with
      | Texp_unr _ -> rmUNR then_e
      | _ -> expr
      end
    | None -> expr
    end
  | _ -> expr
  in map_expression removeUNR exp

let static_contract_checking env (id, exp) = 
  try
  (* print_string "\n raw expression: \n"; print_expression exp; *)
  let sexp =  simpl_expr (update_name env (Pident id)) exp [] in
  (* print_string "\n simplified expression: \n"; print_expression sexp; *)
  let cleaned_sexp = rmUNR sexp in
  (* print_string "\n rmUNR expression: \n"; print_expression cleaned_sexp; *)
  let (counter_example, has_bad) = check cleaned_sexp in
  let validity = if has_bad 
                 then EscSyn.Unknown
                 else EscSyn.Valid
                 (*  if has_function_call 
                           function_names counter_example 
                      then Unknown
                      else Invalid  *)
  in 
  (* print_string "\n validity: "; print_validity validity; *)
  (* write_tasks_to_file ((Ident.name id)^".why") (tasks env); *)
  let rpt = static_report (id, counter_example) validity in
  fprintf std_formatter "%s" rpt;
  (cleaned_sexp, validity)
  with Error(err) -> report_error std_formatter err;
                     raise (Error(err))



 
