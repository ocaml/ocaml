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
 (match (Ctype.repr exp.exp_type).desc with
   | Tarrow _ -> true
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
   | None -> ([], false)
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
| Texp_ifthenelse (expr0, then_expr, exprop) -> 
    let (e0, b0) = check expr0 in
    if b0 then (e0, b0)
    else 
     let (then_expr1, b1) = check then_expr in
     if b1 then 
       ({exp with exp_desc = Texp_ifthenelse (expr0, then_expr1, None)}, b1)
     else 
       begin match exprop with
       | None -> let desc = Texp_unr (UnknownBlame) in
                 ({exp with exp_desc = desc}, false)
       | Some else_expr -> 
	  let (e2, b3) = check else_expr in
	  if b3 then let desc = Texp_match (expr0, [(falsePat expr0,e2)], Partial) in
		     ({exp with exp_desc = desc}, b3)
                else let desc = Texp_unr (UnknownBlame) in
                 ({exp with exp_desc = desc}, false)
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

type cont =  (* f a1 HOLE an *)
    Cargs of ThmEnv.t (* env for undone args *) 
          * outexp (* function *)
          * (outexp option * optional) list  (* args done *)
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
  | Ccons of ThmEnv.t * expression * (* original expression *)
	     Path.t * constructor_description *
	     outexp list * inexp list (* K(HOLE,e) *)
  | Cin of rec_flag * (pattern * outexp) list (* let x = e1 in HOLE *)
            (* let x1 = a1 
                   x = HOLE
	           x2 = a2
               in e2 *)
  | Clet of ThmEnv.t * rec_flag * (pattern * outexp) list *
	     pattern * (pattern * inexp) list * inexp 
  | Cmatch of (ThmEnv.t * expression (* original env and expression *)
            * inalt list * partial) (* match HOLE with x alts *)
  | Cif of (ThmEnv.t * expression * expression option) (* if HOLE then e1 else e2 *)
  | Cthen of (ThmEnv.t * outexp * inexp option) (* if a0 then HOLE else e2 *)
  | Celse of (outexp * outexp) (* if a0 then a1 else HOLE *)
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
  trace "***esc.simpl_expr" print_expression exp;
  let mkexp desc = {exp with exp_desc = desc} in          
  match exp.exp_desc with  
  | Texp_constant (c) -> rebuild exp k
  | Texp_bad (bl) -> rebuild exp k
  | Texp_unr (bl) -> rebuild exp k
  | Texp_ident (path, vdesc) -> 
      begin match path with
	| Pident id -> 
	    begin
	      let aval = lookup_senv id senv in
	      match aval with
	      | NoVal -> if depth senv = 1
	                 then rebuild (try lookup_nonrec_env id senv
                                       with Not_found -> exp) k
		         else rebuild exp k
	      | _ -> rebuildVar (id, exp) aval k
	    end
	| _ -> rebuild exp k
      end
  | Texp_function (pat_expr_list, ptial) ->
        (* NEW: we generate logic v : <type of v> 
           In alt-ergo, logic v, axiom f, goal g is the same as
           forall v. f -> g *)
      let simpl_list = List.map (fun (p,e) ->           
                       let env1 = add_tasks senv (bound_vars_to_logic p) in
		       (p, simpl_expr env1 e []))
                       pat_expr_list in
      rebuild {exp with exp_desc = Texp_function (simpl_list, ptial)} k
  | Texp_apply (e1, eopt_optl_list) ->
      (* A-normalize the arguments *)
     let rec genLet xs = match xs with
     | [] -> ([],[])
     | (arg::l) -> begin match arg with 
       | (Some e, optl) -> 
	   let (args, lets) = genLet l in
	   if is_expression_argable e
           then ((Some e, optl)::args, lets)
           else let x = Ident.create "e" in
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
     | [] ->  
	 simpl_expr senv e1 (Capp (senv, [], args, exp.exp_type)::k) 
     | _ -> 
	 simpl_expr senv ({exp with exp_desc = Texp_let (Nonrecursive, p_e_list, 
	     	          {exp with exp_desc = Texp_apply(e1, args)})}) k
     end
  | Texp_match (e0, pat_expr_list, ptial) ->       
      begin match k with
      | (Capp (env, [], nds, res_type) :: k1) ->  
          (* (match e0 with pi -> ei) val ==> [matchL] 
             match e0 with pi -> ei val                *)
          let p_e_list = List.map (fun (p,e) -> (p, 
                         { exp_desc = Texp_apply(e, nds);
                           exp_loc  = e.exp_loc;
                           exp_env  = e.exp_env;
			   exp_type = res_type;
                         })) 
                         pat_expr_list in
	  let expr1 = {exp with exp_desc = Texp_match (e0, p_e_list, ptial)} in
          simpl_expr senv expr1 k1
      | (Clet (env, rflag, ds, p, uds, e2) :: k1) -> 
        (* let x = (match e0 with K x -> ei) in e2  ==>  [let-match]
	   match e0 with K x -> let x = ei in e2 *)
	   begin match rflag with
           | Nonrecursive ->
	   let rhs = {exp with exp_desc = Texp_match(e0, 
			     List.map (fun (pi,ei) -> (pi, {ei with exp_desc = 
			 Texp_let (rflag, (p,ei)::uds, e2)})) pat_expr_list, ptial)}
           in simpl_expr senv rhs (Cin (rflag, ds)::k1)
	   | _ -> simpl_expr senv e0 (Cmatch (senv, exp, pat_expr_list, ptial) :: k) 
           end
      | _ -> simpl_expr senv e0 (Cmatch (senv, exp, pat_expr_list, ptial) :: k) 
      end
  | Texp_try (e, pat_expr_list) ->
     simpl_expr senv e ((Ctry pat_expr_list) :: k) 
  | Texp_tuple (es) -> 
     let new_es = List.map (fun e -> simpl_expr senv e []) es in
     rebuild (mkexp (Texp_tuple new_es)) k
  | Texp_construct (path, cdesc, es) -> 
     begin match es with
     | [] -> rebuild exp k
     | (e::l) -> simpl_expr senv e (Ccons(senv, exp, path, cdesc, [], l) :: k)
     end
  | Texp_ifthenelse (e0, e1, e2opt) -> 
     begin match k with
     | (Cif (env, e11, e22opt) :: k1) -> 
       (* [if-if] *)
	 let rhs =  {e1 with exp_desc = Texp_ifthenelse (e0, 
	   { e1 with exp_desc = Texp_ifthenelse (e1, e11, e22opt) },
           match e2opt with
	   | None -> None
           | Some e2 -> Some { e1 with exp_desc = Texp_ifthenelse (e2, e11, e22opt)})} 
	 in simpl_expr env rhs k1    
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
     | _ -> begin match pat_expr_list with
       | [] -> simpl_expr senv expr k
       | (p,e)::l ->  begin match (p.pat_desc, e.exp_desc) with
	       | (Tpat_var (id1), Texp_ident (Pident id2, _)) 
                  when rflag = Nonrecursive && id1 = id2 -> 
		    simpl_expr senv ({exp with exp_desc = Texp_let (rflag, l, expr)}) k
	       | _ -> simpl_expr senv e (Clet (senv, rflag, [], p, l, expr) ::k)
		  end
           end
      end
  | Texp_sequence (e1, e2) -> 
     let new_e1 = simpl_expr senv e1 k in 
     let new_e2 = simpl_expr senv e2 k in 
     rebuild (mkexp (Texp_sequence (new_e1, new_e2))) k
  | Texp_array (es) -> 
     let new_es =  List.map (fun e -> simpl_expr senv e []) es in
     rebuild (mkexp (Texp_array new_es)) k
  | others -> rebuild exp k

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
 | _ ->  rebuild exp k
   end
| _ ->  rebuild exp k

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
          | [] -> rebuild f k 
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
      | _ -> rebuild expr k
      end 
    | Texp_ident(path, vd) -> 
	begin match vd.val_kind with
	| Val_prim _ -> simpl_primitive_app senv expr f ds k 
	| _ -> rebuild expr k
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
    | _ -> rebuild expr k
    end    
| ((Some e, optl) :: l) -> 
    simpl_expr senv e (Cargs (senv, f, ds, optl, l, res_type) :: k)
| ((None, optl) :: l) -> 
    simpl_args senv f (ds@[(None, optl)]) l res_type k
          
and rebuildMatch senv scrut aval alts ptial k = 
   let env1 = extend_denv senv scrut aval in
   let goal_tasks = goalTasks senv in 
   let (_,e) = List.hd alts in
   let new_alts = List.map (fun (p,e) -> 
   let env2 = add_tasks env1 (bound_vars_to_logic p) in
   if is_bad e then
    begin
    let env3 = add_tasks env2 (goal_tasks@[toGoal_pneq scrut p]) in
    let ts2  = tasks env3 in 
    let filename = (Location.toString scrut.exp_loc)^"_match.why" in
    write_tasks_to_file filename ts2;
    begin match askErgo filename ts2 with
    | Valid -> (* this branch is unreachable *)
              (p, {e with exp_desc = Texp_unr (UnknownBlame)})
    | _ -> let env4 = add_tasks env2 [toAxiom_peq scrut p] in           
   	   match (scrut.exp_desc, p.pat_desc) with 
           | (Texp_ident (Pident id1, vd),  Tpat_construct (path, cdesc, [])) -> 
   	 (* if pattern is a constructor with no parameters, 
	    we inline this constructor *)
   	   let new_id = Ident.rename id1 in 
   	   let eid    = {e with exp_desc = Texp_ident(Pident new_id, vd)} in
   	   let senv2  = extend_senv env4 id1 eid in 
   	   let senv3  = extend_denv senv2 eid (Inline ({scrut with exp_desc = 
   					     Texp_construct(path,cdesc,[])})) in
             (p, simpl_expr senv3 e []) 
           | _ -> (p,simpl_expr env4 e [])
     end 
     end 
    else let env4 = add_tasks env2 [toAxiom_peq scrut p] in
         (p,simpl_expr env4 e [])
    ) alts in
   rebuild {e with exp_desc = Texp_match (scrut, new_alts, ptial)} k

and ergo_counter = ref 1

and rebuildIf senv scrut e1 e2opt k = 
    let is_prop = is_expression_prop scrut in
    let mkexp desc = {e1 with exp_desc = desc} in
    let unr_exp = {e1 with exp_desc = Texp_unr (UnknownBlame)} in
    let then_senv = extend_denv senv scrut 
                                (Inline (trueExpression scrut)) in
    let then_axiom = if is_prop then toAxiom scrut 
                     else toAxiom_beq scrut in
    let then_senv2 = add_tasks then_senv [then_axiom] in
    let else_senv = extend_denv senv scrut 
                                (Inline (falseExpression scrut)) in
    let else_axiom = if is_prop then toAxiom_neg scrut 
                     else toAxiom_bneq scrut in
    let else_senv2 = add_tasks else_senv [else_axiom] in
    match is_expression_true scrut with
     | Ptrue -> simpl_expr senv e1 k
     | Pfalse -> begin match e2opt with
       | None -> rebuild (mkexp (Texp_ifthenelse (scrut, unr_exp, None))) k
       | Some e2 -> simpl_expr senv e2 k
       end 
     | Pothers -> begin match e2opt with
       | Some e2 -> begin match e2.exp_desc with
	 | Texp_bad _ when (is_expression_ergoble scrut) -> 
             let goal_tasks = goalTasks senv in        
	     let senv1 = if is_prop
                         then add_tasks senv (goal_tasks@[toGoal scrut])
	                 else add_tasks senv (goal_tasks@[toGoal_beq scrut]) in
	     let ts1   = tasks senv1 in     
             let inits = (Location.toString scrut.exp_loc) in
	     let filename = inits^"_"^(string_of_int !ergo_counter)^"_ifelse.why" in
             ergo_counter := !ergo_counter + 1;
	     write_tasks_to_file filename ts1;
	     (* trace ("call altergo "^filename^": \n") print_expression scrut; *)
	     begin match askErgo filename ts1 with 
	     | Valid -> (* scrutinee is Valid *)
		 (* print_string "Valid"; *)
		 simpl_expr then_senv2 e1 k
	     | _ -> (* we omit the negation check for the moment
		       let senv2 = add_tasks senv (goal_tasks@[toGoal_bneg scrut]) in
		       let ts2 = tasks senv2 in
		       write_tasks_to_file "temp2.why" ts2;
		       match askErgo filename ts2 with
		       | Valid -> (* negation of the scrutinee is Valid *)
		       begin match e2opt with
		       | Some e2 -> 
                       let else_senv2 = add_tasks else_senv 
		       [toAxiom_bneq scrut] in
                       simpl_expr else_senv2 e2 k
		       | None -> rebuild (mkexp (Texp_ifthenelse 
		       ((falseExpression scrut), unr_exp, None))) k
		       end 
		       | _ -> *) (* no validity info about scrutinee *)
		 (* print_string "Don't know"; *)
		 let new_e1 = simpl_expr then_senv2 e1 [] in
		 rebuild (mkexp (Texp_ifthenelse 
					   (scrut, new_e1, e2opt))) k
             end
	 | _ -> 
	     begin match e1.exp_desc with
             | Texp_bad _  when (is_expression_ergoble scrut) -> 
		  let goal_tasks = goalTasks senv in        
	     let senv1 = if is_prop
                         then add_tasks senv (goal_tasks@[toGoal_neg scrut])
	                 else add_tasks senv (goal_tasks@[toGoal_bneq scrut]) in
	     let ts1   = tasks senv1 in     
             let inits = (Location.toString scrut.exp_loc) in
	     let filename = inits^"_"^(string_of_int !ergo_counter)^"_ifthen.why" in
             ergo_counter := !ergo_counter + 1;
	     write_tasks_to_file filename ts1;
	     (* trace ("call altergo "^filename^": \n") print_expression scrut; *)
	     begin match askErgo filename ts1 with 
	     | Valid -> (* scrutinee is Valid *)
		 (* print_string "Valid"; *)
		 simpl_expr else_senv2 e2 k
	     | _ -> (* print_string "Don't know"; *)
		 let new_e2 = simpl_expr else_senv2 e2 [] in
		 rebuild (mkexp (Texp_ifthenelse 
					   (scrut, e1, Some new_e2))) k
             end
	     | _ ->
	     let new_e1 = simpl_expr then_senv2 e1 [] in
             let new_e2 = Some (simpl_expr else_senv2 e2 []) in
	     rebuild (mkexp (Texp_ifthenelse (scrut, new_e1, new_e2))) k
            end
        end
       | None -> 	     
	   let new_e1 = simpl_expr then_senv2 e1 [] in
	   rebuild (mkexp (Texp_ifthenelse (scrut, new_e1, None))) k
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
    print_string "No branch is matched";
    let (p,e) = List.hd xs in
    match p.pat_desc with
    | Tpat_construct (path2, cdesc2, ps) -> 
       let rhs = {e with exp_desc = Texp_let (Nonrecursive, List.combine ps es, e)} in
       simpl_expr senv rhs k
    | _ -> raise(Error(KnownCon_impossible_pattern))   
   

and rebuild a k = 
  trace "***esc.rebuild" print_expression a;
  (* print_string "***esc.rebuild: \n"; print_expression a; print_string "\n"; *)
  match (a.exp_desc, k) with
  | (_, []) -> a
  | (Texp_bad (bl), Cmatch _ :: k1) -> rebuild a k1
  | (Texp_bad (bl), Cif _ :: k1) -> rebuild a k1
  | (Texp_bad (bl), Capp _ :: k1) -> rebuild a k1
  | (Texp_bad (bl), Cargs _ :: k1) -> rebuild a k1
  | (Texp_bad (bl), Cprim _ :: k1) -> rebuild a k1
  | (Texp_bad (bl), Ccons _ :: k1) -> rebuild a k1
  | (Texp_bad (bl), Clet _ :: k1) -> rebuild a k1
  | (Texp_bad (bl), Cin _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Cmatch _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Cif _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Capp _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Cargs _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Cprim _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Ccons _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Clet _ :: k1) -> rebuild a k1
  | (Texp_unr (bl), Cin _ :: k1) -> rebuild a k1
  | (Texp_construct (path, cdesc, es), 
     Cmatch (matchenv, e_match, alts, ptial) :: k1) -> 
      knownCon matchenv (path, cdesc) es alts k1
  | (_, CLam (e, p, ptial) :: k1) -> 
      rebuild {e with exp_desc = Texp_function ([(p,a)], ptial)} k1
  | (_, Cargs (env, f, ds, optl, nds, res_type) :: k1) -> 
      simpl_args env f (ds@[(Some a, optl)]) nds res_type k1
  | (_, Capp (env, ds, nds, res_type) :: k1) -> 
      simpl_args env a ds nds res_type k1
  | (_, Ccons(env, e, path, cdesc, ds, uds) :: k1) -> 
       begin match uds with
       | [] -> if List.for_all is_expression_tvalue ds then
	   begin match a.exp_desc with
	   | Texp_match(e0, pat_exp_list, ptial) ->
	       rebuildMatch env e0 NoVal
		   (List.map (fun (pi,ei) -> (pi,
                {ei with exp_desc=Texp_construct(path, cdesc, ds@[ei])}))
			  pat_exp_list) ptial k1
	   | Texp_ifthenelse(a0, a1, a2opt) -> 
	     let new_a1 = {a1 with exp_desc=Texp_construct(path,cdesc,ds@[a1])} in
             let new_a2opt = match a2opt with
	   | None -> None
	   | Some a2 -> Some ({a2 with exp_desc=Texp_construct(path,cdesc,ds@[a2])}) in
	     rebuildIf env a0 new_a1 new_a2opt k1
	   | Texp_let (rflag, pat_exp_list, expr) ->
	       let e2 = {e with exp_desc=Texp_construct(path,cdesc,ds@[expr])} in
	       rebuild ({expr with exp_desc=Texp_let(rflag, pat_exp_list, e2)}) k1
	   | _ -> 
	       rebuild ({e with exp_desc=Texp_construct(path,cdesc,ds@[a])}) k1
           end
            else rebuild ({e with exp_desc=Texp_construct(path,cdesc,ds@[a])}) k1
       | (x::l) -> 
	   if List.for_all is_expression_tvalue ds then
         begin match x.exp_desc with
	 | Texp_match(e0, pat_exp_list, ptial) -> 
	     let alts = List.map (fun (pi,ei) -> (pi, 
	    {ei with exp_desc=Texp_construct(path, cdesc, ds@[a;ei]@l)}))
		 pat_exp_list in
	     simpl_expr env x (Cmatch (env, x, alts, ptial) :: k1)
	 | Texp_ifthenelse(e0, e1, e2opt) ->
           let new_e1 = {e1 with exp_desc=Texp_construct(path,cdesc,ds@[a;e1]@l)} in
           let new_e2opt = match e2opt with
	   | None -> None
	   | Some e2 -> Some ({e2 with exp_desc=Texp_construct(path,cdesc,ds@[a;e2]@l)}) in
	     simpl_expr env e0 (Cif (env, new_e1, new_e2opt) :: k1)
         | _ -> simpl_expr env x (Ccons(env,e,path,cdesc,ds@[a],l) :: k1)
	 end
       else rebuild ({e with exp_desc=Texp_construct(path,cdesc,ds@[a])}) k1    
   end
  | (_, Clet (senv, rflag, ds, p, uds, e3) :: k1) ->       
      begin match p.pat_desc with
      | Tpat_var (id1) when rflag = Nonrecursive &&
  	  ((is_expression_tvalue a) || (match e3.exp_desc with
	  | Texp_ident(Pident id2, vd) -> id2 = id1 
	  | _ -> false)) ->
              let new_id = Ident.rename id1 in 
	      let vd     = { val_type = a.exp_type; val_kind = Val_reg } in
	      let eid    = {a with exp_desc = Texp_ident(Pident new_id, vd)} in
	      let env2   = extend_senv senv id1 eid in
	      let new_senv = extend_denv env2 eid (Inline a) in
           (* else extend_denv senv {a with exp_desc=Texp_ident(Pident id1, vd)} (Inline a) *)
        
	  begin match uds with
	  | [] -> simpl_expr new_senv e3 (Cin (rflag, ds) :: k1)
	  | ((x,e)::l) ->
	      simpl_expr new_senv e (Clet (new_senv, rflag, ds, x, l, e3) :: k1)
	  end	            
     | _ -> begin match a.exp_desc with
       | Texp_let(rec_flag, pelist, a2) -> 
	   (* let x = (let y = a1 in a2) in e3 ==> [let-let]
	      let y = a1 in let x = a2 in e3 *)
	  begin match uds with
	  | [] ->  	  (* (def_to_axioms [(p,a)]) in *)		    
              let env2 = add_tasks senv (
		                (bound_vars_to_logic p)@[toAxiom_peq a p]) in
	      simpl_expr env2 e3 (Cin (rflag, ds@pelist@[(p,a2)]) :: k1)
            | ((x,e)::l) ->
	      let env2 = add_tasks senv ((bound_vars_to_logic p)@[toAxiom_peq a p]) in
	      simpl_expr env2 e (Clet (env2, rflag, ds@[(p,a2)], x, l, e3) :: k1)
	  end 
       | Texp_ifthenelse (a0, a1, a2opt) when rflag = Nonrecursive ->
	 (* let x1 = ..
                x = (if a0 then a1 else a2) 
                xn = ..
            in e3  ==>  [let-if]
            let x1 = ..
            in  if a0 then let x = a1 
	                       xn = ..
                           in e3 
	              else let x = a2 
	                       xn = ..
                           in e3 *)
           let e1 = match a1.exp_desc with
 	          | Texp_bad _ | Texp_unr _ -> a1
	          | _ -> {a1 with exp_desc = Texp_let (rflag, (p,a1)::uds, e3)} in
	   let e2opt = match a2opt with 
	       | None -> None
	       | Some a2 -> begin match a2.exp_desc with
		 | Texp_bad _ | Texp_unr _ -> a2opt
		 | _ -> Some {a2 with exp_desc = 
			               Texp_let (rflag, (p,a2)::uds, e3)}
	       end
	   in rebuildIf senv a0 e1 e2opt (Cin (rflag, ds) :: k1)
       | Texp_match(a0, pat_exp_list, ptial) when rflag = Nonrecursive ->
	   (* let x1 = ..
                x = match a0 with pi-> ei
                xn = ..
              in e3  ==>  [let-match]
              let x1 = ..
              in match a0 with pi -> let x = a1 
	                                xn = ..
                                    in e3 
           *)
	   let alts = List.map (fun (pi,ai) ->
              (pi, match ai.exp_desc with
	      | Texp_bad _ | Texp_unr _ -> ai
	      | _ -> {ai with exp_desc = Texp_let (rflag, (p,ai)::uds, e3)}))
	        pat_exp_list in
	   rebuildMatch senv a0 NoVal alts ptial (Cin (rflag, ds) :: k1)
       | _ -> 	   
	   let env2 = add_tasks senv ((bound_vars_to_logic p)@[toAxiom_peq a p]) in
	   begin match uds with
	   | [] ->  simpl_expr env2 e3 (Cin (rflag, ds@[(p,a)]) :: k1) 
	   | ((x,e)::l) -> 
	      simpl_expr env2 e (Clet (env2, rflag, ds@[(p,a)], x, l, e3) :: k1)
           end
      end
    end
  | (_, Cin (rflag, ds) :: k1) -> 
      begin match ds with 
      | [] -> rebuild a k1
      | _ -> rebuild ({a with exp_desc = Texp_let (rflag, ds, a)}) k1
      end
  | (_, Cmatch (matchenv, e_match, alts, ptial) :: k1) -> 
       let senv = matchenv in
       begin match lookup_denv a senv with
       | Inline e -> (* if scrutinee reappears, replace it by its aval *)
                     rebuildMatch senv e NoVal alts ptial k1
       | _ -> (* [[ a ]]_(K x) *)
	 begin match a.exp_desc with
	 | Texp_match (a0, pat_exp_list, ptial2) ->
	     (* match (match a0 with pi -> ai) with alts 
                ==> [match-match]
                match a0 with pi -> match ai with alts 
             *)
	     let new_alts = List.map (fun (pi,ai) ->
	       (pi, {e_match with exp_desc=Texp_match(ai,alts,ptial)}))
		 pat_exp_list in
	     rebuildMatch matchenv a0 NoVal new_alts ptial k1
	 | _ -> 
         if a.exp_type = Predef.type_bool 
	   then begin 
             let goal_tasks = goalTasks matchenv in
             let senv1  = begin if is_expression_prop a
	                  then add_tasks senv (goal_tasks@[toGoal a])
                          else add_tasks senv (goal_tasks@[toGoal_beq a]) end in
	     let ts1  = tasks senv1 in
             let filename = (Location.toString a.exp_loc)^"cmatch.why" in 
             match askErgo filename ts1 with
             | Valid -> (* scrutinee is Valid *)
	       write_tasks_to_file filename ts1;
	       rebuildMatch matchenv a (Inline {a with exp_desc = trueExp}) 
                            alts ptial k1
             | _ -> (* omit negation test 
		    let g = goal_tasks@[toGoal_neg a] in
                    let ts2 = tasks (add_tasks senv g) in
               match askErgo filename ts2 with
               | Valid -> (* negation of the scrutinee is Valid *)
 	         rebuildMatch matchenv a (Inline {a with exp_desc = falseExp}) alts ptial k1
               | _ -> *)
	        rebuildMatch matchenv a NoVal alts ptial k1 end
            else rebuildMatch matchenv a NoVal alts ptial k1 
       end
    end
  | (_, Cif (env, e1, e2opt) :: k1) ->       
      begin match lookup_denv a env with
      | Inline e -> (* if scrutinee reappears, replace it by its aval *)
                    rebuildIf env e e1 e2opt k1
      | _ -> begin match a.exp_desc with
        | Texp_let (rflag, pelist, a2) -> 
       (* if (let x = a1 in a2) then .. else ..
           ==> [if-let]
          let x = a1 in if a2 then .. else ..
       *)
	  rebuildIf env a2 e1 e2opt (Cin (rflag, pelist) :: k1)     
	| Texp_ifthenelse (a0, a1, a2opt) -> 
	    (* if (if a0 then a1 else a2) then e1 else e2 
              ==> [if-if]
            if a0 then (if a1 then e1 else e2)
            else (if a2 then e1 else e2)
		*)
	  let new_e1 = {e1 with exp_desc = Texp_ifthenelse(a1,e1,e2opt)} in
          let new_e2opt = match a2opt with
	   | None -> None
           | Some a2 -> Some ({e1 with exp_desc = Texp_ifthenelse(a2,e1,e2opt)})
	  in rebuildIf env a0 new_e1 new_e2opt k1
	| Texp_match(a0, pat_exp_list, ptial) ->
	   (* if (match a0 with pi -> ai) then e1 else e2
             ==> [if-match]
              match a0 with pi -> if ai then e1 else e2
	   *)
	  let alts = List.map (fun (pi,ai) ->
               (pi, {e1 with exp_desc = Texp_ifthenelse(ai,e1,e2opt)})) pat_exp_list in
	  rebuildMatch env a0 NoVal alts ptial k1
        | _ -> rebuildIf env a e1 e2opt k1
        end
      end
  | (_, Cthen (env, a0, e2opt) :: k1) -> 
      begin match e2opt with
      | None ->       
          rebuild ({a with exp_desc = Texp_ifthenelse(a0,a,None)}) k1
      | Some e2 -> 
          simpl_expr env e2 (Celse (a0, a) :: k1)
      end
  | (_, Celse (a0, a1) :: k1) -> 
      rebuild ({a1 with exp_desc = Texp_ifthenelse(a0,a1,Some a)}) k1
  | _ -> raise(Error(Rebuild_pattern_not_matched))
  

and rebuildVar ((n, e):(Ident.t * expression)) 
               (v:aval) (k:cont list) : outexp = 
(* e is the expression form of the ident n *)
    match v with
    | Inline a -> rebuild a k
    | NotCon (cs) -> begin match k with
      | Cmatch (senv, e_match, alts, ptial) :: k1 -> 
	  rebuildMatch senv e (NotCon cs) alts ptial k1
      | _ -> rebuild e k
      end
    | _ -> rebuild e k

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
      ("\t"^(Location.toString loc)^": "^(Ident.name id)^" may not satisfy its postcondition!", [])
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
| Texp_constant _ -> false
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
| Texp_apply (e1, _) -> has_function_call gamma e1
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

let inline_rec id exp exp1 = 
  let expand_id e = match e.exp_desc with
  | Texp_ident(path, vd) ->
     if Path.unique_name path = Ident.unique_name id
       then begin 
	    rename_boundvars Tbl.empty exp1
	    end
       else e
  | _ -> e
  in  map_expression expand_id exp

let rec static_contract_checking env contract_flag (id, exp) = 
  try
  trace "raw expression" print_expression exp; 
  let senv = update_name env (Pident id) in
  let sexp =  simpl_expr senv exp [] in
  trace "simplified expression" print_expression sexp; 
  let cleaned_sexp = rmUNR sexp in
  (* trace "***residual code" print_expression cleaned_sexp; *)
  let (counter_example, has_bad) = check cleaned_sexp in
  if has_bad 
      then begin
	   let validity = EscSyn.Unknown in
	   let rpt = static_report (id, counter_example) validity in 
            print_string ("At depth "^string_of_int(depth env)^", ");
            fprintf std_formatter "%s" rpt; 
	   match ThmEnv.depth env with
	   | 0 -> 
	    let new_senv = increase_depth env in  
            static_contract_checking new_senv contract_flag (id,sexp)
	   | 1 ->  
	    let the_exp = lookup_rec_env id env in
            let expanded_exp = inline_rec id sexp the_exp in
	    let new_senv = increase_depth env in  
            static_contract_checking new_senv contract_flag (id,expanded_exp)
           | _ -> 
               (cleaned_sexp, validity)
           end
      else
       let validity =  EscSyn.Valid in
       let rpt = static_report (id, counter_example) validity in
       print_string ("At depth "^string_of_int(depth env)^", ");
       fprintf std_formatter "%s" rpt; 
       (cleaned_sexp, validity)
  with Error(err) -> report_error std_formatter err;
                     raise (Error(err))



 
