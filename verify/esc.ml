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

let ergo_counter = ref 1

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
    | Some e -> ({exp with exp_desc = 
		      Texp_construct (path, constr_desc, [e])}, true)
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
| Texp_sequence (e1, e2) -> 
    let (a1, b1) = check e1 in
    let (a2, b2) = check e2 in
    if b1 then (a1, b1) 
          else if b2 then (a2, b2) 
	             else ({exp with exp_desc = Texp_sequence (e1, e2)}, false)
| _ -> begin print_expression [] exp; raise(Error (Check_expression_not_handled)) end


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
          * pattern * partial (* \x.HOLE *)
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

let cont_to_string c = match c with
    | Cargs _ -> "Cargs"
    | Capp _ -> "Capp"
    | CLam _ -> "CLam"
    | Cprim _ -> "Cprim"
    | Ccons _ -> "Ccons"
    | Cin _ -> "Cin"
    | Clet _ -> "Clet"
    | Cmatch _ -> "Cmatch"
    | Cif _ -> "Cif"
    | Cthen _ -> "Cthen"
    | Celse _ -> "Celse"
    | Ctry _ -> "Ctry"

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
   the logical store. The senv is the one in thmEnv.ml  *)

let rec simpl_expr (senv : ThmEnv.t) 
                   (exp : expression) (k: cont list) : expression =
  let t_decls = type_decls senv in
  trace "***esc.simpl_expr" (print_expression t_decls) exp;
  let mkexp desc = {exp with exp_desc = desc} in          
  match exp.exp_desc with 
  | Texp_constant (c) -> rebuild senv exp k
  | Texp_bad (bl) -> rebuild senv exp k
  | Texp_unr (bl) -> rebuild senv exp k
  | Texp_ident (path, vdesc) -> 
      begin match path with
	| Pident id -> 
	    begin
	      let aval = lookup_senv id senv in
	      match aval with
	      | NoVal -> 
		  if depth senv = 1
	          then
                  try
                  let dfn = lookup_nonrec_env id senv in
		  mtrace "[inline non_rec]";
		  rebuild senv (rename_boundvars [] dfn) k
		  with Not_found -> rebuild senv exp k
		  else rebuild senv exp k
	      | _ -> rebuildVar senv (id, exp) aval k
	    end
	| _ -> rebuild senv exp k
      end
  | Texp_function (pat_expr_list, ptial) ->
        (* NEW: we generate logic v : <type of v> 
           In alt-ergo, logic v, axiom f, goal g is the same as
           forall v. f -> g *)
      begin match k with
      | Capp (env, ds, nds, res_type) :: k1 -> 
	  (* [going to beta] *)
	  simpl_args senv exp ds nds res_type k1 
      | _ -> 
      let simpl_list = List.map (fun (p,e) ->           
                       let env1 = add_tasks senv (bound_vars_to_logic p) in
		       (p, simpl_expr env1 e []))
                       pat_expr_list in
      rebuild senv {exp with exp_desc = Texp_function (simpl_list, ptial)} k
     end
  | Texp_apply (e1, eopt_optl_list) ->
      (* A-normalize the arguments *)
     begin match e1.exp_desc with
     | Texp_function([(p,e)], optl) ->
	 simpl_args senv e1 [] eopt_optl_list e.exp_type k
     | Texp_match(e0, alts, ptial) -> 
	 (* (match e0 with pi -> ei) e ==> [matchL] 
            match e0 with pi -> ei e                *)
	  mtrace "[simpl matchL]";
          let new_alts = List.map (fun (pi,ei) -> (pi, 
                      {exp with exp_desc = Texp_apply(ei, eopt_optl_list)}))
                         alts in
	  let rhs = {exp with exp_desc = Texp_match (e0, new_alts, ptial)} in
          simpl_expr senv rhs k
     | Texp_let(rflag, pat_exp_list, e2) -> 
       (* (let x = e1 in \y.e') e ==> let x = e1 in (\y.e') e *)
       mtrace "[simpl letL]";
       let new_e2 = { exp with exp_desc = Texp_apply(e2, eopt_optl_list) } in
       simpl_expr senv ({exp with exp_desc = Texp_let(rflag, pat_exp_list, new_e2)}) k
     | Texp_ifthenelse(e0, e1, e2opt) -> 
       (* (if e0 then \y.e1 else \y.e2) e ==> [ifL]
          if e0 then (\y.e1) a else (\y.e2) e *)
       mtrace "[simpl ifL]";
       let new_e1 = { exp with exp_desc = Texp_apply(e1, eopt_optl_list) } in
       let new_e2opt = match e2opt with
       | None -> None 
       | Some e2 -> Some {exp with exp_desc = 
			      Texp_apply(e2, eopt_optl_list) } in
       let rhs = {exp with exp_desc = Texp_ifthenelse(e0, new_e1, new_e2opt)} in
       simpl_expr senv rhs k
     | _ -> 
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
    end
  | Texp_match (scrut, pat_expr_list, ptial) ->       
      begin match scrut.exp_desc with
      | Texp_let(rflag, pelist, e2) -> 
        (* match (let x = e1 in e2) with alts
              ==> [match-let]
           let x = e1 in match e2 with alts
         *)
	 mtrace "[simpl match-let]";
         let rhs = {exp with exp_desc = Texp_let(rflag, pelist,
		   {exp with exp_desc = Texp_match (e2, pat_expr_list, ptial)})} in
	 simpl_expr senv rhs k
      | Texp_ifthenelse(e0, e1, e2opt) ->
	  (* match (if e0 then e1 else e2) with alts
	     ==> [match-if]
	     if e0 then match e1 with alts
	     else match e2 with alts
	   *)
	  mtrace "[simpl match-if]";
	  let new_e1 = match e1.exp_desc with
	  | Texp_bad _ | Texp_unr _ -> e1
	  | _ -> {exp with exp_desc = 
			    Texp_match(e1,pat_expr_list,ptial)} in
          let new_e2opt = match e2opt with
          | None -> None
          | Some e2 -> begin match e2.exp_desc with
	    | Texp_bad _ | Texp_unr _ -> e2opt
	    | _ -> Some {exp with exp_desc = 
			     Texp_match(e2,pat_expr_list,ptial)}
                       end  in
          simpl_expr senv e0 (Cif(senv, new_e1, new_e2opt) :: k)
      | _ -> simpl_expr senv scrut 
	         (Cmatch (senv, exp, pat_expr_list, ptial) :: k) 
      end
  | Texp_try (e, pat_expr_list) ->
     simpl_expr senv e ((Ctry pat_expr_list) :: k) 
  | Texp_tuple (es) -> 
     let new_es = List.map (fun e -> simpl_expr senv e []) es in
     rebuild senv (mkexp (Texp_tuple new_es)) k
  | Texp_construct (path, cdesc, es) -> 
     begin match es with
     | [] -> rebuild senv exp k
     | (e::l) -> simpl_expr senv e (Ccons(senv, exp, path, cdesc, [], l) :: k)
     end
  | Texp_ifthenelse (e0, e1, e2opt) -> 
     begin match e0.exp_desc with
     | Texp_let(rflag, pelist, e3) -> 
       (* if (let x = e1 in e3) then .. else ..
           ==> [if-let]
          let x = e1 in if e3 then .. else ..
       *)
	 mtrace "[simpl if-let]";
	 let rhs = {exp with exp_desc = Texp_let(rflag, pelist, 
		   {exp with exp_desc = Texp_ifthenelse(e3, e1, e2opt)})} in
	simpl_expr senv rhs k
     | Texp_ifthenelse (e10, e11, e12opt) ->
        (* if (if e10 then e11 else e12) then e1 else e2 
              ==> [if-if]
            if e10 then (if e11 then e1 else e2)
            else (if e12 then e1 else e2)
	 *)
	 mtrace "[simpl if-if]";
	 let new_e1 = match e11.exp_desc with
           | Texp_bad _ | Texp_unr _ -> e11
           | _ -> {e1 with exp_desc = Texp_ifthenelse (e11, e1, e2opt) } in
	 let new_e2opt = match e12opt with
	   | None -> None
           | Some e12 -> begin match e12.exp_desc with
	       | Texp_bad _ | Texp_unr _ -> e12opt
	       | _ -> Some {e1 with exp_desc = Texp_ifthenelse (e12, e1, e2opt)} 
	       end in
	 simpl_expr senv e10 (Cif (senv, new_e1, new_e2opt) :: k)
     | Texp_match (e10, alts, ptial) -> 
        (* if (match e10 with pi -> ei) then e1 else e2
             ==> [if-match]
              match e10 with pi -> if ei then e1 else e2
	   *)
        mtrace "[simpl if-match]";
        let new_alts = List.map (fun (pi,ei) ->
            (pi, {e1 with exp_desc = 
		  Texp_ifthenelse(ei,e1,e2opt)})) alts in
        simpl_expr senv e10 (Cmatch (senv, e1, new_alts, ptial) :: k) 
     | _ -> simpl_expr senv e0 (Cif (senv, e1, e2opt) :: k) 
     end
  | Texp_let (rflag, pat_exp_list, e3) ->     
     begin match pat_exp_list with
       | [] -> simpl_expr senv e3 k
       | (p,e)::l ->  begin match (p.pat_desc, e.exp_desc) with
	 | (Tpat_var (id1), Texp_ident (Pident id2, _)) 
           when rflag = Nonrecursive && id1 = id2 ->
             (* let x = x in e ==> [unused binding] e *)
             mtrace "[unused binding]";
	     simpl_expr senv ({exp with exp_desc = Texp_let (rflag, l, e3)}) k
	 | (Tpat_var (id1), _) when  rflag = Nonrecursive &&
	     (match e3.exp_desc with
	       | Texp_ident(Pident id2, vd) -> id2 = id1 
	       | _ -> false) -> 
	   (* let x = e in x ==> [redundant binding] e *)
            mtrace "[redundant binding]";
	    simpl_expr senv e k
	 | (_, Texp_let (rflag2, pelist2, e2)) when rflag = Nonrecursive ->
	  (* let x = (let y = e1 in e2) in e3 ==> [let-let] 
             let y = e1 in let x = e2 in e3
           *) 
	  mtrace "[simpl let-let]";
          simpl_expr senv {e with exp_desc=Texp_let(rflag2, pelist2,
                 {e3 with exp_desc=Texp_let(rflag, (p,e2)::l, e3)})} k
	 | (_, Texp_ifthenelse (e0, e1, e2opt)) when rflag = Nonrecursive ->
	     (* let x = (if e0 then e1 else e2) in e3
		==> [let-if]
		if e0 then let x=e1 in e3
		      else let x=e2 in e3
	      *)
	     mtrace "[simpl let-if]";
	     let new_e1 =  match e1.exp_desc with
 	          | Texp_bad _ | Texp_unr _ -> e1
	          | _ -> 
		    {e1 with exp_desc = Texp_let (rflag, (p,e1)::l, e3)} in
	     let new_e2opt = match e2opt with 
	       | None -> None
	       | Some e2 -> begin match e2.exp_desc with
		 | Texp_bad _ | Texp_unr _ -> e2opt
		 | _ -> Some {e2 with exp_desc = 
			               Texp_let (rflag, (p,e2)::l, e3)}
	       end
	     in
	     simpl_expr senv e0 (Cif (senv, new_e1, new_e2opt) :: k)
	| (_, Texp_match (e0, alts, ptial)) when rflag = Nonrecursive -> 
	     (* let x = (match e0 with pi -> ei) in e3
		==> [let-match]
		match e0 with pi -> let x=ei in e3
	      *)
	 mtrace "[simpl let-match]";
         let new_alts = List.map (fun (pi,ei) ->
              (pi, match ei.exp_desc with
	      | Texp_bad _ | Texp_unr _ -> ei
	      | _ -> {ei with exp_desc = Texp_let (rflag, (p,ei)::l, e3)}))
	        alts in
	 simpl_expr senv e0 (Cmatch(senv, e3, new_alts, ptial) :: k)
       | _ -> simpl_expr senv e (Clet (senv, rflag, [], p, l, e3) ::k)
       end
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
| (Some a1, optl1)::(Some a2, optl2)::l ->   
  begin match (get_primitive_op f, is_expression_true a1, is_expression_true a2) with
  | ("&&", Ptrue, _) -> rebuild senv a2 k
  | ("&&", Pfalse, _) -> rebuild senv a1 k
  | ("&&", _, Ptrue) -> rebuild senv a1 k
  | ("&&", _, Pfalse) -> rebuild senv a1 k
  | ("||", Ptrue, _) -> rebuild senv a1 k
  | ("||", Pfalse, _) -> rebuild senv a2 k
  | ("||", _, Ptrue) -> rebuild senv a2 k
  | ("||", _, Pfalse) -> rebuild senv a1 k
  | _ -> 
  begin match a1.exp_desc with
  | Texp_let (rflag, pat_exp_list, e2) -> 
  (* \x.e (let x = e1 in e2) ==> let x = e1 in \x.e e2 *)
  mtrace "[letR]";
      begin 
      let rhs = Texp_apply (f, (Some e2, optl1)::(Some a2, optl2)::l) in
      simpl_expr senv ({exp with exp_desc = Texp_let (rflag, pat_exp_list,
	{exp with exp_desc = rhs})}) k
      end
  | Texp_match (e0, pat_exp_list, ptial) -> 
  (* \x.e (match e0 with Ki xi -> ei) ==> match e0 with Ki xi -> \x.e ei 
     since all variables are fresh, no shadowing in \x.e ei *)
      mtrace "[lam matchR]";
      simpl_expr senv ({exp with exp_desc = Texp_match (e0, 
      List.map (fun (p,e) -> 
	let rhs = Texp_apply (f, (Some e, optl1)::(Some a2, optl2)::l) in
	(p, {exp with exp_desc = rhs})) pat_exp_list, ptial)}) k
  | Texp_ifthenelse (e0, e1, e2opt) -> 
  (* \x.e (if e0 then e1 else e2) ==> if e0 then \x.e e1 else \x.e e2 *)
      mtrace "[ifR]";
      begin match e2opt with
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
	mtrace "[letR]";
	let rhs = Texp_apply (f, (Some a1, optl1)::(Some e2, optl2)::l) in
	simpl_expr senv ({exp with exp_desc = Texp_let (rflag, pat_exp_list,
					      {exp with exp_desc = rhs})}) k
    | Texp_match (e0, pat_exp_list, ptial) -> 
	mtrace "[matchR]";
	simpl_expr senv ({exp with exp_desc = Texp_match (e0, 
			List.map (fun (p,e) ->
	let rhs = Texp_apply (f, (Some a1, optl1)::(Some e, optl2)::l) in
	(p, {exp with exp_desc = rhs})) pat_exp_list, ptial)}) k
   | Texp_ifthenelse (e0, e1, e2opt) -> 
	mtrace "[ifR]";
       begin match e2opt with
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
                begin
		let env3 = extend_denv env2 eid (Inline a) in 
                (* beta reduction: (\x.e1) val ==> e1[val/x]  *)
		mtrace "[beta]";
                simpl_expr env3 e k
		end
	      else
		(* (\x.e1) e2 ==> let x = e2 in e1 *)
		begin 
		mtrace "[lam to let]";
                simpl_expr senv {expr with exp_desc = 
			       Texp_let (Nonrecursive, [(p,a)], e)} k  
                end
	  | (Some a, optl)::rest -> 
              if is_expression_tvalue a then
		let env3 = extend_denv env2 eid (Inline a) in 
                (* beta reduction: (\x.e1) val ==> e1[val/x]  *)
		mtrace "[beta]";
                simpl_args env3 e rest [] e.exp_type k
	      else
		(* (\x.e1) e2 ==> let x = e2 in e1 *)
		simpl_args senv {expr with exp_desc = 
				 Texp_let (Nonrecursive, [(p,a)], e)} 
		           rest [] e.exp_type k
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
    | Texp_match(e0, pat_exp_list, ptial) -> 
	(* (match e0 with pi -> ei) a ==> [matchL] 
            match e0 with pi -> ei a               *)
	mtrace "[matchL]";
       let new_alts = List.map (fun (pi,ei) -> (pi, 
                      {expr with exp_desc = Texp_apply(ei, ds)}))
                         pat_exp_list in
       let rhs = {expr with exp_desc = Texp_match (e0, new_alts, ptial)} in
       simpl_expr senv rhs k
    | Texp_let(rflag, pat_exp_list, e2) -> 
	(* (let x = e1 in \y.e') a ==> let x = e1 in (\y.e') a *)
       mtrace "[letL]";
       let new_e2 = { expr with exp_desc = Texp_apply(e2, ds) } in
       simpl_expr senv ({expr with exp_desc = Texp_let(rflag, pat_exp_list, new_e2)}) k
    | Texp_ifthenelse(e0, then_e, else_eopt) -> 
	(* (if e0 then \y.e1 else \y.e2) a ==> if e0 then (\y.e1) a else (\y.e2) a *)
	mtrace "[ifL]";
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

and simpl_cons senv exp original_cons path cdesc ds uds k = match uds with
       | [] -> 
        if List.for_all is_expression_tvalue ds then
         begin match exp.exp_desc with
	 | Texp_match(a0, pat_exp_list, ptial) -> 
	    mtrace "[push cons to match]";
	    let alts = List.map (fun (pi,ei) -> (pi, 
	    {exp with exp_desc=Texp_construct(path, cdesc, ds@[ei]@uds)}))
		 pat_exp_list in
	    rebuild senv {original_cons with exp_desc =
	                     Texp_match(a0, alts, ptial)} k
	 | Texp_ifthenelse(e0, e1, e2opt) ->
	   mtrace "[push cons to ifthenelse]";
           let new_e1 = {e1 with exp_desc=Texp_construct(path,cdesc,ds@[e1]@uds)} in
           let new_e2opt = match e2opt with
	   | None -> None
	   | Some e2 -> Some ({e2 with exp_desc=Texp_construct(path,cdesc,ds@[e2]@uds)}) in
	     rebuild senv {original_cons with exp_desc =
 	                      Texp_ifthenelse(e0, new_e1, new_e2opt)} k
	  | Texp_let (rflag, pat_exp_list, expr) -> 
	   mtrace "[push cons to let]";
	   let e2 = {original_cons with exp_desc=Texp_construct(path,cdesc,ds@[expr]@uds)} in
           let t_decls = type_decls senv in
	   let new_senv = List.fold_right (fun (pi, ai) default ->
	       add_tasks default ((bound_vars_to_logic pi)@[toAxiom_peq t_decls ai pi])) pat_exp_list senv in
	   rebuild new_senv e2 (Cin (rflag, pat_exp_list) :: k)
         | _ -> 
	 rebuild senv ({original_cons with 
	                    exp_desc=Texp_construct(path,cdesc,ds@[exp])}) k
	 end
       else 
         rebuild senv ({original_cons with 
	                    exp_desc=Texp_construct(path,cdesc,ds@[exp])}) k
       | (e::l) -> 
	 if List.for_all is_expression_tvalue ds then
         begin match exp.exp_desc with
	 | Texp_match(a0, pat_exp_list, ptial) -> 
	    mtrace "[push cons to match]";
	    let alts = List.map (fun (pi,ei) -> (pi, 
	    {exp with exp_desc=Texp_construct(path, cdesc, ds@[ei]@uds)}))
		 pat_exp_list in
	    simpl_expr senv {original_cons with exp_desc =
	                     Texp_match(a0, alts, ptial)} k
	 | Texp_ifthenelse(e0, e1, e2opt) ->
	   mtrace "[push cons to ifthenelse]";
           let new_e1 = {e1 with exp_desc=Texp_construct(path,cdesc,ds@[e1]@uds)} in
           let new_e2opt = match e2opt with
	   | None -> None
	   | Some e2 -> Some ({e2 with exp_desc=Texp_construct(path,cdesc,ds@[e2]@uds)}) in
	     simpl_expr senv {original_cons with exp_desc =
 	                      Texp_ifthenelse(e0, new_e1, new_e2opt)} k
	  | Texp_let (rflag, pat_exp_list, expr) -> 
	   mtrace "[push cons to let]";
	   let e2 = {original_cons with exp_desc=Texp_construct(path,cdesc,ds@[expr]@uds)} in
           let t_decls = type_decls senv in
	   let new_senv = List.fold_right (fun (pi, ai) default ->
	       add_tasks default ((bound_vars_to_logic pi)@[toAxiom_peq t_decls ai pi])) pat_exp_list senv in
	   simpl_expr new_senv e2 (Cin (rflag, pat_exp_list) :: k)
         | _ -> 
	 simpl_expr senv e (Ccons(senv, original_cons, path, cdesc, (ds@[exp]), l):: k)
	 end
       else 
       simpl_expr senv e (Ccons(senv, original_cons, path, cdesc, (ds@[exp]), l) ::  k)
       
   
and rebuildMatch senv scrut aval alts ptial k = 
   let goal_tasks = goalTasks senv in 
   let (_,e) = List.hd alts in
   let t_decls = type_decls senv in
   let new_alts = List.map (fun (p,e) ->     
   if is_bad e then
    begin
    let env2 = add_tasks senv (bound_vars_to_logic p) in
    let env5 = add_tasks env2 (goal_tasks@[toGoal_pneq t_decls scrut p]) in
    let ts2  = tasks env5 in 
    let inits = (Location.toString scrut.exp_loc) in
    let filename = (inits^"_"^(string_of_int !ergo_counter)^"_match.why") in
    ergo_counter := !ergo_counter + 1;
    write_tasks_to_file filename ts2;
    match askErgo filename ts2 with
    | Valid -> (* it means BAD is not reachable *)
              (p, {e with exp_desc = Texp_unr (UnknownBlame)})
    | _ -> (p,e) 
    end
    else begin   
      let env2 = add_tasks senv (bound_vars_to_logic p) in
      let env3 = add_tasks env2 [toAxiom_peq t_decls scrut p] in  
      match scrut.exp_desc with
	| (Texp_ident(Pident id, vd)) -> 
	    let new_id = Ident.rename id in
	    let eid = {scrut with exp_desc= Texp_ident(Pident new_id, vd)} in  
	    let senv2 = extend_senv env3 id eid in
	    let senv3 = extend_denv senv2 eid (Inline (pattern_to_expression p)) in
	    (p, simpl_expr senv3 e []) 
	| _ ->  
	    (p, simpl_expr env3 e [])
      end
    ) alts in
   rebuild senv {e with exp_desc = Texp_match (scrut, new_alts, ptial)} k

and rebuildIf senv scrut e1 e2opt k = 
    let is_prop = is_expression_prop scrut in
    let mkexp desc = {e1 with exp_desc = desc} in
    let unr_exp = {e1 with exp_desc = Texp_unr (UnknownBlame)} in
    let t_decls = type_decls senv in
    let then_senv = extend_denv senv scrut 
                                (Inline (trueExpression scrut)) in
    let then_axiom = if is_prop then toAxiom t_decls scrut 
                     else toAxiom_beq t_decls scrut in
    let then_senv2 = add_tasks then_senv [then_axiom] in
    let else_senv = extend_denv senv scrut 
                                (Inline (falseExpression scrut)) in
    let else_axiom = if is_prop then toAxiom_neg t_decls scrut 
                     else toAxiom_bneq t_decls scrut in
    let else_senv2 = add_tasks else_senv [else_axiom] in
    match is_expression_true scrut with
     | Ptrue -> simpl_expr senv e1 k
     | Pfalse -> begin match e2opt with
       | None -> rebuild senv (mkexp (Texp_ifthenelse (scrut, unr_exp, None))) k
       | Some e2 -> simpl_expr senv e2 k
       end 
     | Pothers -> begin match e2opt with
       | Some e2 -> begin match e2.exp_desc with
	 | Texp_bad _ when (is_expression_ergoble scrut) -> 
             let goal_tasks = goalTasks senv in        
	     let senv1 = if is_prop
                         then add_tasks senv (goal_tasks@[toGoal t_decls scrut])
	                 else add_tasks senv (goal_tasks@[toGoal_beq t_decls scrut]) in
	     let ts1   = tasks senv1 in     
             let inits = (Location.toString scrut.exp_loc) in
	     let filename = (inits^"_"^(string_of_int !ergo_counter)^"_ifelse.why") in
             ergo_counter := !ergo_counter + 1;
	     write_tasks_to_file filename ts1;
	     mtrace ("--> call alt-ergo "^filename); 
	     begin match askErgo filename ts1 with 
	     | Valid -> (* scrutinee is Valid *)
		begin mtrace ("<-- Valid");
		      simpl_expr then_senv2 e1 k
                end
	     | _ -> begin
		 mtrace ("<-- Do not know");
		 let new_e1 = simpl_expr then_senv2 e1 [] in
		 rebuild senv (mkexp (Texp_ifthenelse 
					   (scrut, new_e1, e2opt))) k
                end
             end
	 | _ -> 
	     begin match e1.exp_desc with
             | Texp_bad _  when (is_expression_ergoble scrut) -> 
	     let goal_tasks = goalTasks senv in        
	     let senv1 = if is_prop
                         then add_tasks senv (goal_tasks@[toGoal_neg t_decls scrut])
	                 else add_tasks senv (goal_tasks@[toGoal_bneq t_decls scrut]) in
	     let ts1   = tasks senv1 in     
             let inits = (Location.toString scrut.exp_loc) in
	     let filename = inits^"_"^(string_of_int !ergo_counter)^"_ifthen.why" in
             ergo_counter := !ergo_counter + 1;
	     write_tasks_to_file filename ts1;
	     mtrace ("--> call alt-ergo "^filename);
	     begin match askErgo filename ts1 with 
	     | Valid -> (* scrutinee is Valid *)
		 mtrace "<-- Valid"; 
		 simpl_expr else_senv2 e2 k
	     | _ -> 
		 mtrace "<-- Don't know"; 
		 let new_e2 = simpl_expr else_senv2 e2 [] in
		 rebuild senv (mkexp (Texp_ifthenelse 
					   (scrut, e1, Some new_e2))) k
             end
	     | _ ->
	     let new_e1 = simpl_expr then_senv2 e1 [] in
             let new_e2 = Some (simpl_expr else_senv2 e2 []) in
	     rebuild senv (mkexp (Texp_ifthenelse (scrut, new_e1, new_e2))) k
            end
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

and rebuild senv a k = 
  let t_decls = type_decls senv in
  trace ("***esc.rebuild"^(match k with
         | [] -> "[]"
	 | _ -> cont_to_string (List.hd k))) (print_expression t_decls) a;
  match (a.exp_desc, k) with
  | (_, []) -> a
  | (Texp_bad (bl), Cmatch _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Cif _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Capp _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Cargs _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Cprim _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Ccons _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Clet _ :: k1) -> rebuild senv a k1
  | (Texp_bad (bl), Cin _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cmatch _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cif _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Capp _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cargs _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cprim _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Ccons _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Clet _ :: k1) -> rebuild senv a k1
  | (Texp_unr (bl), Cin _ :: k1) -> rebuild senv a k1
  | (Texp_construct (path, cdesc, es), 
     Cmatch (matchenv, e_match, alts, ptial) :: k1) -> 
      mtrace "[knownCon]";
      let t_decls = type_decls matchenv in
      let (_,e) = List.hd alts in
      let rec knownCon xs = match xs with
          | [] -> rebuild senv {e with exp_desc=Texp_unr (UnknownBlame)} k1
	  | (p,e)::l -> 
	    begin  match p.pat_desc with
 	   | Tpat_construct (path2, cdesc2, ps) 
	      when cdesc.cstr_tag = cdesc2.cstr_tag ->
	     let pelist = List.combine ps es in
	     let senv2 = List.fold_right (fun (pi,ai) default -> 
               match pi.pat_desc with
 	       | Tpat_var(id) ->
	       let new_id = Ident.rename id in
	       let vd = { val_type = ai.exp_type; val_kind = Val_reg } in
	       let eid = {a with exp_desc = Texp_ident(Pident new_id, vd)} in 
	       let env2 = extend_senv default id eid in
               extend_denv env2 eid (Inline ai)
               | _ -> default
               ) pelist matchenv in
	     let senv3 = List.fold_right (fun (pi,ai) default -> 
	       add_tasks default ((bound_vars_to_logic pi)@[toAxiom_peq t_decls ai pi]))
	         pelist senv2  in
	     simpl_expr senv3 e (Cin (Nonrecursive, pelist)::k1) 
	     	 | _ -> knownCon l
             end
      in knownCon alts
  | (Texp_tuple(es), 
     Cmatch (matchenv, e_match, alts, ptial) :: k1) -> 
       mtrace "[knownTuple]";
       let t_decls = type_decls matchenv in
       let (_,e) = List.hd alts in
       let rec knownTuple xs = match xs with
       | [] -> rebuild senv {e with exp_desc = Texp_unr (UnknownBlame)} k1
       | (p,e)::l -> begin match p.pat_desc with
	 | Tpat_tuple (ps) ->
	     let pelist = List.combine ps es in
	     let senv2 =  List.fold_right (fun (pi,ai) default -> 
               match pi.pat_desc with
 	       | Tpat_var(id) -> 
	       let new_id = Ident.rename id in
	       let vd = { val_type = ai.exp_type; val_kind = Val_reg } in
	       let eid = {a with exp_desc = Texp_ident(Pident new_id, vd)} in 
	       let env2 = extend_senv default id eid in
               extend_denv env2 eid (Inline ai)
               | _ -> default
               ) pelist matchenv in
	     let senv3 = List.fold_right (fun (pi,ai) default -> 
		add_tasks default ((bound_vars_to_logic pi)@[toAxiom_peq t_decls ai pi]))
	        pelist senv2  in
	     simpl_expr senv3 e (Cin (Nonrecursive, pelist)::k1)
	 | _ -> knownTuple l
	    end
      	in knownTuple alts
  | (_, CLam (e, p, ptial) :: k1) -> 
      rebuild senv {e with exp_desc = Texp_function ([(p,a)], ptial)} k1
  | (_, Cargs (env, f, ds, optl, nds, res_type) :: k1) -> 
      begin 
	match a.exp_desc with
	| Texp_match(a0, pat_exp_list, ptial) -> 
	 (* f (match a0 with pi -> ai) ==> match e0 with pi -> f ai *)
	 mtrace "[var matchR]";
	 let alts = List.map (fun (pi, ai) -> 
	   (pi, {exp_desc = Texp_apply(f, ds@[(Some ai, optl)]@nds);
                 exp_type = res_type;
                 exp_loc = ai.exp_loc;
	         exp_env = ai.exp_env}))
              pat_exp_list in
         rebuildMatch env a0 NoVal alts ptial k1
	| _ -> 
	    simpl_args env f (ds@[(Some a, optl)]) nds res_type k1
      end
  | (_, Capp (env, ds, nds, res_type) :: k1) -> 
      simpl_args env a ds nds res_type k1
  | (_, Ccons(env, e, path, cdesc, ds, uds) :: k1) -> 
      simpl_cons env a e path cdesc ds uds k1  
  | (_, Clet (senv, rflag, ds, p, uds, e3) :: k1) ->       
      let t_decls = type_decls senv in
      begin match p.pat_desc with
      | Tpat_var (id1) when rflag = Nonrecursive &&
  	  ((is_expression_tvalue a) ||
	   (match e3.exp_desc with
	  | Texp_ident(Pident id2, vd) -> id2 = id1 
	  | _ -> false)) ->
	      trace "[inline]" (print_expression t_decls) a;
              let new_id = Ident.rename id1 in 
	      let vd     = { val_type = a.exp_type; val_kind = Val_reg } in
	      let eid    = {a with exp_desc = Texp_ident(Pident new_id, vd)} in
	      let env2   = extend_senv senv id1 eid in
	      let new_senv = extend_denv env2 eid (Inline a) in
	  begin match uds with
	  | [] -> simpl_expr new_senv e3 (Cin (rflag, ds) :: k1)
	  | ((x,e)::l) ->
	      simpl_expr new_senv e (Clet (new_senv, rflag, ds, x, l, e3) :: k1)
	  end	            
     | _ -> begin match a.exp_desc with
       | Texp_let(rec_flag, pelist, a2) when rec_flag = Nonrecursive -> 
	  (* let x1 = ..                   [ds]
	         x = (let y = a1 in a2)   
                 xn = ...                  [uds]
             in e3 ==> [let-let] 
             let x1 = .. in
	     let y = a1 in 
	     let x = a2 in 
	     let xn = .. in
             e3
          *) 
	  mtrace "[let-let]";
	  begin match uds with
	  | [] ->  	  (* (def_to_axioms [(p,a)]) in *)		    
              let new_senv = List.fold_right (fun (pi,ai) default -> 
		add_tasks default ((bound_vars_to_logic pi)@[toAxiom_peq t_decls ai pi]))
	        pelist senv  in
	      rebuild new_senv a2 (Clet(new_senv,rflag, ds@pelist, p, [], e3) :: k1)
          | ((x,e)::l) ->
	      let new_senv =  List.fold_right (fun (pi,ai) default -> 
		add_tasks default ((bound_vars_to_logic pi)@[toAxiom_peq t_decls ai pi]))
	        pelist senv in
	      rebuild new_senv a2 (Clet(new_senv, rflag, ds@pelist, p, l, e3) :: k1)
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
	   mtrace "[let-if]";
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
	   mtrace "[let-match]";
	   let alts = List.map (fun (pi,ai) ->
              (pi, match ai.exp_desc with
	      | Texp_bad _ | Texp_unr _ -> ai
	      | _ -> {ai with exp_desc = Texp_let (rflag, (p,ai)::uds, e3)}))
	        pat_exp_list in
	   rebuildMatch senv a0 NoVal alts ptial (Cin (rflag, ds) :: k1)
       | _ -> 	   
	   let env2 = add_tasks senv ((bound_vars_to_logic p)@[toAxiom_peq t_decls a p]) in
	   begin match uds with
	   | [] ->  simpl_expr env2 e3 (Cin (rflag, ds@[(p,a)]) :: k1) 
	   | ((x,e)::l) -> 
	      simpl_expr env2 e (Clet (env2, rflag, ds@[(p,a)], x, l, e3) :: k1)
           end
      end
    end
  | (_, Cin (rflag, ds) :: k1) -> 
      begin match ds with 
      | [] -> rebuild senv a k1
      | [(p,e)] when rflag = Nonrecursive &&
           ( match (p.pat_desc, a.exp_desc) with
	   | (Tpat_var(id1), Texp_ident(Pident id2, _)) -> id1 = id2
	   | _ -> false) ->
	   rebuild senv e k1
      | _ -> rebuild senv ({a with exp_desc = Texp_let (rflag, ds, a)}) k1
      end
  | (_, Cmatch (matchenv, e_match, alts, ptial) :: k1) -> 
       let senv = matchenv in
       let t_decls = type_decls senv in
         (* [[ a ]]_(K x) *) 
	 begin match a.exp_desc with
	 | Texp_match (a0, pat_exp_list, ptial2) ->
	     (* match (match a0 with pi -> ai) with alts 
                ==> [match-match]
                match a0 with pi -> match ai with alts 
             *)
	     mtrace "[match-match]";
	     let new_alts = List.map (fun (pi,ai) ->
	       (pi, {e_match with exp_desc=Texp_match(ai,alts,ptial)}))
		 pat_exp_list in
	     rebuildMatch matchenv a0 NoVal new_alts ptial k1
	 | Texp_let (rflag, pat_exp_list, a2) -> 
           (* match (let x = a1 in a2) with alts
              ==> [match-let]
              let x = a1 in match a2 with alts
           *)
	 mtrace "[match-let]";
         let rhs = {e_match with exp_desc = Texp_match (a2, alts, ptial)} in
	 let new_senv = List.fold_right (fun (pi,ai) default ->
             add_tasks default ((bound_vars_to_logic pi)@[toAxiom_peq t_decls ai pi]))
	     pat_exp_list senv in 
         simpl_expr new_senv rhs (Cin (rflag, pat_exp_list) :: k1)
	 | Texp_ifthenelse(a0, a1, a2opt) ->
	     (* match (if a0 then a1 else a2) with alts
		==> [match-if]
		if a0 then match a1 with alts
		else match a2 with alts
	      *)
	   mtrace "[match-if]";
	   let e1 = {e_match with exp_desc = Texp_match(a1,alts,ptial)} in
           let e2opt = match a2opt with
                     | None -> None
                     | Some a2 -> Some
			{e_match with exp_desc = Texp_match(a1,alts,ptial)} in
           rebuildIf senv a0 e1 e2opt k1
	 | _ -> (*
         if a.exp_type = Predef.type_bool 
	   then begin 
             let goal_tasks = goalTasks matchenv in
             let senv1  = begin if is_expression_prop a
	                  then add_tasks senv (goal_tasks@[toGoal a])
                          else add_tasks senv (goal_tasks@[toGoal_beq a]) 
                          end in
	     let ts1  = tasks senv1 in
             let filename = (Location.toString a.exp_loc)^"cmatch.why" in 
	     write_tasks_to_file filename ts1;
             match askErgo filename ts1 with
             | Valid -> (* scrutinee is Valid *)
	       rebuildMatch matchenv a (Inline {a with exp_desc = trueExp}) 
                            alts ptial k1
             | _ -> 
	        rebuildMatch matchenv a NoVal alts ptial k1 end
            else *) rebuildMatch matchenv a NoVal alts ptial k1 
    end 
  | (_, Cif (env, e1, e2opt) :: k1) ->    
      let t_decls = type_decls env in   
   (*  begin match lookup_denv a env with
      | Inline e1 -> (* if scrutinee reappears, replace it by its aval *)
                    rebuildIf env e1 e1 e2opt k1
      | _ -> *) 
      begin match a.exp_desc with
        | Texp_let (rflag, pelist, a2) -> 
       (* if (let x = a1 in a2) then .. else ..
           ==> [if-let]
          let x = a1 in if a2 then .. else ..
       *)
	  mtrace "[if-let]";
	  let new_senv = List.fold_right (fun (x,a1) default -> 
	    add_tasks default ((bound_vars_to_logic x)@[toAxiom_peq t_decls a1 x]))
             pelist env in
	  rebuild new_senv a2 (Cif (new_senv, e1, e2opt) :: Cin (rflag, pelist) :: k1)
	| Texp_ifthenelse (a0, a1, a2opt) -> 
	    (* if (if a0 then a1 else a2) then e1 else e2 
              ==> [if-if]
            if a0 then (if a1 then e1 else e2)
            else (if a2 then e1 else e2)
	    *)
	  mtrace "[if-if]";
	  let new_e1 = match a1.exp_desc with
	  | Texp_bad _ | Texp_unr _ -> a1
	  | _ -> {e1 with exp_desc = Texp_ifthenelse(a1,e1,e2opt)} in
          let new_e2opt = match a2opt with
	   | None -> None
           | Some a2 -> begin match a2.exp_desc with
	     | Texp_bad _ | Texp_unr _ -> a2opt
	     | _ -> Some ({e1 with exp_desc = Texp_ifthenelse(a2,e1,e2opt)})
	              end
	  in rebuild env a0 (Cif (env, new_e1, new_e2opt) ::  k1)
	| Texp_match(a0, pat_exp_list, ptial) ->
	   (* if (match a0 with pi -> ai) then e1 else e2
             ==> [if-match]
              match a0 with pi -> if ai then e1 else e2
	   *)
          mtrace "[if-match]";
	  let alts = List.map (fun (pi,ai) ->
               (pi, {e1 with exp_desc = Texp_ifthenelse(ai,e1,e2opt)})) pat_exp_list in
	  rebuildMatch env a0 NoVal alts ptial k1
        | _ -> rebuildIf env a e1 e2opt k1
        end
   (*    end *)
  | (_, Cthen (env, a0, e2opt) :: k1) -> 
      begin match e2opt with
      | None ->       
          rebuild senv ({a with exp_desc = Texp_ifthenelse(a0,a,None)}) k1
      | Some e2 -> 
          simpl_expr env e2 (Celse (a0, a) :: k1)
      end
  | (_, Celse (a0, a1) :: k1) -> 
      rebuild senv ({a1 with exp_desc = Texp_ifthenelse(a0,a1,Some a)}) k1
  | _ -> raise(Error(Rebuild_pattern_not_matched))
  

and rebuildVar senv ((n, e):(Ident.t * expression)) 
               (v:aval) (k:cont list) : outexp = 
(* e is the expression form of the ident n *)
    let t_decls = type_decls senv in
    trace "[rebuildVar]" (print_expression t_decls) e;
    match v with
    | Inline a -> begin match a.exp_desc with
       | Texp_ident _ -> simpl_expr senv a k
       | _ -> rebuild senv a k
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
| Texp_ident (path, vd) -> memPath path gamma 
| Texp_constant _ -> false
| Texp_bad (bl) -> false
| Texp_unr (bl) -> false
| Texp_let (rec_flag, pat_expr_list, expr1) -> 
  if List.exists (fun (p,e) -> has_function_call gamma e) pat_expr_list
  then true 
  else has_function_call (gamma@(List.flatten (List.map (fun (p,_) -> match p.pat_desc with
                   | Tpat_var(x) -> [Pident x]
		   | _ -> []) pat_expr_list))) expr1
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
	    rename_boundvars [] exp1
	    end
       else e
  | _ -> e
  in map_expression expand_id exp

let rec the_static_contract_checking env (id, exp) = 
  try
  let t_decls = type_decls env in
  trace "raw expression" (print_expression t_decls) exp; 
  let senv = update_name env (Pident id) in
  let sexp =  simpl_expr senv exp [] in
  trace "simplified expression" (print_expression t_decls) sexp; 
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
            the_static_contract_checking new_senv (id,sexp)
	   | 1 ->  
	    let the_exp = lookup_rec_env id env in
            let expanded_exp = inline_rec id sexp the_exp in
	    let new_senv = increase_depth env in  
            the_static_contract_checking new_senv (id,expanded_exp)	 
           | _ ->  
               (cleaned_sexp, validity)
           end
      else
       let validity = EscSyn.Valid in
       let rpt = static_report (id, counter_example) validity in
       print_string ("At depth "^string_of_int(depth env)^", ");
       fprintf std_formatter "%s" rpt; 
       (cleaned_sexp, validity)
  with 
    _ -> (exp, EscSyn.Unknown)
(*
  Error(err) -> report_error std_formatter err;
                     raise (Error(err))
  | ToErgosrc.Error(loc,err) -> 
      Location.print_error std_formatter loc;
      ToErgosrc.report_error std_formatter err;
      raise(ToErgosrc.Error(loc,err))
*)


 
