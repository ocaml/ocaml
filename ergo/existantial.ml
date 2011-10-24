(**************************************************************************)
(*                                                                        *)
(*     The Alt-ergo theorem prover                                        *)
(*     Copyright (C) 2006-2010                                            *)
(*                                                                        *)
(*     Sylvain Conchon                                                    *)
(*     Evelyne Contejean                                                  *)
(*     Stephane Lescuyer                                                  *)
(*     Mohamed Iguernelala                                                *)
(*     Alain Mebsout                                                      *)
(*                                                                        *)
(*     CNRS - INRIA - Universite Paris Sud                                *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C licence     *)
(*                                                                        *)
(**************************************************************************)

open Why_ptree

let eq_var x t = 
  match t.tt_desc with
    | TTvar y -> Symbols.equal x y
    | _ -> false

let rec find_eq x eqs = function
  | TFatom (TAeq [t1;t2]) -> 
      if eq_var x t1 then (x,t2)::eqs 
      else if eq_var x t2 then (x,t1)::eqs
      else eqs
  | TFop(OPand,l) -> List.fold_left (find_eq x) eqs l
  | _ -> eqs (* XXX: TODO *)

let find_equalities lv f = 
  List.fold_left 
    (fun eqs (x,_) -> 
       let l = find_eq x [] f in 
       if l = [] then raise Not_found; l::eqs ) [] lv

let rec apply_subst_term env t = 
  let tt = match t.tt_desc with
    | TTvar x as tt -> 
	(try (List.assoc x env).tt_desc with Not_found -> tt)
    | TTapp(s,l) -> TTapp(s,List.map (apply_subst_term env) l)
    | TTinfix(t1,s,t2) -> 
	TTinfix(apply_subst_term env t1,s,apply_subst_term env t2)
    | tt -> tt
  in
  { t with tt_desc = tt }

let rec apply_subst_formula env f = 
  match f with
    | TFatom ( TAeq l | TAneq l | TAle l | TAlt l | TAbuilt(_,l) as e ) -> 
	let l' = List.map (apply_subst_term env) l in
	let a = match e with
	    TAeq _ -> TAeq l' 
	  | TAneq _ -> TAneq l' 
	  | TAle _ -> TAle l' 
	  | TAlt _ -> TAlt l' 
	  | TAbuilt(s,_) -> TAbuilt(s,l')
	  | _ -> assert false
	in TFatom a
    | TFatom(TApred t) -> TFatom(TApred (apply_subst_term env t))
    | TFop(op,lf) ->
	TFop(op,List.map (apply_subst_formula env) lf)
    | TFforall _ | TFexists _ -> f (* XXX: TODO *)
    | _ -> f
	
let make_instance f = 
  let lt = find_equalities f.qf_bvars f.qf_form in
  apply_subst_formula (List.map List.hd lt) f.qf_form

let make f = 
  if Options.no_rm_eq_existential 
  then TFexists f
  else
    try (*TFop(OPor,[TFexists f;*)make_instance f(*])*) with Not_found -> TFexists f

