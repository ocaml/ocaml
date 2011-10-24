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

open Options
open Format
open Why_ptree

module T = Term
module F = Formula
module A = Literal

let queue = Queue.create ()

let clear () = Queue.clear queue

let varset_of_list = 
  List.fold_left 
    (fun acc (s,ty) -> 
       Term.Set.add (Term.make s [] (Ty.shorten ty)) acc) Term.Set.empty

let rec make_term { tt_ty = ty; tt_desc = tt } = 
  let ty = Ty.shorten ty in
  match tt with
    | TTconst Ttrue -> 
	T.vrai
    | TTconst Tfalse -> 
	T.faux
   | TTconst Tvoid -> 
	T.void
    | TTconst (Tint i) -> 
	T.int i
    | TTconst (Treal n) -> 
	T.real (Num.string_of_num n)
    | TTconst (Tbitv bt) -> 
	T.bitv bt ty
    | TTvar s ->  
	T.make s [] ty 
    | TTapp (s, l) -> 
	T.make s (List.map make_term l) ty
    | TTinfix (t1, s, t2) ->  
	T.make s [make_term t1;make_term t2] ty
    | TTprefix ((Symbols.Op Symbols.Minus) as s, n) ->
	let t1 = if ty = Ty.Tint then T.int "0" else T.real "0"  in
	T.make s [t1; make_term n] ty
    | TTprefix _ -> 
	assert false
    | TTget (t1, t2) ->
	T.make (Symbols.Op Symbols.Get) [make_term t1; make_term t2] ty
    | TTset (t1, t2, t3) ->
	let t1 = make_term t1 in
	let t2 = make_term t2 in
	let t3 = make_term t3 in
	T.make (Symbols.Op Symbols.Set) [t1; t2; t3] ty
    | TTextract (t1, t2, t3) ->
	let t1 = make_term t1 in
	let t2 = make_term t2 in
	let t3 = make_term t3 in
	T.make (Symbols.Op Symbols.Extract) [t1; t2; t3] ty
    | TTconcat (t1, t2) ->
	T.make (Symbols.Op Symbols.Concat) [make_term t1; make_term t2] ty
    | TTlet (s, t1, t2) ->
	let t1 = make_term t1 in
	let subst = Symbols.Map.add s t1 Symbols.Map.empty, Ty.esubst in
	let t2 = make_term t2 in
	T.apply_subst subst t2


let make_form name f = 
  let rec make_form acc = function
    | TFatom a ->
	let a , lit = match a with
	    TAtrue -> 
	      A.vrai , A.vrai::acc
	  | TAfalse -> 
	      A.faux , A.faux::acc
	  | TAeq [t1;t2] -> 
	      let lit = A.make (A.Eq(make_term t1,make_term t2)) in
	      lit , lit::acc
	  | TApred t ->
	      let lit = A.mk_pred (make_term t) in
	      lit , lit::acc
	  | TAneq [t1;t2] -> 
	      let lit = A.make (A.Neq(make_term t1,make_term t2)) in
	      lit , lit::acc
	  | TAle [t1;t2] -> 
	      (try 
		 let ale = Builtin.is_builtin "<=" in
		 let lit = 
		   A.make (A.Builtin(true,ale,[make_term t1;make_term t2]))
		 in lit , lit::acc
	       with Not_found -> assert false)
	  | TAlt [t1;t2] ->  
	      (try 
		 let alt = Builtin.is_builtin "<" in
		 let lit = 
		   A.make (A.Builtin(true,alt,[make_term t1;make_term t2])) 
		 in lit , lit::acc
	       with Not_found -> assert false)
	  | TAbuilt(n,lt) ->
	      let lit = A.make (A.Builtin(true,n,List.map make_term lt)) in
	      lit , lit::acc
	  | _ -> assert false
	in F.mk_lit a , lit

    | TFop((OPand | OPor) as op,[f1;f2]) -> 
	let ff1 , lit1 = make_form acc f1 in
	let ff2 , lit2 = make_form lit1 f2 in
	let op = match op with OPand -> F.mk_and | _ -> F.mk_or in
	op ff1 ff2 , lit2
    | TFop(OPimp,[f1;f2]) -> 
	let ff1 , _ = make_form acc f1 in
	let ff2 , lit = make_form acc f2 in
	F.mk_imp ff1 ff2 , lit
    | TFop(OPnot,[f]) -> 
	let ff , lit = make_form acc f in
	F.mk_not ff , lit
    | TFop(OPif t,[f2;f3]) -> 
	let tt = make_term t in
	let ff2 , lit2 = make_form acc f2 in
	let ff3 , lit3 = make_form lit2 f3 in
	F.mk_if  tt ff2 ff3 , lit3
    | TFop(OPiff,[f1;f2]) -> 
	let ff1 , lit1 = make_form acc f1 in
	let ff2 , lit2 = make_form lit1 f2 in
	F.mk_iff ff1 ff2 , lit2
    | (TFforall qf | TFexists qf) as f -> 
	let bvars = varset_of_list qf.qf_bvars in
	let upvars = varset_of_list qf.qf_upvars in
	let trs = List.map (List.map make_term) qf.qf_triggers in
	let ff , lit = make_form acc qf.qf_form in
	begin match f with
	  | TFforall _ -> F.mk_forall upvars bvars trs ff name , lit
	  | TFexists _ -> F.mk_exists upvars bvars trs ff name , lit
	  | _ -> assert false
	end
    | TFlet(up,lvar,lterm,lf) -> 
	let ff, lit = make_form acc lf in
        F.mk_let (varset_of_list up) lvar (make_term lterm) ff, lit

    | TFnamed(lbl, f) ->
	let ff, lit = make_form acc f in
	F.add_label lbl ff; 
	ff, lit

    | _ -> assert false
  in
  make_form [] f

let push_assume f name loc match_flag = 
  let ff , _ = make_form name f in
  Queue.push {st_decl=Assume(ff,match_flag) ; st_loc=loc} queue

let push_preddef f name loc match_flag = 
  let ff , _ = make_form name f in
  Queue.push {st_decl=PredDef ff ; st_loc=loc} queue
      
let push_query n f loc = 
  let ff, lits = make_form "" f in
  Queue.push {st_decl=Query(n,ff,lits) ; st_loc=loc} queue

let make l = 
  (*  Decl.clear ();
      Logics.clear();
      Types.clear();*)
  clear();
  List.iter
    (fun (d,b) -> match d with
	 TAxiom(loc,name,f) -> push_assume f name loc b
       | TGoal(loc,n,f) -> push_query n f loc
       | TPredicate_def(loc,n,[],f) -> push_assume f n loc b
       | TPredicate_def(loc,n,_,f) -> push_preddef f n loc b
       | TFunction_def(loc,n,_,_,f) -> push_assume f n loc b
       | _ -> ()) l;
  queue
