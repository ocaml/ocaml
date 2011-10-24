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

module A = Literal
module CcX = Cc.Make(Combine.CX)
module F = Formula
module M = Ergo_matching.Make(CcX) 
module SF = F.Set
module MF = F.Map
module Ex = Explanation

type gformula = { 
  f: F.t; 
  age: int; 
  name: F.t option; 
  mf: bool;
  gf: bool
}

type t = { 
    gamma : Ex.t MF.t; 
    delta : (gformula * gformula * Ex.t) list;
    tbox : CcX.t;
    lemmas : (int * Ex.t) MF.t;
    definitions : (int * Ex.t) MF.t;
    matching : M.t
}
      
exception Sat of t
exception Unsat
exception I_dont_know
exception IUnsat of Ex.t

let max_max_size = 96

module Print = struct

  let assume {f=f;age=age;name=lem;mf=mf} = 
    if debug_sat then
      match F.view f with
	  F.U _ -> ()
	    
	| F.C _ -> 
	    printf "@[@{<C.Bold>[sat]@}";
	    printf "@{<C.G_Cyan>I assume a clause@} @[%a@]@]@." F.print f
	      
	| F.Lem _ ->
	    printf "@[@{<C.Bold>[sat]@}";
	    printf "@{<C.G_Yellow>I assume a [%d-atom] lemma@}" (F.size f);
	    printf "@[%a@]@]@." F.print f
	      
	| F.Lit a -> 
	    let s = 
	      match lem with 
		  None -> "" 
		| Some ff -> 
		    (match F.view ff with F.Lem xx -> xx.F.name | _ -> "") 
	    in
	    printf "\n@[@{<C.Bold>[sat]@}";
	    printf "@{<C.G_Blue_B>I assume a literal@}";
	    printf "(%s) %a@]@." s Literal.print a;
	    printf "================================================@.@."
	      
	| F.Sko{F.ssubst=s;ssubst_ty=s_ty;sf=f} ->
	    printf "@[@{<C.Bold>[sat]@} I assume a skolem %a @]@." F.print f 
	      
	| F.Let {F.lvar=lvar;lterm=lterm;lsubst=lsubst;lf=lf} ->
	    printf "@[@{<C.Bold>[sat]@} I assume a let %a =@ %a in@ %a@ @]@." 
              Symbols.print lvar Term.print lterm F.print lf

  let unsat () = 
    if debug_sat then printf "@[@{<C.Bold>[sat]@} @{<C.G_Red_B>unsat@}@]@."

  let mround s = 
    if debug_sat then 
      printf "@[@{<C.Bold>[sat]@} matching round of size %d@]@." s

  let decide f = 
    if debug_sat then 
      printf "@[@{<C.Bold>[sat]@} @{<C.G_Green>I decide@} on %a@]@." F.print f

  let backtracking f = 
    if debug_sat then 
      printf "@[@{<C.Bold>[sat]@} @{<C.G_Green>I backtrack@} on %a@]@." 
	F.print f

  let backjumping f = 
    if debug_sat then 
      (printf "@[@{<C.Bold>[sat]@} @{<C.G_Green>I dont' consider the case@}";
       printf "%a@]@." F.print f)
       
  let elim _ _ = if debug_sat && verbose then printf "@[@{<C.Bold>[elim]@}@."

  let red _ _ = if debug_sat && verbose then printf "@[@{<C.Bold>[red]@}@."

end

(* matching part of the solver *)

let add_terms env s goal age lem = 
  let infos = { 
    Ergo_matching.term_age = age ; 
    term_from_goal = goal ;
    term_orig = lem ;
  }
  in
  { env with matching = Term.Set.fold (M.add_term infos) s env.matching }

(*exception EnoughLemmasAlready of int * (gformula * Ex.t) list*)

exception EnoughLemmasAlready of t * int

let b_max_size = 100

let rec double_until min s =
  let s2 = s + b_max_size in 
    if s2 >= min then s2 else double_until min s2

let mtriggers env formulas max_size = 
  let stop = ref false in
  try
    MF.fold
      (fun lem (age, dep) (env, max_size) ->
	 let size = F.size lem in
	 let max_size = 
	   if size <= max_size then max_size 
	   else 
	     begin
	       if !stop then raise (EnoughLemmasAlready(env, max_size));
	       stop:=true; double_until size max_size
	     end
	 in
	 let env = 
	   match F.view lem with
	       F.Lem {F.trs=tgs; f=f} -> 
		 List.fold_left 
		   (fun env tg ->
		      let info = 
			{ Ergo_matching.pat_age = age ; 
			  pat_orig = lem ;
			  pat_formula = f ;
			  pat_dep = dep }
		      in
		      { env with 
			  matching = 
			  M.add_pat (info, tg) env.matching env.tbox })
		   env tgs
	     | _ -> assert false		 
	 in 
	 (env, max_size)
      )
      formulas (env, max_size)
  with EnoughLemmasAlready(env, max_size) -> env, max_size

let new_facts mode env = 
  List.fold_left
    (fun acc ({Ergo_matching.pat_formula=f; 
	       pat_age=age; pat_dep=dep }, subst_list) ->
       List.fold_left
	 (fun acc {Ergo_matching.sbt=s;gen=g;goal=b} ->
	    if mode && not b then acc
	    else
	      begin
		let nf = F.apply_subst s f in
		if MF.mem nf env.gamma then acc else
		  let p = {f=nf;age=1+(max g age);name=Some f;mf=true;gf=b} in
		  (p,dep)::acc
	      end
	 ) 
	 acc subst_list
    )
    [] (M.query env.matching env.tbox)


let mround predicate mode env max_size =
  let round mode =
    Print.mround max_size;
    let axioms = if predicate then env.definitions else env.lemmas in
    let env, max_size = mtriggers env axioms max_size in
    let rec bouclage n (env, lf) = 
      if n <=0 then (env, lf)
      else
        let env = 
	  List.fold_left 
	    (fun env (f,_) -> add_terms env (F.terms f.f) mode f.age None)
	    env lf
        in
        bouclage (n-1) (env, (new_facts mode env))
    in
    let _, lf = bouclage Options.bouclage (env, []) in
    max_size, lf 
  in
  let max_size, lf = round (mode || Options.goal_directed) in 
  if Options.goal_directed && lf = [] then round false 
  else max_size, lf
  

let extract_model t = 
  let s = ref SF.empty in
  MF.iter 
    (fun f _ -> 
       let lbl = F.label f in
       if not (Hstring.equal Hstring.empty lbl) then
	 s := SF.add f !s
    ) 
    t.gamma;
  !s

let print_model fmt s = 
  SF.iter (fprintf fmt "%a\n" F.print) s

(* sat-solver *)

let elim {f=f} env = 
  MF.mem f env.gamma ||
    match F.view f with 
	F.Lit a -> CcX.query a (CcX.add a env.tbox)  
      | _ -> false

let size_formula = 1000

let red {f=f} env = 
  let nf = F.mk_not f in
  try 
    Some(MF.find nf env.gamma)
  with Not_found -> 
    match F.view nf with
	F.Lit a -> 
	  let tbox = CcX.add a env.tbox in
	  if CcX.query a tbox then Some (CcX.explain a tbox) else None
      | _ -> None

let pred_def env f = 
  { env with definitions = MF.add f (0,Ex.empty) env.definitions }

let rec assume env ({f=f;age=age;name=lem;mf=mf;gf=gf} as ff ,dep) =
  try
    (try raise (IUnsat (Ex.union dep (MF.find (F.mk_not f) env.gamma)))
     with Not_found -> ());
    if MF.mem f env.gamma then env
    else 
      begin
	let size = F.size f in
	if size > size_formula then env
	else
	  let env =
	    if mf && glouton  && size < size_formula then 
	      add_terms env (F.terms f) gf age lem else env in
	  let env = { env with gamma = MF.add f dep env.gamma } in
	  Print.assume ff;
	  match F.view f with
	      F.U l -> 
		List.fold_left assume env 
		  (List.map (fun x->{f=x;age=age;
				     name=lem;mf=mf;gf=gf},dep) l)

	    | F.C(f1,f2) -> 
		let p1 = {f=f1;age=age;name=lem;mf=mf;gf=gf} in
		let p2 = {f=f2;age=age;name=lem;mf=mf;gf=gf} in
		bcp { env with delta = (p1,p2,dep)::env.delta }

	    | F.Lem _ ->
		let age , dep = 
		  try 
		    let age' , dep' = MF.find f env.lemmas in
		    min age age' , Ex.union dep dep' 
		  with Not_found -> age , dep 
		in
		bcp { env with lemmas=MF.add f (age,dep) env.lemmas }

	    | F.Lit a -> 
		let env = 
		  if mf && size < size_formula then 
		    add_terms env (A.terms_of a) gf age lem
		  else env 
		in
		let env = { env with tbox = CcX.assume a dep env.tbox } in
		bcp env

	    | F.Sko{F.ssubst=s;ssubst_ty=s_ty;sf=f} -> 
		let f' = F.apply_subst (s,s_ty) f in
		assume env ({f=f';age=age;name=lem;mf=mf;gf=gf},dep)

            | F.Let {F.lvar=lvar;lterm=lterm;lsubst=lsubst;lf=lf} ->
                let f' = F.apply_subst (lsubst,Ty.esubst) lf in
                let v = Symbols.Map.find lvar lsubst in
                let env = assume env ({f=F.mk_lit (A.make (A.Eq(v,lterm)));
				       age=age;name=lem;mf=mf;gf=gf},dep) 
		in
                assume env ({f=f';age=age;name=lem;mf=mf;gf=gf},dep)
      end
  with Exception.Inconsistent -> raise (IUnsat Ex.everything)
    
and bcp env = 
  let cl , u = 
    List.fold_left 
      (fun (cl,u) ((f1,f2,d) as fd) -> 
         Print.elim f1 f2;
	 if elim f1 env || elim f2 env  then (cl,u)
	 else 
           (Print.red f1 f2;
	   match red f1 env with
	       Some d1 -> (cl,(f2,Ex.union d d1)::u)
	     | None -> 
		 match red f2 env with
		     Some d2 -> (cl,(f1,Ex.union d d2)::u)
		   | None -> fd::cl , u)
      ) ([],[]) env.delta
  in
  List.fold_left assume {env with delta=cl} u
    
let rec unsat_rec env fg stop max_size = 
  try
    if stop < 0 then raise I_dont_know;
    back_tracking (assume env fg) stop max_size
  with IUnsat d-> Print.unsat (); d

and back_tracking env stop max_size = match env.delta with
    []  when stop >= 0  -> 
      let _ , l2 = mround true false env max_max_size in 
      let env = List.fold_left assume env l2 in

      let max_size , l1 = mround false false env max_size in 
      let env = List.fold_left assume env l1 in

      let env = 
	List.fold_left 
	  (fun env ({f=f; age=g; name=lem; gf=gf},_) -> 
	     add_terms env (F.terms f) gf g lem) env l1 
      in
      (match l1,l2 with
	   [], [] -> 
	     let m = extract_model env in
	     if all_models then 
	       begin
		 Format.printf "--- SAT ---\n";
		 Format.printf "%a@." print_model m;
		 raise (IUnsat (Ex.make m))
	       end;
	     raise (Sat env)
	 | l1, l2 -> 
	     back_tracking 
	       (List.fold_left assume  (List.fold_left assume env l2) l1) 
	       (stop-1) (max_size + b_max_size))
  | [] -> 
      raise I_dont_know
  | ({f=f;age=g;name=lem;mf=mf} as a,b,d)::l -> 
      Print.decide f;
      let dep = unsat_rec {env with delta=l} (a,Ex.singleton f) stop max_size in
      try
	let dep' = Ex.remove f dep in
	Print.backtracking (F.mk_not f);
	unsat_rec
	  (assume {env with delta=l} (b, Ex.union d dep'))
	  ({a with f=F.mk_not f},dep') stop max_size
      with Not_found -> Print.backjumping (F.mk_not f); dep 
	
let unsat env fg stop = 
  try
    let env = assume env (fg,Ex.empty) in
    let env = add_terms env (F.terms fg.f) fg.gf fg.age fg.name in

    let _ , l = mround true false env max_max_size in
    let env = List.fold_left assume env l in

    let _ , l = mround false true env max_max_size in
    let env = List.fold_left assume env l in

    ignore(back_tracking env stop 100)
  with IUnsat _ -> Print.unsat ()

let assume env fg = 
  try assume env (fg,Ex.empty) with IUnsat _ -> raise Unsat

let empty = { 
  gamma = MF.empty;
  delta = [] ;
  tbox = CcX.empty (); 
  lemmas = MF.empty ; 
  matching = M.empty;
  definitions = MF.empty
} 
