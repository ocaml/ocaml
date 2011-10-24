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

open Format
open Options

module T = Term
module F = Formula

type gsubst = { 
  sbt : T.subst ; 
  gen : int ;     (* l'age d'une substitution est l'age du plus vieux 
		     terme qu'elle contient *)
  goal : bool     (* vrai si la substitution contient un terme ayant un lien 
		     avec le but de la PO *)
}

type pat_info = {
  pat_age : int ; (* age d'un trigger *)
  pat_orig : Formula.t ; (* lemme d'origine *)
  pat_formula : Formula.t ; (* formule associee au trigger *)
  pat_dep : Explanation.t ;
}

type term_info = {
  term_age : int ;   (* age du terme *)
  term_from_goal : bool ; (* vrai si le terme provient du but de la PO *)
  term_orig : Formula.t option (* lemme d'origine du terme *)
}

module type X = sig
  type t

  val class_of : t -> Term.t -> Term.t list
  val query : Literal.t -> t -> bool

end

module type S = sig
  type t
  type uf

  val empty : t
  val add_term : term_info -> Term.t -> t -> t 
  val add_pat : pat_info * Term.t list -> t -> uf -> t
  val query : t -> uf -> (pat_info * gsubst list) list  
end

module Make (X : X) = struct

  type uf = X.t

  module MT = T.Map

  type info = {
    age : int ; (* age du terme *)
    lem_orig : F.t option ; (* lemme d'ou provient eventuellement le terme *)
    but : bool  (* le terme a-t-il un lien avec le but final de la PO *)
  }

  type t = { 
    fils : T.t list MT.t Ergo_subst.t ; 
    info : info MT.t ; 
    pats : (pat_info * Term.t list) list 
  }

  exception Echec
    
  module SubstT = Ergo_subst.Make(T)

  let empty = { 
    fils = SubstT.empty ; 
    info = MT.empty ;
    pats = [ ];
  }

  let age_limite = Options.age_limite
    (* l'age limite des termes, au dela ils ne sont pas consideres par le
       matching *)

  let infos op_gen op_but t g b env = 
    try 
      let i = MT.find t env.info in
      op_gen i.age g , op_but i.but b 
    with Not_found -> g , b

  let add_term { term_age = age; term_from_goal = but; term_orig = lem} t env =
    let rec add_rec env t = 
      let {T.f=f;xs=xs} = T.view t in
      let env = 
	let map_f = try SubstT.find f env.fils with Not_found -> MT.empty in
	
	(* - l'age d'un terme est le min entre l'age passe en argument
	   et l'age dans la map 
	   - un terme est en lien avec le but de la PO seulement s'il
	   ne peut etre produit autrement (d'ou le &&)
	   - le lemme de provenance est le dernier lemme
	*)
	let g , b = infos min (&&) t age but env in
	{ env with
	    fils = SubstT.add f (MT.add t xs map_f) env.fils; 
	    info= MT.add t {age=g; lem_orig=lem; but=b} env.info }
      in
      List.fold_left add_rec env xs
    in
    if age>age_limite then env else add_rec env t
      
  let add_pat p env _ = { env with pats = p::env.pats }

  exception Deja_vu
  let deja_vu lem1 = 
    function None -> false | Some lem2 -> F.compare lem1 lem2=0

  let all_terms f ty env pinfo {sbt=(s_t,s_ty); gen=g; goal=b} = 
    SubstT.fold 
      (fun k s l-> 
	 MT.fold 
	   (fun t _ l -> 
	      try
		let s_ty = Ty.matching s_ty ty (T.view t).T.ty in
		let ng , but = 
		  try 
		    let {age=ng;lem_orig=lem'; but=bt} = MT.find t env.info in
		    if deja_vu pinfo.pat_orig lem' then raise Deja_vu;
		    max ng g , bt or b
		  with Not_found -> g , b
		in
		{sbt=(SubstT.add f t s_t, s_ty);gen=ng; goal=but}::l
	      with Ty.TypeClash _ | Deja_vu-> l
	   )
	   s l)
      env.fils []

  let add_msymb uf f t ({sbt=(s_t,s_ty)} as sg)= 
    try 
      let t' = SubstT.find f s_t in
      if X.query (Literal.make(Literal.Eq(t,t'))) uf then sg 
      else raise Echec
    with Not_found ->  {sg with sbt=(SubstT.add f t s_t,s_ty) }

  let rec iter_exception f m = function
      [] -> raise Echec
    | x::l -> try f m x with Echec -> iter_exception f m l
	
  let rec matchterm env uf ( {sbt=(s_t,s_ty);gen=g;goal=b} as sg) pat t =
    let {T.f=f_pat;xs=pats;ty=ty_pat} =  T.view pat in
    match f_pat with
	Symbols.Var _ -> 
	  (try
	     let s_ty = Ty.matching s_ty ty_pat (T.view t).T.ty in
	     let g',b' = infos max (||) t g b env in
	     add_msymb uf f_pat t {sbt=(s_t,s_ty);gen=g';goal=b'}
	   with Ty.TypeClash _ -> raise Echec)
      | _ -> 
	  let l = List.map T.view (X.class_of uf t) in
	  let s_ty , l = 
	    List.fold_left
	      (fun (s_ty,l) ({T.f=f;ty=ty_t} as t) -> 
		 if Symbols.compare f_pat f=0 then 
		   try
		     let s_ty = Ty.matching s_ty ty_pat ty_t in
		     s_ty , t::l 
		   with Ty.TypeClash _ -> s_ty , l
		 else s_ty , l
	      ) (s_ty,[]) l 
	  in
	  iter_exception (* pas sur que ce soit correct ici *)
	    (fun m {T.xs=xs} -> matchterms env uf m pats xs) 
	    { sg with sbt = (s_t,s_ty)} l
	  
  and matchterms env uf sg pats xs = 
    try List.fold_left2 (matchterm env uf) sg pats xs 
    with Invalid_argument _ -> raise Echec

  let matchpat env uf pat_info lsubst ({gen=g; goal=b} as sg,pat) = 
    let {T.f=f;xs=pats;ty=ty} = T.view pat in
    match f with
	Symbols.Var _ -> all_terms f ty env pat_info sg
      | _ -> 
	  try  
	    MT.fold 
	      (fun t xs l -> 
		 try 
		   let gen , but = infos max (||) t g b env in
		   (matchterms env uf
			{sg with gen=gen ; goal=but } pats xs)::l 
		 with Echec -> l)
	      (SubstT.find f env.fils) lsubst
	  with Not_found -> lsubst
	    
  let matchpats env uf pat_info lsubsts pat = 
    let lpats = 
      List.map (fun sg -> (sg,T.apply_subst sg.sbt pat)) lsubsts in
    List.flatten (List.map (matchpat env uf pat_info []) lpats)

  let matching (pat_info, pats) env uf = 
    let egs = {sbt=(SubstT.empty,Ty.esubst) ; gen = 0; goal = false} in
    List.fold_left (matchpats env uf pat_info) [egs] pats

  let query env uf = 
    List.fold_left 
      (fun r ((pat_infos, pats) as v) -> (pat_infos, matching v env uf)::r)
      [] env.pats 
end
