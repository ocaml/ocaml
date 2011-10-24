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
open Sig


module type S = sig
  type t

  val empty : unit -> t
  val add : Literal.t -> t -> t
  val assume : Literal.t -> Explanation.t -> t -> t
  val query : Literal.t -> t -> bool
  val class_of : t -> Term.t -> Term.t list
  val explain : Literal.t -> t -> Explanation.t
  val extract_model : t -> Literal.t list * Literal.t list
end

module Make (X : Sig.X) = struct    

  module Ex = Explanation
  module Use = Use.Make(X)
  module Uf = Uf.Make(X)
  module SetF = Formula.Set
  module T = Term
  module A = Literal
  module SetT = Term.Set
  module SetA = A.Set
  module SAeq = A.SetEq
  module S = Symbols

  module SetX = Set.Make(struct type t = X.r let compare = X.compare end)
    
  type env = { 
    use : Use.t;  
    uf : Uf.t ;
    relation : X.Rel.t
  }

  type t = { 
    gamma : env;
    gamma_finite : env ;
    choices : (X.r Literal.view * bool) list; 
  }

  module Print = struct


    let make_cst t ctx =
      if ctx = [] then ()
      else begin
        fprintf fmt "[cc] contraints of make(%a)@." Term.print t;
        let cpt = ref 0 in
        List.iter 
          (fun a ->
             incr cpt;
             fprintf fmt " %d) %a@." !cpt A.print a) ctx
      end

    let lrepr fmt = List.iter (fprintf fmt "%a " X.print)

    let congruent t1 t2 flg = 
      fprintf fmt "@{<C.Bold>[cc]@} cong %a=%a ? [%s]@." 
	T.print t1 T.print t2 flg

    let add_to_use t = fprintf fmt "@{<C.Bold>[cc]@} add_to_use: %a@." T.print t
	
    let leaves t lvs = 
      fprintf fmt "@{<C.Bold>[cc]@} leaves of %a@.@." T.print t; lrepr fmt lvs
  end

  let compat_leaves env lt1 lt2 = 
    List.fold_left2
      (fun dep x y -> Ex.union (Uf.explain env.uf x y) dep) Ex.empty lt1 lt2

  let terms_congr env u1 u2 = 
    if Term.compare u1 u2 = 0 then raise Exception.Trivial;
    let {T.f=f1;xs=xs1;ty=ty1} = T.view u1 in
    let {T.f=f2;xs=xs2;ty=ty2} = T.view u2 in
    if Symbols.equal f1 f2 && Ty.equal ty1 ty2 then
      let ex = compat_leaves env xs1 xs2  in
      if debug_cc then Print.congruent u1 u2 "Yes";
      ex
    else 
      begin
	if debug_cc then Print.congruent u1 u2 "No";
	raise Exception.NotCongruent
      end

	      
  let concat_leaves uf l = 
    let one, _ = X.make (Term.make (S.name "@bottom") [] Ty.Tint) in 
    let rec concat_rec acc t = 
      match  X.leaves (Uf.find uf t) , acc with
	  [] , _ -> one::acc
	| res, [] -> res
	| res , _ -> List.rev_append res acc
    in
    match List.fold_left concat_rec [] l with
	[] -> [one]
      | res -> res

  let semantic_view env a = 
    match A.view a with
      | A.Eq(t1,t2) -> 
	  A.Eq(Uf.find env.uf t1, Uf.find env.uf t2)
      | A.Neq(t1, t2) -> 
	  A.Neq(Uf.find env.uf t1, Uf.find env.uf t2)
      | A.Builtin(b,s,l) -> A.Builtin(b,s,List.map (Uf.find env.uf) l)

(*
  For pairs, we can deduce equalities by congruence 

  let is_constant t = match (T.view t).T.xs with [] -> true | _ -> false
    
  let uninterpreted rt = match X.leaves rt with
    | []  -> false     (* true aurait ete plus general *)
    | [y] -> X.equal rt y
    | _   -> false
*)

  let rec close_up t1 t2 dep env =
    if debug_cc then 
      printf "@{<C.Bold>[cc]@} close_up: %a = %a@." T.print t1 T.print t2;

    (* we merge equivalence classes of t1 and t2 *)
    let r1 = Uf.find env.uf t1 in
    let r2 = Uf.find env.uf t2 in
    close_up_r r1 r2 dep env

  and close_up_r r1 r2 dep env =
    let uf, res = Uf.union env.uf r1 r2 dep in
    List.fold_left 
      (fun env (p,touched,v) ->
	 
	 (* we look for use(p) *)
      	 let gm_p_t, gm_p_a = Use.find p env.use in
	 
	 (* we compute terms and atoms to consider for congruence *)
	 let repr_touched = List.map snd touched in
	 let st_others, sa_others = Use.congr_close_up env.use p repr_touched in
	 
	 (* we update use *)
	 let nuse = Use.up_close_up env.use p v in
	 
	 (* we print updates in Gamma and Ac-Gamma *)
	 if debug_use then Use.print nuse;
	 
	 (* we check the congruence of the terms. 
	    It's only done on uninterpreted functions symbols *)
	 (*
	 let u_gm_p_t = 
	   T.Set.filter (fun t -> uninterpreted(X.make t)) gm_p_t in
	 let u_st_others = 
	   T.Set.filter (fun t -> uninterpreted(X.make t)) st_others in
	 *)
	 let env = replay_terms gm_p_t st_others {env with use=nuse} in
	 
       	 let eqs_nonlin = 
	   List.map (fun (x,y)-> (A.Eq(x, y), None)) touched 
	 in
         replay_atom env (SetA.union gm_p_a sa_others) eqs_nonlin
	   
      ) {env with uf=uf}  res
      

  and replay_terms gm_p_t st_others env = 
    SetT.fold 
      (fun x env -> 
	 SetT.fold 
	   (fun y env -> 
              try close_up x y (terms_congr env x y) env
	      with Exception.NotCongruent | Exception.Trivial -> env
	   ) st_others env
      ) gm_p_t env


  and replay_atom env sa eqs_nonlin = 
    let sa = SetA.fold 
      (fun a acc -> (semantic_view env a, Some a)::acc) sa eqs_nonlin 
    in
    replay_atom_r env sa

  and replay_atom_r env sa = 
    let rel, leqs  = X.Rel.assume env.relation sa in
    let rel, atoms = X.Rel.instantiate rel sa (Uf.class_of env.uf) in
    let env = play_eqset {env with relation = rel} leqs in
    (* XXX * Que fait-il mettre comme Explication ? *)
    List.fold_left (fun env a -> assume a Ex.everything env) env atoms

  and play_eqset env leqs =
    List.fold_left
      (fun env (ra, a) -> 
         match ra with
             A.Eq(r1,r2) -> 
	       let env = { env with uf =
                   Uf.add_semantic (Uf.add_semantic env.uf r1) r2 } in
               let r1 = Uf.find_r env.uf r1 in
	       let r2 = Uf.find_r env.uf r2 in
	       let st_r1, sa_r1 = Use.find r1 env.use in
	       let st_r2, sa_r2 = Use.find r2 env.use in
	       let sa_r1', sa_r2' = match a with 
	         | Some a -> SetA.remove a sa_r1, SetA.remove a sa_r2 
	         | None -> sa_r1, sa_r2
	       in
	       let use =  Use.add r1 (st_r1, sa_r1') env.use in
	       let use =  Use.add r2 (st_r2, sa_r2') use in	     
	       close_up_r r1 r2 Ex.everything { env with use = use}

           (* XXX: les tableaux peuvent retourner des diseq aussi ! 
              Il faut factoriser un peu le code par la suite *)
           | A.Neq(r1,r2) -> 
               let dep = Ex.everything in
	       let env = 
		 {env with uf = Uf.distinct_r env.uf r1 r2 dep dep dep} in
               let env = replay_atom_r env [ra,None] in
               env
            (* XXX fin *)

           | _ -> assert false

      ) env leqs

  and congruents e t s acc = 
    SetT.fold 
      (fun t2 acc ->
	 if T.equal t t2 then acc
	 else 
	   try (t,t2,terms_congr e t t2)::acc
	   with Exception.NotCongruent -> acc
      ) s acc
	   
  (* add a new term in env *)   	

  and add_term (env, ct) t = 
    if debug_cc then Print.add_to_use t;
    (* nothing to do if the term already exists *)
    if Uf.mem env.uf t then (env,ct)
    else
      (* we add t's arguments in env *)
      let {T.f = f; xs = xs} = T.view t in
      let env , ct = List.fold_left add_term (env,ct) xs in
      (* we update uf and use *)
      let nuf, ctx  = Uf.add env.uf t in (* XXX *)
      if debug_fm then Print.make_cst t ctx;
      let rt   = Uf.find nuf t in
      let nuse = Use.up_add env.use t rt (concat_leaves nuf xs) in
      
      (* If finitetest is used we add the term to the relation *)
      let rel = X.Rel.add env.relation rt in

      (* print updates in Gamma *)
      if debug_use then Use.print nuse;

      (* we compute terms to consider for congruence *)
      (* we do this only for non-atomic terms with uninterpreted head-symbol *)
      let lvs = concat_leaves nuf xs in
      let st_uset = Use.congr_add nuse lvs in
      
      (* we check the congruence of each term *)
      let env = {uf = nuf; use = nuse; relation = rel} in 
      let env = 
        List.fold_left (fun env a -> assume a Ex.everything env) env ctx in

      (env,congruents env t st_uset ct)
	
  and add a env =
    let st = A.terms_of a in
    let env = 
      SetT.fold
	(fun t env -> 
	   let env , ct = add_term (env,[]) t in
	   List.fold_left
	     (fun e (x,y,dep) -> close_up x y dep e) env ct) st env
    in 
    match A.view a with
	A.Eq _ | A.Neq _ -> env
      | _ ->
	  let lvs = concat_leaves env.uf (Term.Set.elements st) in
	  List.fold_left
	    (fun env rx ->
	       let st_uset , sa_uset = Use.find rx env.use in
	       {env with use=Use.add rx (st_uset,SetA.add a sa_uset) env.use}
	    ) env lvs

  and negate_prop t1 uf bol = 
    match T.view t1 with
	{T.f=f1 ; xs=[a]} ->
	  List.fold_left 
	    (fun acc t2 ->
	       match T.view t2 with
		   {T.f=f2 ; xs=[b]} when S.equal f1 f2 ->
		       (A.make(A.Neq(a,b))) :: acc
		 | _ -> acc
	  
	    )[] (Uf.class_of uf bol)
      | _ -> []
	  
  and assume_rec dep env a = 
    match A.view a with
	A.Eq(t1,t2) ->
	  (*let env = replay_atom env (SetA.singleton a) [] in*)
	  let env = close_up t1 t2 dep env in
	  if Options.nocontracongru then env
	  else begin
	    let facts = match T.equal t2 T.faux , T.equal t2 T.vrai with
	      | true , false -> negate_prop t1 env.uf T.vrai
	      | false, true  -> negate_prop t1 env.uf T.faux
	      | _ , _        -> []
	    in 
            if debug_cc then
              begin
                fprintf fmt "[cc] %d equalities by contra-congruence@." 
                  (List.length facts);
                List.iter (fprintf fmt "\t%a@." A.print) facts;
              end;
	    List.fold_left (assume_rec dep) env facts
	  end
      | A.Neq(t1, t2)-> 
	  let env = {env with uf = Uf.distinct env.uf t1 t2 dep} in
	  let env = replay_atom env (SetA.singleton a) [] in
	  if Options.nocontracongru then env
	  else begin
	    let r1 = Uf.find env.uf t1 in
	    let r2 = Uf.find env.uf t2 in
	    begin
	      match T.view t1,T.view t2 with
		| {T.f = f1; xs = [a]},{T.f = f2; xs = [b]}
		    when (S.equal f1 f2 
			  && X.equal (X.term_embed t1) r1 
			  && X.equal (X.term_embed t2) r2) 
		      -> assume_rec dep env (A.make(A.Neq(a,b)))
		| _,_ -> env
	    end
	  end
	    
      | _ -> replay_atom env (SetA.singleton a) []

  and assume a dep env = 
    let env = assume_rec dep (add a env) a in
    if debug_uf then Uf.print fmt env.uf;
    env

  let assume_r env ra =
    let dep = Ex.everything in
    match ra with
	A.Eq(r1, r2) ->
          
          (* XXX: Hack (arrays); pour passer l'égalité à la relation meme si 
             elle est triviale. Peut etre utilisée pour le case_split 
             de arith ? *)
          let env = replay_atom_r env [ra,None] in
          (* XXX fin *)

          let env = {env with uf =
              Uf.add_semantic (Uf.add_semantic env.uf r1) r2} in
          close_up_r r1 r2 dep env
      | A.Neq(r1, r2)->
	  let env = {env with uf = Uf.distinct_r env.uf r1 r2 dep dep dep} in
          let env = replay_atom_r env [ra,None] in
          env
      | _ ->
          replay_atom_r env [ra,None]


  let rec look_for_sat ?(bad_last=false) t base_env l =
    let dl = List.rev t.choices in
    let rec aux bad_last dl base_env = function
      | [] -> 
	  begin
            match X.Rel.case_split base_env.relation with
	      | [] -> 
		  { t with 
		      gamma_finite = base_env; 
		      choices = List.rev dl }
	      | l -> 
		  let l = List.map (fun (c,_) -> (c, false)) l in
		  aux false dl base_env l
	  end
      | ((c, true) as a)::l ->
	  let base_env = assume_r base_env c in
	  aux bad_last (a::dl) base_env l      

      | [(c, false)] when bad_last ->
          let neg_c = Literal.neg_view c in
          if debug_cc || debug_fm then
            fprintf fmt "[case-split] I backtrack on %a@." 
              (A.print_view X.print) neg_c;
	  aux false dl base_env [(neg_c, true)] 

      | ((c, false) as a)::l ->
	  try
	    let base_env = assume_r base_env c in
	    aux bad_last (a::dl) base_env l
	  with Exception.Inconsistent ->
            let neg_c = Literal.neg_view c in
            if debug_cc || debug_fm then
              fprintf fmt "[case-split] I backtrack on %a@." 
                (A.print_view X.print) neg_c;
	    aux false dl base_env [(neg_c, true)] 
    in
    aux bad_last dl base_env l

  let try_it f t =
    if debug_cc || debug_fm then
      fprintf fmt "============= Debut FINITE ===============@.";
    let r =
    if t.choices = [] then 
      look_for_sat t t.gamma []
    else
      try
	let env = f t.gamma_finite in
	look_for_sat t env []
      with Exception.Inconsistent ->
	look_for_sat ~bad_last:true { t with choices = []} t.gamma t.choices
    in
    if debug_cc || debug_fm then
      fprintf fmt "============= fin FINITE ===============@.";
    r
  
  let assume a ex t = 
    let t = { t with gamma = assume a ex t.gamma } in
    try_it (assume a ex) t  (* XXX: voir les explications *)

  let class_of t term = Uf.class_of t.gamma.uf term

  let explain a t = 
    let env = t.gamma in
    try
    (match A.view a with
      | A.Eq(x,y) -> Uf.explain env.uf x y
      | A.Neq(x,y) -> Uf.neq_explain env.uf x y
      | _ -> Ex.everything)
  with Exception.NotCongruent -> assert false
	      
  let query a t = 
    let env = t.gamma in
    if debug_use then Use.print env.use;
    let env = add a env in
    match A.view a with
      |	A.Eq(t1,t2)  -> Uf.equal env.uf t1 t2
      | A.Neq(t1,t2) -> 
	  Uf.are_distinct env.uf t1 t2
      | _ -> 
          let na = A.neg a in 
          X.Rel.query (semantic_view env na,Some na) env.relation

  let add a t = 
    let t = { t with gamma = add a t.gamma } in
    try_it (add a) t

  let empty () = 
    let env = { 
      use = Use.empty ; 
      uf = Uf.empty ; 
      relation = X.Rel.empty ();
    }
    in
    let t = { gamma = env; gamma_finite = env; choices = [] } in
    assume (A.make (A.Neq(T.vrai,T.faux))) Ex.empty t

  let extract_model env = 
    [], []

end
