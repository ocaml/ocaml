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
open Exception
open Sig

module type S = sig
  type t

  module R : Sig.X

  val empty :  t
  val add : t -> Term.t -> t * Literal.t list
  val add_semantic : t -> R.r -> t 

  val mem : t -> Term.t -> bool

  val find : t -> Term.t -> R.r
  val find_r : t -> R.r -> R.r
  
  val union : 
    t -> R.r -> R.r -> Explanation.t -> t * (R.r * (R.r * R.r) list * R.r) list

  val distinct : t -> Term.t -> Term.t -> Explanation.t -> t

  val equal : t -> Term.t -> Term.t -> bool
  val are_distinct : t -> Term.t -> Term.t -> bool
  val class_of : t -> Term.t -> Term.t list

  val explain : t -> Term.t -> Term.t -> Explanation.t
  val neq_explain : t -> Term.t -> Term.t -> Explanation.t
  val print : Format.formatter -> t -> unit

  val distinct_r : 
    t -> R.r -> R.r -> Explanation.t -> Explanation.t -> Explanation.t -> t

end
  
module Make ( R : Sig.X ) = struct

  module Ac = Ac.Make(R)
  module L  = List
  module HS = Hstring
  module Ex = Explanation
  module R = R
  module S = Symbols
  module T = Term
  module F = Formula
  module MapT = Term.Map
  module SetT = Term.Set
  module SetF = Formula.Set
  
  let equations = Queue.create ()

  module MapR = Map.Make(struct type t = R.r let compare = R.compare end)
	
  module SetR = Set.Make(struct type t = R.r let compare = R.compare end)

  module SetAc = Set.Make(struct type t = Ac.t let compare = Ac.compare end)

  module SetRL = Set.Make
    (struct 
       type t = Ac.t * R.r
       let compare (ac1,_) (ac2,_)= Ac.compare ac1 ac2
     end)

  module RS = struct
    include Map.Make(struct type t = S.t let compare = S.compare end)
    
    let find k m = try find k m with Not_found -> SetRL.empty

    let add_rule (ac,d) m = add ac.h (SetRL.add (ac,d) (find ac.h m)) m

    let remove_rule (ac,d) m = add ac.h (SetRL.remove (ac,d) (find ac.h m)) m
  end

  type t = { 

    (* term -> [t] *)
    make : R.r MapT.t; 
    
    (* representative table *)
    repr : (R.r * Ex.t) MapR.t; 
    
    (* r -> class (of terms) *)
    classes : SetT.t MapR.t;
    
    (*associates each value r with the set of semantical values whose
      representatives contains r *)
    gamma : SetR.t MapR.t; 
    
    (* the disequations map *)
    neqs: Ex.t MapR.t MapR.t; 
    
    (*AC rewrite system *)
    rs : SetRL.t RS.t
  }
      
  let empty = { 
    make  = MapT.empty; 
    repr = MapR.empty;
    classes = MapR.empty; 
    gamma = MapR.empty;
    neqs = MapR.empty;
    rs = RS.empty
  }

  module Print = struct

    let rs_print fmt = SetR.iter (fprintf fmt "\t%a@." R.print)
    let rm_print fmt = MapR.iter (fun k _ -> fprintf fmt "%a " R.print k)

    let t_print fmt = SetT.iter (fprintf fmt "%a " T.print)
      
    let pmake fmt m = 
      fprintf fmt "[.] map:\n";
      MapT.iter (fun t r -> fprintf fmt "%a -> %a\n" T.print t R.print r) m
	
    let prepr fmt m = 
      fprintf fmt "------------- UF: Representatives map ----------------@.";
      MapR.iter 
	(fun r rr -> fprintf fmt "%a --> %a\n" R.print r R.print (fst rr)) m

    let prules fmt s = 
      fprintf fmt "------------- UF: Rewrite rules ----------------------@.";
      RS.iter
	(fun k srl -> 
	   SetRL.iter
	     (fun (ac,d)-> fprintf fmt "%a ~~> %a\n" 
                R.print (R.ac_embed ac) R.print d)srl )s
	
    let pclasses fmt m = 
      fprintf fmt "------------- UF: Class map --------------------------@.";
      MapR.iter 
	(fun k s -> fprintf fmt "%a -> %a\n" R.print k Term.print_list 
	   (SetT.elements s)) m

    let pgamma fmt m = 
      fprintf fmt "------------- UF: Gamma map --------------------------@.";
      MapR.iter (fun k s -> fprintf fmt "%a -> \n%a" R.print k rs_print s) m 
		
    let pneqs fmt m = 
      fprintf fmt "Disequations map:\n";
      MapR.iter (fun k s -> fprintf fmt "%a -> %a\n" R.print k rm_print s) m

    let all fmt env = 
      fprintf fmt "------------------------------------------------------@.";
      fprintf fmt "%a %a %a %a" 
        pmake env.make prepr env.repr prules env.rs pneqs env.neqs;
      fprintf fmt "------------------------------------------------------@."

  end

  let mem env t = MapT.mem t env.make
      
  let lookup_by_t t env =
    try MapR.find (MapT.find t env.make) env.repr
    with Not_found -> 
      if debug_uf then fprintf fmt "Uf: Not_found %a@." Term.print t;
      assert false (*R.make t, Ex.empty*) (* XXXX *)

  let lookup_by_r r env = 
    try MapR.find r env.repr with Not_found -> r, Ex.empty

  let lookup_for_neqs env r =
    try MapR.find r env.neqs with Not_found -> MapR.empty
    

  module Env = struct

    let add_to_classes t r classes =  
      MapR.add r 
	(SetT.add t (try MapR.find r classes with Not_found -> SetT.empty))
	classes
	
    let update_classes c nc classes = 
      let s1 = try MapR.find c classes with Not_found -> SetT.empty in
      let s2 = try MapR.find nc classes with Not_found -> SetT.empty in
      MapR.add nc (SetT.union s1 s2) classes
	
    let add_to_gamma r c gamma = 
      L.fold_left
	(fun gamma x -> 
	   let s = try MapR.find x gamma with Not_found -> SetR.empty in
	   MapR.add x (SetR.add r s) gamma) gamma (R.leaves c)
	
    let merge r1 m1 r2 m2 dep neqs = 
      let m , neqs = 
	MapR.fold 
	  (fun k ex1 (m,neqs) -> 
	     if MapR.mem k m2 then
	       m , MapR.add k (MapR.remove r1 (MapR.find k neqs)) neqs
	     else
	       let ex = Ex.union ex1 dep in
	       let mk = MapR.add r2 ex (MapR.remove r1 (MapR.find k neqs)) in
	       MapR.add k ex m , MapR.add k mk neqs
	  )
	  m1 (m2,neqs)
      in
      MapR.add r2 m neqs

    let update_neqs r1 r2 dep env = 
      let neqs, m1 = 
	try env.neqs, MapR.find r1 env.neqs 
	with Not_found -> MapR.add r1 MapR.empty env.neqs, MapR.empty in
      let neqs, m2 = 
	try neqs, MapR.find r2 neqs 
	with Not_found -> MapR.add r2 MapR.empty neqs, MapR.empty in
      if MapR.mem r2 m1 or MapR.mem r1 m2 then raise Inconsistent;
      merge r1 m1 r2 m2 dep neqs

    let disjoint_union l_1 l_2 = 
      let rec di_un (l1,c,l2) (l_1,l_2)= 
        match l_1,l_2 with
	  | [],[] -> l1, c, l2
	  | l, [] -> di_un (l @ l1,c,l2) ([],[])
	  | [], l -> di_un (l1,c,l @ l2) ([],[])
	  | (a,m)::r, (b,n)::s ->
	    let cmp = R.compare a b in
	    if cmp = 0 then
	      if m = n then di_un (l1,(a,m)::c,l2) (r,s)
	      else if m > n then di_un ((a,m-n)::l1,(a,n)::c,l2) (r,s)
	      else di_un (l1,(b,n)::c,(b,n-m)::l2) (r,s)
	      else if cmp > 0 then di_un ((a,m)::l1,c,l2) (r,(b,n)::s)
	      else di_un (l1,c,(b,n)::l2) ((a,m)::r,s)
      in di_un ([],[],[]) (l_1,l_2)

    exception List_minus_exn
    let list_minus l_1 l_2 = 
      let rec di_un l1 l_1 l_2 = 
        match l_1, l_2 with
	  [],[] -> l1
	| l, [] -> l @ l1
	| [], l -> raise List_minus_exn
	| (a,m)::r, (b,n)::s ->
	    let cmp = R.compare a b in
	    if cmp = 0 then
	      if m = n then di_un l1 r s
	      else if m > n then di_un ((a,m-n)::l1) r s
	      else raise List_minus_exn
	    else if cmp > 0 then di_un ((a,m)::l1) r ((b,n)::s)
	    else raise List_minus_exn
      in di_un [] l_1 l_2


    exception Not_possible of Ac.t * bool
    
    let rec apply_rule (p,v) (ar,fixpoint) =
      let c = Ac.compare ar p in
      if c = 0 then {ar with l=[v,1]}, fixpoint
      else if c < 0 then raise (Not_possible (ar,fixpoint))
      else 
        try 
          let ar = {ar with l=Ac.add ar.h (v,1) (list_minus ar.l p.l)} in
          apply_rule (p,v) (ar, false)
        with List_minus_exn -> (ar,fixpoint)


    let rec apply_rs ac rls = 
      let ac2,fixpoint = 
        try SetRL.fold apply_rule rls (ac,true) 
        with Not_possible (ac2, fixpoint) -> ac2,fixpoint in
      if fixpoint then ac2 else apply_rs ac2 rls
  				     

    let filter_leaves r = 
      L.fold_left 
	(fun (p,q) r -> match R.ac_extract r with 
	   | None    -> SetR.add r p,q
	   | Some ac -> p,SetAc.add ac q
	)(SetR.empty,SetAc.empty) (R.leaves r)
	
    let canon_empty st env = 	
      SetR.fold
	(fun p z -> 
           let q,_ = lookup_by_r p env in 
	   if R.equal p q then z else (p,q)::z )st []

    let canon_ac st env = 
      SetAc.fold
	(fun ac z ->
	   let rac = apply_rs ac (RS.find ac.h env.rs) in
	   if Ac.compare ac rac = 0 then z
	   else (R.color ac, R.color rac) :: z )st []
	
    let canon_aux rx = L.fold_left (fun r (p,v) -> R.subst p v r) rx
      
    let rec canon env r = 
      let se,sac = filter_leaves r in
      let se = canon_empty se env in
      let sac= canon_ac sac env in
      let r2 = canon_aux (canon_aux r sac) se in
      if R.equal r r2 then r2, Ex.empty else canon env r2

    let find_or_canon env r =
      try MapR.find r env.repr with Not_found -> canon env r

    (* A revoir *)
    let add_sm dep env r rr = 
      if debug_uf then 
        fprintf fmt "add_sm:  %a --> %a@." R.print r R.print rr;
      if MapR.mem r env.repr then env 
      else 
	{ env with
	    repr    = MapR.add r (rr,Ex.empty) env.repr;
	    classes = update_classes r rr env.classes;
	    gamma   = add_to_gamma r rr env.gamma ;
	    neqs    = update_neqs r rr dep env } 
      
    let init_term env t = 
      let mkr, ctx = R.make t in
      let rp, ex = canon env mkr in
      {env with
	 make    = MapT.add t mkr env.make; 
	 repr    = MapR.add mkr (rp,ex) env.repr;
	 classes = add_to_classes t rp env.classes;
	 gamma   = add_to_gamma mkr rp env.gamma;
	 neqs    = 
	  if MapR.mem rp env.neqs then env.neqs 
	  else MapR.add rp MapR.empty env.neqs}, ctx


    let add_cp dep env (x,y) = 
      let rx, _ = find_or_canon env x in
      let ry, _ = find_or_canon env y in
      if R.equal rx ry then env
      else begin
        Queue.add (rx, ry) equations;
        if debug_ac then
          fprintf fmt "[uf] critical pair: %a = %a@." R.print rx R.print ry;
        add_sm dep (add_sm dep env ry ry) rx rx
      end

    let head_cp dep env ({h=h} as ac) v = 
      try SetRL.fold
	(fun (p,_P) env ->
	   match disjoint_union ac.l p.l with
	     | _  , [] , _  -> env
	     | l1 , cm , l2 -> 
		 let x = {ac with l = Ac.add h (_P,1) l1} in
		 let y = {p  with l = Ac.add h (v,1)  l2} in
		 add_cp dep env (R.color x, R.color y)
	)(RS.find h env.rs) env
      with Not_found -> env
	
    let comp_collapse dep env p v = 
      RS.fold
	(fun h rls env -> 
          SetRL.fold
	    (fun (g,d) env ->
	      let env = {env with rs = RS.remove_rule (g,d) env.rs} in
	      let g2, _ = canon env (Ac.subst p v g) in
	      let d2, _ = canon env (R.subst p v d) in
	      let gx = R.color g in
	      if R.equal g2 gx then {env with rs= RS.add_rule (g,d2) env.rs}
	      else begin
		Queue.push (g2,d2) equations;
                if debug_ac then
                  fprintf fmt "[uf] collapse: %a = %a@." R.print g2 R.print d2; 
	        add_sm dep (add_sm dep env g2 g2) d2 d2
              end
	    )rls env
	)env.rs env
	
    let update_rs dep env p v = 
      match R.ac_extract p with
	| None    -> comp_collapse dep env p v
	| Some ac -> 
	  let env = {env with rs = RS.add_rule (ac,v) env.rs} in
	  let env = comp_collapse dep env p v in
	  head_cp dep env ac v
						 
    let up_uf_sigma dep env tch p v =
      let use_p = try MapR.find p env.gamma
      with Not_found ->
        fprintf fmt "The key %a is not found@." R.print p;
        assert false
      in
      try SetR.fold 
	(fun r (env, touched) -> 
	   let rr,ex = lookup_by_r r env in
	   let nrr   = R.subst p v rr in
	   if R.equal rr nrr then env, touched
	   else 
	     let ex  = Ex.union ex dep in
	     let env = {
	       env with
		 repr = MapR.add r (nrr,ex) env.repr;
		 classes = update_classes rr nrr env.classes;
		 gamma = add_to_gamma r nrr env.gamma ;
		 neqs = update_neqs rr nrr dep env } in
	     env, (r,nrr)::touched
	) use_p (env,[])
      with Not_found -> assert false
	(* il faut faire le menage dans les maps *)

    (* quelle vraie valeur pour dep ? 
    let up_uf_rs dep env =
      MapR.fold
	(fun r (rr,ex) env ->
	   let nrr,_ = canon env rr in
	   if R.equal nrr rr then env 
	   else 
	   {env with
	      repr = MapR.add r (nrr,ex) env.repr;
	      classes = update_classes rr nrr env.classes;
	      gamma = add_to_gamma r nrr env.gamma ;
	      neqs = update_neqs rr nrr dep env }
	)env.repr env
    *)
    let up_uf_rs dep env tch =
      MapR.fold
	(fun r (rr,ex) (env,tch) ->
	   let nrr,_ = canon env rr in
	   if R.equal nrr rr then env, tch
	   else 
             let env = 
	       {env with
	         repr = MapR.add r (nrr,ex) env.repr;
	         classes = update_classes rr nrr env.classes;
	         gamma = add_to_gamma r nrr env.gamma ;
	         neqs = update_neqs rr nrr dep env } in
             env, (r,[r,nrr],nrr)::tch
	)env.repr (env,tch)

    let update_uf dep env tch p v =  
      let env, touched = up_uf_sigma dep env tch p v in 
      up_uf_rs dep env ((p,touched,v) :: tch)
	
  end
 
  let add env t = 
    if MapT.mem t env.make then env, [] else Env.init_term env t

  let add_semantic env r =
    if MapR.mem r env.repr then env 
    else 
      let rr, _ = Env.canon env r in
      Env.add_sm Ex.everything env r rr



  let ac_solve dep (env,tch) (p,v) = 
    if debug_uf then printf "[uf] ac-solve: %a |-> %a@." R.print p R.print v;
    let rp, _ = Env.find_or_canon env p in
    let rv, _ = Env.find_or_canon env v in
    
    if R.equal p rp then
      (*pour garder les représentants des valeurs ac sinon ça cause pb*)
      let env = match R.ac_extract p with
        | None    -> Env.add_sm dep env p p  (*env *)
        | Some ac -> Env.add_sm dep env p v 
      in
      let env      = Env.update_rs dep env rp rv in
      let env, tch = Env.update_uf dep env tch rp rv in
      env, tch
    else
      begin
        fprintf fmt "@.[uf] ac_solve: %a |-> %a, but we have@." R.print p R.print v;
        fprintf fmt "[uf] ac_solve: repr (%a) = %a !! bad substitution !@."
          R.print p R.print rp;
        assert false
      end
      (* XXX : pas util puisque les substs doivent être 
         triées vis à vis du membre gauche 
      let env = Env.add_sm dep env rp rp in
      let env = Env.add_sm dep env rv rv in
      env, tch, (rp,rv) :: psi
      *)

  let x_solve env r1 r2 = 
    let rr1, _ = lookup_by_r r1 env in
    let rr2, _ = lookup_by_r r2 env in
    if debug_uf then 
      printf "[uf] x-solve: %a = %a@." R.print rr1 R.print rr2;

    if R.equal rr1 rr2 then [] (* Remove rule *)
    else 
      begin
        (* if rr1 is known to be different from rr2, there is inconsistency *)
        let nq_rr1 = lookup_for_neqs env rr1 in
        let nq_rr2 = lookup_for_neqs env rr2 in
        if MapR.mem rr2 nq_rr1 then raise Inconsistent;
        if MapR.mem rr1 nq_rr2 then raise Inconsistent;

        (* solve the equation rr1 = rr2 *)
        let repr r = fst (lookup_by_r r env) in
        R.solve repr rr1 rr2 
      end
        
  let rec ac_x dep env tch =
    try
      let r1, r2 = Queue.pop equations in
      if debug_uf then 
	printf "[uf] ac(x): delta (%a) = delta (%a)@." R.print r1 R.print r2;
      let sbs = x_solve env r1 r2 in
      let env,tch = L.fold_left (ac_solve dep) (env,tch) sbs in
      if debug_uf then Print.all fmt env;
      ac_x dep env tch
    with Queue.Empty -> env, tch

	    
  let union env r1 r2 dep =
    try 
      Queue.clear equations;
      Queue.push (r1,r2) equations;
      ac_x dep env []
    with Unsolvable -> raise Inconsistent

  let make_distinct env r1 r2 dep = 
    let d1 = lookup_for_neqs env r1 in
    let d2 = lookup_for_neqs env r2 in
    let neqs = 
      if MapR.mem r2 d1 then env.neqs else 
	MapR.add r1 (MapR.add r2 dep d1) 
	  (MapR.add r2 (MapR.add r1 dep d2) env.neqs) 
    in
    { env with neqs = neqs}

  let rec distinct_r env r1 r2 ex1 ex2 dep =
    let r1, ex1 = lookup_by_r r1 env in
    let r2, ex2 = lookup_by_r r2 env in
    let dep' = Ex.union ex1 (Ex.union ex2 dep) in
    (* r1 and r2 could not be equal *)
    if R.equal r1 r2 then raise Inconsistent;
    let env = make_distinct env r1 r2 dep' in
    let repr r = fst (lookup_by_r r env) in
    match (try R.solve repr r1 r2 with Unsolvable -> []) with
	[a,b] -> make_distinct env a b dep'
      | _     -> env


  let rec distinct env t1 t2 dep = 
    if debug_uf then 
      printf "[uf] distinct %a <> %a@." T.print t1 T.print t2;
    
    (* add is already done in Cc.assume ? 
       let env = add (add env t1) t2 in*)
    let r1, ex1 = lookup_by_t t1 env in
    let r2, ex2 = lookup_by_t t2 env in
      if debug_uf then 
	begin
	  printf "[uf] delta (%a) = %a@." T.print t1 R.print r1;
	  printf "[uf] delta (%a) = %a@." T.print t2 R.print r2
	end;
      distinct_r env r1 r2 ex1 ex2 dep

  let equal env t1 t2 = 
    let r1, _ = lookup_by_t t1 env in
    let r2, _ = lookup_by_t t2 env in
    R.equal r1 r2

  let are_in_neqs env r1 r2 = 
    (try MapR.mem r1 (MapR.find r2 env.neqs) with Not_found -> false) ||
    (try MapR.mem r2 (MapR.find r1 env.neqs) with Not_found -> false)

  let are_distinct env t1 t2 = 
    let b= 
      let r1,_ = lookup_by_t t1 env in
      let r2,_ = lookup_by_t t2 env in
      if R.equal r1 r2 then false
      else
	are_in_neqs env r1 r2 ||
          try 
            let repr r = fst (lookup_by_r r env) in
	    L.exists 
	      (fun (a,b) -> are_in_neqs env a b) 
	      (R.solve repr r1 r2)
              (* True because r1=r2 <-> /\_{(a,b)in(R.solve r1 r2)}  a=b *)
          with Unsolvable -> true
(*      try
	match T.view t1 , T.view t2 with
	    {T.f=S.Int n1} , {T.f=S.Int n2} -> HS.compare n1 n2 <> 0
	  | _ -> 
	      let nt1 = MapR.find (find m t1) m.neqs in
	      let nt2 = MapR.find (find m t2) m.neqs in
	      SetT.mem t1 nt2 || SetT.mem t2 nt1
      with Not_found -> false*)
    in     
    if debug_uf then
      printf " [uf] are_distinct %a <> %a ? %b@." 
	T.print t1 T.print t2 b; 
    b

  let explain env t1 t2 = 
    if Term.equal t1 t2 then Ex.empty
    else
      let r1, ex1 = MapR.find (MapT.find t1 env.make) env.repr in
      let r2, ex2 = MapR.find (MapT.find t2 env.make) env.repr in
      if R.equal r1 r2 then Ex.union ex1 ex2 
      else raise NotCongruent

  let neq_explain env t1 t2 = 
    let r1, ex1 = lookup_by_t t1 env in
    let r2, ex2 = lookup_by_t t2 env in
    if not (R.equal r1 r2) then Ex.union ex1 ex2 
    else raise NotCongruent
    
  let find env t = fst (lookup_by_t t env)

  let find_r env r = fst (lookup_by_r r env)

  let class_of env t = 
    try 
      let rt, _ = MapR.find (MapT.find t env.make) env.repr in
      SetT.elements (MapR.find rt env.classes)
    with Not_found -> [t]

  let print = Print.all 

end
