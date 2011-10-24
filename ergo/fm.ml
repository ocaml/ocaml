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

open Num
open Format
open Options
open Sig

let ale = Hstring.make "<=" 
let alt = Hstring.make "<"
let is_le n = Hstring.compare n ale = 0
let is_lt n = Hstring.compare n alt = 0

module A = Literal
  
exception NotConsistent of Literal.Set.t

module type EXTENDED_Polynome = sig
  include Polynome.T
  val poly_of : r -> t
  val alien_of : t -> r
end

module Make 
  (X : Sig.X)
  (P : EXTENDED_Polynome with type r = X.r) = struct

  module SP = Set.Make(P)
  module MP = Map.Make(P)
  module MX = Map.Make(struct type t = X.r include X end)
  
  type r = P.r

  module Seq = 
    Set.Make
      (struct
         type t = r A.view * A.t option 
         let compare (a,_) (b,_) = 
           let values_of = function
             | A.Eq (r1,r2) | A.Neq(r1,r2) | A.Builtin(_,_,[r1;r2]) -> r1, r2
             | _ -> assert false 
           in 
           let r1, r2 = values_of a in
           let s1, s2 = values_of b in
           let c = X.compare r1 s1 in
           if c <> 0 then c else X.compare r2 s2
       end)

      
  module Inequation = struct
    type t = { 
      ple0 : P.t; 
      is_le : bool;
      dep : (Literal.t * num * P.t * bool) list
    }
	
    let print fmt ineq = fprintf fmt "%a %s 0" P.print ineq.ple0
      (if ineq.is_le then "<=" else "<")

    let create p1 p2 is_le a = 
      let p = P.add p1 (P.mult (P.create [] (Int (-1)) (P.type_info p1)) p2) in
      { ple0 = p; is_le = is_le; dep = [a, Int 1, p, is_le] }

    let choose ineq = snd (P.choose ineq.ple0)

    let find x ineq = P.find x ineq.ple0

    let is_monomial ineq = P.is_monomial ineq.ple0

  end

  type t = { 
    inequations : (Literal.t * Inequation.t) list ;
    new_inequations : (Literal.t * Inequation.t) list ;
    monomes: Intervals.t MX.t;
    polynomes : Intervals.t MP.t;
    known_eqs : SP.t;
  }

  module Debug = struct
 
    let list_of_ineqs fmt = List.iter (fprintf fmt "%a  " Inequation.print)

    let assume a = 
      if debug_fm then 
	fprintf fmt "[fm] We assume: %a@." (A.print_view X.print) a
	
    let cross x cpos cneg others ninqs =
      if debug_fm then begin
	fprintf fmt "[fm] We cross on %a@." X.print x;
	fprintf fmt "with:@.  cpos = %a@.  cneg = %a@.  others = %a@."
	  list_of_ineqs cpos list_of_ineqs cneg 
	  list_of_ineqs others;
	fprintf fmt "result:@.  ninqs = %a@."
	  list_of_ineqs ninqs
      end

    let env env = 
      if debug_fm then begin
	fprintf fmt "------------- FM: inequations-------------------------@.";
        List.iter
          (fun (a,{Inequation.ple0=p; is_le=is_le}) ->
             fprintf fmt "%a%s0  |  %a@." 
               P.print p (if is_le then "<=" else "<") A.print a
          )env.inequations;
        fprintf fmt "------------- FM: monomes ----------------------------@.";
	MX.iter 
	  (fun x i -> 
		fprintf fmt "%a : %a@." X.print x Intervals.print i) 
	  env.monomes;
	fprintf fmt "------------- FM: polynomes---------------------------@.";
	MP.iter 
	  (fun p i -> 
		fprintf fmt "%a : %a@." P.print p Intervals.print i) 
	  env.polynomes;
	fprintf fmt "------------------------------------------------------@."
      end

    let implied_equalities l = 
      if debug_fm then 
        begin
          fprintf fmt "[fm] %d implied equalities@." (List.length l);
          List.iter 
	    (fun (ra, _) -> 
               fprintf fmt "  %a@." (A.print_view X.print) ra) l
        end
  end
      
  let empty _ = { 
    inequations = [] ; 
    new_inequations = [] ; 
    monomes = MX.empty ; 
    polynomes = MP.empty ; 
    known_eqs = SP.empty ; 
  }

  let replace_inequation env x ineq = 
    { env with
	inequations = List.remove_assoc x env.inequations;
	new_inequations = 
	(x, ineq)::(List.remove_assoc x env.new_inequations) }

  let oldify_inequations env =
    { env with
	inequations = env.inequations@env.new_inequations;
	new_inequations = [] }
    

  let update_min env eqs ineq = assert false
      
  let mult_bornes_vars vars env ty=
    List.fold_left
      (fun ui (y,n) ->
	 let ui' = try
	   MX.find y env.monomes
	 with Not_found -> Intervals.undefined ty
	 in
	 Intervals.mult ui (Intervals.power n ui')
      ) (Intervals.point (Int 1) ty) vars 

  let rec tighten_non_lin x env =
    try
      let ty = X.type_info x in
      let u =
	try MX.find x env.monomes
	with Not_found -> Intervals.undefined ty in
      match X.ac_extract x with
	| Some {h=h;t=t;l=[x,n]} 
            when Symbols.equal h (Symbols.Op Symbols.Mult) && n mod 2 = 0  ->
	    let u = Intervals.sqrt u in
	    let x = if n > 2 then X.ac_embed {h=h;t=t;l=[x,n/2]} else x in
	    let u =
	      let pu = 
		try MX.find x env.monomes 
		with Not_found -> Intervals.undefined ty
	      in
	      Intervals.intersect pu u in
	    if n = 2 then { env with monomes = MX.add x u env.monomes}
	    else
	      let u = 
		if (n/2) mod 2 = 0 then
		  Intervals.new_borne_inf (Int 0) true u
		else u
	      in
	      let env = tighten_non_lin x env in
	      { env with monomes = MX.add x u env.monomes }
	| Some {h=h;t=t;l=[x,n]} when Symbols.equal h (Symbols.Op Symbols.Mult)
	    && n > 2 ->
	    let u = Intervals.root n u in
	    let u = 
	      let pu = try 
		MX.find x env.monomes 
	      with Not_found -> Intervals.undefined ty
	      in
	      Intervals.intersect pu u in
	    { env with monomes = MX.add x u env.monomes }
	| _ -> env
    with Intervals.Not_a_float -> env
      


  let init_monomes env p = 
    let ty = P.type_info p in
    let u = Intervals.undefined ty in
    let mon = 
      List.fold_left
	(fun mon (_, x) ->
	   if MX.mem x mon then mon
	   else 
             let ui = match  X.ac_extract x with
	       | Some {h=h; l=l} 
		   when Symbols.equal h (Symbols.Op Symbols.Mult) ->
	           Intervals.intersect (mult_bornes_vars l env ty) u
	       | _ -> u 
             in 
             MX.add x ui mon
        ) env.monomes (fst (P.to_list p))
    in
    { env with monomes = mon }

  let update_ple0 env p is_le =
    if P.is_empty p then env
    else 
      let ty = P.type_info p in
      let a, _ = P.choose p in
      let p, change =
	if a </ Int 0 then
	  P.mult (P.create [] (Int (-1)) ty) p, true
	else p, false in
      let p, c = P.normal_form p in
      let c = minus_num c in
      let u =
	if change then
	  Intervals.new_borne_inf c is_le (Intervals.undefined ty)
	else
	  Intervals.new_borne_sup c is_le (Intervals.undefined ty) in
      let u =
	try Intervals.intersect u (MP.find p env.polynomes)
	with Not_found -> u 
      in
      { env with polynomes = MP.add p u env.polynomes }

  let intervals_from_monomes env p =
    let pl, v = P.to_list p in
    List.fold_left
      (fun i (a, x) ->
	 let i_x = MX.find x env.monomes in
	 Intervals.add (Intervals.scale a i_x) i
      ) (Intervals.point v (P.type_info p)) pl

  let update_polynomes env =
    let polynomes = MP.fold
      (fun p i polynomes ->
	 let new_i = intervals_from_monomes env p in
	 let i = Intervals.intersect i new_i in
	 MP.add p i polynomes
      ) env.polynomes env.polynomes in
    {env with polynomes = polynomes }


  let find_one_eq x u =
    match Intervals.is_point u with
      | Some v when X.type_info x <> Ty.Tint or is_integer_num v ->
          let eq = A.Eq (x,(P.alien_of (P.create [] v (X.type_info x)))) in
	  Some (eq,None)
      | _ -> None

  let find_eq eqs x u env =
    match find_one_eq x u with
      | None -> eqs
      | Some eq1 ->
	  begin
            match X.ac_extract x with
	      | Some {h=h;l=[y,n]} 
		  when Symbols.equal h (Symbols.Op Symbols.Mult) ->
		  let neweqs = try
		    let u = MX.find y env.monomes in
		    match find_one_eq y u with
		      | None -> eq1::eqs
		      | Some eq2 -> eq1::eq2::eqs
		  with Not_found -> eq1::eqs
		  in neweqs
	      | _ -> eq1::eqs
	  end

  type ineq_status = 
    | Trivial_eq
    | Trivial_ineq of num
    | Bottom
    | Monome of num * P.r * num
    | Other

  let ineq_status ({Inequation.ple0 = p ; is_le = is_le} as ineq) = 
    match Inequation.is_monomial ineq with
	Some (a, x, v) -> Monome (a, x, v)
      | None -> 
	  if P.is_empty p then
	    let _, v = P.to_list p in 
	    let c = compare_num v (Int 0) in
	    if c > 0 || (c >=0 && not is_le) then Bottom
	    else 
	      if c = 0 && is_le then Trivial_eq
	      else Trivial_ineq v
	  else Other
	    
  (*let ineqs_from_dep dep borne_inf is_le =
    List.map
      (fun {poly_orig = p; coef = c} -> 
	 let (m,v,ty) = P.mult_const minusone p in
	 (* quelle valeur pour le ?????? *)
	 { ple0 = {poly = (m, v +/ (borne_inf // c), ty); le = is_le} ;
	   dep = []}
      )dep*)

  let mk_equality p =
    let r1 = P.alien_of p in
    let r2 = P.alien_of (P.create [] (Int 0) (P.type_info p)) in
    A.Eq(r1,r2)

  let fm_equalities env eqs {Inequation.ple0 = p; dep = dep} =
    let inqs, ninqs, eqs =
      List.fold_left
	(fun (inqs, ninqs, eqs) (a, _, p, _) -> 
           List.remove_assoc a inqs, List.remove_assoc a ninqs,
	   (mk_equality p, Some a) :: eqs
	) (env.inequations, env.new_inequations, eqs) dep
    in
    { env with inequations = inqs; new_inequations = ninqs }, eqs

  let update_intervals env eqs (a, x, v) is_le =
    let uints = 
      match X.ac_extract x with
	| Some {h=h; l=l} 
	    when Symbols.equal h (Symbols.Op Symbols.Mult) ->
	    let u' = MX.find x env.monomes in
	    Intervals.intersect
	      (mult_bornes_vars l env (X.type_info x)) u'
	| _ -> MX.find x env.monomes
    in
    let b = ((Int (-1)) */ v) // a in
    let u =
      if a >/ (Int 0) then
	Intervals.new_borne_sup b is_le uints
      else   
	Intervals.new_borne_inf b is_le uints in
    let env = { env with monomes = MX.add x u env.monomes } in
    let env =  tighten_non_lin x env in
    env, (find_eq eqs x u env)
  
  let add_inequations acc lin = 
    List.fold_left
      (fun (env, eqs) ineq ->
	 match ineq_status ineq with
	   | Bottom           -> 
	       raise Exception.Inconsistent
		 
	   | Trivial_eq       -> 
	       fm_equalities env eqs ineq
		 
	   | Trivial_ineq  c  ->
	       let n, pp = 
		 List.fold_left 
		   (fun ((n, pp) as acc) (_, _, p, is_le) ->  
		      if is_le then acc else 
			match pp with
			  | Some _ -> n+1, None
			  | None when n=0 -> 1, Some p
			  | _ -> n+1, None) (0,None) ineq.Inequation.dep
		    in
	       let env = 
		 List.fold_left
		   (fun env (_, coef, p, is_le) ->
		      let ty = P.type_info p in
		      let is_le = 
			match pp with 
			    Some x -> P.compare x p = 0 | _ -> is_le && n=0 
		      in
		      let p' = P.sub (P.create [] (c // coef) ty) p in
		      update_ple0 env p' is_le
		   ) env ineq.Inequation.dep
	       in
	       env, eqs

	   | Monome (a, x, v) ->
	       let env, eqs = 
		 update_intervals env eqs (a, x, v) ineq.Inequation.is_le in
               
	       (*let env,eqs = update_bornes env eqs ((a,x),c) ineq.ple0.le in
		 let env,eqs = update_polynomes env eqs ineq in
		 env, pers_ineqs, eqs*)
	       env, eqs

	   | Other            -> 
	       env, eqs
	       (*t env,eqs = update_polynomes env eqs ineq in
	       env, pers_ineqs, eqs*)

	       
      ) acc lin

  let mult_list c = 
    List.map (fun (a, coef, p, is_le) -> (a, coef */ c, p, is_le))

  let cross x cpos cneg = 
    let rec cross_rec acc = function 
      | [] -> acc
      | { Inequation.ple0 = p1; is_le = k1; dep = d1 } :: l ->
	  let n1 = abs_num (P.find x p1) in
	  let acc = 
	    List.fold_left 
	      (fun acc {Inequation.ple0 = p2; is_le = k2; dep=d2} ->
		 let n2 = abs_num (P.find x p2) in
		 let p = P.add
		   (P.mult (P.create [] n2 (P.type_info p2)) p1)
		   (P.mult (P.create [] n1 (P.type_info p1)) p2) in
		 let d1 = mult_list n2 d1 in
		 let d2 = mult_list n1 d2 in
		 let ni = 
		   { Inequation.ple0 = p;  is_le = k1&&k2; dep = d1@d2 }
		 in 
		 ni::acc
	      ) acc cpos
	  in 
	  cross_rec acc l
    in
    cross_rec [] cneg

  let split x l = 
    let rec split_rec (cp, cn, co) ineq =
      try
	let a = Inequation.find x ineq in
	if a >/ (Int 0) then ineq::cp, cn, co 
	else cp, ineq::cn, co
      with Not_found ->	cp, cn, ineq::co
    in 
    List.fold_left split_rec ([], [], []) l

  let length s = SP.fold (fun _ acc -> acc+1) s 0          

  let rec fourier ( (env, eqs) as acc) l nl = match nl with
    | [] -> acc
    | ineq:: nl' ->
	try
	  let x = Inequation.choose ineq in
	  let ncpos, ncneg, nothers = split x nl in
	  let cpos, cneg, others = split x l in
	  let ninqs =
	    (cross x ncpos ncneg)@(cross x ncpos cneg)@(cross x cpos ncneg) in
	  Debug.cross x (cpos@ncpos) (cneg@ncneg) (others@nothers) ninqs;
	  fourier (add_inequations acc (ncpos@ncneg)) others (ninqs@nothers)
	with Not_found -> fourier (add_inequations acc [ineq]) l nl'

  let fm env eqs = 
    fourier (env, eqs)
      (List.map snd env.inequations)
      (List.map snd env.new_inequations)
  
  let is_num r = 
    let ty = X.type_info r in ty = Ty.Tint || ty = Ty.Treal

  let add_disequality env eqs p =
    let ty = P.type_info p in
    match P.to_list p with
      | ([], (Int 0)) ->
	  raise Exception.Inconsistent
      | ([], v) ->
	  env, eqs
      | ([a, x], v) -> 
	  let b = (minus_num v) // a in
	  let i1 = Intervals.point b ty in
	  let i2 = 
	    try 
	      MX.find x env.monomes 
	    with Not_found -> Intervals.undefined ty 
	  in
	  let i = Intervals.exclude i1 i2 in
	  let env ={ env with monomes = MX.add x i env.monomes } in
	  let env = tighten_non_lin x env in
	  env, find_eq eqs x i env
      | _ ->
	  let a, _ = P.choose p in
	  let p = if a >=/ Int 0 then p
	  else P.mult (P.create [] (Int (-1)) ty) p in
	  let p, c = P.normal_form p in
	  let i1 = Intervals.point (minus_num c) ty in
	  let i2 = 
	    try 
	      MP.find p env.polynomes 
	    with Not_found -> Intervals.undefined ty
	  in
	  let i = Intervals.exclude i1 i2 in
	  let env ={ env with polynomes = MP.add p i env.polynomes } in
	  env, eqs

  let add_equality env eqs p =
    let ty = P.type_info p in
    match P.to_list p with	
      | ([], Int 0) -> env, eqs
      | ([], v) ->
	  raise Exception.Inconsistent
      | ([a, x], v) -> 
	  let b = (minus_num v) // a in
	  let i = Intervals.point b ty in
	  let i = 
	    try Intervals.intersect (MX.find x env.monomes) i
	    with Not_found -> i  
	  in
	  let env = { env with monomes = MX.add x i env.monomes} in
	  let env = tighten_non_lin x env in
	  env, find_eq eqs x i env
      | _ ->
	  let a, _ = P.choose p in
	  let p = if a >=/ Int 0 then p
	  else P.mult (P.create [] (Int (-1)) ty) p in
	  let p, c = P.normal_form p in
	  let i = Intervals.point (minus_num c) ty in
	  let i = 
	    try Intervals.intersect (MP.find p env.polynomes) i
	    with Not_found -> i 
	  in
	  let env = { env with 
                        polynomes = MP.add p i env.polynomes;
                        known_eqs = SP.add p env.known_eqs
                    } in
	  env, eqs

  let normal_form = function
    | A.Builtin(false,n,[r1;r2]) when is_le n && X.type_info r1 = Ty.Tint ->
        let pred_r1 = P.sub (P.poly_of r1) (P.create [] (Int 1) Ty.Tint) in
	A.Builtin(true, n, [r2; P.alien_of pred_r1])

    | A.Builtin(true,n,[r1;r2]) when 
	not (is_le n) && X.type_info r1 = Ty.Tint ->
        let pred_r2 = P.sub (P.poly_of r2) (P.create [] (Int 1) Ty.Tint) in
	A.Builtin(true, ale, [r1; P.alien_of pred_r2])

    | A.Builtin(false,n,[r1;r2]) when is_le n -> 
	A.Builtin(true, alt, [r2;r1])

    | A.Builtin(false,n,[r1;r2]) when is_lt n ->
	A.Builtin(true, ale, [r2;r1])

    | a -> a
	  
  let remove_trivial_eqs eqs la = 
      let set_of l = 
        List.fold_left (fun s e -> Seq.add e s) Seq.empty l 
      in
      Seq.elements (Seq.diff (set_of eqs) (set_of la))
          

  let equalities_from_polynomes env eqs =
    let known, eqs = 
      MP.fold
      (fun p i (knw, eqs) ->
         if SP.mem p knw then knw, eqs
         else 
           match Intervals.is_point i with
             Some num ->
               let r1 = P.alien_of p in
               let r2 = P.alien_of (P.create [] num (P.type_info p)) in
               SP.add p knw, (A.Eq(r1, r2),None) :: eqs
           | None -> knw, eqs
      ) env.polynomes  (env.known_eqs, eqs)
    in {env with known_eqs= known}, eqs
                            
  let assume env la  = 
    Debug.env env;
    try
      let env, eqs, new_ineqs = 
	List.fold_left
	  (fun (env, eqs, new_ineqs) (a, root) ->
	     let a = normal_form a in
             if debug_fm then Debug.assume a;
             match a with
	       | A.Builtin(_, n, [r1;r2]) when is_le n || is_lt n ->
                   let root = match root with
	               Some a -> a | None -> assert false in
		   let p1 = P.poly_of r1 in
		   let p2 = P.poly_of r2 in
		   let ineq = Inequation.create p1 p2 (is_le n) root in
		   let env = init_monomes env ineq.Inequation.ple0 in
		   let env = update_ple0 env ineq.Inequation.ple0 (is_le n) in
		   let env = replace_inequation env root ineq in
		   env, eqs, true

	       | A.Neq(r1, r2) when is_num r1 && is_num r2 -> 
		   let p = P.sub (P.poly_of r1) (P.poly_of r2) in
		   let env = init_monomes env p in
		   let env, eqs = add_disequality env eqs p in
                   env, eqs, new_ineqs
		  
	       | A.Eq(r1, r2) when is_num r1 && is_num r2 -> 
		   let p = P.sub (P.poly_of r1) (P.poly_of r2) in
		   let env = init_monomes env p in
		   let env, eqs = add_equality env eqs p in
                   env, eqs, new_ineqs

	       | _ -> (env, eqs, new_ineqs) )
	  (env, [],false) la in

      (* we only call fm when new ineqs are assumed *)
      let env, eqs = if new_ineqs then fm env eqs else env,eqs in
      let env = oldify_inequations env in
      let env = update_polynomes env in
      let env, eqs = equalities_from_polynomes env eqs in
      Debug.env env;
      let eqs = remove_trivial_eqs eqs la in
      Debug.implied_equalities eqs;
      env, eqs

    with Intervals.NotConsistent  -> raise Exception.Inconsistent

  let query ra env =
    try 
      ignore(assume env [ra]); 
      false 
    with Exception.Inconsistent -> true 

  let case_split_polynomes env = 
    let o = MP.fold
      (fun p i o ->
	 match Intervals.finite_size i with
	   | Some s when s >/ (Int 1) ->
	       begin
		 match o with
		   | Some (s',p',n') when s' <=/ s -> o
		   | _ -> Some (s, p, Intervals.borne_inf i)
	       end
	   | _ -> o
      ) env.polynomes None in
    match o with 
      | Some (s,p,n) -> 
          let split = Intervals.point n (P.type_info p) in
          let pls = MP.add p split env.polynomes in
          let r1 = P.alien_of p in
	  let r2 = P.alien_of (P.create [] n  (P.type_info p)) in
	  if debug_fm then
	    fprintf fmt "[case-split] %a = %a@." X.print r1 X.print r2;
	  { env with polynomes = pls }, [A.Eq(r1, r2),None]
      | None -> 
	  if debug_fm then fprintf fmt "[case-split] polynomes: nothing@.";
	  env, []

  let case_split_monomes env = 
    let o = MX.fold
      (fun x i o ->
	 match Intervals.finite_size i with
	   | Some s when s >/ (Int 1) ->
	       begin
		 match o with
		   | Some (s',p',n') when s' <=/ s -> o
		   | _ -> Some (s, x, Intervals.borne_inf i)
	       end
	   | _ -> o
      ) env.monomes None in
    match o with 
      | Some (s,x,n) -> 
          let ty = X.type_info x in
          let split = Intervals.point n ty in
          let mns = MX.add x split env.monomes in
          let r1 = x in
	  let r2 = P.alien_of (P.create [] n  ty) in
	  if debug_fm then
	    fprintf fmt "[case-split] %a = %a@." X.print r1 X.print r2;
	  { env with monomes = mns }, [A.Eq(r1, r2),None]
      | None -> 
	  if debug_fm then fprintf fmt "[case-split] monomes: nothing@.";
	  env, []
   
  let case_split env = 
    match case_split_polynomes env with
      | _ , []     -> snd (case_split_monomes env)
      | _, choices -> choices
   
  let add env _ = env

  let instantiate env _ _ = env, []

end
