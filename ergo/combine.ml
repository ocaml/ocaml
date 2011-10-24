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

module rec CX : sig
  include Sig.X

  val extract1 : r -> X1.t option
  val embed1 : X1.t -> r

  val extract2 : r -> X2.t option
  val embed2 : X2.t -> r

  val extract3 : r -> X3.t option
  val embed3 : X3.t -> r

  val extract4 : r -> X4.t option
  val embed4 : X4.t -> r

end =
struct

  (* Xi < Term < Ac *)
  type r =
    | Term  of Term.t (* XXX changement de l'ordre pour Arrays *)
    | Ac    of AC.t
    | X1    of X1.t 
    | X2    of X2.t 
    | X3    of X3.t 
    | X4    of X4.t 
    
  let extract1 = function X1 r   -> Some r | _ -> None
  let extract2 = function X2 r   -> Some r | _ -> None
  let extract3 = function X3 r   -> Some r | _ -> None
  let extract4 = function X4 r   -> Some r | _ -> None
  
  let embed1 x = X1 x
  let embed2 x = X2 x
  let embed3 x = X3 x
  let embed4 x = X4 x
	
  let is_an_eq a = 
    match Literal.view a with Literal.Builtin _ -> false | _ -> true

  let is_int v = 
    let ty  = match v with
      | X1 x -> X1.type_info x
      | X2 x -> X2.type_info x
      | X3 x -> X3.type_info x
      | X4 x -> X4.type_info x
      | Term t -> (Term.view t).Term.ty
      | Ac x -> AC.type_info x
    in 
    ty = Ty.Tint
      
  let rec compare a b = 
    let c = compare_tag a b in 
    if c = 0 then comparei a b else c

  and compare_tag a b = 
    Pervasives.compare (theory_num a) (theory_num b)
      
  and comparei a b = 
    match a, b with
      | X1 x, X1 y -> X1.compare x y
      | X2 x, X2 y -> X2.compare x y
      | X3 x, X3 y -> X3.compare x y
      | X4 x, X4 y -> X4.compare x y
      | Term x  , Term y  -> Term.compare x y
      | Ac x    , Ac    y -> AC.compare x y
      | _                 -> assert false

  and theory_num x = Obj.tag (Obj.repr x)

  let equal a b = compare a b = 0

  module MR = Map.Make(struct type t = r let compare = compare end)
    
  let print fmt r = 
    if Options.term_like_pp then
      match r with
        | X1 t    -> fprintf fmt "%a" X1.print t
        | X2 t    -> fprintf fmt "%a" X2.print t
        | X3 t    -> fprintf fmt "%a" X3.print t
        | X4 t    -> fprintf fmt "%a" X4.print t
        | Term t  -> fprintf fmt "%a" Term.print t
        | Ac t    -> fprintf fmt "%a" AC.print t
    else
      match r with
        | X1 t    -> fprintf fmt "X1(%s):%a" X1.name X1.print t
        | X2 t    -> fprintf fmt "X2(%s):%a" X2.name X2.print t
        | X3 t    -> fprintf fmt "X3(%s):%a" X3.name X3.print t
        | X4 t    -> fprintf fmt "X3(%s):%a" X4.name X4.print t
        | Term t  -> fprintf fmt "%a" Term.print t
        | Ac t    -> fprintf fmt "Ac:%a" AC.print t
            
  let leaves r = 
    match r with 
      | X1 t -> X1.leaves t 
      | X2 t -> X2.leaves t 
      | X3 t -> X3.leaves t 
      | X4 t -> X4.leaves t 
      | Ac t -> r :: (AC.leaves t)
      | Term _ -> [r]

  let ac_extract = function
      Ac t   -> Some t
    | _ -> None

  let ac_embed ({Sig.l = l} as t) = 
    match l with
      | []    -> 
	  assert false
      | [x, 1] -> x
      | l     -> 
	  let sort = List.fast_sort (fun (x,n) (y,m) -> compare x y) in
	  Ac { t with Sig.l = List.rev (sort l) }

  let term_embed t = Term t

  let term_extract = function Term t -> Some t | _ -> None

  let subst p v r = 
    if equal p v then r 
    else match r with
      | X1 t   -> X1.subst p v t
      | X2 t   -> X2.subst p v t
      | X3 t   -> X3.subst p v t
      | X4 t   -> X4.subst p v t
      | Ac t   -> if equal p r then v else AC.subst p v t
      | Term _ -> if equal p r then v else r

  let rec make t = 
    let {Term.f=sb} = Term.view t in
    match 
      X1.is_mine_symb sb,
      X2.is_mine_symb sb,
      X3.is_mine_symb sb,
      X4.is_mine_symb sb,
      AC.is_mine_symb sb 
    with
      | true  , false , false, false, false -> X1.make t
      | false , true  , false, false, false -> X2.make t
      | false , false , true , false, false -> X3.make t
      | false , false , false, true , false -> X4.make t
      | false , false , false, false, true  -> AC.make t
      | false , false , false, false, false -> Term t, []
      | _ -> assert false
	  
  let color ac = 
    match 
      X1.is_mine_symb ac.Sig.h, 
      X2.is_mine_symb ac.Sig.h, 
      X3.is_mine_symb ac.Sig.h, 
      X4.is_mine_symb ac.Sig.h, 
      AC.is_mine_symb ac.Sig.h with 
	| true  , false , false, false, false -> X1.color ac
	| false , true  , false, false, false -> X2.color ac
	| false , false , true , false, false -> X3.color ac
	| false , false , false, true , false -> X4.color ac
	| false , false , false, false, true  -> ac_embed ac
	| _ -> assert false
    

  let add_mr =
    List.fold_left 
      (fun solved (p,v) -> 
	 MR.add p (v::(try MR.find p solved with Not_found -> [])) solved)

  let unsolvable = function
    | X1 x -> X1.unsolvable x
    | X2 x -> X2.unsolvable x 
    | X3 x -> X3.unsolvable x 
    | X4 x -> X4.unsolvable x 
    | Ac _ | Term _  -> true
	
  let partition tag = 
    List.partition 
      (fun (u,t) -> 
	 (theory_num u = tag || unsolvable u) && 
	   (theory_num t = tag || unsolvable t))

  let rec solve_list repr solved l =
    List.fold_left
      (fun solved (a,b) -> 
         if debug_combine then
           fprintf fmt "solve_list %a=%a@." print a print b;
	 let cmp = compare a b in
	 if cmp = 0 then solved else
	   match a , b with
	       (* both sides are empty *)
	     | (Term _ | Ac _) , (Term _ | Ac _) -> 
		 add_mr solved (unsolvable_values cmp repr a b)
		   
	     (* only one side is empty *)
	     | (a, b) 
                 when unsolvable a || unsolvable b ||  compare_tag a b = 0 ->
		 let a,b = if unsolvable a then b,a else a,b in
		 let cp , sol = partition (theory_num a) (solvei repr b a) in
		 solve_list repr (add_mr solved cp) sol
		   
	     (* both sides are not empty *)
	     | a , b -> solve_theoryj repr solved a b
      ) solved l

  and unsolvable_values cmp repr a b =
    match a, b with
      (* Clash entre theories: On peut avoir ces pbs ? *)
      | X1 _, (X2 _ | X3 _ | X4 _) 
      | (X2 _ | X3 _ | X4 _), X1 _ 
      | X2 _, (X3 _ | X4 _) 
      | (X3 _ | X4 _), X2 _ 
      | X3 _, X4 _
      | X4 _, X3 _ -> assert false

      (* theorie d'un cote, vide de l'autre *)
      | X1 _, _ | _, X1 _ -> X1.solve repr a b
      | X2 _, _ | _, X2 _ -> X2.solve repr a b
      | X3 _, _ | _, X3 _ -> X3.solve repr a b
      | X4 _, _ | _, X4 _ -> X4.solve repr a b
      | (Ac _|Term _), (Ac _|Term _) -> [if cmp > 0 then a,b else b,a]

  and solve_theoryj repr solved xi xj =
    if debug_combine then
      fprintf fmt "solvej %a=%a@." print xi print xj;
    let cp , sol = partition (theory_num xj) (solvei repr xi xj) in
    solve_list repr (add_mr solved cp) (List.rev_map (fun (x,y) -> y,x) sol)

  and solvei repr a b =
    if debug_combine then
      fprintf fmt "solvei %a=%a@." print a print b;
    match b with
      | X1 _ -> X1.solve repr a b
      | X2 _ -> X2.solve repr a b
      | X3 _ -> X3.solve repr a b
      | X4 _ -> X4.solve repr a b
      | Term _ | Ac _ -> 
          (* XXX pour Arrays *)
          match a with
            | X4 _  -> X4.solve repr a b
            | _ -> 
	        fprintf fmt "solvei %a = %a @." print a print b;
	        assert false

  let rec solve_rec repr mt ab = 
    let mr = solve_list repr mt ab in
    let mr , ab = 
      MR.fold 
	(fun p lr ((mt,ab) as acc) -> match lr with
	     [] -> assert false
	   | [_] -> acc
	   | x::lx -> 
	       MR.add p [x] mr , List.rev_map (fun y-> (x,y)) lx)	 
	mr (mr,[])
    in 
    if ab=[] then mr else solve_rec repr mr ab
      
  let solve repr a b =
    MR.fold 
      (fun p lr ret -> 
	 match lr with [r] -> (p ,r)::ret | _ -> assert false) 
      (solve_rec repr MR.empty [a,b]) []


  let solve repr a b =
    if debug_combine then 
      fprintf fmt "[combine] solving %a = %a yields:@." print a print b;
    let sbs = solve repr a b in
    if debug_combine then begin
      let cpt = ref 0 in
      List.iter 
        (fun (p,v) -> 
           incr cpt;
           fprintf fmt " %d) %a |-> %a@." !cpt print p print v) sbs
    end;
    sbs




  let rec type_info = function
    | X1 t   -> X1.type_info t
    | X2 t   -> X2.type_info t
    | X3 t   -> X3.type_info t
    | X4 t   -> X4.type_info t
    | Ac x   -> AC.type_info x
    | Term t -> let {Term.ty = ty} = Term.view t in ty
	
  module Rel =
  struct
    type elt = r
    type r = elt

    type t = { r1: X1.Rel.t; r2: X2.Rel.t; r3: X3.Rel.t;  r4: X4.Rel.t}
    let empty _ = {
      r1=X1.Rel.empty (); 
      r2=X2.Rel.empty (); 
      r3=X3.Rel.empty ();
      r4=X4.Rel.empty ();
    }
	
    let assume env sa = 
      let env1, seq1 = X1.Rel.assume env.r1 sa in
      let env2, seq2 = X2.Rel.assume env.r2 sa in
      let env3, seq3 = X3.Rel.assume env.r3 sa in
      let env4, seq4 = X4.Rel.assume env.r4 sa in
      {r1=env1; r2=env2; r3=env3; r4=env4}, seq1@seq2@seq3@seq4

    let instantiate env sa class_of = 
      let env1, seq1 = X1.Rel.instantiate env.r1 sa class_of in
      let env2, seq2 = X2.Rel.instantiate env.r2 sa class_of in
      let env3, seq3 = X3.Rel.instantiate env.r3 sa class_of in
      let env4, seq4 = X4.Rel.instantiate env.r4 sa class_of in 
      {r1=env1; r2=env2; r3=env3; r4=env4}, seq1@seq2@seq3@seq4
	
    let query a env = 
      X1.Rel.query a env.r1 
      || X2.Rel.query a env.r2 
      || X3.Rel.query a env.r3
      || X4.Rel.query a env.r4

    let case_split env = 
      let seq1 = X1.Rel.case_split env.r1 in
      let seq2 = X2.Rel.case_split env.r2 in
      let seq3 = X3.Rel.case_split env.r3 in
      let seq4 = X4.Rel.case_split env.r4 in
      seq1 @ seq2 @ seq3 @ seq4

    let add env r =
      {r1=X1.Rel.add env.r1 r;
       r2=X2.Rel.add env.r2 r;
       r3=X3.Rel.add env.r3 r;
       r4=X4.Rel.add env.r4 r}
  end

end

and TX1 : Polynome.T with type r = CX.r = Arith.Type(CX)

and X1 : Sig.THEORY  with type t = TX1.t and type r = CX.r =
  Arith.Make(CX)(TX1)
    (struct
       type t = TX1.t
       type r = CX.r
       let extract = CX.extract1
       let embed =  CX.embed1
     end)

and X2 : Sig.THEORY with type r = CX.r and type t = CX.r Pairs.abstract =
  Pairs.Make
    (struct
       include CX
       let extract = extract2
       let embed = embed2
     end)

and X3 : Sig.THEORY with type r = CX.r and type t = CX.r Bitv.abstract =
  Bitv.Make
    (struct
       include CX
       let extract = extract3
       let embed = embed3
     end)

and X4 : Sig.THEORY with type r = CX.r and type t = CX.r Arrays.abstract =
  Arrays.Make
    (struct
       include CX
       let extract = extract4
       let embed = embed4
     end)

 (* Its signature is not Sig.THEORY because it doen't provide a solver *)
and AC : Ac.S with type r = CX.r = Ac.Make(CX)
