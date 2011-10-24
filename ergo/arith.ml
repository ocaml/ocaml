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
open Num
module A = Literal
module Sy = Symbols


let ale = Hstring.make "<=" 
let alt = Hstring.make "<"
let is_le n = Hstring.compare n ale = 0
let is_lt n = Hstring.compare n alt = 0
let mod_symb = Sy.name "@mod"

module Type (X:Sig.X) : Polynome.T with type r =  X.r = struct

  module Ac = Ac.Make(X)

  let mult v1 v2 = 
    X.ac_embed
      { h = Sy.Op Sy.Mult;
	t = X.type_info v1;
	l = let l2 = match X.ac_extract v1 with
	  | Some {h=h; l=l} when Sy.equal h (Sy.Op Sy.Mult) -> l 
	  | _ -> [v1, 1]
	in Ac.add (Sy.Op Sy.Mult) (v2,1) l2 }

  include Polynome.Make(struct include X let mult = mult end)

end

module Make 
  (X : Sig.X)
  (P : Polynome.T with type r = X.r)
  (C : Sig.C with type t = P.t and type r = X.r) = struct

  type t = P.t

  type r = P.r

  let name = "arith"

  let is_mine_a a = 
    match A.view a with
      | A.Builtin (_,p,_) -> is_le p || is_lt p
      | _ -> false

  let is_mine_symb = function
    | Sy.Int _ | Sy.Real _ 
    | Sy.Op 
	(Sy.Plus | Sy.Minus | Sy.Mult 
	| Sy.Div | Sy.Modulo) -> true
    | _ -> false

  let is_mine_type p = 
    let ty = P.type_info p in 
    ty = Ty.Tint || ty = Ty.Treal

  let unsolvable _ = false
	  
  let empty_polynome ty = P.create [] (Int 0) ty

  let is_mine p = 
    match P.is_monomial p with
      | Some (a,x,b) when a =/ (Int 1) && b =/ (Int 0) -> x
      | _ -> C.embed p
    
  let embed r =
    match C.extract r with
      | Some p -> p
      | _ -> P.create [Int 1, r] (Int 0) (X.type_info r)  

  let check_int exn p =  
    if P.type_info p = Ty.Tint then
      let _, c = P.to_list p in
      let ppmc = P.ppmc_denominators p in
      if not (is_integer_num (ppmc */ c)) then raise exn
      
  let fresh_var = 
    let cpt = ref (-1) in
    fun () -> 
      incr cpt; 
      let sy = Sy.name (sprintf "k_%d" !cpt) in
      let t = Term.make sy [] Ty.Tint in 
      fst (X.make t)
      
  let fresh_term = 
    let cpt = ref (-1) in
    fun () -> 
      incr cpt; 
      Term.make (Sy.name (sprintf "k_%d" !cpt)) [] Ty.Tint

  let tbl = Hashtbl.create 17      
  

  (* t1 % t2 = md  <-> 
     c1. 0 <= md ;
     c2. md < t2 ;
     c3. exists k. t1 = t2 * k + t ;
     c4. t2 <> 0 (already checked) *)
  let mk_modulo md t1 t2 ctx = 
    let zero = Term.int "0" in
    let c1 = A.make (A.Builtin(true, ale, [zero; md])) in
    let c2 = A.make (A.Builtin(true, alt, [md; t2])) in
    let k  = fresh_term () in
    let t3 = Term.make (Sy.Op Sy.Mult) [t2;k] Ty.Tint in
    let t3 = Term.make (Sy.Op Sy.Plus) [t3;md] Ty.Tint in
    let c3 = A.make (A.Eq (t1, t3)) in
    c3 :: c2 :: c1 :: ctx    

  let mk_euc_division p p2 t1 t2 ctx = 
    match P.to_list p2 with
      | [], coef_p2 ->
          let md = Term.make (Sy.Op Sy.Modulo) [t1;t2] Ty.Tint in
          let r, ctx' = X.make md in
          let rp = P.mult (P.create [] ((Int 1) //coef_p2) Ty.Tint) (embed r) in
          P.sub p rp, ctx' @ ctx
      | _ -> assert false

  let rec mke coef p t ctx =
    let {Term.f = sb ; xs = xs; ty = ty} = Term.view t in
    match sb, xs with
      | (Sy.Int n | Sy.Real n) , _  -> 
	  let c = coef */ (num_of_string (Hstring.view n)) in
	  P.add (P.create [] c ty) p, ctx

      | Sy.Op Sy.Mult, [t1;t2] ->
	  let p1, ctx = mke coef (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke (Int 1) (empty_polynome ty) t2 ctx in
	  P.add p (P.mult p1 p2), ctx

      | Sy.Op Sy.Div, [t1;t2] -> 
	  let p1, ctx = mke coef (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke (Int 1) (empty_polynome ty) t2 ctx in
	  let p3, ctx = 
	    try 
              let p, approx = P.div p1 p2 in
              if approx then mk_euc_division p p2 t1 t2 ctx
              else p, ctx
	    with Division_by_zero | Polynome.Maybe_zero -> 
              P.create [coef, X.term_embed t] (Int 0) ty, ctx
	  in
	  P.add p p3, ctx
		
      | Sy.Op Sy.Plus , [t1;t2] -> 
	  let p2, ctx = mke coef p t2 ctx in
	  mke coef p2 t1 ctx

      | Sy.Op Sy.Minus , [t1;t2] -> 
	  let p2, ctx = mke (minus_num coef) p t2 ctx in
	  mke coef p2 t1 ctx

      | Sy.Op Sy.Modulo , [t1;t2] -> 
	  let p1, ctx = mke coef (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke (Int 1) (empty_polynome ty) t2 ctx in
          let p3, ctx = 
            try P.modulo p1 p2, ctx
            with e ->
	      let t = Term.make mod_symb [t1; t2] Ty.Tint in    
              let ctx = match e with
                | Division_by_zero | Polynome.Maybe_zero -> ctx
                | Polynome.Not_a_num -> mk_modulo t t1 t2 ctx
                | _ -> assert false 
              in 
              P.create [coef, X.term_embed t] (Int 0) ty, ctx 
	  in         
	  P.add p p3, ctx
	    
      | _ ->
(*      | (Sy.Name _ | Sy.Var _), _ -> *)
	  begin
	    let a, ctx' = X.make t in
	    let ctx = ctx' @ ctx in
	    match C.extract a with
	      | Some p' -> P.add p (P.mult (P.create [] coef ty) p'), ctx
	      | _ -> P.add p (P.create [coef, a] (Int 0) ty), ctx
	  end

(*      | _ -> 
	  eprintf "%a @." Sy.print sb; assert false*)

  let make t = 
    let {Term.ty = ty} = Term.view t in
    let p, ctx = mke (Int 1) (empty_polynome ty) t [] in
    is_mine p, ctx

  let rec expand (p,n) acc =
    assert (n >=0);
    if n = 0 then acc else expand (p, n-1) (p::acc)

    let color_not_omega {h=sy; l=rl; t=ty} =
    let mlt = List.fold_left (fun l (r,n) -> expand (embed r,n) l) [] rl in
    let mlt = List.fold_left P.mult (P.create [] (Int 1) ty) mlt in
    is_mine mlt 

      
  let color_omega ({h=sy; l=rl; t=ty} as ac) = 
    let rec ppow (pi,i) p =
      assert (i >=0);
      if i = 0 then p else ppow (pi, i-1) (P.mult pi p) 
    in 
    let mlt = 
      List.fold_left 
        (fun l (r,n) -> 
           let pr = match C.extract r with
             | Some p -> p
             | None   -> P.create [(Int 1,r)] (Int 0) ty in
           ppow (pr,n) l
        ) (P.create [] (Int 1) ty) rl in
    match P.to_list mlt with
      | [], c            -> C.embed mlt
      | [Int 1, x], Int 0 -> x
      | [a,x], Int 0 -> 
          begin match X.ac_extract x with
            | Some ac when Sy.equal (Sy.Op Sy.Mult) ac.h -> 
                let a' = C.embed (P.create [] a ty) in
                X.ac_embed { ac with l=(a', 1) :: ac.l }
            | _ -> C.embed mlt
          end
      | _ -> X.ac_embed ac


  let color ac =
    if not omega 
    then color_not_omega ac
    else color_omega ac

  let type_info p = P.type_info p

  let is_int r = X.type_info r = Ty.Tint

  module XS = Set.Make(struct type t = X.r let compare = X.compare end)
    
  let xs_of_list = 
    List.fold_left (fun s x -> XS.add x s) XS.empty
      
  let rec leaves p = 
    let s = 
      List.fold_left
	(fun s (_, a) -> XS.union (xs_of_list (X.leaves a)) s)
	XS.empty (fst (P.to_list p))
    in
    XS.elements s

  let subst x t p = 
    let p = P.subst x (embed t) p in
    let ty = P.type_info p in
    let l, c = P.to_list p in
    let p  = 
      List.fold_left
        (fun p (ai, xi) ->
	   let xi' = X.subst x t xi in
	   let p' = match C.extract xi' with
	     | Some p' -> P.mult (P.create [] ai ty) p'
	     | _ -> P.create [ai, xi'] (Int 0) ty
	   in
	   P.add p p')
        (P.create [] c ty) l
    in 
    check_int Exception.Inconsistent p;
    is_mine p


  let compare = P.compare

  let mod_sym a b = 
    let r = 
      let m = mod_num a b in 
      let m = 
        if m </ Int 0 then
          if m >=/ minus_num b then m +/ b else assert false
        else 
          if m <=/ b then m else assert false

      in
      if m </ b // (Int 2) then m else m -/ b
    in
    if debug_arith then
      Format.eprintf "mod_sym %s %s = %s@." 
	(string_of_num a)
	(string_of_num b)
	(string_of_num r);
    r

  let mult_const p c =
    P.mult p (P.create [] c (P.type_info p))
  
      
  let choose_min acc (a,x) =
    match acc with
      | None -> Some (a,x)
      | Some (b,y) when abs_num a </ abs_num b -> Some (a,x)
      | Some _  -> acc

  let map_poly_list f l ax =
    let res =
      List.fold_left
        (fun acc (a,x) -> 
           let a = f a in
           if a =/ Int 0 then acc else (a,x) :: acc)
        [] l in
    List.rev (ax :: res)

  let rec subst_bigger_and_remove x l' (rest, sbs) = 
    match l' with
      | [] -> assert false
      | (b,y) :: l ->
          let c = X.compare y x in 
          if c = 0 then List.rev_append rest l, sbs
          else if c > 0 then
            match X.ac_extract y with
              | None   -> 
                  let l' = List.filter (fun (_,y) -> not (X.equal x y)) l' in
                  List.rev_append rest l', sbs
              | Some _ -> 
                  let fv = fresh_var () in
                  let sbs = (y, embed fv)::sbs in
                  subst_bigger_and_remove x l ((b,fv)::rest, sbs)
          else assert false

 

  let is_mine_p = List.map (fun (x,p) -> x, is_mine p)

  (* Decision Procedures. Page 131 *)
  let rec omega (l,b) ty = 
    let min = List.fold_left choose_min None l in 
    match min with
      | None -> []
      | Some (a,x) -> 
          let l, sbs = subst_bigger_and_remove x l ([],[]) in
          let p = (P.create l b ty) in       
          if a =/ Int 1 then 
            let p = mult_const (P.create l b ty) (Int (-1)) in 
            (x, is_mine p) :: (is_mine_p sbs)
          else if a =/ Int (-1) then 
            (x,is_mine p) :: (is_mine_p sbs)
          else
            let a,l,b = 
              (* Should be more efficient *)
              if compare_num a (Int 0) < 0 
              then 
		let ma = minus_num a in
		let mb = minus_num b in
		ma, List.map (fun (a,x) -> minus_num a,x) l, mb
              else a, l, b in
            let m = a +/ Int 1 in
            let sigma = fresh_var () in
            let l' = 
              map_poly_list (fun a -> mod_sym a m) l (minus_num m, sigma) in
            let p = P.create l' (mod_sym b m) ty in
            let sbs = (x, p) :: sbs in
            let convert a = round_num (a // m) +/ mod_sym a m in
            let l'' = map_poly_list convert l (minus_num a,sigma) in 
            let b = convert b in
            let sbs2 = solve_int (P.create l'' b ty ) ty in
            let sbs = 
              List.map
                (fun (p,v) ->
                   let v' = 
                     List.fold_left 
                       (* il faut plutot un subst qui retourne 
                          ce qu'on veut et simplifier cette partie*)
                       (fun v (p2,v2) -> embed (subst p2 v2 v))
                       v sbs2 
                   in 
                   p,is_mine v'
                ) sbs in

            let sbs2 = List.filter (fun (y,_) -> y != sigma) sbs2 in
            List.rev_append sbs sbs2


  and solve_int p ty = 
    if debug_arith then Format.eprintf "on resoud : %a@." P.print p;
    if P.is_empty p then raise Not_found;
    let pgcd = P.pgcd_numerators p in
    let ppmc = P.ppmc_denominators p in
    let p = mult_const p (ppmc // pgcd)  in
    let l,c = P.to_list p in
    if is_integer_num c then omega (l,c) ty
    else raise Exception.Unsolvable

  let solve_real p =
      let a, x = P.choose p in
      let p = P.mult (P.create [] ((Int (-1)) // a) (P.type_info p))
	(P.remove x p) in
      (* If we don't use omega for int we want to make this test *)
      check_int Exception.Unsolvable p;
      [x, is_mine p]

  let solve repr r1 r2 =
    if debug_arith then 
      fprintf fmt "[arith] we solve %a=%a@." X.print r1 X.print r2;
    let p1, p2 = 
      match (C.extract r1 , r1) ,(C.extract r2,r2) with
	| (Some p1, _), (Some p2, _) ->  p1, p2
	| (Some p1, _), (None, r) | (None, r), (Some p1, _) -> p1, embed r
	| (None, _) , (None, _) -> assert false 
    in
    let ty = P.type_info p2 in
    let p = P.add p1 (P.mult (P.create [] (Int (-1)) ty) p2) in
    try if ty = Ty.Treal || (not Options.omega)
    then solve_real p else solve_int p ty
    with Not_found -> 
      if snd (P.to_list p) <>/ (Int 0) then raise Exception.Unsolvable; 
      []


  let solve repr r1 r2 = 
    let sbs = solve repr r1 r2 in
    let sbs = List.fast_sort (fun (a,_) (x,y) -> X.compare x a)sbs in
    if debug_arith then begin
      fprintf fmt "[arith] solving %a = %a yields:@." X.print r1 X.print r2;
      let cpt = ref 0 in
      List.iter 
        (fun (p,v) -> 
           incr cpt;
           fprintf fmt " %d) %a |-> %a@." !cpt X.print p X.print v) sbs
    end;
    sbs

  let print = P.print

  module Rel = Fm.Make (X) 
    (struct
       include P 
       let poly_of = embed
       let alien_of = is_mine
     end)
    
end
