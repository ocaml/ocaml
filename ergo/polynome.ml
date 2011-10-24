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
open Num

exception Not_a_num
exception Maybe_zero

module type S = sig
  type r
  val compare : r -> r -> int
  val term_embed : Term.t -> r
  val mult : r -> r -> r
  val print : Format.formatter -> r -> unit
end

module type T = sig

  type r
  type t

  val compare : t -> t -> int
  val create : (num * r) list -> num -> Ty.t-> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t * bool
  val modulo : t -> t -> t

  val is_empty : t -> bool
  val find : r -> t -> num
  val choose : t -> num * r
  val subst : r -> t -> t -> t
  val remove : r -> t -> t
  val to_list : t -> (num * r) list * num
    
  val print : Format.formatter -> t -> unit
  val type_info : t -> Ty.t
  val is_monomial : t -> (num * r * num) option

  val ppmc_denominators : t -> num
  val pgcd_numerators : t -> num
  val normal_form : t -> t * num
end

module Make (X : S) = struct

  type r = X.r
      
  module M : Map.S with type key = r = 
    Map.Make(struct type t = r let compare x y = X.compare y x end)
      
  type t = { m : num M.t; c : num; ty : Ty.t }

  let compare p1 p2 = 
    let c = Ty.compare p1.ty p2.ty in
    if c <> 0 then c
    else
      let c = compare_num p1.c p2.c in
      if c = 0 then M.compare compare_num p1.m p2.m else c

  let pprint fmt p =
    M.iter
      (fun x n ->
         let s, n, op = match n with
           | Int 1  -> "+", "", ""
           | Int -1 -> "-", "", ""
           | n ->   
               if n >/ Int 0 then "+", string_of_num n, "*" 
               else "-", string_of_num (minus_num n), "*" 
         in
         fprintf fmt "%s%s%s%a" s n op X.print x
      )p.m;
    let s, n = if p.c >=/ Int 0 then "+", string_of_num p.c 
    else "-", string_of_num (minus_num p.c) in
    fprintf fmt "%s%s" s n


  let print fmt p =
    if Options.term_like_pp then pprint fmt p 
    else begin
      M.iter 
        (fun t n -> fprintf fmt "%s*%a " (string_of_num n) X.print t) p.m;
      fprintf fmt "%s" (string_of_num p.c);
      fprintf fmt " [%a]" Ty.print p.ty
    end



  let is_num p = M.is_empty p.m

  let find x m = try M.find x m with Not_found -> Int 0

  let create l c ty = 
    let m = 
      List.fold_left 
	(fun m (n, x) -> 
	   let n' = n +/ (find x m) in
	   if n' =/ (Int 0) then M.remove x m else M.add x n' m) M.empty l
    in
    { m = m; c = c; ty = ty }
      
  let add p1 p2 = 
    let m = 
      M.fold 
	(fun x a m -> 
	   let a' = (find x m) +/ a in
	   if a' =/ (Int 0) then M.remove x m  else M.add x a' m)
	p2.m p1.m
    in 
    { m = m; c = p1.c +/ p2.c; ty = p1.ty }

  let mult_const n p = 
    if n =/ (Int 0) then { m = M.empty; c = Int 0; ty = p.ty }
    else { p with m = M.map (mult_num n) p.m; c =  n */ p.c }

  let mult_monome a x p  = 
    let ax = { m = M.add x a M.empty; c = (Int 0); ty = p.ty} in
    let acx = mult_const p.c ax in
    let m = 
      M.fold
	(fun xi ai m -> M.add (X.mult x xi) (a */ ai) m) p.m acx.m 
    in 
    { acx with m = m}
      
  let mult p1 p2 =
    let p = mult_const p1.c p2 in
    M.fold (fun x a p -> add (mult_monome a x p2) p) p1.m p

  let sub p1 p2 = 
    add p1 (mult (create [] (Int (-1)) p1.ty) p2)

  let div p1 p2 = 
    if M.is_empty p2.m then
      if p2.c =/ Int 0 then raise Division_by_zero
      else 
        let p = mult_const ((Int 1) // p2.c) p1 in
        match M.is_empty p.m, p.ty with
          | true, Ty.Tint  -> {p with c = floor_num p.c}, false 
          | true, Ty.Treal  ->  p, false
          | false, Ty.Tint ->  p, true
          | false, Ty.Treal ->  p, false
          | _ -> assert false
    else raise Maybe_zero


  let modulo p1 p2 = 
    if M.is_empty p2.m then
      if p2.c =/ Int 0 then raise Division_by_zero
      else 
        if M.is_empty p1.m then { p1 with c = mod_num p1.c p2.c }
        else raise Not_a_num
    else raise Maybe_zero
      
  let find x p = M.find x p.m

  let is_empty p = M.is_empty p.m

  let choose p =
    let tn= ref None in
    (*version I : prend le premier element de la table*)
    (try M.iter
       (fun x a -> tn := Some (a, x); raise Exit) p.m with Exit -> ());
    (*version II : prend le dernier element de la table i.e. le plus grand 
    M.iter (fun x a -> tn := Some (a, x)) p.m;*)
    match !tn with Some p -> p | _ -> raise Not_found

  let subst x p1 p2 =
    try
      let a = M.find x p2.m in
      add (mult_const a p1) { p2 with m = M.remove x p2.m}
    with Not_found -> p2
      
  let remove x p = { p with m = M.remove x p.m }
      
  let to_list p = 
    let l = M.fold (fun x a aliens -> (a, x)::aliens ) p.m [] in
    List.rev l, p.c

  let type_info p = p.ty

  let is_monomial p  = 
    try 
      M.fold
	(fun x a r -> 
	   match r with
	     | None -> Some (a, x, p.c)
	     | _ -> raise Exit)
	p.m None
    with Exit -> None

  let denominator = function
    | Num.Int _ | Num.Big_int _ -> Big_int.unit_big_int
    | Num.Ratio rat -> Ratio.denominator_ratio rat

  let numerator = function
    | Num.Int i -> Big_int.big_int_of_int i 
    | Num.Big_int b -> b
    | Num.Ratio rat -> Ratio.numerator_ratio rat

  let pgcd_bi a b = Big_int.gcd_big_int a b
      
  let ppmc_bi a b = Big_int.div_big_int (Big_int.mult_big_int a b) (pgcd_bi a b)
     
  let ppmc_denominators {m=m} = 
    let res =   
      M.fold
        (fun k c acc -> ppmc_bi (denominator c) acc)
        m Big_int.unit_big_int in
    abs_num (Num.Big_int res)
  

  let pgcd_numerators {m=m} = 
    let res =   
      M.fold
        (fun k c acc -> pgcd_bi (numerator c) acc)
        m Big_int.zero_big_int in
    abs_num (Num.Big_int res)

  let normal_form p =
    let ppcm = ppmc_denominators p in
    let pgcd = pgcd_numerators p in
    let p = mult_const (ppcm // pgcd) p in
    { p with c = Int 0 }, p.c

end

