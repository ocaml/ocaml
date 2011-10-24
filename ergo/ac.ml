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
module L = List
module HS = Hstring
module F = Format
module Sy = Symbols

module type S = sig

  (* embeded AC semantic values *)
  type r 

  (* extracted AC semantic values *)
  type t = r Sig.ac
      
  (* builds an embeded semantic value from an AC term *)
  val make : Term.t -> r * Literal.t list
    
  (* tells whether the given term is AC*)
  val is_mine_symb : Sy.t -> bool

  (* compares two AC semantic values *)
  val compare : t -> t -> int

  (* returns the type infos of the given term *)
  val type_info : t -> Ty.t

  (* prints the AC semantic value *)
  val print : F.formatter -> t -> unit
    
  (* returns the leaves of the given AC semantic value *)
  val leaves : t -> r list

  (* replaces the first argument by the second one in the given AC value *)
  val subst : r -> r -> t -> r

  (* add flatten the 2nd arg w.r.t HS.t, add it to the given list 
     and compact the result *)
  val add : Symbols.t -> r * int -> (r * int) list -> (r * int) list

end

module Make (X : Sig.X) = struct

  open Sig 

  type r = X.r

  type t = X.r Sig.ac

  let flatten h (r,m) acc = 
    match X.ac_extract r with
      | Some ac when Sy.equal ac.h h -> 
	  L.fold_left (fun z (e,n) -> (e,m * n) :: z) acc ac.l
      | _ -> (r,m) :: acc
	  
  let sort = L.fast_sort (fun (x,n) (y,m) -> X.compare x y)
    
  let rev_sort l = L.rev (sort l)
    
  let compact xs =
    let rec f acc = function 
	[] -> acc
      | [(x,n)] -> (x,n) :: acc
      | (x,n) :: (y,m) :: r ->
	  if X.equal x y then f acc ((x,n+m) :: r)
	  else f ((x,n)::acc) ((y,m) :: r) 
    in
      f [] (sort xs) (* increasing order - f's result in a decreasing order*)

  let fold_flatten sy f = 
    L.fold_left (fun z (rt,n) -> flatten sy ((f rt),n) z) []

  let expand = 
    L.fold_left 
      (fun l (x,n) -> let l= ref l in for i=1 to n do l:=x::!l done; !l) []

(*
  let make tsy rl ty = 
  let rl = L.map (fun x -> (x,1)) rl in 
    let l = compact (fold_flatten sy (fun x->x) rl) in
    X.ac_embed {h=sy;t=ty;l=l}
*)

  let make t = 
    match Term.view t with
      | {Term.f= sy; xs=xs; ty=ty} when Sy.is_ac sy ->
	  let xs = L.map (fun x -> (x,1)) xs in 
	  let ctx = ref [] in
	  let xmake t = let r, l = X.make t in ctx := l@(!ctx); r in
	  X.ac_embed {h=sy; l=compact (fold_flatten sy xmake xs); t=ty}, !ctx
      | _ -> assert false  

    
  let is_mine_symb = Sy.is_ac

  let type_info {t=ty} = ty

  let leaves { l=l } = L.fold_left (fun z (a,_) -> (X.leaves a) @ z)[] l
      
  let rec mset_cmp = function
    |  []   ,  []   ->  0
    |  []   , _::_  -> -1
    | _::_  ,  []   ->  1
    | (a,m)::r  , (b,n)::s  -> 
	let c = X.compare a b in 
	if c <> 0 then c 
	else 
	  let c = m - n in 
	  if c <> 0 then c 
	  else mset_cmp(r,s)
	
  let size = 
    L.fold_left (fun z (rx,n) -> z + n * (L.length (X.leaves rx))) 0
      
  (* x et y are sorted in a decreasing order *)
  let compare {h=f ; l=x} {h=g ; l=y} = 
    let c = Sy.compare f g in
    if c <> 0 then c 
    else
      let c = size x - size y in
      if c <> 0 then c
      else (*mset_cmp (rev_sort x , rev_sort y)*)
        mset_cmp (x , y)

  let rec pr_elt sep fmt (e,n) = 
    assert (n >=0);
    if n = 0 then ()
    else F.fprintf fmt "%s%a%a" sep X.print e (pr_elt sep) (e,n-1)

  let pr_xs sep fmt = function
    | [] -> assert false
    | (p,n)::l  -> 
	F.fprintf fmt "%a" X.print p; 
	L.iter (F.fprintf fmt "%a" (pr_elt sep))((p,n-1)::l)
	  
  let print fmt {h=h ; l=l} = 
    if Sy.equal h (Sy.Op Sy.Mult) && Options.term_like_pp then
      F.fprintf fmt "%a" (pr_xs "'*'") l
    else
      F.fprintf fmt "%a(%a)" Sy.print h (pr_xs ",") l



  let subst p v ({h=h;l=l;t=t} as tm)  =
    if debug_ac then
      F.fprintf fmt "[ac] subst %a by %a in %a@." 
	X.print p X.print v X.print (X.ac_embed tm);
    X.color {tm with l=compact (fold_flatten h (X.subst p v) l)}

      
  let add h arg arg_l = compact (flatten h arg arg_l)

end

