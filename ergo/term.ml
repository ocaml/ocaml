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
open Hashcons

module Sy = Symbols

type view = {f: Sy.t ; xs: t list; ty: Ty.t}
and t = view hash_consed

type subst = t Ergo_subst.t * Ty.subst
    
module H = struct
  type t = view
  let equal t1 t2 = try
    Sy.equal t1.f t2.f 
    && List.for_all2 (==) t1.xs t2.xs 
    && Ty.equal t1.ty t2.ty
  with Invalid_argument _ -> false
      
  let hash t =
    abs (List.fold_left 
	   (fun acc x-> acc*19 +x.tag) (Sy.hash t.f + Ty.hash t.ty) 
	   t.xs)
end

module T = Make(H)
  
let tbl = T.create 100007
  
let view t = t.node


(* fresh variables must be smaller than problem's variables.
   thus, Instead of comparinf t1.tag with t2.tag, 
   we compare t2.tag and t1.tag *)
let compare t1 t2 = Pervasives.compare t2.tag t1.tag

let sort = List.sort compare

let make s l ty = T.hashcons tbl {f=s;xs=l;ty=ty}

let shorten t = 
  let {f=f;xs=xs;ty=ty} = view t in
  T.hashcons tbl {f=f;xs=xs;ty=Ty.shorten ty}

let vrai = make (Sy.True) [] Ty.Tbool
let faux = make (Sy.False) [] Ty.Tbool
let void = make (Sy.Void) [] Ty.Tunit

let int i = make (Sy.int i) [] Ty.Tint
let real r = make (Sy.real r) [] Ty.Treal
let bitv bt ty = make (Sy.Bitv bt) [] ty

let rec print fmt t = 
  let {f=x;xs=l;ty=ty} = view t in
  match x, l with
    | Sy.Op Sy.Get, [e1; e2] ->
	fprintf fmt "%a[%a]" print e1 print e2

    | Sy.Op Sy.Set, [e1; e2; e3] ->
	fprintf fmt "%a[%a<-%a]" print e1 print e2 print e3

    | Sy.Op Sy.Concat, [e1; e2] ->
	fprintf fmt "%a@%a" print e1 print e2

    | Sy.Op Sy.Extract, [e1; e2; e3] ->
	fprintf fmt "%a^{%a,%a}" print e1 print e2 print e3

    | Sy.Op op, [e1; e2] -> 
	fprintf fmt "(%a %a %a)" print e1 Sy.print x print e2

    | _, [] -> 
        fprintf fmt "%a" Sy.print x
          
    | _, _ ->
        fprintf fmt "%a(%a)" Sy.print x print_list l

and print_list fmt = function
  | [] -> ()
  | [t] -> print fmt t
  | t::l -> Format.fprintf fmt "%a,%a" print t print_list l

let is_int t = (view t).ty= Ty.Tint
let is_real t = (view t).ty= Ty.Treal
      
let equal t1 t2 =  t1.tag == t2.tag
  
let hash t = t.tag
  
let pred t = make (Sy.Op Sy.Minus) [t;int "1"] Ty.Tint
  
let dummy = make Sy.dummy [] Ty.Tint
  (* verifier que ce type est correct et voir si on ne peut pas
  supprimer ce dummy*)
  
let vars_of = 
  let rec vars_of s t = 
    match view t with
	{ f=(Sy.Var _ as v);xs=[]} -> Sy.Set.add v s
      | {xs=l} -> List.fold_left vars_of s l
  in fun t -> vars_of Sy.Set.empty t
    
let rec apply_subst ((s_t,s_ty) as s) t = 
  let {f=f;xs=xs;ty=ty} = view t in
  try Sy.Map.find f s_t
  with Not_found -> 
    make f (List.map (apply_subst s) xs) (Ty.apply_subst s_ty ty)

module Set = 
  Set.Make(struct type t' = t type t=t' let compare=compare end)
    
module Map = 
  Map.Make(struct type t' = t type t=t' let compare=compare end)


let rec subterms acc t = 
  let {xs=xs} = view t in List.fold_left subterms (Set.add t acc) xs
