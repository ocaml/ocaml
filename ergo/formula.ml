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
open Options

module T = Term
module Sy = Symbols

type lemma = {
  vars: Sy.Set.t;
  trs : T.t list list;
  f : t;
  name : string
}

and llet = {
  lvar: Sy.t;
  lsubst : T.t Ergo_subst.t;
  lsubst_ty : Ty.subst;
  lterm : T.t;
  lf : t;
}

and ssko = {
  ssubst : T.t Ergo_subst.t;
  ssubst_ty : Ty.subst;
  sf : t;
}

and view = 
    U of t list
  | C of t*t  
  | Lit of Literal.t
  | Lem of lemma
  | Sko of ssko
  | Let of llet

and iview = { pos : view ; neg : view ; size : int}

and t = iview hash_consed
    
module View = struct
  type t = iview
      
  let rec compare_list compare l1 l2 = match l1 , l2 with
      [] , [] -> 0
    | [] , _ -> 1
    | _ , [] -> -1
    | x1::l1 , x2::l2 -> 
	let c = compare x1 x2 in 
	if c<>0 then c else compare_list compare l1 l2
	
  let rec compare_pclause v1 v2 = match v1 , v2 with
      U l1 , U l2 -> compare_list compare_t l1 l2
    | U _ , _ -> -1
    | _, U _ -> 1
    | C(x1,y1) , C(x2,y2) -> 
	  let c = compare_t x1 x2 in if c<>0 then c else compare_t y1 y2
    | C _ , _ -> -1
    | _ , C _ -> 1
    | Lit a1 , Lit a2 -> Literal.compare a1 a2
    | Lit _ , _ -> -1
    | _ , Lit _ -> 1
    | Lem l1 , Lem l2 ->  compare_lemme l1 l2
    | Lem _ , _ -> -1
    | _ , Lem _ -> 1
    | Let l1, Let l2 -> compare_let l1 l2
    | Let _, _ -> -1
    | _, Let _ -> 1
    | Sko s1 , Sko s2 -> compare_skolem s1 s2
	
  and compare_t t1 t2  = Pervasives.compare t1.tag t2.tag
  and compare_view v1 v2 = 
    let c = compare_pclause v1.pos v2.pos in
    if c<>0 then c else compare_pclause v1.neg v2.neg
  and compare_lemme l1 l2 = 
    let c = compare_t l1.f l2.f in
    if c<>0 then c else
      let c = Sy.Set.compare l1.vars l2.vars in
      if c<>0 then c else 
	compare_list (compare_list T.compare) l1.trs l2.trs
  and compare_skolem s1 s2 = 
    let c = compare_t s1.sf s2.sf in
    if c<>0 then c else
      let c = Ty.compare_subst s1.ssubst_ty s2.ssubst_ty in
        if c<>0 then c else Sy.Map.compare T.compare s1.ssubst s2.ssubst
  and compare_let l1 l2 =
    let c = compare_t l1.lf l2.lf in
    if c<>0 then c else 
      let c = T.compare l1.lterm l2.lterm in
      if c<>0 then c else 
        let c = Sy.compare l1.lvar l2.lvar in
        if c<>0 then c else
          let c = Ty.compare_subst l1.lsubst_ty l2.lsubst_ty in
            if c<>0 then c else
              Sy.Map.compare T.compare l1.lsubst l2.lsubst
      
  let sort l = 
    let l = List.sort compare_t l in
    List.fold_left 
	(fun acc x -> match acc with
	     [] -> [x]
	   | h::l when x==h -> acc
	   | _ -> x::acc) [] l
      
  let eqc c1 c2 = match c1,c2 with
      U l1 , U l2 -> 
	(try List.for_all2 (==) (sort l1) (sort l2)
	 with Invalid_argument _ -> false)

    | C(f1,f2) , C(g1,g2) -> f1==g1 && f2==g2 || f1==g2 && f2==g1

    | Lit x , Lit y -> Literal.equal x y

    | Lem({trs=lt1;f=f1}) ,Lem({trs=lt2;f=f2}) -> 
	(try List.for_all2 (List.for_all2 T.equal) lt1 lt2 && f1==f2
	 with Invalid_argument _ -> false)

    | Sko {ssubst=s1;ssubst_ty=ty1;sf=f1},Sko{ssubst=s2;ssubst_ty=ty2;sf=f2} -> 
	f1==f2 
	&& (Sy.Map.compare T.compare s1 s2 = 0)
	&& Ty.compare_subst ty1 ty2 = 0

    | Let l1, Let l2 -> 
	l1.lf == l2.lf 
	&& Sy.equal l1.lvar l2.lvar 
	&& Term.equal l1.lterm l2.lterm 
        && (Sy.Map.compare T.compare l1.lsubst l2.lsubst = 0)
	&& Ty.compare_subst l1.lsubst_ty l2.lsubst_ty = 0
	    
    | _, _ -> false
	
  let hashlt = List.fold_left (fun acc x->acc*19 + T.hash x)
  let hashllt = List.fold_left (fun acc x->acc*19 + hashlt 0 x)
    
  let hashc acc = function 
      U l -> List.fold_left (fun acc f -> acc * 19 + f.tag) 1 (sort l)
    | C(f1,f2) -> 
	let min = min f1.tag f2.tag in
	let max = max f1.tag f2.tag in
	(acc*19 + min)*19 + max
    | Lem({vars=vars;trs=trs;f=f}) -> 
	hashllt (Hashtbl.hash (f.tag,vars)) trs
    | Lit x -> Literal.hash x
    | Sko{ssubst=s;sf=f} -> 
	Sy.Map.fold 
	  (fun s t acc ->acc * 19 + Sy.hash s) s f.tag
    | Let ({lvar=lvar;lterm=lterm;lsubst=lsubst;lf=lf}) -> 
        Sy.Map.fold (fun s t acc ->acc * 19 + Sy.hash s) lsubst 
          (lf.tag * 19 * 19 + Sy.hash lvar * 19 + acc)
	  
  let equal f1 f2 = eqc f1.pos f2.pos && eqc f1.neg f2.neg
  let hash f = abs (hashc (hashc 1 f.pos) f.neg)
end
  
  
module H = Make(View)
  
let tbl = H.create 100007
let iview f = f.node

let view t = t.node.pos

let rec print fmt f = match view f with
  | Lit a -> 
      Literal.print fmt a
  | Lem {trs=trs;f=f;name=n} -> 
      if verbose then
	fprintf fmt "(lemme: %s)[%a] %a" 
	  n
	  (fun fmt -> List.iter (fprintf fmt "%a@ |" T.print_list)) 
	  trs print f
      else 
	fprintf fmt "lem %s" n

  | U l -> Print_color.print_list "& " print fmt l

  | C(f1,f2) -> fprintf fmt "(%a@ v %a) " print f1 print f2

  | Sko{sf=f} -> fprintf fmt "<sko> (%a)" print f

  | Let l -> 
      fprintf fmt 
	"let %a =@ %a in@ %a" Sy.print l.lvar Term.print l.lterm print l.lf

let union_subst s1 ((s2,s2_ty) as subst) = 
  Sy.Map.fold 
    (fun k x s2 -> Sy.Map.add k x s2) (Sy.Map.map (T.apply_subst subst)  s1) s2

(* this function should only be applied with ground substitutions *)
let rec apply_subst subst f = 
  let {pos=p;neg=n;size=s} = iview f in
  H.hashcons tbl {pos=iapply_subst subst p; neg=iapply_subst subst n; size=s}

and iapply_subst ((s_t,s_ty) as subst) = function
  | Lit a -> 
      Lit(Literal.apply_subst subst a)

  | Lem({vars=vars; trs=trs; f=f} as e)->
      let s_t = Sy.Set.fold Sy.Map.remove vars s_t in
      let subst = s_t , s_ty in
      let f =  apply_subst subst f in
      let trs = List.map (List.map (T.apply_subst subst)) trs in
      Lem({e with trs=trs; f=f})

  | U l -> 
      U(List.map (apply_subst subst) l)

  | C(f1,f2) -> 
      C(apply_subst subst f1,apply_subst subst f2)

  | Sko e -> 
      Sko{e with 
	    ssubst = union_subst e.ssubst subst;
            ssubst_ty = Ty.union_subst e.ssubst_ty s_ty}

  | Let ({lsubst=lsubst;lsubst_ty=lsubst_ty;lterm=lterm} as e) ->
     let lterm = T.apply_subst subst lterm in
     let lsubst = union_subst lsubst subst in
     Let {e with 
	    lsubst=lsubst;lsubst_ty=Ty.union_subst lsubst_ty s_ty; lterm=lterm;}
   
let size t = t.node.size

let compare f1 f2 = 
  let c = Pervasives.compare (size f1) (size f2) in 
  if c=0 then compare f1.tag f2.tag else c
	  
let equal f1 f2 = f1.tag == f2.tag


(* smart constructors *)

let mk_lit a = H.hashcons tbl {pos=Lit a; neg=Lit(Literal.neg a);size=1}
  
let mk_not f = 
  let f = iview f in H.hashcons tbl ({pos=f.neg;neg=f.pos;size=f.size})	  

let mk_skolem_subst bv v = 
  T.Set.fold 
    (fun x m -> 
       let {T.f=x;ty=ty} = T.view x in
       let bv = T.Set.fold (fun y acc-> y::acc) bv [] in
       let t = T.make (Sy.fresh "sko") bv ty in
       Sy.Map.add x t m) 
    v Sy.Map.empty

let symbols_of_terms v = 
  T.Set.fold 
    (fun t sy -> let {T.f=f} = T.view t in Sy.Set.add f sy) 
    v Sy.Set.empty
  
(* name: (forall bv [trs]. f[fv]) *)
let mk_forall up bv trs f name = 
  let sy = symbols_of_terms bv in
  let lem = {vars=sy; trs=trs; f=f ; name=name} in
  let sko = {ssubst = mk_skolem_subst up bv;
             ssubst_ty = Ty.esubst;
             sf = mk_not f} in
    H.hashcons tbl {pos=Lem(lem) ; neg=Sko(sko); size=size f }
    
(* forall upbv.  name: (exists bv [trs]. f) *)
let mk_exists up bv trs f name = 
  let sy = symbols_of_terms bv in
  let lem = {vars=sy; trs=trs; f=mk_not f; name=name} in
  let sko = {ssubst = mk_skolem_subst up bv;
             ssubst_ty = Ty.esubst;
             sf = f} in
  H.hashcons tbl {pos=Sko(sko)  ; neg=Lem(lem) ; size = size f}

(* forall up. let bv = t in f *)
let mk_let up bv t f =
  let {Term.ty=ty} = Term.view t in
  let up = T.Set.fold (fun y acc-> y::acc) up [] in
  let subst = Sy.Map.add bv (T.make (Sy.fresh "let") up ty) Sy.Map.empty in
   H.hashcons tbl 
     {pos=Let{lvar=bv;lsubst=subst;lsubst_ty=Ty.esubst;lterm=t;lf=f};
      neg=Let{lvar=bv;lsubst=subst;lsubst_ty=Ty.esubst;lterm=t;lf=mk_not f}; 
      size=size f }
    
let mk_and f1 f2 =
  if equal f1 f2 then f1 else
    let size = size f1 + size f2 in
    H.hashcons tbl {pos=U([f1;f2]); neg=C(mk_not f1,mk_not f2); size=size}
      
let mk_or f1 f2 = 
  if equal f1 f2 then f1 else
    let size = size f1 + size f2 in
    H.hashcons tbl {pos=C(f1,f2); neg=U([mk_not f1;mk_not f2]); size=size}
      
let mk_imp f1 f2 = 
  let size = size f1 + size f2 in
  H.hashcons tbl {pos=C(mk_not f1,f2); neg=U([f1;mk_not f2]); size=size}

let mk_if t f2 f3 = 
  let lit = mk_lit (Literal.mk_pred t) in
  mk_or (mk_and lit f2) (mk_and (mk_not lit) f3)
    
let mk_iff f1 f2 = 
  let a = mk_or f1 f2 in
  let b = mk_or (mk_not f1) (mk_not f2) in
  let c = mk_or (mk_not f1) f2 in
  let d = mk_or f1 (mk_not f2) in
  H.hashcons tbl {pos=U([c;d]); neg=U([a;b]) ; size=2*(size f1+size f2)}

let add_label lbl f = 
  match view f with
    | Lit a -> 
	Literal.add_label lbl a;
	Literal.add_label lbl (Literal.neg a)
    | _ -> ()

let label f = 
  match view f with
    | Lit l -> Literal.label l
    | _ -> Hstring.empty
	
let free_vars =
  let rec free_rec acc f = 
    match view f with
	Lit a -> Sy.Set.union (Literal.vars_of a) acc
      | Lem {vars=v;f=f} -> let s = free_rec acc f in Sy.Set.diff s v
      | U l -> List.fold_left free_rec acc l
      | C(f1,f2) -> free_rec (free_rec acc f1) f2
      | Sko{ssubst=subst;sf=f} -> 
	  let sy = free_rec acc f in
	  Sy.Map.fold 
	    (fun s t sy -> 
	       if Sy.Set.mem s sy then 
		 Sy.Set.remove s (Sy.Set.union sy (Term.vars_of t))
	       else sy
	    ) subst sy
      | Let {lsubst=subst;lterm=t;lf=lf} ->
	  let ss =  
	    Sy.Set.filter (fun x -> Sy.Map.mem x subst) (free_rec acc lf) in
          let sy = Sy.Set.union (Term.vars_of t) ss in
          Sy.Map.fold
	    (fun s t sy -> 
	       if Sy.Set.mem s sy then 
		 Sy.Set.remove s (Sy.Set.union sy (Term.vars_of t))
	       else sy
	    ) subst sy
          
  in free_rec Sy.Set.empty
    
let terms = 
  let rec terms acc f = match view f with
      Lit a -> 
	let s = 
	  T.Set.filter 
	    (fun t->Sy.Set.is_empty (T.vars_of t)) (Literal.terms_of a)
	in
	T.Set.union s acc
    | Lem {trs=trs;f=f} -> terms acc f
    | U l -> List.fold_left terms acc l
    | C(f1,f2) -> terms (terms acc f1) f2
    | Sko{ssubst=s;sf=f} -> terms acc f
    | Let {lterm=t;lf=lf} -> 
        let st = 
	  T.Set.filter (fun t->Sy.Set.is_empty (T.vars_of t)) 
	    (Term.subterms Term.Set.empty t) 
	in
        terms (T.Set.union st acc) lf
  in terms T.Set.empty

module Set = Set.Make(struct type t'=t type t=t' let compare=compare end)
module Map = Map.Make(struct type t'=t type t=t' let compare=compare end)

