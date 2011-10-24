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

open Hashcons

type 'a view = 
    Eq of 'a * 'a 
  | Neq of 'a * 'a 
  | Builtin of bool * Hstring.t * 'a list

type t = (Term.t view) hash_consed

module SS = Symbols.Set

module V = struct 
  type t = Term.t view 
      
  let equal a1 a2 = 
    match a1,a2 with
      |	Eq(t1,t2) , Eq(u1,u2) 
      | Neq(t1,t2) , Neq(u1,u2) -> Term.equal t1 u1 && Term.equal t2 u2
      | Builtin(b1,n1,l1) , Builtin(b2,n2,l2) -> 
	  (try b1=b2 && 
	      Hstring.equal n1 n2 && List.for_all2 Term.equal l1 l2
	   with Invalid_argument _ -> false)
      | _ -> false
	  
  let hash a = match a with
    | Eq(t1,t2) | Neq(t1,t2) -> 
	let k = match a with  Eq _ -> 1 | Neq _-> 3 | _ -> assert false in
	abs ((k*19 + Term.hash t1)*19+Term.hash t2)
    | Builtin(b,n,l) -> 
	let x = if b then 1 else 0 in
	List.fold_left 
	  (fun acc t-> acc*19 + Term.hash t)(Hstring.hash n+x) l
end

module H = Make(V)

let compare a1 a2 = Pervasives.compare a1.tag a2.tag
let equal a1 a2 = a1.tag = a2.tag
let hash a1 = a1.tag

module T = struct 
  type t' = t 
  type t = t' 
  let compare=compare
  let equal = equal
  let hash = hash
end

(* ------  labels ------ *)

module Labels = Hashtbl.Make(T)

let labels = Labels.create 100007

let add_label lbl t = Labels.replace labels t lbl

let label t = try Labels.find labels t with Not_found -> Hstring.empty

(* -----------------------*)

let tbl = H.create 100007

let make t = H.hashcons tbl t
let mk_pred t = make (Eq(t,Term.vrai))

let vrai = mk_pred Term.vrai
let faux = mk_pred Term.faux
  
let view a = a.node

let rec print_view print_z fmt = function
  | Eq(z1,z2) -> 
      Format.fprintf fmt "%a=%a" print_z z1 print_z z2
  | Neq(z1,z2) -> 
      Format.fprintf fmt "%a<>%a" print_z z1 print_z z2
  | Builtin(true,n,l) ->
      Format.fprintf fmt "%s(%a)" (Hstring.view n) (view_list print_z) l
  | Builtin(false,n,l) ->
      Format.fprintf fmt "~%s(%a)" (Hstring.view n) (view_list print_z) l

and view_list print_z fmt = function
    [] -> ()
  | z :: l ->
      Format.fprintf fmt "%a" print_z z;
      List.iter(Format.fprintf fmt ", %a" print_z) l

let print fmt a = 
  match Hstring.view (label a) with
    | ""  -> Format.fprintf fmt "%a" (print_view Term.print) (view a)
    | lbl -> Format.fprintf fmt "%s:%a" lbl (print_view Term.print) (view a)

let print_list fmt l = List.iter (print fmt) l
  
let apply_subst subst a = 
  let f = Term.apply_subst subst in
  let v = match view a with
      Eq(t1,t2) -> Eq(f t1,f t2)
    | Neq(t1,t2) -> Neq(f t1,f t2)
    | Builtin(b,n,l) -> Builtin(b,n,List.map f l)
  in
  make v

let neg_view = function
  | Eq(x, y) -> Neq(x, y)
  | Neq(x, y) -> Eq(x, y)
  | Builtin(b, n, l) -> Builtin(not b, n, l)
	  
let neg a = 
  let v = match view a with
    | Eq(t1,t2) when Term.equal t2 Term.faux -> Eq(t1,Term.vrai)
    | Eq(t1,t2) when Term.equal t2 Term.vrai -> Eq(t1,Term.faux)
    | Eq(t1,t2) -> Neq(t1,t2)
    | Neq(t1,t2) -> Eq(t1,t2)
    | Builtin(b,n,l) -> Builtin(not b,n,l)
  in 
  make v

let terms_of a = 
  let l = 
    match view a with Eq(t1,t2) | Neq(t1,t2) -> [t1;t2] | Builtin(_,_,l) -> l in
  List.fold_left Term.subterms  Term.Set.empty l

let is_var = function Symbols.Var _ -> true | _ -> false

let vars_of a = 
  Term.Set.fold (fun t -> SS.union (Term.vars_of t)) (terms_of a) SS.empty
        
module SetEq = Set.Make
  (struct 
     type t' = t * Term.t * Term.t
     type t = t'
     let compare (a1,_,_) (a2,_,_) = compare a1 a2
   end)

module Set = Set.Make(T)
module Map = Map.Make(T)

