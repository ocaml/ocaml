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

type 'a abstract = 
    Pair of 'a abstract * 'a abstract * Ty.t
  | Fst of 'a abstract * Ty.t
  | Snd of 'a abstract * Ty.t
  | Other of 'a * Ty.t

module type ALIEN = sig
  include Sig.X
  val embed : r abstract -> r
  val extract : r -> (r abstract) option
end

module Make (X : ALIEN) = struct 

  module XS = Set.Make(struct type t = X.r let compare = X.compare end)

  let name = "pairs"

  type t = X.r abstract
  type r = X.r

  let rec print fmt = function
    | Pair(t, u,_) -> fprintf fmt "<%a, %a>" print t print u
    | Fst(t,_) -> fprintf fmt "fst(%a)" print t
    | Snd(t,_) -> fprintf fmt "snd(%a)" print t
    | Other(t,_) -> X.print fmt t

  let print_mine fmt = function
      Sig.No r -> X.print fmt r
    | Sig.Yes t -> print fmt t

  let is_pair =
    let c = Hstring.make "pair" in
    fun s -> Hstring.compare s c = 0

  let is_fst =
    let n = Hstring.make "fst" in
    fun s -> Hstring.compare s n = 0

  let is_snd =
    let n = Hstring.make "snd" in
    fun s -> Hstring.compare s n = 0

  let rec raw_comp t' u' =
    match t', u' with
      | Other(t,_), Other(u,_) -> X.compare t u
      | Other _, _ -> -1
      | _, Other _ -> 1
      | Snd(p,_), Snd(q,_) -> raw_comp p q
      | Snd _, _ -> -1
      | _, Snd _ -> 1
      | Fst(p,_), Fst(q,_) -> raw_comp p q
      | Fst _, _ -> -1
      | _, Fst _ -> 1
      | Pair(t, u,_), Pair(t', u',_) ->
	  let c = raw_comp t t' in
	    if c = 0 then raw_comp u u'
	    else c
	      
  let rec normalize = function
    | Pair(u, v, t) -> 
	let u' = normalize u and v' = normalize v in
	  (match u',v' with
	     | Fst(u1,_), Snd(u2,_) when raw_comp u1 u2 = 0 -> u1
	     | _ -> Pair(u', v',t))
    | Fst(p,t) ->
	(match normalize p with
	   | Pair(a, _,_) -> a
	   | p' -> Fst(p',t))
    | Snd(p,t) ->
	(match normalize p with
	   | Pair(_, b,_) -> b
	   | p' -> Snd(p',t))
    | Other(r,t) -> Other(r,t)
	
  let is_mine t =
    match normalize t with
      | Other(r,_) -> r
      | x -> X.embed x

  let make t = 
    let rec make_rec t ctx =
      match (Term.view t) with
	| {Term.f = Symbols.Name (f,_); xs = l; ty=ty} ->
	    begin
	      if is_pair f then
		match l with 
		  | [lc; rc] -> 
		      let f, ctx = make_rec lc ctx in
		      let s, ctx = make_rec rc ctx in
		      Pair (f, s,ty), ctx
		  | _ -> assert false
	      else if is_fst f then
		match l with
		  | [p] -> 
		      let f, ctx = make_rec p ctx in
		      Fst (f,ty), ctx
		  | _ -> assert false
	      else if is_snd f then
		match l with
		  | [p] -> 
		      let s, ctx = make_rec p ctx in
		      Snd (s,ty), ctx
		  | _ -> assert false
	      else
		let r, ctx' = X.make t in
		let ctx = ctx'@ctx in
		Other (r,ty), ctx
	    end
	| {Term.ty=ty} -> 
	    let r, ctx' = X.make t in
	    let ctx = ctx'@ctx in
	    Other (r,ty), ctx
    in 
    let r, ctx = make_rec t [] in
    is_mine r, ctx

  let color _ = assert false
    
  let embed r = 
    match X.extract r with
      | Some p -> p
      | None -> Other(r, X.type_info r)

  let xs_of_list = List.fold_left (fun s x -> XS.add x s) XS.empty

  let leaves t = 
    let rec leaves t = 
      match (normalize t) with
	| Pair(lc, rc,_) -> XS.union (leaves lc) (leaves rc)
	| Fst (p,_) 
	| Snd (p,_) -> leaves p
	| Other(x,_) -> xs_of_list (X.leaves x)
    in
    XS.elements (leaves t)

  let compare t u =
    raw_comp (normalize t) (normalize u)


(* This module is a naive union-find structure for *)
(* elements of type t, implemented as a set of lists. *)
  module UF = 
  struct 
    type tag = Left | Right

    module S = Set.Make(
      struct
	type t = (X.r abstract * tag) list
	let rec comp l1 l2 = match l1, l2 with
	  | [], [] -> 0
	  | _, [] -> 1
	  | [], _ -> -1
	  | a::q1, b::q2 ->
	      let c = raw_comp (fst a) (fst b) in
		if c = 0 then comp q1 q2 else c
	let compare = comp
      end)
      
    type t = S.t
	
    let empty = S.empty
    let in_class t = List.exists (fun e -> raw_comp t (fst e) = 0)
    let mem t = S.exists (in_class t)
	
    let add t tag uf =
      if mem t uf then uf else S.add [t,tag] uf
    
    let classof_ uf t = 
      match S.elements (S.filter (in_class t) uf) with
	| [] -> raise Not_found
	| [c] -> c
	| _ -> assert false 
	    (* on ne doit pas trouver deux classes pour un element donne *)
    let classof uf t = fst (List.split (classof_ uf t))

    let find uf t = List.hd (classof_ uf t)
	
    let union uf t1 t2 =
      let c1 = classof_ uf t1 and c2 = classof_ uf t2 in
	S.add (c2@c1) (S.remove c2 (S.remove c1 uf))

    let find_mate uf t tag =
      match List.filter (fun e -> raw_comp t (fst e) <> 0) (classof_ uf t) with
	| [] -> None
	| (e,_)::_ -> Some e

    let rec find_new_mate uf t tag =
      match List.filter (fun e -> raw_comp t (fst e) <> 0) (classof_ uf t) with
	| [] -> None
	| (((e,_)::_) as l) -> 
	    let r =
	      match List.partition (fun (_,t) -> t = tag) l with
		| (e,_)::_,_ -> Some e
		| [],l -> build_new_mate uf tag l
	    in match r with None -> Some e | _ -> r
    
    and build_new_mate uf tag = function
      | [] -> None
      | (Other(_,_),_)::q ->
	  build_new_mate uf tag q
      | (Fst(t,ty),_)::q ->
	  begin
	    match find_new_mate uf t tag with
	      | Some t' -> Some (Fst(t',ty))
	      | None -> build_new_mate uf tag q
	  end
      | (Snd(t,ty),_)::q ->
	  begin
	    match find_new_mate uf t tag with
	      | Some t' -> Some (Snd(t',ty))
	      | None -> build_new_mate uf tag q
	  end
      | (Pair(t1,t2,ty),_)::q ->
	  begin
	    match 
	      find_new_mate uf t1 tag,
	      find_new_mate uf t2 tag with
		| Some t1', Some t2' -> Some (Pair(t1',t2',ty))
		| Some t1', None -> Some (Pair(t1',t2,ty))
		| None, Some t2' -> Some (Pair(t1,t2',ty))
		| None, None -> build_new_mate uf tag q
	  end
  end
    
  module Map = 
    Map.Make (struct type t = X.r abstract let compare = compare end)
  module Set = 
    Set.Make (struct type t = X.r abstract let compare = compare end)
    
  let rec subst_rec p v r = 
    match r with
      | Other(t,ty) -> 
	  let v' = if X.compare p t = 0 then v else X.subst p v t in
	  (match X.extract v' with Some x -> x | None -> Other(v',ty))
      | Fst(t,ty) -> Fst(subst_rec p v t,ty)
      | Snd(t,ty) -> Snd(subst_rec p v t,ty)
      | Pair(t, u,ty) -> Pair (subst_rec p v t, subst_rec p v u,ty)
	  
  let subst p v r = is_mine (subst_rec p v r)
      
  let is_mine_symb =  function 
    | Symbols.Name (f,_) when pairs -> is_fst f || is_snd f || is_pair f
    | _ -> false
      
  let is_mine_a _ = false

  let rec is_mine_type = function
      Pair _ -> true
    | Other _ -> false
    | Fst(t,_) | Snd(t,_) -> is_mine_type t
	
  let unsolvable _ = false

  let type_info = 
    function Pair(_,_,t) | Fst(_,t) | Snd(_,t) | Other(_,t) -> t

  let type_snd t = 
    match type_info t with 
	Ty.Text(([_;ty] | [ty]),_) -> ty | _ -> assert false

  let type_fst t = 
    match type_info t with Ty.Text(([ty;_]|[ty]),_) -> ty  
      | _ -> assert false

(** [decompose uf t] adds [t] and all its subterms to [uf] *)
  let decompose tag uf = 
    let add p = UF.add p tag in
    let rec dec uf = function
    | Pair(t, u,_) as p -> 
	add p (dec (dec uf t) u)
    | Fst(t,ty) as p ->
	let t' = Snd(t,type_snd t) in
	let t'' = Pair(p,t',type_info t) in
	let uf = add t' (add t'' (add p (dec uf t))) in
	  UF.union uf t'' t
    | Snd(t,_) as p ->
	let t' = Fst(t,type_fst t) in
	let t'' = Pair(t',p,type_info t) in
	let uf = add t' (add t'' (add p (dec uf t))) in
	  UF.union uf t'' t
    | p -> add p uf
    in dec uf

(** [find_new_eqs uf] tries to find new equalities between elements
    of [uf] by applying simple rules.
    When no equality is found, raises Not_found. *)
  let find_new_eqs uf =
    (* Three possible origins of new equalities : *)
    (* _ fst(pair(x,y)) = x *)
    let find_fst_pair uf =
      UF.S.fold
	(fun c acc -> List.fold_left 
	   (fun acc (t,_) -> match t with
	      | Fst(e,_) -> 
		  (List.fold_left 
		     (fun acc e' -> match e' with 
			| Pair(a, _,_) ->
			    if UF.in_class a c then acc else (t,a)::acc
			| _ -> acc)
		     acc (UF.classof uf e))
	      | _ -> acc) acc c)
	uf in
    (* _ snd(pair(x,y)) = y *)
    let find_snd_pair uf =
      UF.S.fold
	(fun c acc -> List.fold_left 
	   (fun acc (t,_) -> match t with 
	      | Snd(e,_) -> 
		  (List.fold_left 
		     (fun acc e' -> match e' with
			| Pair(_, b,_) ->
			    if UF.in_class b c then acc else (t,b)::acc
			| _ -> acc)
		     acc (UF.classof uf e))
	      | _ -> acc) acc c)
	uf in
    (* _ cons(x,y) = cons(x',y') -> x=x' /\ y=y' *)
    let find_pair_pair uf =
      let rec apparier acc = function
	| [] -> acc
	| (t1,t2)::q -> 
	    let newacc = 
	      (List.fold_left 
		 (fun acc (u1,u2) ->
		    let add = ref [] in
		      if not (List.mem u1 (UF.classof uf t1)) then
			add := (t1, u1)::!add;
		      if not (List.mem u2 (UF.classof uf t2)) then 
			add := (t2, u2)::!add;
		      (!add)@acc) acc q)
	    in apparier newacc q
      in
	UF.S.fold
	  (fun c acc -> 
	     apparier acc 
	       (List.fold_left 
		  (fun acc i -> 
		     match i with 
		       | Pair(t1,t2,_), _ -> (t1,t2)::acc
		       | _ -> acc) [] c))
	  uf
    in
      (** we search all kinds of equations *)
      find_fst_pair uf (find_snd_pair uf (find_pair_pair uf []))
	
  let solve repr r1 r2 = 
    let leaves t = 
      let rec leaves t = 
	match (normalize t) with
	  | Pair(lc, rc,_) -> XS.union (leaves lc) (leaves rc)
	  | Fst(p,_) 
	  | Snd(p,_) -> leaves p
	  | Other(x,_) -> XS.singleton x
      in
      XS.elements (leaves t)
    in
    if debug_pairs then 
      printf "[pairs] Solving %a = %a @." X.print r1 X.print r2;
    (* populate the uf *)
    let t1 = match X.extract r1 with 
	Some t1 -> t1 | None -> Other(r1, X.type_info r1) in
    let t2 = match X.extract r2 with 
	Some t2 -> t2 | None -> Other(r2, X.type_info r2) in
    let uf = decompose UF.Right (decompose UF.Left UF.empty t1) t2 in
    (* merge t1 and t2 *)
    let uf' = UF.union uf t1 t2 in

    (* close the uf wrt the new equalities *)
    let rec closure uf =
      match find_new_eqs uf with
	| [] -> uf
	| lt -> closure 
	    (List.fold_left (fun acc (t1, t2) -> UF.union acc t1 t2) uf lt)
    in
    let ufsat = closure uf' in

    (* For each leaf, we find a term in its class which was not
       already equal to itself, and which contaings parts with tag *)
    let f tag =
      List.fold_left
	(fun acc t -> 
	   match UF.find_new_mate ufsat (Other(t, X.type_info t)) tag with
	     | Some e -> (t, is_mine e)::acc
	     | None -> (* Format.printf "ici : %a\n" X.print t; *) 
		 acc) []
    in
    let res = match t1,t2 with
	Other _ , Other _ -> assert false
      | Other _ , _ -> f UF.Left (leaves t2) 
      | _ , Other _ -> f UF.Right (leaves t1) 
      | _ , _ -> 
	  let sols (l,t) (l',t') =
	    match f t l with [] -> f t' l' | r -> r
	  in
	  let l1, l2 = leaves t1, leaves t2 in
(* 	    if List.length l1 > List.length l2 then *)
	      sols (l1,UF.Right) (l2,UF.Left)
(* 	    else  *)
(* 	      sols (l2,UF.Left) (l1,UF.Right) *)
    in

    if debug_pairs then 
      (printf "[pairs] Results :@ ";
       List.iter (fun (t, r) -> 
		    printf "%a |-> %a@ ;@ " X.print t X.print r) res);
    
    (* AC ordering : Take 1*) 
    List.fold_left
      (fun acc (p,v) ->
	 if X.unsolvable v && X.compare  v p > 0 then (v,p) :: acc 
	 else (p,v) :: acc
      )[] res
          
  module Rel =
  struct
    type r = X.r
    type t = unit
    exception Inconsistent    
    let empty _ = ()
    let assume _ _ = (), []
    let query _ _  = false
    let case_split env = []
    let add env _ = env
    let instantiate env _ _ = env, []
  end
end
