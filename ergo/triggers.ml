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
open Format
open Why_ptree

module Vterm = Symbols.Set
module Vtype = Set.Make(struct type t=int let compare=Pervasives.compare end)

module STRS = Set.Make(
  struct
    type t = Why_ptree.tterm * Vterm.t * Vtype.t

    let rec compare_term t1 t2 = match t1.tt_desc,t2.tt_desc with
      | TTvar s1 , TTvar s2 -> Symbols.compare s1 s2
      | TTapp (s1,l1) , TTapp(s2,l2) ->
	  let c = Symbols.compare s1 s2 in
	  if c=0 then compare_list l1 l2 else c
      | TTinfix(a1,s1,b1) , TTinfix(a2,s2,b2) ->
	  let c = Symbols.compare s1 s2 in
	  if c=0 then 
	    let c=compare_term a1 b1 in if c=0 then compare_term a2 b2 else c
	  else c
      | x , y -> Pervasives.compare x y
    and compare_list l1 l2 = match l1,l2 with
	[] , _ -> -1
      | _ , [] -> 1
      | x::l1 , y::l2 -> 
	  let c = compare x y in if c=0 then compare_list l1 l2 else c

    let compare (t1,_,_) (t2,_,_) = compare_term t1 t2
  end)

let sort = 
  List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2) )
      
let compare_tconstant c1 c2 =
  match c1, c2 with
    | Tint s1, Tint s2 -> String.compare s1 s2
    | Tint s1, _ -> 1
    | _, Tint s1 -> -1
    | Treal s1, Treal s2 -> Num.compare_num s1 s2
    | Treal s1, _ -> 1
    | _, Treal s2 -> -1
    | Tbitv s1, Tbitv s2 -> String.compare s1 s2
    | Tbitv s1, _ -> 1
    | _, Tbitv s2 -> -1
    | _ -> Pervasives.compare c1 c2

let rec depth_tterm t =
  match t.tt_desc with
    | TTconst _ | TTvar _->  0
    | TTapp (_, tl) ->
	1 + (List.fold_left
	       (fun acc t -> max (depth_tterm t) acc) 0 tl)
    | TTinfix _ | TTprefix _ -> 
	0 (* arithmetic triggers are not suitable *)
    | TTget (t1, t2) | TTconcat (t1, t2) ->
	max (depth_tterm t1) (depth_tterm t2)
    | TTset (t1, t2, t3) | TTextract (t1, t2, t3) ->
	max (depth_tterm t1) (max (depth_tterm t2) (depth_tterm t3))
    | TTlet (s, t1, t2) ->
	max (depth_tterm t1 + 1) (depth_tterm t2)

exception Out of int

  (* pourquoi cette fonction de comparaison est-elle si compliquee? *)
let rec compare_tterm t1 t2 =
  match t1.tt_desc, t2.tt_desc with
    | TTconst c1, TTconst c2 -> compare_tconstant c1 c2
    | TTconst _, _ -> -1
    | _, TTconst _ -> 1
    | TTvar v1, TTvar v2 -> Symbols.compare v1 v2
    | TTvar _, _ -> -1
    | _, TTvar _ -> 1
    | TTinfix (tu1, s, tu2), TTinfix (tu1', s', tu2') ->
	let c = (depth_tterm t1) - (depth_tterm t2) in
	if c <> 0 then c
	else let c = Symbols.compare s s' in
	if c <> 0 then c
	else let c = compare_tterm tu1 tu1' in
	if c <> 0 then c
	else compare_tterm tu2 tu2'
    | TTinfix _, _ -> -1
    | _, TTinfix _ -> 1
    | TTprefix (s1, t1), TTprefix (s2, t2) ->
	let c = Symbols.compare s1 s2 in
	if c<>0 then c else compare_tterm t1 t2
    | TTprefix _, _ -> -1
    | _, TTprefix _ -> 1
    | TTapp (s1, tl1), TTapp (s2, tl2) ->
	let l1 = List.map depth_tterm tl1 in
	let l2 = List.map depth_tterm tl2 in
	let l1 = List.fast_sort compare l1 in
	let l2 = List.fast_sort compare l2 in
	let c = try
	  List.iter2
	    (fun n m -> 
	       if n <> m then raise (Out (n-m))
	    ) l1 l2; 0
	with 
	  | Out c -> c 
	  | _ -> (List.length l1) - (List.length l2) in
	if c <> 0 then c
	else let c = Symbols.compare s1 s2 in
	if c <> 0 then c
	else begin try
	  List.iter2
	    (fun t1 t2 ->
	       let c = compare_tterm t1 t2 in
	       if c <> 0 then raise (Out c)
	    ) tl1 tl2; 0 
	with Out c -> c end
    | TTapp _, _ -> -1
    | _, TTapp _ -> 1
    | TTget (t1, t2), TTget (u1, u2) ->
	let c = compare_tterm t1 u1 in
	if c<>0 then c else compare_tterm t2 u2
    | TTget _, _ -> -1
    | _, TTget _ -> 1
    | TTset(t1, t2, t3) , TTset(u1, u2, u3) ->
	let c = compare_tterm t1 u1 in
	if c<>0 then c else
	  let c = compare_tterm t2 u2 in
	  if c<>0 then c else compare_tterm t3 u3
    | TTset _, _ -> -1
    | _, TTset _ -> 1
    | TTextract(t1, t2, t3) , TTextract(u1, u2, u3) ->
	let c = compare_tterm t1 u1 in
	if c<>0 then c else
	  let c = compare_tterm t2 u2 in
	  if c<>0 then c else compare_tterm t3 u3
    | TTextract _, _ -> -1
    | _, TTextract _ -> 1
    | TTconcat (t1, t2), TTconcat (u1, u2) ->
	let c = compare_tterm t1 u1 in
	if c<>0 then c else compare_tterm t2 u2
    | TTconcat _, _ -> -1
    | _, TTconcat _ -> 1
    | TTlet (s1, t1, u1) , TTlet (s2, t2, u2) ->
	let c = Symbols.compare s1 s2 in
	if c<>0 then c else
	  let c = compare_tterm t1 u1 in
	  if c<>0 then c else compare_tterm u1 u2

let compare_tterm_list tl2 tl1 =
  let l1 = List.map depth_tterm tl1 in
  let l2 = List.map depth_tterm tl2 in
  let l1 = List.rev (List.fast_sort compare l1) in
  let l2 = List.rev (List.fast_sort compare l2) in
  let c = try
    List.iter2
      (fun n m -> 
	 if n <> m then raise (Out (n-m))
      ) l1 l2; 0
  with 
    | Out c -> c 
    | _ -> (List.length l2) - (List.length l1) in
  if c <> 0 then c
  else begin try
    List.iter2
      (fun t1 t2 ->
	 let c = compare_tterm t1 t2 in
	 if c <> 0 then raise (Out c)
      ) tl1 tl2; 0 
  with Out c -> c end
    

let at_most n l =
  let l = List.fast_sort compare_tterm_list l in
  let rec atmost acc n l = 
    match n, l with
      | n, _ when n <= 0 -> acc
      | _ , [] -> acc
      | n, x::l -> 
          if List.mem x acc then atmost acc n l
          else atmost (x::acc) (n-1) l
  in
  List.rev (atmost [] n l)

let is_var t = match t.tt_desc with
  | TTvar _ -> true
  | TTapp (_,l) -> l=[]
  | _ -> false

module SLLT = 
  Set.Make(
    struct 
      type t = Why_ptree.tterm list * Vterm.t * Vtype.t
      let compare (_, y1, _) (_, y2, _)  = 
	Vterm.compare y1 y2
    end)

let parties bv vty l = 
  let rec parties_rec (llt, llt_ok)  l = 
    match l with 
	[] -> llt_ok
      | (t, bv1, vty1)::l -> 
	  let llt, llt_ok = 
	    SLLT.fold
	      (fun (l, bv2, vty2) (llt, llt_ok) ->
		 let lt, bv3, vty3 = 
		   t::l, Vterm.union bv2 bv1, Vtype.union vty2 vty1 
		 in 
		 let e = (lt, bv3, vty3) in
		 if Vterm.subset bv bv3 && Vtype.subset vty vty3 then
		   llt, SLLT.add e llt_ok
		 else 
		   SLLT.add e llt, llt_ok) 
	      llt (llt, llt_ok)
	  in
	  parties_rec (SLLT.add ([t],bv1,vty1) llt, llt_ok) l
  in 
  SLLT.elements (parties_rec (SLLT.empty, SLLT.empty) l)
	 
let strict_subset bv vty = 
  List.exists 
    (fun (_,bv',vty') -> 
       (Vterm.subset bv bv' && not(Vterm.equal bv bv')  
	&& Vtype.subset vty vty') 
       or (Vtype.subset vty vty' && not(Vtype.equal vty vty') 
	   && Vterm.subset bv bv') )

let simplification bv_a vty_a = 
  let rec simpl_rec acc = function
    | [] -> acc
    | ((t, bv, vty) as e)::l -> 
	if strict_subset bv vty l or strict_subset bv vty acc or
	  (Vterm.subset bv_a bv && Vtype.subset vty_a vty) or
	  (Vterm.equal (Vterm.inter bv_a bv) Vterm.empty &&
	     Vtype.equal (Vtype.inter vty_a vty) Vtype.empty)
	then simpl_rec acc l
	else  simpl_rec (e::acc) l
  in simpl_rec []

let rec vars_of_term bv acc t = match t.tt_desc with
  | TTvar x -> if Vterm.mem x bv then Vterm.add x acc else acc
  | TTapp (_,lt) -> List.fold_left (vars_of_term bv) acc lt
  | TTinfix (t1,_,t2) -> List.fold_left (vars_of_term bv) acc [t1;t2]
  | _ -> acc

let underscoring_term mvars underscores t = 
  let rec under_rec t =  
    { t with tt_desc = under_rec_desc t.tt_desc}
  and under_rec_desc t = match t with
    | TTvar x when Vterm.mem x mvars -> 
	if not (Vterm.mem x !underscores) then 
	  (	underscores := Vterm.add x !underscores; t)
	else 
	  TTvar (Symbols.underscoring x)
    | TTvar _ -> t
    | TTapp (s,lt) -> TTapp(s,List.map under_rec lt)
    | TTinfix (t1,op,t2) -> TTinfix(under_rec t1,op,under_rec t2)
    | _ -> t
  in 
  under_rec t

let underscoring_mt bv mt = 
  let vars , mvars = 
    List.fold_left 
      (fun (vars,mvars) t -> 
	 let vs = vars_of_term bv Vterm.empty t in
	 let mvars = Vterm.union mvars (Vterm.inter vars vs) in
	 Vterm.union vars vs , mvars) (Vterm.empty,Vterm.empty) mt in
  let underscores = ref Vterm.empty in
  List.map (underscoring_term mvars underscores) mt
    
let multi_triggers gopt bv vty trs =
  let terms = simplification bv vty trs in
  let l_parties = parties bv vty terms  in 
  let lm = List.map (fun (lt,_,_) -> lt) l_parties in
(*    List.fold_left 
      (fun acc (lt, bv', vty') ->
	 if Vterm.subset bv bv' && Vtype.subset vty vty' then lt::acc else acc) 
      [] l_parties*)
  let mv , mt = List.partition (List.exists is_var) lm in
  let mv , mt = sort mv , sort mt in
  let lm = if gopt || triggers_var then mt@mv else mt in
  let m = at_most redondance  lm in
  (*(List.map (underscoring_mt bv) m)@
    (List.map (underscoring_mt bv) (List.map List.rev m))*)
  at_most redondance m 

let rec vty_ty acc t = 
  let t = Ty.shorten t in
  match t with
    | Ty.Tvar { Ty.v = i ; value = None } -> Vtype.add i acc 
    | Ty.Text(l,_) -> List.fold_left vty_ty acc l
    | _ -> acc
    
let rec vty_term acc t = 
  let acc = vty_ty acc t.tt_ty in
  match t.tt_desc with
    | TTapp (_,l) -> List.fold_left vty_term acc l
    | TTinfix (t1,_,t2) -> vty_term (vty_term acc t1) t2
    | _ -> acc

let rec vty_form acc = function
  | TFatom(TAeq l | TAneq l | TAle l | TAlt l | TAbuilt(_,l))-> 
      List.fold_left vty_term acc l
  | TFatom TApred t -> vty_term acc t
  | TFop(_,l) -> List.fold_left vty_form acc l
  | _ -> acc (* we don't go through quantifiers *)

let csort = Symbols.name "c_sort"

let filter_mono vterm vtype (t, bv_t, vty_t) = 
  Vterm.subset vterm bv_t && Vtype.subset vtype vty_t && 
    match t.tt_desc with
      | TTapp(s,_) -> 
	  not (Symbols.equal s csort)
      | _ -> true
	    

let as_bv bv s = not (Vterm.is_empty (Vterm.inter bv s))
let as_tyv vty s = not (Vtype.is_empty (Vtype.inter vty s))

let potential_triggers = 
  let rec potential_rec ((bv,vty) as vars) acc t = 
    let vty_t = vty_term Vtype.empty t in
    match t.tt_desc with
      | TTvar x ->
	  if Vterm.mem x bv or as_tyv vty vty_t then
	    STRS.add (t,Vterm.singleton x, vty_t) acc
	  else acc
      | TTapp(s,lf)-> 
	  let vty_lf = List.fold_left vty_term vty_t lf in
	  let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty lf in
	  if as_bv bv bv_lf or as_tyv vty vty_lf then
	    List.fold_left (potential_rec vars) 
	      (STRS.add (t,bv_lf,vty_lf) acc) lf
	  else acc
      | TTinfix(t1,_,t2) -> 
	  let vty_lf = List.fold_left vty_term vty_t [t1;t2] in
	  let bv_lf = List.fold_left (vars_of_term bv) Vterm.empty [t1;t2] in
	  if as_bv bv bv_lf or as_tyv vty vty_lf then
	    List.fold_left (potential_rec vars) 
	      (STRS.add (t,bv_lf,vty_lf) acc) (*acc*) [t1;t2]
	  else acc
      | _ -> acc
  in fun vars -> List.fold_left (potential_rec vars) STRS.empty

let filter_good_triggers (bv,vty) = 
  List.filter 
    (fun l ->
       let s1 = List.fold_left (vars_of_term bv) Vterm.empty l in
       let s2 = List.fold_left vty_term Vtype.empty l in
       Vterm.subset bv s1 && Vtype.subset vty s2)

let make_triggers gopt vterm vtype trs = 
  match List.filter (filter_mono vterm vtype) trs with
      [] -> 
	multi_triggers gopt vterm vtype trs
    | trs' -> 
	let f l = at_most redondance (List.map (fun (t,_,_) -> [t]) l) in
	let trs_v, trs_nv = 
	  List.partition (fun (t,_,_) -> is_var t) trs' in
	let ll = 
	  if trs_nv=[] then
	    if triggers_var || gopt then 
	      f trs_v 
	    else [](*multi_triggers vars trs*)
	  else f trs_nv 
	in 
	if glouton then ll@(multi_triggers gopt vterm vtype trs) else ll

let rec make_rec gopt vterm vtype f = match f with
  | TFatom (TAfalse | TAtrue) ->
      f, STRS.empty
  | TFatom a ->
      if Vterm.is_empty vterm && Vtype.is_empty vtype then 
	f, STRS.empty
      else 
	let l = match a with    
	  | TAeq l | TAneq l | TAle l | TAlt l | TAbuilt(_,l) -> l
	  | TApred t -> [t]
	  | _ -> assert false
	in
	f, potential_triggers (vterm, vtype) l
  | TFop(op, lf) -> 
      let lf, trs = 
	List.fold_left
	  (fun (lf, trs1) f ->
	     let f, trs2 = make_rec gopt vterm vtype f in
	     f::lf, STRS.union trs1 trs2) ([], STRS.empty) lf in
      TFop(op,List.rev lf), trs

  | TFforall ({ qf_form=TFop(OPiff,[TFatom _ as f1;f2])} as qf) -> 
      let vtype' = vty_form Vtype.empty qf.qf_form in
      let vterm' = 
	List.fold_left (fun b (s,_) -> Vterm.add s b) Vterm.empty qf.qf_bvars 
      in
      let vterm'' = Vterm.union vterm vterm' in
      let vtype'' = Vtype.union vtype vtype' in
      let f1', trs1 = make_rec gopt vterm'' vtype'' f1 in
      let f2', trs2 = make_rec gopt vterm'' vtype'' f2 in
      let trs12 = 
	if Options.notriggers || qf.qf_triggers = [] then
	  (make_triggers false vterm' vtype' (STRS.elements trs1))@
	    (make_triggers false vterm' vtype' (STRS.elements trs2))
	else 
	  begin
	    let lf = filter_good_triggers (vterm', vtype') qf.qf_triggers in
	    if lf<>[] then lf
	    else
	      (make_triggers false vterm' vtype' (STRS.elements trs1))@
		(make_triggers false vterm' vtype' (STRS.elements trs2))
	  end
      in
      let trs = 
	STRS.filter 
	  (fun (_, bvt, _) -> Vterm.is_empty (Vterm.inter bvt vterm')) 
	  (STRS.union trs1 trs2) in
      let r  = { qf with 
		   qf_triggers = trs12 ; 
		   qf_form = TFop(OPiff,[f1'; f2'])}
      in
      begin
	match f with 
	  | TFforall _ -> TFforall r, trs 
	  | _ -> TFexists r , trs
      end


  | TFforall qf | TFexists qf -> 
      let vtype' = vty_form Vtype.empty qf.qf_form in
      let vterm' = 
	List.fold_left (fun b (s,_) -> Vterm.add s b) Vterm.empty qf.qf_bvars in
      let f', trs = 
	make_rec gopt 
	  (Vterm.union vterm vterm') (Vtype.union vtype vtype') qf.qf_form in
      let trs' = 
	if Options.notriggers || qf.qf_triggers=[] then
	  make_triggers gopt vterm' vtype' (STRS.elements trs)
	else 
	  let lf = filter_good_triggers (vterm',vtype') qf.qf_triggers in
	  if lf <> [] then lf
	  else make_triggers gopt vterm' vtype' (STRS.elements trs)
      in
      let trs = 
	STRS.filter 
	  (fun (_,bvt,_) -> Vterm.is_empty (Vterm.inter bvt vterm')) trs in
      let r  = {qf with qf_triggers = trs' ; qf_form = f'} in
      (match f with TFforall _ -> TFforall r , trs | _ -> TFexists r , trs)


  | TFlet (up, v, t, f) -> 
      let f, trs = make_rec gopt vterm vtype f in 
      TFlet (up, v, t, f), trs

  | TFnamed(lbl, f) -> 
      let f, trs = make_rec gopt vterm vtype f in
      TFnamed(lbl, f), trs
	  
let make gopt f = match f with
  | TFforall _ | TFexists _ -> 
      let f, _ = make_rec gopt Vterm.empty Vtype.empty f in
      f
  | _  ->  
      let vty = vty_form Vtype.empty f in
      let f, trs = make_rec gopt Vterm.empty vty f in
      if Vtype.is_empty vty then f
      else 
	let trs = STRS.elements trs in
	let trs = make_triggers gopt Vterm.empty vty trs in
	TFforall 
	  {qf_bvars=[]; qf_upvars=[]; qf_triggers=trs; qf_form=f }

