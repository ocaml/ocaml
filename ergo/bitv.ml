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
module T = Term

type sort_var = A | B | C
    
type tvar = { var : int ; sorte : sort_var }

type 'a xterm = Var of tvar | Alien of 'a
    
type 'a alpha_term = {
  bv : 'a;
  sz : int;
}


type 'a simple_term_aux = 
  | Cte of bool 
  | Other of 'a xterm
  | Ext of 'a xterm * int * int * int (*// id * size * i * j //*)
      
type 'a simple_term = ('a simple_term_aux) alpha_term

type 'a abstract =  ('a simple_term) list 
    

(* for the solver *)

type solver_simple_term_aux = 
    | S_Cte of bool 
    | S_Var of tvar

type solver_simple_term = solver_simple_term_aux alpha_term


module type ALIEN = sig
  include Sig.X
  val embed : r abstract -> r
  val extract : r -> (r abstract) option
end

module Make(X : ALIEN) = struct

  type t = X.r abstract
  type r = X.r

  let name = "bitv"

  let is_mine_a _ = false

  let is_mine_symb = function
    | Sy.Bitv _ | Sy.Op (Sy.Concat | Sy.Extract)  -> true
    | _ -> false

  let is_mine_type _ = assert false

  let unsolvable = function
      [{bv =Other (Var _)}] -> true
    | _ -> false
	
  let embed r =
    match X.extract r with
      | None ->
	  begin
	    match X.type_info r with
	      | Ty.Tbitv n -> [{bv = Other (Alien r) ; sz = n}]
	      | _  -> assert false
	  end
      | Some b -> b


  let err = err_formatter

  let compare_xterm xt1 xt2 = match xt1,xt2 with
    | Var v1, Var v2 -> 
	let c1 = compare v1.sorte v2.sorte in 
	if c1 <> 0 then c1 
	else -(compare v1.var v2.var)
	  (* on inverse le signe : les variables les plus fraiches sont 
	     les plus jeunes (petites)*)

    | Alien t1, Alien t2 -> X.compare t1 t2
    | Var v, Alien t -> 1
    | Alien t, Var v -> -1

  let compare_simple_term st1 st2 = 
    if st1.sz <> st2.sz then st1.sz - st2.sz
    else 
      begin
	match st1.bv,st2.bv with
	  | Cte b,Cte b' -> compare b b'
	  | Cte false , _ | _ , Cte true -> -1
	  | _ , Cte false | Cte true,_ -> 1
	      
	  | Other t1 , Other t2 -> compare_xterm t1 t2
	  | _ , Other _ -> -1
	  | Other _ , _ -> 1
	  | Ext(t1,s1,i1,_) , Ext(t2,s2,i2,_) ->
	      let c1 = compare s1 s2 in
	      if c1<>0 then c1
	      else let c2 = compare i1 i2 in
	      if c2 <> 0 then c2 else compare_xterm t1 t2
      end
       
  module ST_Set = Set.Make (
    struct 
      type t = solver_simple_term 
      let compare st1 st2 = 
	if st1.sz <> st2.sz then st1.sz - st2.sz
	else 
	  begin
	    match st1.bv,st2.bv with
	      | S_Cte b, S_Cte b' -> compare b b'
	      | S_Cte false, _ | _, S_Cte true -> -1
	      | _ , S_Cte false | S_Cte true,_ -> 1
	      | S_Var v1, S_Var v2 -> 
		  let c1 = compare v1.sorte v2.sorte
		  in if c1 <> 0 then c1 else compare v1.var v2.var
	  end
    end)
    
  module Canonizer = struct
    
    type term_aux  = 
      | I_Cte of bool 
      | I_Other of X.r xterm
      | I_Ext of term * int * int 
      | I_Comp of term * term
	  
    and term = term_aux alpha_term

    (** **)
    let rec alpha t = match t.bv with
      |I_Cte _ -> [t]
      |I_Other _ -> [t]
      |I_Comp (t1,t2) -> (alpha t1)@(alpha t2)
      |I_Ext(t',i,j) ->
	 begin
	   match t'.bv with
	     |I_Cte _ -> [{t' with sz = j-i+1}]
	     |I_Other _ -> [t]
	     |I_Ext(t'',k,_) -> 
		alpha {t with bv = I_Ext(t'',i+k,j+k)}  

	     |I_Comp(u,v) when j < v.sz -> 
		alpha{t with bv =I_Ext(v,i,j)}

	     |I_Comp(u,v) when i >= v.sz ->
		alpha{t with bv=I_Ext(u,i-v.sz,j-v.sz)}

	     |I_Comp(u,v) -> 
		(alpha {sz = j-v.sz+1 ; bv = I_Ext(u,0,j-v.sz)}) 
		@(alpha{sz = v.sz-i ; bv = I_Ext(v,i,v.sz-1)})
	 end
	   
    (** **)   
    let rec beta lt = 
      let simple_t st = match st.bv with
	|I_Cte b -> {bv = Cte b ; sz = st.sz}
	|I_Other x -> {bv = Other x ; sz = st.sz}
	|I_Ext(t',i,j) -> 
	   begin
	     match t'.bv with
	       |I_Other v -> 
		  let siz = j-i+1 
		  in {sz=siz ;
		      bv =if siz=t'.sz then Other v else Ext(v,t'.sz,i,j)}
	       |I_Comp _ |I_Ext _ |I_Cte _ -> assert false
	   end	
	|I_Comp(_,_) -> assert false
      
      in match lt with
	|[] -> [] (*on peut passer de 2 elts a 0 elts*)
	|[s] -> [simple_t s]
	|s::t::tl' ->
	   begin
	     match s.bv , t.bv with
	       |I_Cte b1,I_Cte b2 when b1=b2 ->beta({s with sz=s.sz+t.sz}::tl')
	       |I_Ext(d1,i,j),I_Ext(d2,k,l) when d1=d2 && l=i-1 ->
		  let tmp = {sz = s.sz + t.sz ; bv = I_Ext(d1,k,j)}
		  in if k=0 then (simple_t tmp)::(beta tl') else beta (tmp::tl')
	       |_ -> (simple_t s)::(beta (t::tl'))
	   end
	
    (** **)     
    let sigma term = beta (alpha term)
      
    let bitv_to_icomp =
      List.fold_left (fun ac bt ->{ bv = I_Comp (ac,bt) ; sz = bt.sz + ac.sz })

    let string_to_bitv s =
      let tmp = ref[] in
      String.iter(fun car -> tmp := (car<>'0',1)::(!tmp)) s;
      let rec f_aux l acc = match l with
	| [] -> assert false
	| [(b,n)] -> { sz = n ; bv = I_Cte b }::acc
	| (b1,n)::(b2,m)::r when b1 = b2 -> f_aux ((b1,n+m)::r) acc
	| (b1,n)::(b2,m)::r -> 
	    (f_aux ((b2,m)::r)) ({ sz = n ; bv = I_Cte b1 }::acc)
      in 
      let res = f_aux (!tmp) [] in 
      bitv_to_icomp (List.hd res) (List.tl res)

    let make t =
      let rec make_rec t' ctx = match T.view t' with
	| {T.f = Sy.Bitv s } -> string_to_bitv s, ctx
	| {T.f = Sy.Op Sy.Concat ; xs = [t1;t2] ; ty = Ty.Tbitv n} ->
	    let r1, ctx = make_rec t1 ctx in
	    let r2, ctx = make_rec t2 ctx in
	    { bv = I_Comp (r1, r2) ; sz = n }, ctx
	| {T.f = Sy.Op Sy.Extract; xs = [t1;ti;tj] ; ty = Ty.Tbitv n} ->
	    begin 
	      match T.view ti , T.view tj with
		| { T.f = Sy.Int i } , { T.f = Sy.Int j } -> 
		    let i = int_of_string i.node in
		    let j = int_of_string j.node in
		    let r1, ctx = make_rec t1 ctx in
		    { sz = j - i + 1 ; bv = I_Ext (r1,i,j)}, ctx
		| _ -> assert false
	    end
	| {T.ty = Ty.Tbitv n} -> 
	    let r', ctx' = X.make t' in
	    let ctx = ctx' @ ctx in
	    {bv = I_Other (Alien r') ; sz = n}, ctx
	| _ -> assert false
      in 
      let r, ctx = make_rec t [] in
      sigma r, ctx
  end

  module Print = struct
    open Canonizer
    
    let print_tvar fmt ({var=v;sorte=s},sz) = 
      fprintf fmt "%s_%d[%d]@?" 
	(match s with | A -> "a" | B -> "b" | C -> "c")
	v sz
      
    let rec print_I_ast fmt ast = match ast.bv with
      | I_Cte b -> fprintf fmt "%d[%d]@?" (if b then 1 else 0) ast.sz
      | I_Other (Alien t) -> fprintf fmt "%a[%d]@?" X.print t ast.sz
      | I_Other (Var tv) -> fprintf fmt "%a@?" print_tvar (tv,ast.sz)
      | I_Ext (u,i,j) -> fprintf fmt "%a<%d,%d>@?" print_I_ast u i j
      | I_Comp(u,v) -> fprintf fmt "@[(%a * %a)@]" print_I_ast u print_I_ast v
   
    let print fmt ast = match ast.bv with 
      | Cte b -> fprintf fmt "%d[%d]@?" (if b then 1 else 0) ast.sz
      | Other (Alien t) -> fprintf fmt "%a@?" X.print t
      | Other (Var tv) -> fprintf fmt "%a@?" print_tvar (tv,ast.sz)   
      | Ext (Alien t,sz,i,j) -> 
	  fprintf fmt "%a@?" X.print t;
	  fprintf fmt "<%d,%d>@?" i j
      | Ext (Var tv,sz,i,j) -> 
	  fprintf fmt "%a@?" print_tvar (tv,ast.sz);
	  fprintf fmt "<%d,%d>@?" i j

    let print_C_ast fmt = function
	[] -> assert false
      | x::l -> print fmt x; List.iter (fprintf fmt " @@ %a" print) l

    let print_s fmt ast = match ast.bv with 
      | S_Cte b -> fprintf fmt "%d[%d]@?" (if b then 1 else 0) ast.sz
      | S_Var tv -> fprintf fmt "%a@?" print_tvar (tv,ast.sz)

    let print_S_ast fmt = function
	[] -> assert false
      | x::l -> print_s fmt x; List.iter (fprintf fmt " @@ %a" print_s) l

    let print_sliced_sys fmt l = 
      fprintf fmt "\nSlicing :\n";
      List.iter (fun (a,b) -> fprintf fmt " %a == %a\n" print a print b) l

    let print_c_solve_res fmt l = 
      fprintf fmt "\n(map)c_solve :\n";
      List.iter (fun (a,b) -> fprintf fmt " %a == %a\n" print a print_S_ast b) l

    let print_partition_res fmt l = 
      fprintf fmt "\npartition :\n";
      List.iter 
	(fun (t,cte_l) ->
	   fprintf fmt " %a%a \n" print t
	     (fun fmt -> 
		List.iter (fun l' -> fprintf fmt " == %a" print_S_ast l')) 
	     cte_l) l

    let print_final_solution fmt l = 
      fprintf fmt "\nSolution :\n";
      List.iter
	(fun (a,value) -> 
	   fprintf fmt " %a = %a \n" print a print_C_ast value ) l;
      fprintf fmt "@."
      
  end 

  module Solver = struct

    exception Valid

    let add elt l = if List.mem elt l then l else elt::l
      
    let get_vars = List.fold_left 
      (fun ac st -> match st.bv with 
	|Other v |Ext(v,_,_,_) -> add v ac  |_ -> ac )[] 

    let st_slice st siz = 
      let siz_bis = st.sz - siz in match st.bv with
	|Cte b -> {st with sz = siz},{st with sz = siz_bis}
	|Other x ->
	   let s1 = Ext(x,st.sz, siz_bis, st.sz - 1) in
	   let s2 = Ext(x,st.sz, 0, siz_bis - 1) in
	     {bv = s1 ; sz = siz},{bv = s2 ; sz = siz_bis}
	|Ext(x,s,p,q) -> 
	   let s1 = Ext(x,s,p+siz_bis,q) in
	   let s2 = Ext(x,s,p,p+siz_bis-1) in
	     {bv = s1 ; sz = siz},{bv = s2 ; sz = siz_bis}

    let slice t u  = 
      let f_add (s1,s2) acc = 
	if (s1 = s2 || List.mem (s1,s2) acc || List.mem (s2,s1) acc) then acc
	else (s1,s2)::acc
      in let rec f_rec acc = function
	|[],[] | _,[] | [],_ -> assert false
	|[s1],[s2] ->if s1.sz<>s2.sz then assert false else f_add (s1,s2) acc
	|s1::r1,s2::r2  ->
	   if s1.sz = s2.sz then f_rec (f_add (s1,s2) acc) (r1,r2)
	   else begin
	       if s1.sz > s2.sz then 
		 let (s11,s12) = st_slice s1 s2.sz
		 in f_rec (f_add (s11,s2) acc) (s12::r1,r2)
	       else 
		 let (s21,s22) = st_slice s2 s1.sz
		 in f_rec (f_add (s1,s21) acc) (r1,s22::r2)
	     end
      in f_rec [] (t,u) 

    let fresh_var = 
      let cpt = ref 0 in fun t -> incr cpt; { var = !cpt ; sorte = t}
   	
    let fresh_bitv genre size = 
      if size <= 0 then [] 
      else [ { bv = S_Var (fresh_var genre) ; sz = size } ] 
	   
    let cte_vs_other bol st = st , [{bv = S_Cte bol ; sz = st.sz}]
      
    let cte_vs_ext bol xt s_xt i j =
      let a1  = fresh_bitv A i in
      let a2  = fresh_bitv A (s_xt - 1 - j) in
      let cte = [ {bv = S_Cte bol ; sz =j - i + 1 } ] in
      let var = { bv = Other xt ; sz = s_xt }
      in var, a2@cte@a1
	
    let other_vs_other st1 st2 = 
      let c = fresh_bitv C st1.sz in [ (st1,c) ; (st2,c) ]

    let other_vs_ext st xt s_xt i j =        
      let c  = fresh_bitv C st.sz in
      let a1 = fresh_bitv A i in
      let a2 = fresh_bitv A (s_xt - 1 - j) in
      let extr = { bv = Other xt ; sz = s_xt }
      in [ (st,c) ; (extr,a2 @ c @ a1) ]

    let ext1_vs_ext2 (id,s,i,j) (id',s',i',j') = (* id != id' *)
      let c   = fresh_bitv (C) (j - i + 1) in
      let a1  = fresh_bitv A i  in
      let a1' = fresh_bitv A i' in
      let a2  = fresh_bitv A (s - 1 - j)   in
      let a2' = fresh_bitv A (s' - 1 - j') in
      let x_v = { sz = s  ; bv = Other id  } in
      let y_v = { sz = s' ; bv = Other id' } in
	[ (x_v , a2 @ c @ a1) ; (y_v , a2' @ c @ a1') ]

    let ext_vs_ext xt siz (i1,i2) tai = 
      let overl = i1 + tai -i2 in
	if overl <= 0 then begin
	    let a1 = fresh_bitv A i1     in
	    let a2 = fresh_bitv A (-overl) in
	    let a3 = fresh_bitv A (siz - tai - i2) in
	    let b  = fresh_bitv  B tai
	    in ({ bv = Other xt ; sz = siz } , a3 @ b @ a2 @ b @ a1)
	  end
	else begin
	    let b_box = i2 + tai - i1 in
	    let nn_overl = tai - overl in(* =i2-i1 >0 sinon egalite sytaxique*)
	    let sz_b1 = b_box mod nn_overl in
	    let a1 = fresh_bitv A i1                 in
	    let a3 = fresh_bitv A (siz - tai - i2) in
	    let b1 = fresh_bitv B sz_b1              in
	    let b2 = fresh_bitv B (nn_overl - sz_b1 )in
	    let acc = ref b1 in
	    let cpt = ref nn_overl in
	      while !cpt <= b_box do
		acc := b1 @ b2 @(!acc);
		cpt := !cpt + nn_overl
	      done;
	      ({ bv = Other xt ; sz = siz } , a3 @ (!acc) @ a1)
	  end
	  
    let sys_solve sys = 
      let c_solve (st1,st2) = match st1.bv,st2.bv with
	|Cte _, Cte _ -> raise Exception.Unsolvable (* forcement un 1 et un 0 *)

	|Cte b, Other (Var _) -> [cte_vs_other b st2]
	|Other (Var _), Cte b -> [cte_vs_other b st1]

	|Cte b, Other (Alien t) -> [cte_vs_other b st2]
	|Other (Alien t), Cte b -> [cte_vs_other b st1]

	|Cte b, Ext(xt,s_xt,i,j) -> [cte_vs_ext b xt s_xt i j]
	|Ext(xt,s_xt,i,j), Cte b -> [cte_vs_ext b xt s_xt i j]
	|Other _, Other _ -> other_vs_other st1 st2 

	|Other _, Ext(xt,s_xt,i,j) -> 
	   other_vs_ext st1 xt s_xt i j

	|Ext(xt,s_xt,i,j), Other _ -> other_vs_ext st2 xt s_xt i j
	|Ext(id,s,i,j), Ext(id',s',i',j') -> 
	   if id <> id' then ext1_vs_ext2 (id,s,i,j) (id',s',i',j')
	   else[ext_vs_ext id s (if i<i' then (i,i') else (i',i)) (j - i + 1)]

      in List.flatten (List.map c_solve sys)


    let partition l = 
      let rec add acc (t,cnf) = match acc with
	|[] -> [(t,[cnf])]
	|(t',cnf')::r -> if t = t' then (t',cnf::cnf')::r
	  else (t',cnf')::(add r (t,cnf))
      in List.fold_left add [] l


    let rec slicing_pattern s_l =
      let rec f_aux l1 l2 = match (l1,l2) with
	|[],[] -> []
	|a::r1,b::r2 when a = b -> a::(f_aux r1 r2)
	|a::r1,b::r2 -> 
	   if a < b then a::(f_aux r1 ((b-a)::r2))
	   else b::(f_aux ((a-b)::r1) r2)
	|_ -> assert false
      in List.fold_left f_aux (List.hd s_l)(List.tl s_l)
    
    let slice_var var s1 = 
      let s2 = var.sz - s1 in    
      match var.bv with
	|S_Cte _ -> {var with sz = s1},{var with sz = s2},None
	|S_Var v -> 
	   let (fs,sn,tr) = match v.sorte with
	     |A -> (fresh_var A), (fresh_var A), A
	     |B -> (fresh_var B), (fresh_var B), B
	     |C -> (fresh_var C), (fresh_var C), C
	   in {bv = S_Var fs; sz = s1},{bv = S_Var sn; sz = s2},Some tr
	      
    let rec slice_composition eq pat (ac_eq,c_sub) = match (eq,pat) with
      |[],[] -> (ac_eq,c_sub)
      |st::_,n::_  when st.sz < n -> assert false 
      |st::comp,n::pt -> 
	 if st.sz = n then slice_composition comp pt (st::ac_eq , c_sub)
	 else let (st_n,res,flag) = slice_var st n
	 in begin
	   match flag with
	     |Some B -> let comp' = List.fold_right
		 (fun s_t acc -> if s_t <> st then s_t::acc
		  else st_n::res::acc
		 )comp  []
	       in slice_composition (res::comp') pt (st_n::ac_eq,c_sub)

	     |Some C -> let ac' = (st_n::ac_eq,(st,(st_n,res))::c_sub)
	       in slice_composition (res::comp) pt ac'
		    
	     | _ -> slice_composition (res::comp) pt (st_n::ac_eq,c_sub)
	 end
      | _ -> assert false

    let uniforme_slice vls = 
      let pat = slicing_pattern(List.map (List.map(fun bv ->bv.sz))vls) in
      let rec f_aux acc subs l_vs = match l_vs with
	|[] -> acc,subs
	|eq::eqs -> let (eq',c_subs) = slice_composition eq pat ([],[])
	  in f_aux (List.rev eq'::acc) (c_subs@subs) eqs	   
      in f_aux [] [] vls

    let rec apply_subs subs sys = 
      let rec f_aux = function 
	|[] -> assert false
	|v::r -> try let (v1,v2) = List.assoc v subs in v1::v2::(f_aux r)
	  with _ -> v::(f_aux r)
      in List.map (fun (t,vls) ->(t,List.map f_aux vls))sys   

    let equations_slice parts = 
      let rec slice_rec bw = function
	|[] -> bw 
	|(t,vls)::r -> 
	   let (vls',subs) = uniforme_slice vls
	   in if subs =[] then slice_rec ((t,vls')::bw) r
	   else 
	     begin
	       let _bw = apply_subs subs bw in 
	       let _fw = apply_subs subs r in
	       if _bw = bw then slice_rec ((t,vls')::bw) _fw
	       else slice_rec [] (bw@((t,vls'):: _fw))
	     end
      in slice_rec [] parts   
  
    let rec union_sets sets = 
      let included e1 e2 = 
	try 
	  ST_Set.iter (fun at -> if ST_Set.mem at e2 then raise Exit)e1;
	  false
	with Exit -> true 
      in match sets with 
	|[] -> []
	|st::tl -> 
	   let (ok,ko) = List.partition (included st) tl in 
	   if ok = [] then st::union_sets tl
	   else union_sets ((List.fold_left ST_Set.union st ok)::ko)
	     
    let rec init_sets vals =
      let acc = List.map (fun at -> ST_Set.singleton at) (List.hd vals) in
      let tl = (List.tl vals) in
      let f_aux = List.map2 (fun ac_e e -> ST_Set.add e ac_e)      
      in List.fold_left f_aux acc tl
      
    let equalities_propagation eqs_slic = 
      let init_sets = List.map (fun (t,vls) -> init_sets vls) eqs_slic in
      let init_sets = List.flatten init_sets
      in List.map
	   (fun set -> 
	      let st1 = ST_Set.min_elt set and st2 = ST_Set.max_elt set
	      in  match st1.bv , st2.bv with
		|S_Cte false, S_Cte true -> raise Exception.Unsolvable 
		|S_Cte false , _ -> st1,set 
		|_ , _ -> st2,set    
	   ) (union_sets init_sets)
  
    let build_solution unif_slic sets =  
      let get_rep var = 
	fst(List.find ( fun(rep,set)->ST_Set.mem var set ) sets) in
      let to_external_ast v = 
	{sz = v.sz;
	 bv = match v.bv with
           |S_Cte b -> Cte b
	   |S_Var _ -> 
	      begin
		match (get_rep v).bv with
		  |S_Cte b -> Cte b
		  |S_Var tv -> Other (Var tv)
	      end 
	}in 
      let rec cnf_max l = match l with
	|[] -> []
	|[elt]-> [elt]
	|a::b::r -> 
	   begin
	     match a.bv,b.bv with
	       |Cte bol,Cte bol' when bol = bol' -> 
		  cnf_max ({ b with sz = a.sz + b.sz }::r)	    
	       | _,Cte _ -> a::(cnf_max (b::r))
	       | _ -> a::b::(cnf_max r)
	   end
      in List.map
	(fun (t,vls) -> 
	  t,cnf_max (List.map to_external_ast (List.hd vls))
	)unif_slic
	

    let solve u v =
      if u = v then raise Valid
      else begin
	let varsU = get_vars u in 
	let varsV = get_vars v in
	if varsU = [] && varsV = [] then raise Exception.Unsolvable
	else 
	  begin	      
	    let st_sys = slice u v in
	    let sys_sols = sys_solve st_sys in
	    let parts = partition sys_sols in
	    let unif_slic = equations_slice parts in
	    let eq_pr = equalities_propagation unif_slic in
	    let sol = build_solution unif_slic eq_pr in 
	    if Options.debug_bitv then 
	      begin
		Print.print_sliced_sys err st_sys;
		Print.print_c_solve_res err sys_sols;
		Print.print_partition_res err parts;
		Print.print_partition_res err unif_slic;
		Print.print_final_solution err sol;
	      end;
	    sol
	  end
      end

  end

  let compare b1 b2 = 
    let rec comp l1 l2 = match l1,l2 with
	[] , [] -> 0
      | [] , _ -> -1
      | _ , [] -> 1
      | st1::l1 , st2::l2 -> 
	  let c = compare_simple_term st1 st2 in
	  if c<>0 then c else comp l1 l2
    in comp b1 b2
    
  let leaves bitv = 
    List.fold_left 
      (fun acc x -> 
	 match x.bv with 
	   | Cte _  -> acc
	   | Ext( Var v,sz,_,_) -> 
	       (X.embed [{bv=Other (Var v) ; sz = sz }])::acc
	   | Other (Var _)  -> (X.embed [x])::acc
	   | Other (Alien t) | Ext(Alien t,_,_,_) -> (X.leaves t)@acc
      ) [] bitv

  let is_mine = function [{bv = Other (Alien r)}] -> r | bv -> X.embed bv

  let print = Print.print_C_ast

  let make t = 
    let r, ctx = Canonizer.make t in
    is_mine r, ctx

  let color _ = assert false

  let type_info bv = 
    let sz = List.fold_left (fun acc bv -> bv.sz + acc) 0 bv in
    Ty.Tbitv sz
  
  let to_i_ast biv = 
    let f_aux st = 
      {sz = st.sz;
       bv = match st.bv with
	 | Cte b -> Canonizer.I_Cte b
	 | Other tt -> Canonizer.I_Other tt 
	 | Ext(tt,siz,i,j)  -> 
	     let tt' = { sz = siz ; bv = Canonizer.I_Other tt } 
	     in Canonizer.I_Ext(tt',i,j)
      } in
    List.fold_left
      (fun acc st -> 
	 let tmp = f_aux st
	 in { bv = Canonizer.I_Comp(acc,tmp) ; sz = acc.sz + tmp.sz }
      ) (f_aux (List.hd biv)) (List.tl biv)
      
  let size_of r = 
    match X.type_info r with Ty.Tbitv i -> i | _ -> 
      Format.eprintf "ici=%a@." X.print r;
      assert false
    
  let extract r ty = 
    match X.extract r with 
	Some (u::_ as bv) -> to_i_ast bv
      | None -> {bv =  Canonizer.I_Other (Alien r); sz = ty}
      | Some [] -> assert false

  let extract_xterm r = 
    match X.extract r with 
	Some ([{bv=Other(Var _ as x)}]) -> x
      | None -> Alien r
      | _ -> assert false

  let var_or_term x = 
    match x.bv with
	Other (Var _) -> X.embed [x]
      | Other (Alien r) -> r
      | _ -> assert false


  (* ne resout pas quand c'est deja resolu *)
  let solve repr u t = 
    if Options.debug_bitv then 
      eprintf "[Bitv] solve %a = %a@." X.print u X.print t;
    
    match X.extract u , X.extract t with 
      | None   , None   -> assert false
      | None   , Some _ -> [u , t]
      | Some _ , None   -> [t , u]
      | Some u , Some t -> 
	  try
	    List.map 
	      (fun (p,v) -> var_or_term p,is_mine v) 
	      (Solver.solve u t)
	  with Solver.Valid -> []
	      

 
  let rec subst_rec x subs biv = 
    match biv.bv , extract_xterm x with
      | Canonizer.I_Cte _ , _ -> biv
      | Canonizer.I_Other (Var y) , Var z when y=z -> extract subs biv.sz
      | Canonizer.I_Other (Var _) , _ -> biv
      | Canonizer.I_Other (Alien tt) , _ -> 
	  if X.compare x tt = 0 then 
	    extract subs biv.sz
	  else extract (X.subst x subs tt) biv.sz
      | Canonizer.I_Ext (t,i,j) , _ -> 
	  { biv with bv = Canonizer.I_Ext(subst_rec x subs t,i,j) }
      | Canonizer.I_Comp (u,v) , _ -> 
	  { biv with 
	      bv = Canonizer.I_Comp(subst_rec x subs u ,subst_rec x subs v)}

  let subst x subs biv = 
    if Options.debug_bitv then 
      eprintf "[Bitv] subst %a |-> %a in %a@." X.print x X.print subs print biv;
    if biv = [] then is_mine biv
    else 
      let r = Canonizer.sigma (subst_rec x subs (to_i_ast biv)) in
      is_mine r

    module M =  Map.Make 
      (struct 
         type t = X.r
         let compare = X.compare 
       end)


  module Map = Map.Make 
    (struct 
       type t = (X.r simple_term) list
       let compare = compare 
     end)
    
  module Set = Set.Make (
    struct 
      type t = (X.r simple_term) list
      let compare = compare
    end)

  module Rel = struct

    type r = X.r
        (* ces X.t sont toujours des constantes *)
        (* Cette map associe à chaque représentant r de type bitvector
           une constante c tel que toutes les constantes plus petite
           que c sont différentes de r.
           Un représentant r est associé à None si c'est une constante*)
    type elt = t
    type t =  (elt option) M.t

    exception Inconsistent    
    let empty () = M.empty

    let is_bitv t = 
      match (Term.view t).T.ty with 
        | Ty.Tbitv _ -> true
        | _ -> false

    let assume env la = 
      let leqs = 
	List.fold_left 
          (fun acc (a, root) -> 
             match a with
               | Literal.Neq(rx1,rx2) ->
                   begin
		     match X.extract rx1, X.extract rx2 with
                       | Some [r1], Some [r2] when r1.sz = 1 -> 
                           begin
			     match r1.bv, r2.bv with
                               | Cte b, Other o -> 
				   let v = X.embed [r1] in
				   (Literal.Eq(rx2,v),root)::acc  
			       | Other o, Cte b -> 
				   let v = X.embed [r2] in
				   (Literal.Eq(rx1,v),root)::acc  
                               | _ -> acc
			   end
                       | _ -> acc
                   end
               | _ -> acc) [] la
      in 
      env, leqs
 
    let cst0 n = 
      let rec aux acc = function
        | n when n <= 0 -> acc
        | n -> aux ({bv=Cte false;sz=1}::acc) (n-1) in
      aux [] n
        
    let add env r = 
      match X.type_info r with
        | Ty.Tbitv i when not (M.mem r env) ->     
            let cst0 = Some (cst0 i) in
            M.add r cst0 env
        | _ -> env

    let rec succ = function
      | [] -> raise Exception.Inconsistent
      | ({bv=false} as x)::l -> {x with bv = true }::l
      | ({bv=true } as x)::l -> {x with bv = false}::(succ l)

    let case_split env = []    
    let query _ _ = false
    let instantiate env _ _ = env, []
  end

end
