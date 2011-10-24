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

open Why_ptree

module F = Formula

module HS = struct
  type t = H of Hstring.t | STrue | SFalse
  let compare x y = match x,y with
    | H x, H y -> Hstring.compare x y
    | H _, _ -> -1
    | _, H _ -> 1
    | STrue, STrue -> 0
    | STrue, SFalse -> -1
    | SFalse, STrue -> 1
    | SFalse, SFalse -> 0
	
  let hash = function 
      H x -> Hstring.hash x | STrue -> 0 | SFalse -> 1
	
  let equal x y = compare x y = 0

  let view = function
      H x -> Hstring.view x | STrue -> "True" | SFalse -> "False"
end

module GF = Graph.Persistent.Digraph.ConcreteLabeled
  (HS)
  (struct 
     type t = string
     let compare = Pervasives.compare
     let default = "_default_axiom_"
   end)

module SetH = Set.Make (Hstring)
module SetHS = Set.Make (HS)
module SetS = Set.Make (struct type t = string let compare = compare end)
module HtblHS = Hashtbl.Make(Hstring)

let graph_attrs = ref (fun g -> [])
let edge_attrs = ref (fun e -> [])
let vertex_attrs = ref (fun v -> [])

module PrintG = Graph.Graphviz.Dot
  (struct
     include GF
     let graph_attributes g = (!graph_attrs g)
     let default_vertex_attributes g = []
     let vertex_name v = HS.view v
     let vertex_attributes v = `Style(`Filled)::(!vertex_attrs v)
     let get_subgraph v = None
     let default_edge_attributes g = []
     let edge_attributes e = `Label(E.label e)::(!edge_attrs e)
   end)

let print_graph g =
  let name , cout = Filename.open_temp_file "tmp_graph" ".dot" in
  let ps = name^".ps" in
  PrintG.output_graph cout g;
  ignore(Sys.command ("dot -Tps "^name^" > "^ps^" && evince "^ps))

module Polarite = struct
  type t = Plus | Moins | Neutre

  let not = function
      Plus -> Moins
    | Moins -> Plus
    | Neutre -> Neutre
	
  let xor b1 b2 =
    match b1,b2 with
	Plus , b | b , Plus -> b
      | Neutre , _ | _ , Neutre -> Neutre
      | Moins , Moins -> Plus

  let add p1 p2 = if p1 = p2 then p1 else Neutre

end


module PInfo = struct

  type spn = { mutable pos : SetH.t; mutable neg :  SetH.t; }
  type t_symb = { 
    all_symbs : spn; 
    conclusion : spn; 
    args : (Hstring.t * ((Polarite.t * bool) option) ref ) list
  }
      
  let defs = HtblHS.create 17

  let var_of_symb =  function
      Symbols.Var hs -> hs
    | _ -> assert false
 
  let init s l =  
    HtblHS.add defs s 
      {all_symbs = {pos = SetH.empty ; neg = SetH.empty };
       conclusion = {pos = SetH.empty ; neg = SetH.empty };
       args = List.rev_map (fun x -> (var_of_symb x,ref None)) l}

  let find p = try
    let { all_symbs = {pos=spos;neg=sneg}; 
	  conclusion = {pos=cpos;neg=cneg}; args=args} = HtblHS.find defs p in
    let args = List.map (fun (_,x) -> !x) args in
    spos , sneg , cpos, cneg , args
  with Not_found -> 
    SetH.empty , SetH.empty, SetH.singleton p, SetH.empty , []

  let update_spn v pol s = 
    match pol with
	Polarite.Plus -> v.pos <- SetH.add s v.pos
      | Polarite.Moins -> v.neg <- SetH.add s v.neg
      | Polarite.Neutre -> v.pos <- SetH.add s v.pos; v.neg <- SetH.add s v.neg

  let add p pol concl s = try
    let v = HtblHS.find defs p in
    update_spn (if concl then v.conclusion else v.all_symbs) pol s
  with Not_found -> assert false
    
  let set p pol concl x = try
    let {args=args} =  HtblHS.find defs p in
    (try 
       let info = List.assoc x args in
       info := 
	 match !info with
	     None -> Some(pol,concl)
	   | Some(pol',concl') -> Some(Polarite.add pol pol',concl || concl')
     with Not_found -> ())
  with Not_found -> assert false

    
end

let symbs_of_term add set pol concl t = 
  let pol = ref pol in
  let rec symb_rec t = 
    match t.tt_desc with
      | TTvar (Symbols.Name (hs,_)) -> add !pol concl hs
      | TTvar (Symbols.Var hs) -> set !pol concl hs
      | TTapp (Symbols.Name (hs,_), l) ->
	  add !pol concl hs;
	  List.iter symb_rec l
      | TTinfix (t1,_,t2) -> symb_rec t1; symb_rec  t2
      | _ -> ()
  in symb_rec t

let symbs_in_formula add set f = 
  let rec symbs_rec pol concl = function
    | TFatom (TApred {tt_desc =  TTapp(Symbols.Name (hs,_), l)}) -> 
	let spos , sneg , cpos , cneg , args = PInfo.find hs in
	SetH.iter (add pol false) spos;
	SetH.iter (add (Polarite.not pol) false) sneg;
	SetH.iter (add pol concl) cpos;
	SetH.iter (add (Polarite.not pol) concl) cneg;
	begin
	  match args with 
	      [] -> 
		List.iter (symbs_of_term add set pol concl) l
	    | _ -> 
		List.iter2
		  (fun t x -> 
		     match x with 
			 None -> ()
		       | Some(pol',concl') -> 
			   let pol'' = Polarite.xor pol pol' in
			   let concl'' = concl' && concl in
			   symbs_of_term add set pol'' concl'' t
		  ) l args
	end

    | TFatom (TAbuilt(_,l)  | TAeq l | TAneq l | TAle l | TAlt l) ->
	List.iter (symbs_of_term add set pol concl) l

    | TFatom _ -> ()

    | TFop((OPand | OPor),fl) ->
	List.iter (symbs_rec pol concl) fl

    | TFop(OPnot,[f]) ->
	symbs_rec (Polarite.not pol) concl f

    | TFop(OPimp,[f1;f2]) ->
	symbs_rec (Polarite.not pol) false f1;
	symbs_rec pol concl f2

    | TFop(OPiff,[f1;f2]) ->
	let imp f1 f2 = TFop(OPimp,[f1;f2]) in
	symbs_rec pol concl (TFop(OPand,[imp f1 f2; imp f2 f1]))

    | TFop(OPif _,[f1;f2]) ->
	failwith "OPif is not implemented"

    | TFforall {qf_form = f} | TFexists {qf_form = f} -> 
	symbs_rec pol concl f

    | TFlet _ ->
        failwith "let is not implemented"

    | _ -> assert false
  in
  symbs_rec Polarite.Plus true f

let analyze_formula s g f =
  let spos = ref SetH.empty in
  let sneg = ref SetH.empty in
  let add p _ s = match p with
      Polarite.Plus -> spos := SetH.add s !spos 
    | Polarite.Moins -> sneg := SetH.add s !sneg 
    | Polarite.Neutre -> spos := SetH.add s !spos; sneg := SetH.add s !sneg 
  in
  symbs_in_formula add (fun _ _ _ -> ()) f;
  let spos , sneg = !spos , !sneg in
  match SetH.is_empty spos, SetH.is_empty sneg with
    | true, true -> 
	GF.add_edge_e g (GF.E.create HS.SFalse s HS.STrue)
    | true, false ->
	SetH.fold 
	  (fun sn g -> 
	     GF.add_edge_e g (GF.E.create (HS.H sn) s HS.SFalse))
	  sneg g
    | false, true ->
	SetH.fold 
	  (fun sp g -> 
	     GF.add_edge_e g (GF.E.create HS.STrue s (HS.H sp)))
	  spos g
    | false, false ->
	SetH.fold 
	  (fun sp g -> 
	     SetH.fold 
	       (fun sn g -> 
		  GF.add_edge_e g (GF.E.create (HS.H sn) s (HS.H sp)))
	       sneg g)
	  spos g
	  
let analyze_deps decl_list =
  List.fold_left
    (fun (g,gls) d -> match d with
       | TAxiom (_,s,f) ->
	   analyze_formula s g f, gls
       | TPredicate_def 
	   (_,s,_,TFforall {qf_form = TFop(OPiff,[f1;f2]); 
			    qf_bvars = lvars}) ->
	   let l , _ = List.split lvars in
	   let s = Hstring.make s in
	   PInfo.init s l;
	   symbs_in_formula (PInfo.add s) (PInfo.set s) f2;
	   (g, gls)
       | TPredicate_def _ -> assert false
       | TGoal (l,s,f) -> (g, (s,f)::gls)
       | _ -> (g,gls))
    (GF.empty, []) decl_list

let find_relevant b deps q = 
  let rec find b deps q vus acc =
    try
      let step , get = 
	if b then GF.fold_succ_e , GF.E.dst else GF.fold_pred_e , GF.E.src in
      let v, d = Queue.pop q in
	if d <= 0 then acc 
	else
	  let acc' =
	    try
	      let prevd = d-1 in
	      if SetHS.mem v vus then acc else
		step (fun e acc -> Queue.push (get e, prevd) q; 
			SetS.add (GF.E.label e) acc)  deps v acc
	    with Invalid_argument _ -> acc
	  in
	    find b deps q (SetHS.add v vus) acc'
    with Queue.Empty -> acc
  in
    find b deps q SetHS.empty SetS.empty

let rouge = 0x990000
let vert = 0x009900
let bleu = 0x000099
let jaune = 0x999900

let prune_goal depth s f g =
  let spos , sneg = ref SetH.empty , ref SetH.empty in
  let add p c s = 
    if c then
      begin
	match p with
	    Polarite.Plus -> spos := SetH.add s !spos 
	  | Polarite.Moins -> sneg := SetH.add s !sneg 
	  | Polarite.Neutre -> 
	      spos := SetH.add s !spos; sneg := SetH.add s !sneg 
      end
  in
  symbs_in_formula add (fun _ _ _ -> ()) f;
  let spos , sneg = !spos , !sneg in
  vertex_attrs :=
    (fun v -> 
       match v with 
	 | HS.STrue -> [`Fillcolor vert]
	 | HS.SFalse -> [`Fillcolor jaune]
	 | HS.H s -> (match SetH.mem s spos, SetH.mem s sneg with
			  | true, true -> [`Fillcolor jaune]
			  | false, true -> [`Fillcolor rouge]
			  | true, false -> [`Fillcolor vert]
			  | false, false -> []));
    let qin, qout = Queue.create (), Queue.create () in
      SetH.iter (fun s -> Queue.push (HS.H s, depth) qin) sneg; 
      SetH.iter (fun s -> Queue.push (HS.H s, depth) qout) spos;
      Queue.push (HS.SFalse, depth) qin; 
      Queue.push (HS.SFalse, depth) qout; 
      Queue.push (HS.STrue, depth) qout;
      SetS.union (find_relevant true g qin)
	(find_relevant false g qout)
      
let print_flag = true

let split_and_prune depth decl_list = 
  let deps, goals = analyze_deps decl_list in
  match goals with 
      [] -> [List.map (fun f -> f,true) decl_list]
    | _ ->
	List.map
	  (fun (s,f) ->
	     let df = prune_goal depth s f deps in
	     graph_attrs := (fun g -> [`Label ("Goal: "^s)]);
	     edge_attrs := 
	       (fun e -> if SetS.mem (GF.E.label e) df then 
		  [`Fontcolor bleu; `Color bleu] else []);
	     if print_flag then 
	       begin
		 print_graph deps;
		 Printf.printf "Goal %s:\n" s;
		 SetS.iter (fun s -> Printf.printf "\t %s\n" s) df; 
		 flush stdout;
	       end;
	     List.fold_right
	       (fun f acc -> match f with
		  | TAxiom(_,s',TFforall{qf_bvars=_::_}) -> 
		      if SetS.mem s' df then (f,true)::acc else acc
		  | TAxiom(loc,s',f') -> 
		      if SetS.mem s' df then (f,true)::acc
		      else (TAxiom(loc,s',f'),false)::acc
		  | TGoal (_,s',_) -> if s = s' then (f,true)::acc else acc
		  | _ -> (f,true)::acc) decl_list [])
	  goals

