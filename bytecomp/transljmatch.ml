(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*                Qin Ma, projet MOSCOVA, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*
  Carry out the compilation from join definition with pattern matching
  of message contents to equivalent join definition without the requirement
  of pattern matching on message contents.

  cf. Compiling Pattern Matching in Join-Patterns
*)

open Typedtree
open Joinmatch

let null_ex = 
  {exp_desc = Texp_null;
    exp_loc = Location.none;
    exp_type = Ctype.none;
    exp_env = Env.empty
}

let null_pat = Parmatch.omega

(************************************************************************) 
(*   
     Collect all the channels along with their arguments
     from joinclause array into an association list.
   
     joinclause array -> (Ident.t * pattern list) list
                 
*************************************************************************)

let collect cls =
  (*tbl_id_args: Ident.t * pattern list Hashtbl.t*)
  let tbl_id_args = Hashtbl.create 10 in
  (*
    collect_jpat records channel and its corresponding arg into tbl_id_arg
     
    joinpattern -> () 
  *)
  let rec collect_jpat jpat =
    let (jid,arg) = jpat.jpat_desc in
    let former_args = 
      try
	Hashtbl.find tbl_id_args jid.jident_desc
      with Not_found -> []
    in
    Hashtbl.replace tbl_id_args jid.jident_desc (arg::former_args) in
  (*joinclause -> ()*)
  let collect_clause cl =
    let (jpats,ex) = cl.jclause_desc in
    List.iter collect_jpat jpats in
  Array.iter collect_clause cls;
  let f id args ls = (id,args)::ls in
  Hashtbl.fold f tbl_id_args []



(*************************************************************************)
(*  Build dispatcher on one channel
    Ident.t -> pattern Agraph.t -> pattern Agraph.node * Ident.t Hashtbl.t 
    -> partial -> dispatcher

make_disp id_ch2be_dispatched dag tbl_node2newchid is_partial

**************************************************************************)

let make_disp id dag tbl_node2id par =
  (*ns_sorted: nodes in dag sorted in a queue*)
  let ns_sorted = 
    try
      Agraph.top_sort dag
    with Agraph.Cyclic -> assert false in
  let z = Ident.create "z" in
  (*build the (pattern, newid) list from a queue of nodes*)
  let rec build_rules q =
    if Queue.is_empty q
    then []
    else
      let n = Queue.take q in
      let pat = Agraph.info dag n in
      let newid = Hashtbl.find tbl_node2id n in
      (pat,newid) :: (build_rules q) in
  let rules = build_rules ns_sorted in
  (id, z, rules, par)


(*************************************************************************)
(*  Rewrite reaction rules from match_clause format to match_clause format
    Ident.t -> match_clause Array.t -> pattern Agraph.t -> 
    (pattern Agraph.node * Ident.t) Hashtbl.t
    -> match_clause Array.t 
    
rewrtite id_ch2be_rewrite match_clauses dag tbl_node2newchid

**************************************************************************)

let rewrite id mcls dag tbl_node2id =
  (*rewrite one reaction rule*)
  let rewrite_mclause mcl =
    match mcl with
    | Dispatcher _ -> mcl
    | Reaction (jpats_ls,gd) ->
	let jpat_has_id jpat id =
	  let (jid, _) = jpat.jpat_desc in
	  Ident.unique_name jid.jident_desc == Ident.unique_name id in
	(* has_id: joinpattern list list -> Ident.t -> 
	   (joinpattern list list * joinpattern) option

	   has_id [[jpat_11;...];...;[jpat_n1;...]] id means:
	   if there exists a jpat_ij such that (jpat_has_id jpat_ij id) is true,
	   then return Some 
	                ([[jpat_11;...;jpat_1n];
	                  ...;
	                  [jpat_(i-1)1;...;jpat_(i_1)n];
	                  ...;
	                  [jpat_(i+1)1;...;jpat_(i+1)n];
	                  [jpat_n1;...;jpat_nm]], jpat_ij)
	   else return None 
	   
	   By linearity of join patterns, such an i is unique.
	   Moreover, j=1, this is an invariant of the compilation scheme.
	   *)
	let rec has_id jpats_ls id =
	  match jpats_ls with
	  | [] -> None
	  | jpats::jpats_ls' ->
	      let result = has_id jpats_ls' id in
	      match result with
	      |	None -> 
		  let r =
		    try
		      let jpat =
			List.find (fun jpat -> jpat_has_id jpat id) jpats in
		      Some ([],jpat)
		    with Not_found -> None in
		  (match r with
		  | None -> None
		  | Some (_,jpat) -> Some (jpats_ls',jpat))
	      |	Some (remains, jpat) -> Some (jpats::remains, jpat) in
	match has_id jpats_ls id with
	| None -> mcl
	| Some (remains, jpat) ->
	    let (jid, pat) = jpat.jpat_desc in
	    let xi = Ident.create "xi" in
	    let xi_pat = {pat with pat_desc = Tpat_var xi} in
	    let nodes = Agraph.nodes dag in
	    let has_info n =
	      (Parmatch.le_pat (Agraph.info dag n) pat) &&
	      (Parmatch.le_pat pat (Agraph.info dag n)) in
	    let pat_node = List.find has_info nodes in
	    let preds = Agraph.prec dag pat_node in
	    let rec build_or nds =
	      match nds with
	      | [nd] ->
		  [{jpat with jpat_desc = 
                      ({jid with jident_desc = (Hashtbl.find tbl_node2id nd)}, 
		      xi_pat)}]
	      | hd::tl ->
		  {jpat with jpat_desc = 
                      ({jid with jident_desc = (Hashtbl.find tbl_node2id hd)}, 
		      xi_pat)} ::
		  build_or tl
	      | [] -> assert false in
	    let new_or_jpats = build_or (pat_node::preds) in
	    let (id2pat_ls, ex) = gd in
	    Reaction (new_or_jpats::remains, ((xi, pat)::id2pat_ls, ex)) in
  Array.map rewrite_mclause mcls


(*******************************************************************************)
(*      Build the dag from a pairwise distinct list of patterns
        Ident.t -> pattern list -> 
	pattern Agraph.t * (pattern Agraph.node * Ident.t) Hashtbl.t

build_dag id_ch2be_builtdag pat_args 

********************************************************************************)

let build_dag id pats =
  let dag = Agraph.create null_pat in
  let fresh_id = fun () -> Ident.create (Ident.name id) in
  (*tbl_node2id: hash table from node in dag to new channel id*)
  let tbl_node2id = Hashtbl.create 10 in
  let rec do_build_dag pats =
    (match pats with
    | [] -> ()
    | pat::pats' ->
	do_build_dag pats';
	let oldnodes = Agraph.nodes dag in
	let newnode = Agraph.new_node dag pat in
	let newid = fresh_id () in
	Hashtbl.add tbl_node2id newnode newid;
	let rec draw_edge newn oldns =
	  match oldns with
	  | [] -> ()
	  | hd_n::tl_ns ->
	      if Parmatch.le_pat (Agraph.info dag hd_n) pat
	      then Agraph.new_edge dag newn hd_n
	      else Agraph.new_edge dag hd_n newn;
	      draw_edge newn tl_ns in
	draw_edge newnode oldnodes) in
  do_build_dag pats;
  (dag, tbl_node2id)
      

(********************************************************************************)
(*
     Compile join defintion from and to match_automaton format 
  according to one channel. 

     match_automaton -> Ident.t -> pattern list -> match_automaton

  y match_auto id_ch2be_compiled pat_args

*********************************************************************************)

let y mauto id args =
  (*trim_eq_pat gets rid of equivalent patterns in a pattern list*)
  (*trim_eq_pat: pattern list -> pattern list*)
  let rec trim_eq_pat pats =
    match pats with
    | [] -> []
    | pat::pats' ->
	let trimed_pats' = trim_eq_pat pats' in
	(*f merges a pattern into a pairwise distinct pattern list
	  and the result list is still pairwise distinct.
	  f pat pats means:
          if there is a pattern pat' in pats equivalent to pat, keep lub pat pat', 
	  else, add pat *)
        (*f: pattern -> pattern list -> pattern list*)
	let rec f pt pts =        
	  match pts with
	  | [] -> [pt]
	  | hd::tl ->
	      if (Parmatch.le_pat pt hd) && (Parmatch.le_pat hd pt)
	      then (Parmatch.lub pt hd)::tl
	      else hd::(f pt tl) in
	f pat trimed_pats' in
  let pi'= trim_eq_pat args in
  let patch = List.map (fun pat -> (pat,null_ex)) pi' in
  let par = Parmatch.check_partial mauto.jauto_loc patch in 
  match pi' with
  | [] -> assert false         (*every channel takes a pat arg*)
  | [pat] ->
      (match par with
      |	Total -> mauto
      |	Partial ->
	  let (dag, tbl_node2id) = build_dag id pi' in
	  let old_jc = List.assoc id mauto.jauto_names in
	  let nchans = ref mauto.jauto_nchans in
	  let new_jauto_names =
	    List.map 
	      (fun newid ->
		nchans := (!nchans)+1;
		(newid, {old_jc with jchannel_id = (!nchans) - 1}))
	      (Hashtbl.fold 
		 (fun node id ids -> id::ids) tbl_node2id []) in
	  let disp = make_disp id dag tbl_node2id par in
	  let mcls' = rewrite id mauto.jauto_desc dag tbl_node2id in
	  let new_mcls = Array.append mcls' (Array.make 1 (Dispatcher disp)) in
	  {mauto with jauto_desc = new_mcls; 
             jauto_names = mauto.jauto_names @ new_jauto_names;
             jauto_nchans = !nchans}
		  )
  | _ -> 
      (* Compute all possible lubs of some patterns fromp pats
         ref. the F function in Step 1
         pattern list -> pattern list *)
      let rec compute_lubs pats =
	match pats with
	| [] -> []
	| pat::pats' ->
	    (*compute the lubs between pat and the pattern in pts*)
	    let rec lubs pat pts =
	      match pts with
	      | [] -> []
	      | pt::pts' ->
		  try 
		    Parmatch.lub pat pt :: (lubs pat pts') 
		  with Parmatch.Empty -> lubs pat pts' in
	    let lubs' = compute_lubs pats' in
	    (pat::lubs') @ lubs pat lubs' in
      let gamma = compute_lubs pi' in
      let gamma' = trim_eq_pat gamma in
      let (dag, tbl_node2id) = build_dag id gamma' in
      let old_jc = List.assoc id mauto.jauto_names in
      let nchans = ref mauto.jauto_nchans in
      let new_jauto_names =
	List.map 
	  (fun newid ->
	    nchans := (!nchans)+1;
	    (newid, {old_jc with jchannel_id = (!nchans) - 1}))
	  (Hashtbl.fold 
	     (fun node id ids -> id::ids) tbl_node2id []) in
      let disp = make_disp id dag tbl_node2id par in
      let mcls' = rewrite id mauto.jauto_desc dag tbl_node2id in
      let new_mcls = Array.append mcls' (Array.make 1 (Dispatcher disp)) in
      {mauto with jauto_desc = new_mcls; 
         jauto_names = mauto.jauto_names @ new_jauto_names;
         jauto_nchans = !nchans}


(*****************************************************************************)  
(*
     Compile join definition with pattern matching on message content 
     into equivalent one without pattern matching on message content

     joinautomaton -> match_automaton

******************************************************************************)

let transl_jmatch auto =
  (*names record (id_ch, args) pair list
    (Ident.t * pattern list) list*)
  let names = collect auto.jauto_desc in
  (*mauto is just auto but of type match_automaton*)
  let mauto =
    (*trivial_clause rewrites joinclause into match_clause format*)
    let trivial_clause cl =
      let (jpats,ex) = cl.jclause_desc in
      let jpats_ls = List.map (fun jpat -> [jpat]) jpats in
      Reaction (jpats_ls, ([],ex)) in
    { auto with jauto_desc =
        Array.map trivial_clause auto.jauto_desc} in
  (*do_tranl_jmatch carries out the compilation, from and to match_automaton format
    do_transl_jmatch: match_automaton -> 
    (Ident.t * pattern list) list -> match_automaton*)
  let rec do_transl_jmatch mauto names =
    match names with
    | [] -> mauto
    | (id,args)::names' ->
	do_transl_jmatch (y mauto id args) names' in
  do_transl_jmatch mauto names

