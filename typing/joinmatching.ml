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


(* Nul process *)
let null_ex = 
  { exp_desc = Texp_null;
    exp_loc = Location.none;
    exp_type = Ctype.none;
    exp_env = Env.empty ; }

(* omega pattern *)
let null_pat = Parmatch.omega

(* pattern equivalence test *)
let eq_pat p1 p2 = Parmatch.le_pat p1 p2 && Parmatch.le_pat p2 p1

(*
     Collect all the channels along with their arguments
*)

let collect cls =

  let tbl_id_args =
    (Hashtbl.create 17 :
       (Ident.t, pattern list) Hashtbl.t) in

  let collect_jpat jpat =
    let (jid,arg) = jpat.jpat_desc in
    try 
      let args = Hashtbl.find tbl_id_args jid.jident_desc in
      Hashtbl.replace tbl_id_args jid.jident_desc (arg::args)
    with
    | Not_found ->
        Hashtbl.add tbl_id_args jid.jident_desc [arg] in

  let collect_clause ((_, jpats, _),_) =  List.iter collect_jpat jpats in
  List.iter collect_clause cls;
  let f id args ls = (id,args)::ls in
  Hashtbl.fold f tbl_id_args []



(*  Build dispatcher on one channel *)

let make_disp id dag par =
  let fresh_id = fun () -> Ident.create (Ident.name id) in
  let sorted_ns = 
    try
      Agraph.top_sort dag
    with Agraph.Cyclic -> assert false in
  let sorted_pats = List.map (fun n -> n,Agraph.info dag n) sorted_ns in
  let usage = Parmatch.useful (List.map snd sorted_pats) in
  let sorted_pats =
    List.fold_right2
      (fun c useful r -> if useful then c::r else r)
      sorted_pats usage [] in
(* Now sorted_pats collects usefuls pairs node X pattern *)
  let newids = List.map (fun _ -> fresh_id ()) sorted_pats in
  let node2id = 
    List.map2 (fun (n,_) id -> n,id) sorted_pats newids in
  let rules =
    List.map2
      (fun (_,pat) id -> Parmatch.remove_binders pat, id)
      sorted_pats newids in
  (id, rules, par), node2id


(***************************)
(*  Rewrite reaction rules *)
(***************************)

(* 
  has_id [p1; pn] id means:
  there exists a pi with id as a channel
  then return Some ([p1 ; ... ; p_{i-1} ; p_{i+1} ; ... ; pn], pi)
  otherwise return None 

  Notice: by linearity of join patterns, such an i is unique.
*)


let jpat_has_id jpat id =
  let (jid, _) = jpat.jpat_desc in
  Ident.unique_name jid.jident_desc = Ident.unique_name id

let rec has_id jpats id = match jpats with
| [] -> None
| jpat::rem ->
    match  has_id rem id with
    | Some (rem, found) -> Some (jpat::rem, found)
    | None ->
        if jpat_has_id jpat id then
          Some (rem, jpat)
        else
          None
            
(* Simple rewrite of reaction rules: No dispatcher
   only perform transfert of pattern from joinpattern to guarded process *)
let rewrite_simple_one id reac =
  let old, (to_do, already_done), gd = reac in
  match has_id to_do id with
  | None -> reac 
  | Some (rem, found) ->
      let (jid, pat) = found.jpat_desc in
      let xi = Ident.create ("_"^Ident.name jid.jident_desc) in
      let xi_pat = {pat with pat_desc = Tpat_var xi} in
      old,
      (rem, [{found with jpat_desc = (jid, xi_pat)}]::already_done),
      (xi, pat)::gd

let rewrite_simple id reacs = List.map (rewrite_simple_one id) reacs
    
(* Complete rewrite of reaction rules: dispatcher
    - perform transfert of pattern from joinpattern to guarded process
    - replace channel by list of dispatched channels in joinpattern *)

let rewrite_one id dag node2id reac =
  let old, (to_do, already_done), gd = reac in
  match has_id to_do id with
  | None -> reac 
  | Some (rem, found) ->
      let (jid, pat) = found.jpat_desc in
      let xi = Ident.create ("_"^Ident.name jid.jident_desc) in
      let xi_pat = {pat with pat_desc = Tpat_var xi} in
      let new_or_jpats =
	let nodes = Agraph.nodes dag in
	let has_info n = eq_pat (Agraph.info dag n) pat in
	let pat_node =
          try List.find has_info nodes with Not_found -> assert false in
	let preds = Agraph.prec dag pat_node in
        List.fold_right
          (fun nd r ->
(* nodes whose patterns are useless are not present in node2id *)
            try
              let ch_id = List.assoc nd node2id in
              let ch_id = { jid with jident_desc = ch_id } in
              {found with jpat_desc = (ch_id,xi_pat)}::r
            with Not_found -> r)
          (pat_node::preds) [] in
      old,
      (rem, new_or_jpats::already_done),
      (xi, pat)::gd

let rewrite id dag node2id reacs =
  List.map (rewrite_one id dag node2id) reacs
      
    
    
(*******************************************************************************)
(*      Build the dag from a pairwise distinct list of patterns
        pattern list -> pattern Agraph.t 

	build_dag pat_args 

********************************************************************************)

let build_dag pats =
  let dag = Agraph.create null_pat in
  let rec do_build_dag pats =
    (match pats with
    | [] -> ()
    | pat::pats' ->
        do_build_dag pats';
	let oldnodes = Agraph.nodes dag in
	let newnode = Agraph.new_node dag pat in
	let rec draw_edge newn oldns =
	  match oldns with
	  | [] -> ()
	  | hd_n::tl_ns ->
              begin
	      if Parmatch.le_pat (Agraph.info dag hd_n) pat then
                Agraph.new_edge dag newn hd_n
	      else if Parmatch.le_pat pat (Agraph.info dag hd_n) then
                Agraph.new_edge dag hd_n newn
              end ;
	      draw_edge newn tl_ns in
	draw_edge newnode oldnodes) in
  do_build_dag pats;
  dag
      

(***********************************)
(* Complete compilation w.r.t. dag *)
(***********************************)

let yfinal (disps, reacs, new_names) id par dag =
  let (disp, node2id) = make_disp id dag par in
  let reacs = rewrite id dag node2id reacs in
  let new_names = (id,List.map snd node2id)::new_names in
  disp::disps, reacs, new_names

(* gets rid of equivalent patterns in a pattern list*)
let rec add pt pts = match pts with
| [] -> [pt]
| hd::tl ->
    if eq_pat pt hd
    then (Parmatch.lub pt hd)::tl
    else hd::add pt tl

let rec trim_eq_pats = function
  | [] -> []
  | pat::pats -> add pat (trim_eq_pats pats)

(*
  Compile matching for name id,
  args is the list of patters collected from reaction rules
*)
let y auto_loc ((disp, reac, new_names) as auto) id args =
  let pi'= trim_eq_pats args in
  let bidon = List.map (fun pat -> (pat,null_ex)) pi' in
  let par = Parmatch.check_partial auto_loc bidon in
  match pi' with
  | [] -> assert false (* since args is not empty *)
  | [pat] ->
      begin match par with
      |	Total -> 
	  let reac = rewrite_simple id reac in
          disp, reac, new_names
      |	Partial ->
	  let dag = build_dag pi' in
	  yfinal auto id par dag
      end
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
      let gamma' = trim_eq_pats gamma in
      let dag = build_dag gamma' in
      yfinal auto id par dag

(***********************************************************)  
(* Access point:                                           *)
(*   compile join definitions with pattern arguments into  *)
(*   ordinary join definitions + dispatchers               *)
(***********************************************************)

type 'a reaction = Location.t * joinpattern list * 'a

type dispatcher =
  Ident.t * (pattern * Ident.t) list * partial

type ('a, 'b) guard =
  ('a reaction * 'b) * (* old clause *)
  (joinpattern list list * (* new joinpattern *)
  (Ident.t * Typedtree.pattern) list) (* inserted matching *)

let compile auto_loc cls =
  let name_args = collect cls
  and cls =
    let trivial_clause (((_, jpats, _),_) as cl) =
      cl, (jpats,[]), [] in
    List.map trivial_clause cls in

  let rec do_rec r = function
    | [] ->
        let disp, reacs, new_names = r in
        let reacs =
          List.map
            (fun (old,(to_do, already_done),gd) ->
              assert (to_do=[]) ;
              old, (already_done, gd))
            reacs in
        (disp, reacs), new_names
    | (id, args)::names ->
       do_rec (y auto_loc r id args) names in

  do_rec ([], cls, []) name_args

