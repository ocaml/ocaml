(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Heuristics for matching *)

open Typedtree
open Discr

(*************************************)
(* put column no k in first position *)
(*************************************)

let rec swap k x = function
  | [] -> assert false
  | y::ys ->
      if k = 0 then y,x::ys
      else
	let z,ys = swap (k-1) x ys in
	z,y::ys

let swap k xs = match k with
| 0 -> xs
| _ ->
    match xs with
    | x::xs ->
	let z,xs = swap (k-1) x xs in
	z::xs
    | [] -> assert false

let swap_all k xs pss =
  let xs = swap k xs
  and pss = List.map (fun (ps,e) -> swap k ps,e) pss in
  xs,pss

(*************************)
(* To combine heuristics *)
(*************************)

let mapi f xs =
  let rec map_rec i = function
    | [] -> []
    | x::xs -> f i x::map_rec (i+1) xs in
  map_rec 0 xs

let select_mins ws =
  let ws =
    List.sort
      (fun (i1,w1) (i2,w2) ->
	let c = Pervasives.compare w1 w2 in
	if c=0 then Pervasives.compare i1 i2 else c)
      ws in
  match ws with
| [] -> assert false
| (i,w)::rem ->
    let rec do_rec = function
      | [] -> []
      | (ii,ww)::rem ->
	  if w=ww then ii::do_rec rem
	  else [] in
    i::do_rec rem


let (++) f g =
  fun ks -> match ks with
  | [k] -> k
  | _ ->
      match select_mins (f ks)  with
      | [] -> assert false
      | [k] -> k
      | ks -> g ks

let (<<) f g = g ++ f

let first ks = match ks with
| k::_ -> k
| [] -> assert false

  
(********************************)
(* Compute 'partial' directions *)
(********************************)

(* A few matrix manipulations *)

let extract_first_col pss =
  List.fold_right
    (fun ps (fst,rest)-> match ps with
    | p::ps -> p::fst,ps::rest
    | [] -> assert false)
    pss ([],[])

let rec conses ps qss = match ps,qss with
| [],[] -> []
| p::ps,qs::qss -> (p::qs)::conses ps qss
|  _ -> assert false

let rec appends pss qss = match pss,qss with
| [],[] -> []
| ps::pss,qs::qss -> (ps@qs)::appends pss qss
|  _ -> assert false


(* Variables in tuple/record/one constr types are not duplicated *)

let no_test ps =
  let ds = Discr.collect_ps ps in
  match DSet.cardinal ds with
  | 0 -> true
  | 1 -> not (Discr.has_default ds)
  | _ -> false

(* Does omega matches a pattern ? *)
let rec match_omega p = match p.pat_desc with
  | Tpat_any|Tpat_var _ -> true
  | Tpat_alias (p,_) -> match_omega p
  | Tpat_or (p1,_,_) -> match_omega p1
  | (Tpat_array _|Tpat_record _|Tpat_variant (_, _, _)
  | Tpat_construct (_, _)| Tpat_tuple _|Tpat_constant _)
      -> false

let direction col pss =
  let rec do_rec col qss pss = match col,pss with
  | [],[] -> 0
  | c::col, ps::pss ->
      if match_omega c && Parmatch.satisfiable qss ps then
	0
      else
	do_rec col (ps::qss) pss - 1
  | _ -> assert false in
  do_rec col [] pss


let directions xs pss ks =
  let rec do_rec i left right ks xs = match ks,xs with
    | [],_ -> []
    | k::rk,_::xs ->
	let ps,qss = extract_first_col right in
	if i < k then
	  do_rec (i+1) (conses ps left) qss ks xs
	else
	  let d =
	    if false && no_test ps then 0-List.length ps
	    else
	      direction ps (appends left right) in
	  (k,d)::do_rec (i+1) (conses ps left) qss rk xs
    | _ -> assert false in

  let ws =  do_rec 0 (List.map (fun _ -> []) pss) pss ks xs in
  
  if Matchcommon.verbose > 0 then begin
    prerr_string "DIRECTIONS: " ;
    List.iter (fun (k,w) -> Printf.eprintf "<%i,%i>" k w) ws ;
    prerr_endline ""
  end ;

  ws
      

(********************************************)
(* Minimize specialized/default matrix size *)
(********************************************)

(* size is approximated by the number of rows *)  

let rec nwilds_pat p = match p.pat_desc with
| Tpat_any | Tpat_var _ -> 1
| Tpat_alias (p,_) -> nwilds_pat p
| Tpat_or (p1,p2,_) -> nwilds_pat p1 + nwilds_pat p2
| Tpat_construct _ | Tpat_variant _ 
| Tpat_array _|Tpat_record _|Tpat_tuple _|Tpat_constant _ ->
  0

let rec nwilds ps =
  List.fold_left
    (fun r p -> r+nwilds_pat p)
    0 ps

let nmats ps =
  let ds = Discr.collect_ps ps in
  match DSet.cardinal ds with
  | 0 -> 1
  | n -> n + (if Discr.has_default ds then 1 else 0)

let ndups col = nwilds col * nmats col

let nrows xs pss ks =
  let rec do_rec  i right ks xs = match ks,xs with
    | [],_ -> []
    | k::rk,_::xs ->
	let ps,qss = extract_first_col right in
	if i < k then
	  do_rec (i+1) qss ks xs
	else
	  (k,ndups ps)::do_rec (i+1) qss rk xs
    | _ -> assert false in
  let ws =  do_rec 0 pss ks xs in

  if Matchcommon.verbose > 0 then begin
    prerr_string "NROWS: " ;
    List.iter (fun (k,w) -> Printf.eprintf "<%i,%i>" k w) ws ;
    prerr_endline ""
  end ;

  ws
  

(******************************************)
(* Select minimal variable, this enforces *)
(* irrelevance of previous column swaps   *)
(******************************************)

let vars xs ks =
  let rec do_rec i xs ks = match xs,ks with
  | _,[] -> []
  | x::xs,k::rk ->
      if i < k then
	do_rec (i+1) xs ks
      else
	(k,Ident.stamp x)::do_rec (i+1) xs rk
  | _ -> assert false in
  do_rec 0 xs ks
	


let best_column xs pss = match xs with
|[]|[_]  -> assert false
| _ ->
    let pss = List.map fst pss
    and ks = mapi (fun i _ -> i) xs in

    if Matchcommon.verbose > 0 then begin
    prerr_endline "** HEURISTICS **" ;
    Parmatch.pretty_matrix pss
    end ;

    let zyva =
      first << vars xs << nrows xs pss << directions xs pss in
    zyva ks

(*
    let css = Pat.collects pss in
    let wilds = eval_dups pss in
    let ncs = List.map (fun cs -> ConstrSet.cardinal cs) css in
    let ks =
      List.map2 (fun wild ncs ->
	let c = wild * ncs in
	c)
	wilds ncs in
    let ks = List.combine ds ks in
    let vs = List.map (fun x -> x.id_id) xs in
    let ks = List.map2 (fun (x,y) z -> (x,y,z)) ks vs in
    let ks = List.map2 (fun (x,y,t) z -> (x,y,z,t)) ks ncs in
    if verbose > 1 then begin
      fprintf stderr "*** HEURISTIC **\n" ;
      Pat.print_matrix stderr pss ;
      List.iter (fun (d,k,v,z) -> fprintf stderr " (%d,%d,%d,%d)" d k v z) ks ;
      prerr_newline ()
  end ;
  min ks
*)

let choose xs pss =  match xs with
|[]|[_] -> xs,pss
| _ ->
    let k = best_column xs pss in
    swap_all k xs pss

let opt =
  if Matchcommon.direction then
    choose
  else
    (fun xs pss -> xs,pss)

