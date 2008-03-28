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

let conses ps qss = List.map2 (fun x xs -> x::xs) ps qss
and appends pss qss = List.map2 (@) pss qss

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

let direction k col pss =
  let rec do_rec n col qss pss = match col,pss with
  | [],[] -> 0
  | c::col, ps::pss ->
      if match_omega c then begin
(*
	Printf.eprintf "** QSS **\n" ;
	Parmatch.pretty_matrix qss ;
	Printf.eprintf "** PS **\n" ;
	Parmatch.pretty_line ps ;
	prerr_newline () ;
*)
	if Parmatch.satisfiable qss ps then 0
	else begin
	  if Matchcommon.verbose > 0 then begin
	    Printf.eprintf "** Non obvious direction: col=%i, row=%i\n"  k n
	  end ;
	  do_rec (n+1) col (ps::qss) pss - 1
	end
      end else 
	do_rec (n+1) col (ps::qss) pss - 1
  | _ -> assert false in
  do_rec 0 col [] pss


let directions xs pss ks =
  let rec do_rec i left right ks xs = match ks,xs with
    | [],_ -> []
    | k::rk,_::xs ->
	let col,right = extract_first_col right in
	if i < k then
	  do_rec (i+1) (conses col left) right ks xs
	else
	  let d = direction k col (appends left right) in
	  (k,d)::do_rec (i+1) (conses col left) right rk xs
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

(*****************************)
(* Minimize branching factor *)  
(*****************************)

let nbranchs xs pss ks =
  let rec do_rec  i right ks xs = match ks,xs with
    | [],_ -> []
    | k::rk,_::xs ->
	let ps,qss = extract_first_col right in
	if i < k then
	  do_rec (i+1) qss ks xs
	else
	  (k,-nmats ps)::do_rec (i+1) qss rk xs
    | _ -> assert false in
  let ws =  do_rec 0 pss ks xs in

  if Matchcommon.verbose > 0 then begin
    prerr_string "NBRANCH: " ;
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
    and xs = List.map (fun (_,x,_) -> x) xs
    and ks = mapi (fun i _ -> i) xs in

    if Matchcommon.verbose > 0 then begin
    prerr_endline "** HEURISTICS **" ;
    Parmatch.pretty_matrix pss
    end ;

    let zyva =
      first << vars xs << nbranchs xs pss
	<< nrows xs pss << directions xs pss in
    zyva ks

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

