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
open Printf

type occ = int list

let print_occ =
  let rec p_rec chan = function
    | [] -> ()
    | [x]  -> fprintf chan "%i" x
    | x::xs  -> fprintf chan "%i.%a" x p_rec xs in
  fun chan xs ->
    fprintf chan "<%a>" p_rec (List.rev xs)


(*************************************)
(* put column no k in first position *)
(*************************************)

let rec swap k x = function
  | [] -> assert false
  | y::ys ->
      if k = 1 then y,x::ys
      else
	let z,ys = swap (k-1) x ys in
	z,y::ys

let swap k xs = match k with
| 1 -> xs
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

(**************************************)
(* Bricks to build/combine heuristics *)
(**************************************)

let mapi f xs =
  let rec map_rec i = function
    | [] -> []
    | x::xs -> f i x::map_rec (i+1) xs in
  map_rec 0 xs



let get_mins name ws =
  if Matchcommon.verbose > 0 then begin
    eprintf "%s: " name ;
    List.iter (fun (k,w) -> eprintf "<%i,%i>" k w) ws
  end ;
  let ws =
    List.sort
      (fun (i1,w1) (i2,w2) ->
	let c = Pervasives.compare w1 w2 in
	if c=0 then Pervasives.compare i1 i2 else c)
      ws in
  let ks =
    match ws with
    | [] -> assert false
    | (i,w)::rem ->
	let rec do_rec = function
	  | [] -> []
	  | (ii,ww)::rem ->
	      if w=ww then ii::do_rec rem
	      else [] in
	i::do_rec rem in
  if Matchcommon.verbose > 0 then begin
    eprintf " -> " ;
    List.iter (fun k -> eprintf "<%i>" k) ks ;
    prerr_newline ()
  end ;
  ks

let compose h1 h2 ks = match h1 ks with
| [] -> assert false
| [k] -> k
| ks -> h2 ks


(* Generic heuristic that look at one column only *)

let extract_first_col pss =
  List.fold_right
    (fun ps (fst,rest)-> match ps with
    | p::ps -> p::fst,ps::rest
    | [] -> assert false)
    pss ([],[])

let conses ps qss = List.map2 (fun x xs -> x::xs) ps qss
and appends pss qss = List.map2 (@) pss qss

let generic name score = 
  let rec do_rec  i right ks xs = match ks,xs with
    | [],_ -> []
    | k::rk,_::xs ->
	let ps,qss = extract_first_col right in
	if i < k then
	  do_rec (i+1) qss ks xs
	else
	  (k,score ps)::do_rec (i+1) qss rk xs
    | _ -> assert false in
  fun xs pss ks ->
    let ws =  do_rec 1 pss ks xs in
    get_mins name ws



(********************************)
(* Compute 'partial' directions *)
(********************************)


let generic2 name score =
  let rec do_rec i left right ks xs = match ks,xs with
    | [],_ -> []
    | k::rk,_::xs ->
	let col,right = extract_first_col right in
	if i < k then
	  do_rec (i+1) (conses col left) right ks xs
	else
	  let d = score col (appends left right) in
	  (k,d)::do_rec (i+1) (conses col left) right rk xs
    | _ -> assert false in
  fun xs pss ks ->
    let ws =  do_rec 1 (List.map (fun _ -> []) pss) pss ks xs in
    get_mins name ws


(* Does omega matches a pattern ? *)
let rec match_omega p = match p.pat_desc with
  | Tpat_any|Tpat_var _ -> true
  | Tpat_alias (p,_) -> match_omega p
  | Tpat_or (p1,_,_) -> match_omega p1
  | (Tpat_array _|Tpat_record _|Tpat_variant (_, _, _)
  | Tpat_construct (_, _)| Tpat_tuple _|Tpat_constant _)
      -> false

let direction col pss =
  let rec do_rec n col qss pss = match col,pss with
  | [],[] -> 0
  | c::col, ps::pss ->
      if match_omega c then begin
	if Parmatch.satisfiable qss ps then 0
	else begin
	  do_rec (n+1) col (ps::qss) pss - 1
	end
      end else 
	do_rec (n+1) col (ps::qss) pss - 1
  | _ -> assert false in
  do_rec 0 col [] pss


let directions = generic2 "DIRECTIONS" direction
      
(************************)
(* Counted needed lines *)
(************************)

let needed_one col pss =
  let rec do_rec n col qss pss = match col,pss with
  | [],[] -> 0
  | c::col, ps::pss ->
      if match_omega c then begin
	if Parmatch.satisfiable qss ps then
	  do_rec (n+1) col (ps::qss) pss
	else begin
	  do_rec (n+1) col (ps::qss) pss - 1
	end
      end else 
	do_rec (n+1) col (ps::qss) pss - 1
  | _ -> assert false in
  do_rec 0 col [] pss

let needed = generic2 "NEEDED" needed_one


(********************************************)
(* Minimize specialized/default matrix size *)
(********************************************)

(* size is approximated by the number of rows *)  

let rec nwilds_pat p = match p.pat_desc with
| Tpat_any | Tpat_var _ -> 1
| Tpat_alias (p,_) -> nwilds_pat p
| Tpat_or (p1,p2,_) -> nwilds_pat p1 + nwilds_pat p2
| Tpat_construct _ | Tpat_variant _ 
| Tpat_array _|Tpat_record _|Tpat_tuple _|Tpat_constant _ -> 0

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

let rec dupconstr_pat p = match p.pat_desc with
  | Tpat_any|Tpat_var _ -> 0
  | Tpat_alias (p,_) -> dupconstr_pat p
  | Tpat_or (p1,p2,_) -> dupconstr_pat p1 + dupconstr_pat p2
  | (Tpat_array _|Tpat_record _|Tpat_variant (_, _, _)
  | Tpat_construct (_, _)| Tpat_tuple _|Tpat_constant _)
      -> 1

let dupconstr col =
  List.fold_left
    (fun r p -> r + dupconstr_pat p)
    0 col

      
let nrows = generic "NROWS" (fun ps -> ndups ps+dupconstr ps)


let ndefaults = generic "DEFAULTS" nwilds


let nbranchs = generic "NBRANCH" nmats


let is_constr p = not (match_omega p)

let first =
  generic "FIRST"
    (fun ps -> match ps with
    | p::_ -> if is_constr p then -1 else 0
    | [] -> assert false)


let arity ps =
  let ds = Discr.collect_ps ps in
  DSet.fold
    (fun c r -> r + Discr.arity c)
    ds 0

let arities = generic "ARITIES" arity

let rec prefix_one ps = match ps with
| p::ps when is_constr p -> 1 + prefix_one ps
| _ -> 0

let prefix =
  generic "PREFIX"
    (fun ps -> -prefix_one ps)

(******************************************)
(* Select minimal variable, this enforces *)
(* irrelevance of previous column swaps   *)
(******************************************)

let as_some = function
  | Some (k,_) -> k
  | None -> assert false

let cmp_opt cmp oo k2 o2 = match oo with
| Some (k1,o1) when cmp o1 o2 -> oo
| _ -> Some (k2,o2)

let min_occs cmp  =
  let rec do_rec min i os ks = match ks,os with
  | [],_ -> as_some min
  | k::rk,o::os ->
      if i < k then
	do_rec min (i+1) os ks
      else
	do_rec (cmp_opt cmp min k o) (i+1) os rk
  | _,_ -> assert false in
  fun os ks -> do_rec None 1 os ks


let cmp_rev cmp o1 o2 = cmp (List.rev o1) (List.rev o2)

let rec cmp_left o1 o2 = match o1,o2 with
| i1::o1,i2::o2 ->
    if i1 < i2 then true
    else if i2 < i1 then false
    else cmp_left o1 o2
| _,_ -> assert false

let cmp_lr o1 o2 = 
  let l1 = List.length o1 and l2 = List.length o2 in
  if l1 < l2 then true
  else if l1 > l2 then false
  else cmp_rev cmp_left o1 o2

let cmp_rl o1 o2 = 
  let l1 = List.length o1 and l2 = List.length o2 in
  if l1 < l2 then true
  else if l1 > l2 then false
  else cmp_rev cmp_left o2 o1

let left = min_occs (cmp_rev cmp_left)
let leftright =  min_occs cmp_lr
let rightleft = min_occs cmp_rl

(*************************)
(* Heuristic definitions *)
(*************************)

let exists_constr ps = List.exists is_constr ps

let withconstr xs pss =
  let rec do_rec  k right xs = match xs with
    | [] -> []
    | _::xs ->
	let ps,qss = extract_first_col right in
	if exists_constr ps then
	  k::do_rec (k+1) qss xs
	else
	  do_rec (k+1) qss xs in
  let ws =  do_rec 1 pss  xs in
  ws

module C = Matchcommon

let build h xs mat =
  let rec do_rec i =
    if i >= String.length h then
      left xs
    else
      let h2 = do_rec (i+1)
      and h1 = match h.[i] with
      | 'a' -> arities xs mat
      | 'b' -> nbranchs xs mat
      | 'd' -> ndefaults xs mat
      | 'f' -> first xs mat
      | 'p' -> directions xs mat
      | 'q' -> prefix xs mat
      | 'n' -> needed xs mat
      | 'r' -> nrows xs mat
      | 'L' -> (fun ks -> [left xs ks])
      | 'i' -> (fun x -> x)
      | c -> Misc.fatal_error (sprintf "Bad heuristic: %c" c) in
      compose h1 h2 in
  do_rec 0


let best_column xs pss = match xs with
|[]|[_]  -> assert false
| _ ->
    let pss = List.map fst pss
    and xs = List.map (fun (_,_,_,occ) -> occ) xs in
    let ks = withconstr xs pss in
    match ks with
    | [] -> 1
    | [k] -> k
    | _ ->
	if C.verbose > 0 then begin
	  prerr_endline "** HEURISTICS **" ;
	  List.iter
	    (fun occ ->  eprintf " %a" print_occ  occ)
	    xs ;
	  prerr_newline () ;
	  Parmatch.pretty_matrix pss
	end ;
	let k =
	  match C.heuristic with
	  | C.No -> left xs ks
	  | C.Opt -> build "qba" xs pss ks
	  | C.Custom h -> build h xs pss ks in
	if C.verbose > 0 then begin
	  eprintf "Selected: %i\n" k ;
	  flush stderr
	end ;
	k

let opt xs pss =  match xs with
|[]|[_] -> xs,pss
| _ ->
    let k = best_column xs pss in
    swap_all k xs pss
