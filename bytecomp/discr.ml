open Asttypes
open Primitive
open Types
open Typedtree
open Lambda
open Parmatch
open Matchcommon

(********************************)
(* Target language for automata *)
(********************************)

let alias x y lam = bind Alias x (Lvar y) lam


(******************)
(* Discriminators *)
(******************)

type discr = 
  | Constant of constant
  | Construct of constructor_description
  | Tuple of int
  | Record of label_description array
  | Variant of label * bool * row_desc ref
  | Array of array_kind * int

let p_discr chan = function
  | Variant (lab,_,_) -> Printf.fprintf chan "`%s" lab
  | _ -> ()

let compare d1 d2 = match d1,d2 with
| Constant c1,Constant c2 -> compare c1 c2
| Construct c1,Construct c2 -> compare c1.cstr_tag c2.cstr_tag
| Tuple _,Tuple _ -> 0
| Record _,Record _ -> 0
| Variant (lab1,_,_), Variant (lab2,_,_) -> compare lab1 lab2
| Array (_,x1),Array (_,x2) -> compare x1 x2
| _,_ -> assert false

module DSet = Set.Make
    (struct
      type t = discr
      let compare = compare
    end)

let collect_pat p = match p.pat_desc with
| Tpat_any -> DSet.empty
| Tpat_constant c -> DSet.singleton (Constant c)
| Tpat_tuple ps -> DSet.singleton (Tuple (List.length ps))
| Tpat_construct (c,_) -> DSet.singleton (Construct c)
| Tpat_variant (lab,op,row_r) ->
    let row = Btype.row_repr !row_r in
    if try Btype.row_field_repr (List.assoc lab row.row_fields) = Rabsent
    with Not_found -> true then
      DSet.empty
    else
      let has_arg = match op with Some _ -> true | None -> false in
      DSet.singleton (Variant (lab,has_arg,row_r))
| Tpat_array ps ->
    DSet.singleton
      (Array
	 (Typeopt.array_pattern_kind p,
	  List.length ps))
| Tpat_record lbls ->
    let lbl_all = match lbls with
    | (lbl,_)::_ -> lbl.lbl_all | [] -> assert false in
    DSet.singleton (Record lbl_all)
| Tpat_var _|Tpat_alias (_,_)|Tpat_or (_,_,_) -> assert false

let collect pss =
  List.fold_left
    (fun r (ps,_) ->
      match ps with
      | p::_ -> DSet.union (collect_pat p) r
      | [] -> r)
    DSet.empty pss

(* Default matrix *)

let default_ps (ps,act) k = match ps with
| [] -> assert false
| p::ps -> begin match p.pat_desc with
  | Tpat_any -> (ps,act)::k
  | _ -> k
end

let default pss = List.fold_right default_ps pss []

(* Generalized specialization *)
let specialize_constant c (ps,e) k = match ps with
| {pat_desc=Tpat_constant c2}::ps->
    if c=c2 then (ps,e)::k
    else k
| {pat_desc=Tpat_any}::ps -> (ps,e)::k
| _ -> assert false

and specialize_construct tag arity (ps,e) k = match ps with
| {pat_desc=Tpat_construct (c2,qs)}::ps ->
    if c2.cstr_tag = tag then (qs@ps,e)::k
    else k
| {pat_desc=Tpat_any}::ps -> (omegas arity@ps,e)::k
| _ -> assert false

and specialize_variant lbl has_arg (ps,e) k = match ps with
| {pat_desc=Tpat_variant (lbl2,op,_)}::ps->
    if lbl=lbl2 then match op with
    | Some q -> (q::ps,e)::k
    | None -> (ps,e)::k
    else k
| {pat_desc=Tpat_any}::ps ->
    if has_arg then (omega::ps,e)::k
    else (ps,e)::k
| _ -> assert false
      
and specialize_tuple arity (ps,e) k = match ps with
| {pat_desc=Tpat_tuple qs}::ps -> (qs@ps,e)::k
| {pat_desc=Tpat_any}::ps -> (omegas arity@ps,e)::k
| _ -> assert false


and specialize_record arity (ps,e) k = match ps with
| {pat_desc=Tpat_record lbls}::ps ->
    let ps = List.fold_right (fun (_,p) ps -> p::ps) lbls ps in
    (ps,e)::k
| {pat_desc=Tpat_any}::ps -> (omegas arity @ ps,e)::k
| _ -> assert false

and specialize_array arity (ps,e) k = match ps with
| {pat_desc=Tpat_array qs}::ps ->
    if List.length qs = arity then (qs@ps,e)::k
    else k
| {pat_desc=Tpat_any}::ps -> (omegas arity @ ps,e)::k
| _ -> assert false

let specialize_ps d = match d with
| Constant c -> specialize_constant c
| Construct c -> specialize_construct c.cstr_tag c.cstr_arity
| Variant (lbl,has_arg,_) -> specialize_variant lbl has_arg
| Tuple a -> specialize_tuple a
| Record lbl_all -> specialize_record (Array.length lbl_all)
| Array (_,a) -> specialize_array a

let specialize d pss = 
  let r = List.fold_right (specialize_ps d) pss [] in
(*
  Printf.eprintf "SPECIALIZE %a\n" p_discr d ;
  pretty_match pss ;
  prerr_endline "IS" ;
  pretty_match r ;
  prerr_endline "" ;
*)
  r


(* Extract data fields *)
let mk_fids mk_lam first_pos last_pos =
  let rec mk_rec pos =
    if pos > last_pos then [],[]
    else
      let ys,lams = mk_rec (pos+1) in
      let y = Ident.create "f" in
      y::ys,mk_lam pos::lams in
  mk_rec first_pos
      
let fids_constant _x = [],[]

and fids_block arity x =
  mk_fids
    (fun pos -> Alias,Lprim(Pfield pos, [Lvar x]))
    0 (arity-1)

let fids_construct tag arity = match tag with
| Cstr_constant _ -> fids_constant
| Cstr_block _ -> fids_block arity
| Cstr_exception _ ->
    (fun x ->
      mk_fids
	(fun pos -> Alias,Lprim(Pfield pos, [Lvar x]))
	1 arity)

and fids_variant has_arg = 
  if has_arg then
    (fun x ->
      mk_fids
	(fun pos -> Alias,Lprim(Pfield pos, [Lvar x]))
	1 1)
  else
    fids_constant

and fids_tuple = fids_block

and fids_record lbl_all x =
  mk_fids
    (fun pos ->
      let lbl = lbl_all.(pos) in
      let access = match lbl.lbl_repres with
      | Record_regular -> Pfield pos
      | Record_float -> Pfloatfield pos
      and str = match lbl.lbl_mut with
      | Immutable -> Alias
      | Mutable -> StrictOpt in
      str,Lprim (access,[Lvar x]))
    0 (Array.length lbl_all-1)

and fids_array kind sz x =
  mk_fids
    (fun pos ->
      StrictOpt,
      Lprim (Parrayrefu kind,[Lvar x;Lconst(Const_base(Const_int pos))]))
    0 (sz-1)
      

let field_ids d = match d with
| Constant c -> fids_constant
| Construct c -> fids_construct c.cstr_tag c.cstr_arity
| Variant (lbl,has_arg,_) -> fids_variant has_arg
| Tuple a -> fids_tuple a
| Record lbl_all -> fids_record lbl_all
| Array (kind,a) -> fids_array kind a

(* Put let's for fields *)
 let get_fields =
   let rec get_rec ys es k = match ys,es with
   | [],[] -> k
   | y::ys,(str,e)::es -> Llet (str, y, e, get_rec ys es k)
    | _ -> assert false in
    get_rec

(* Need to add a default case ? *)

let has_default ds =
  DSet.is_empty ds ||
  (let d = DSet.choose ds in
  match d with
  | Constant (Const_char _) -> DSet.cardinal ds < 256
  | Constant _ -> true
  | Construct c ->
      c.cstr_consts < 0 || (* exceptions always have default *)
      DSet.cardinal ds < c.cstr_consts + c.cstr_nonconsts
  | Variant (_,_,row) ->
      let nconstrs = count_variants !row in
      let present = DSet.cardinal ds in
(*      Printf.eprintf "nconstrs=%i, present=%i\n" nconstrs present ; *)
      present < nconstrs
  | Tuple _|Record _ -> false
  | Array (_,_) -> true)

(* Switch, on peut dire que c'est une horreur *)

let sw_constant c x cls fail =
  Matchcommon.switch_constant c (Lvar x)
    (List.map
       (function (Constant c,lam) -> c,lam | _ -> assert false)
       cls)
    fail

let no_switch cls = match cls with
| [_,act] -> act
| _ -> assert false
  
let switch x cls fail = match cls, fail with
| [],Some lam -> lam
| [],None -> assert false
| (d,_)::_,_ ->
    match d with
    | Constant c -> sw_constant c x cls fail
    | Construct c ->
	let cls =
	  List.map
	    (function (Construct c,lam) -> c.cstr_tag,lam | _ -> assert false)
	    cls in
	begin match c.cstr_tag with
	| Cstr_exception _ ->
	    switch_exn (Lvar x) cls fail
	| _ ->
	    switch_constr c (Lvar x) cls fail
	end
    | Variant (_,_,_) ->
	let cls =
	  List.map
	    (function
	      | (Variant (lab,true,_),act) ->
		  Cstr_block (Btype.hash_variant lab),act
	      | (Variant (lab,false,_),act) ->
		  Cstr_constant (Btype.hash_variant lab),act
	      | _ -> assert false)
	    cls in
	switch_variant (Lvar x) cls fail
    | Tuple _|Record _ -> no_switch cls
    | Array (kind,_) ->
	let cls =
	  List.map
	    (function
	      | (Array (_,n),act) -> n,act
	      | _ -> assert false)
	    cls in
	switch_array kind (Lvar x) cls fail
