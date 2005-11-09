(***********************************************************************)
(*                                                                     *)
(*                               G'Caml                                *)
(*                                                                     *)
(*                  Jun Furuse, University of Tokyo                    *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Types
open Btype
open Longident

                  (**********************************************)
                  (*              Konstraints                   *)
                  (**********************************************)

let type_variables tyl =
  let tvars = ref [] in
  let visited = ref [] in 
  let rec f ty =
    match ty.desc with
    | Tvar -> if not (List.memq ty !tvars) then tvars := ty :: !tvars
    | _ -> 
	(* iter_type_expr may loop infinitely, without the following check *)
	if not (List.memq ty !visited) then begin
	  visited := ty :: !visited;
	  iter_type_expr f ty
	end
  in
  List.iter f tyl;
  List.rev !tvars

let type_variables_of_konst_elem kelem =
  type_variables 
    (match kelem.kdepend with
    | None -> [kelem.ktype]
    | Some t -> [t; kelem.ktype])

type variable_path = type_expr * int list

(* Get type variables of a type expression and their positions 
   in the type's AST. *)
let rec varpath_of_type ty =
  let varpath = ref [] in
  let visited = ref [] in
  let rec f parent_path current_pos_ref ty =
    let ty = repr ty in
    (* iter_type_expr may loop without this visit check *)
    if List.memq ty !visited then ()
    else begin
      visited := ty :: !visited;
      let current_path = List.rev (!current_pos_ref :: parent_path) in
      incr current_pos_ref; (* increment for the next sub-node *)  
      match ty.desc with
      | Tvar -> varpath := (ty,current_path) :: !varpath
      | _ -> iter_type_expr (f current_path (ref 0)) ty
    end
  in
  f [] (ref 0) ty;
  !varpath
  
(* debugging function for varpath_of_type *)
let print_varpath ppf path =
  List.iter (fun p -> 
    Format.fprintf ppf "%d-" p) path;
  Format.fprintf ppf "."

let mem_cache t1 t2 cache = 
  try List.memq t2 !(List.assq t1 !cache) with Not_found -> false

let add_cache t1 t2 cache =
  try
    let l = List.assq t1 !cache in
    l := t2 :: !l
  with
  | Not_found -> cache := (t1, ref [t2]) :: !cache

let create_cache () = ref []

let rec normalize_type ty =
  let ty = repr ty in
  let cache = ref [] in

  let rec normalize_type ty =
    (* we assume the form of generic types: Tkonst and Toverload must 
       appear in the specific places: only at the head of type scheme *)
    let ty = repr ty in
    if List.memq ty !cache then ()
    else begin
      cache := ty :: !cache;
      match ty.desc with
      | Tkonst (k,t) -> ty.desc <- Tkonst (normalize_konstraint t k, t)
      | Toverload odesc -> 
	  (* odesc.over_aunif must be parametric type, 
	     therefore no need of normalization *) 
	  List.iter normalize_type odesc.over_cases
      | _ -> ()
    end

  and normalize_konstraint t k =
    (* kelems are sorted in the context of t *)
    List.iter normalize_konst_elem k; 
    let vp = varpath_of_type t in
    let sorted = 
      List.sort (fun ke1 ke2 -> 
	let c,fvcomp = compare_konst_elem (create_cache ()) vp vp ke1 ke2 in
	if c = 0 && fvcomp then begin
	  let print_ke ppf ke =
	    Format.fprintf ppf "{ktype=@[%a@];@ kdepend=@[%a@]}"
	      Gdebug.print_type_scheme ke.ktype
	      (fun ppf -> function 
		  | None -> Format.fprintf ppf "None"
		  | Some t -> Format.fprintf ppf "Some (@[%a@])" Gdebug.print_type_scheme t) ke.kdepend
	  in
	  Format.eprintf "@[@[%a@]@ <?> @[%a@]@]@." 
	    print_ke ke1
	    print_ke ke2;
	  raise (Failure "free variable comparison")
	end else c) k
    in
    let rec uniq = function 
      | [] -> []
      | [x] -> [x]
      | x::y::l -> 
	  if compare_konst_elem (create_cache ())
	      vp vp x y = (0, false) then uniq (y::l)
	  else x :: uniq (y::l)
    in
    uniq sorted

  and normalize_konst_elem ke =
    match (repr ke.ktype).desc with
    | Tkonst (_,_) | Toverload _ -> assert false
    | _ ->
	match ke.kdepend with
	| None -> ()
	| Some t -> normalize_type t
  in
  normalize_type ty

and compare_konst_elem cache vp1 vp2 ke1 ke2 =
    match ke1, ke2 with
    | {ktype=t1; kdepend=None}, {ktype=t2; kdepend=None} -> 
	compare_types cache vp1 vp2 t1 t2
    | {ktype=t1; kdepend=Some s1}, {ktype=t2; kdepend=Some s2} ->
	compare_type_lists cache vp1 vp2 [t1;s1] [t2;s2]
    | {kdepend=Some _}, {kdepend=None}   -> 1, false
    | {kdepend=None},   {kdepend=Some _} -> -1, false

and compare_types cache vp1 vp2 ty1 ty2 =
  if ty1 == ty2 then 0, false else 
  let ty1 = repr ty1 
  and ty2 = repr ty2 in
  let id_of_desc desc =
    let odesc = Obj.repr desc in
    if Obj.is_int odesc then - (Obj.obj odesc + 1)
    else Obj.tag odesc
  in
  let c = compare (id_of_desc ty1.desc) (id_of_desc ty2.desc) in
  if c <> 0 then c, false else
  match ty1.desc, ty2.desc with
  | Tvar, Tvar ->
      begin match (ty1.level = generic_level), (ty2.level = generic_level) with
      | true, true -> 
	  (* compare the paths *)
	  compare (List.assq ty1 vp1) (List.assq ty2 vp2), false
      | true, false -> 1, false
      | false, true -> -1, false
      | false, false -> 
	  (* We do not decide the ordering here... 
	     Just memorize the fact that free variables are compared. *)
	  0, true
      end
  | Tarrow (l1,t11,t12,c1), Tarrow (l2,t21,t22,c2) ->
      (* Zut!, we have no polymorphic recursion... *)
      compare_type_args cache vp1 vp2
	(Obj.repr (l1,c1),[t11;t12]) (Obj.repr (l2,c2),[t21;t22])
  | Ttuple typs1, Ttuple typs2 ->
      compare_type_lists cache vp1 vp2 typs1 typs2
  | Tconstr (p1,typs1,_), Tconstr (p2,typs2,_) ->
      (* Zut!, we have no polymorphic recursion... *)
      compare_type_args cache vp1 vp2 (Obj.repr p1,typs1) (Obj.repr p2,typs2)

  | Tobject (_,_), Tobject (_,_)
  | Tfield (_,_,_,_), Tfield (_,_,_,_)
  | Tvariant _, Tvariant _ 
  | Tnil, Tnil
  | Tunivar, Tunivar
  | Tpoly (_,_), Tpoly (_,_) -> raise (Failure "compare_types: not supported") 

  | Tlink _, Tlink _ 
  | Tsubst _, Tsubst _ 
  | Tpath _, Tpath _ -> assert false

  | Tkonst (k1,t1), Tkonst (k2,t2) ->
      let c, fvcomp = compare_types cache vp1 vp2 t1 t2 in
      if c <> 0 then c, fvcomp else
      let c, fvcomp' = compare_konstraint cache vp1 vp2 k1 k2 in
      c, fvcomp || fvcomp'

  | Toverload odesc1, Toverload odesc2 ->
      if mem_cache ty1 ty2 cache then 0,false (* really? *) 
      else begin
	add_cache ty1 ty2 cache;
	let c = 
	  compare 
	    (List.length odesc1.over_cases) (List.length odesc2.over_cases) 
	in
	if c <> 0 then c, false else 
	compare_type_lists cache vp1 vp2 odesc1.over_cases odesc2.over_cases
      end
  | _ -> assert false

(* Zut!, we have no polymorphic recursion... *)
and compare_type_args cache vp1 vp2 (nt1,typs1) (nt2,typs2) =
  let c = compare nt1 nt2 in
  if c <> 0 then c, false else
  compare_type_lists cache vp1 vp2 typs1 typs2

and compare_type_lists cache vp1 vp2 typs1 typs2 =
  match typs1, typs2 with
  | [], [] -> 0, false
  | [], _ | _, [] -> assert false
  | ty1::typs1, ty2::typs2 ->
      let c, fvcomp = compare_types cache vp1 vp2 ty1 ty2 in
      if c <> 0 then c, fvcomp else
      let c, fvcomp' = compare_type_lists cache vp1 vp2 typs1 typs2 in
      c, fvcomp || fvcomp'

and compare_konstraint cache vp1 vp2 k1 k2 =
  (* sort konstraints! *)
  let c = compare (List.length k1) (List.length k2) in
  if c <> 0 then c, false else
  let rec compare_konst_elems k1 k2 = 
    match k1, k2 with
    | [], [] -> 0, false
    | [], _ | _, [] -> assert false
    | ke1::k1, ke2::k2 ->
	let c, fvcomp = compare_konst_elem cache vp1 vp2 ke1 ke2 in
	if c <> 0 then c, fvcomp else
	let c, fvcomp' = compare_konst_elems k1 k2 in
	c, fvcomp || fvcomp'
  in
  compare_konst_elems k1 k2

let compare_types t1 t2 =
(*
  Format.eprintf "@[<2>@[%a@]@ <?> @[%a@]@]@." 
    Gdebug.print_type_scheme t1
    Gdebug.print_type_scheme t2;
*)
  normalize_type t1;
  normalize_type t2;
  let vp1 = varpath_of_type t1 
  and vp2 = varpath_of_type t2 in
  let c, fvcomp = compare_types (create_cache ()) vp1 vp2 t1 t2 in
(*
  Format.eprintf "@[@[%a@]@ <?> @[%a@]@] = %d@." 
    Gdebug.print_type_scheme t1
    Gdebug.print_type_scheme t2
    c;
*)
  if c = 0 && fvcomp then begin
    Format.eprintf "@[@[%a@]@ <?> @[%a@]@] = %d@." 
      Gdebug.print_type_scheme t1
      Gdebug.print_type_scheme t2
      c;
    raise (Failure "free variable comparison")
  end else c

