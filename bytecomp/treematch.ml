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

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Lambda
open Parmatch
open Printf
open Matchcommon

(************************************************)
(* Get rid of variables, aliases and or-pattern *)
(************************************************)

(* Since the various matching algorithms may violate the invariant
   of unique let-bound names, refresh them all *)

let refresh vars lam =

  let rec do_pairs do_rec seen env = function
    | [] -> seen,[]
    | (x,e)::rem ->
	let seen,e = do_rec seen env e in
	let seen,rem = do_pairs do_rec seen env rem in
	seen,(x,e)::rem in


  let do_bd seen env v =
    if IdentSet.mem v seen then begin
	let w = Ident.rename v in
(*
	Printf.eprintf "RENAME: %s -> %s\n"
	  (Ident.unique_name v) (Ident.unique_name w) ;
*)
	seen,Ident.add v w env,w
    end else
      IdentSet.add v seen,env,v in

  let rec do_bds seen env = function
    | [] -> seen,env,[]
    | x::xs ->
	let seen,env,x = do_bd seen env x in
	let seen,env,xs = do_bds seen env xs in
	seen,env,x::xs in

  let do_var env v =
    try Ident.find_same v env
    with Not_found -> v in

	  
  let rec do_rec seen env e = match e with
  | Lvar v -> seen,Lvar (do_var env v)
  | Lconst _ -> seen,e
  | Lapply (f,es,loc) ->
      let seen,f = do_rec seen env f in
      let seen,es = do_args seen env es in
      seen,Lapply (f,es,loc)
  | Lfunction (k,xs,e) ->
      let seen,env,xs = do_bds seen env xs in
      do1 seen env e (fun e ->  Lfunction (k,xs,e))
  | Llet (str,v,ev,e) ->
      let seen,ev = do_rec seen env ev in
      let seen,env,v = do_bd seen env v in
      do1 seen env e (fun e -> Llet (str,v,ev,e))
  | Lletrec(xes,e) ->
      let xs,es = List.split xes in
      let seen,env,xs = do_bds seen env xs in
      let seen,es = do_args seen env es in
      let seen,e = do_rec seen env e in
      seen,Lletrec (List.combine xs es,e)
  | Lprim (p,es) ->
      let seen,es = do_args seen env es in
      seen,Lprim (p,es)
  | Lswitch (e,sw) ->
      let seen,e = do_rec seen env e in
      let seen,consts = do_pairs do_rec seen env sw.sw_consts in
      let seen,blocks = do_pairs do_rec seen env sw.sw_blocks in
      let seen,failaction = match sw.sw_failaction with
      | None -> seen,None
      | Some e ->
	  let seen,e = do_rec seen env e in
	  seen,Some e in
      seen,Lswitch
	(e,
	 {sw with
	  sw_consts=consts ; sw_blocks=blocks; sw_failaction=failaction; })
  | Lstaticraise (i,es) ->
      let seen,es = do_args seen env es in
      seen,Lstaticraise (i,es)
  | Lstaticcatch(e1, (i,xs), e2) ->
      let seen,e1 = do_rec seen env e1 in
      let seen,env,xs = do_bds seen env xs in
      let seen,e2 = do_rec seen env e2 in
      seen,Lstaticcatch(e1, (i,xs), e2)
  | Ltrywith(e1, v, e2) ->
      let seen,e1 = do_rec seen env e1 in
      let seen,env,v = do_bd seen env v in
      let seen,e2 = do_rec seen env e2 in
      seen,Ltrywith(e1, v, e2)
  | Lifthenelse(e1, e2, e3) ->
      let seen,e1 = do_rec seen env e1 in
      let seen,e2 = do_rec seen env e2 in
      let seen,e3 = do_rec seen env e3 in
      seen,Lifthenelse(e1, e2, e3)
  | Lsequence(e1, e2) ->
      do2 seen env e1 e2 (fun e1 e2 ->  Lsequence(e1, e2))
  | Lwhile(e1, e2) ->
      do2 seen env e1 e2 (fun e1 e2 ->  Lwhile(e1, e2))
  | Lfor(v, e1, e2, dir, e3) ->
      let seen,e1 = do_rec seen env e1 in
      let seen,e2 = do_rec seen env e2 in
      let seen,env,v = do_bd seen env v in
      let seen,e3 = do_rec seen env e3 in
      seen, Lfor(v, e1, e2, dir, e3)
  | Lassign(v, e) ->
      let v = do_var env v in
      do1 seen env e (fun e -> Lassign(v, e)) 
  | Lsend(k, m, o, el) ->
      let seen,m = do_rec seen env m in
      let seen,o = do_rec seen env o in
      let seen,el = do_args seen env el in
      seen, Lsend(k, m, o, el)
  | Levent(l, ev) ->
      do1 seen env l (fun l ->  Levent(l, ev))
  | Lifused(v, e) ->
      let v = do_var env v in
      do1 seen env e (fun e ->  Lifused(v, e))

  and do1 seen env e k =
    let seen,e = do_rec seen env e in
    seen,k e

  and do2 seen env e1 e2 k =
    let seen,e1 = do_rec seen env e1 in
    let seen,e2 = do_rec seen env e2 in
    seen, k e1 e2

  and do_args seen env = function
    | [] -> seen,[]
    | e::es ->
	let seen,e = do_rec seen env e in
	let seen,es = do_args seen env es in
	seen,e::es in

  let _,lam = do_rec vars Ident.empty lam in lam

(*********************************)
(* Target of the match compiler  *)
(*********************************)


module type Out = sig
  type out
  (* forth and back *)
  val final : lambda -> out
  val to_lambda : out -> lambda

  (* need that for guards *)
  val is_guarded : out -> bool
  val patch_guarded : out -> out -> out
  val event_branch : int ref option -> out -> out

  (* constructors *)
  val alias : Ident.t -> Ident.t -> out -> out
  val field_ids : Discr.discr -> Ident.t -> (let_kind * Ident.t * lambda) list
  val bind : let_kind -> Ident.t -> lambda -> out -> out
  val switch : Ident.t -> (Discr.discr * out) list -> out option -> out
end


(* Lambda version of Ot *)
module OutLambda = struct
  type out = lambda
  let final lam = lam
  let to_lambda lam = lam

  let is_guarded = is_guarded
  let patch_guarded = patch_guarded
  let event_branch = event_branch

  let alias = Discr.alias
  let field_ids = Discr.field_ids (fun _lam ->  Ident.create "f")
  let bind = Lambda.bind
  let switch = Discr.switch
end


(**************************************************)
(* Match compiler parametrized by output language *)
(**************************************************)


module Make (Out : Out) = struct


(* Variables, aliases and or-pattern
   are handled by preprocessing,
   Records are also normalized *)

let precompile x cls =
  
  let rec simplify = function
    | [] -> []
    | ((pat :: patl, action) as cl) :: rem ->
        begin match pat.pat_desc with
        | Tpat_var id ->
            (omega :: patl, Out.alias id x action) ::
            simplify rem
        | Tpat_any ->
            cl :: simplify rem
        | Tpat_alias(p, id) ->
            simplify ((p :: patl, Out.alias id x action) :: rem)
        | Tpat_record [] ->
            (omega :: patl, action)::
            simplify rem
        | Tpat_record lbls ->
            let all_lbls = all_record_args lbls in
            let full_pat = {pat with pat_desc=Tpat_record all_lbls} in
            (full_pat::patl,action)::
            simplify rem
        | Tpat_or (p1,p2,_) -> (* or expansion *)
            simplify ((p1::patl,action)::(p2::patl,action) :: rem)
        | _ -> cl :: simplify rem
        end
    | _ -> assert false in

  simplify cls

(* Spot variable pattern *)
let rec is_var_pat p = match p.pat_desc with
| Tpat_any|Tpat_var _ -> true
| Tpat_alias (p,_) -> is_var_pat p
| Tpat_or (p1,p2,_) -> is_var_pat p1 && is_var_pat p2
| _ -> false

let all_vars ps = List.for_all is_var_pat ps

let rec compile_match repr xs pss lam_fail = match pss with
| [] -> lam_fail
| (ps,act)::pss when all_vars ps ->
    compile_row repr xs ps act xs pss lam_fail
| _ ->
    let xs,pss = Heuristic.opt xs pss in
    if Matchcommon.verbose > 1 then begin
      prerr_endline "** MATCH **" ;
      Parmatch.pretty_match pss ;
    end ;
    begin  match xs with
    | (str,x,ex)::xs ->
	Out.bind str x ex
	  (do_compile_matching repr x xs pss lam_fail)
    | [] -> assert false
    end

and compile_row repr xs ps act ys pss lam_fail = match xs,ps with
| [],[] ->
    if Out.is_guarded act then
      let lam = compile_match None ys pss lam_fail in
      Out.event_branch repr (Out.patch_guarded lam act)
    else
      Out.event_branch repr act
| (str,x,ex)::xs,p::ps ->
    begin match p.pat_desc with
    | Tpat_any ->
	let lam = compile_row repr xs ps act ys pss lam_fail in
	begin match str with
	| Strict -> Out.bind str x ex lam
	| Variable|StrictOpt|Alias -> lam
	end
    | _ ->
	let rec do_rec p act = match p.pat_desc with
	| Tpat_any ->
	     compile_row repr xs ps act ys pss lam_fail
	| Tpat_var id ->
	    compile_row repr xs ps (Out.alias id x act) ys pss lam_fail
	| Tpat_alias (p,id) ->
	    do_rec p (Out.alias id x act)
	| Tpat_or (p,_,_) ->
	    do_rec p act
	| _ -> assert false in
	Out.bind str x ex (do_rec p act)
    end
| _,_ -> assert false


and do_compile_matching repr x xs pss lam_fail =
  let pss = precompile x pss in
  let ds = Discr.collect pss in
(*
  prerr_endline "MATCH" ;
  pretty_match pss ;
  prerr_endline "" ;
*)
  let cls =
    Discr.DSet.fold
      (fun d k ->
	let pss = Discr.specialize d pss in
	let ys = Out.field_ids d x in
	let lam = compile_match repr (ys@xs) pss lam_fail in
	(d,lam)::k)
      ds [] in
  Out.switch x cls
    (if Discr.has_default ds then
      Some (compile_match repr xs (Discr.default pss) lam_fail)
    else
      None)
;;


(* Free variables in a pattern *)
let vars_pat p = IdentSet.elements (extract_vars IdentSet.empty p)

and vars_pats ps =
  IdentSet.elements
    (List.fold_left extract_vars IdentSet.empty ps)

and vars_pss pss =
  List.fold_left
    (fun r (ps,_) -> List.fold_left extract_vars r ps)
    IdentSet.empty pss

(* Split actions into exit/trap pair *)
let rec split_guarded lam_exit = function
  | Lifthenelse (cond, body, Lstaticraise (0,[])) ->
      Lifthenelse (cond, lam_exit, Lstaticraise (0,[])),body
  | Llet(str, id, lam, body) ->
      let lam1,lam2 = split_guarded lam_exit body in
      Llet (str, id, lam, lam1),lam2
  | Levent(lam, ev) ->
      let lam1,lam2 = split_guarded lam_exit lam in
      Levent (lam1, ev),lam2
  | _ -> assert false
;;

let split vars_pat pat_act_list =
  List.fold_right
    (fun (p,act) cls ->
      let num_exit = next_raise_count ()
      and ids = vars_pat p in
      let lam_exit =
	Lstaticraise (num_exit, List.map (fun id -> Lvar id) ids) in
      let match_act, def_act =
	if is_guarded act then
	  split_guarded lam_exit act
	else
	  lam_exit, act in
      ((p,match_act),(num_exit, ids, def_act))::cls)
    pat_act_list []
;;

(* Translate actions to out.out and back *)

let compile_match_out repr xs pss fail =
  let pss = List.map (fun (ps,lam) -> ps,Out.final lam) pss
  and fail = Out.final fail in
  let out = compile_match repr xs pss fail in
  let vars = vars_pss pss in
  refresh vars (Out.to_lambda out)

(******************)

let add_defs lam defs =
  List.fold_left
    (fun lam (num,ids,body) -> Lstaticcatch (lam,(num,ids),body))
    lam defs

let compile_matching loc repr handler_fun arg pat_act_list _partial =
(*
  if verbose > 1 then Location.prerr_warning loc Warnings.Deprecated ;
*)
  let pat_exits = split vars_pat pat_act_list in
  let pat_act_list,defs = List.split pat_exits in
  let pss = List.map (fun (p,act)  -> [p],act) pat_act_list
  and num_fail = next_raise_count ()
  and v = match arg with Lvar v -> v | _ -> Ident.create "m" in
  let lam =
    compile_match_out repr [Strict,v,arg] pss (Lstaticraise (num_fail,[])) in
  let lam = add_defs lam defs in
  Lstaticcatch (lam,(num_fail,[]),handler_fun ())



(************************)

let rec super_flatten size (p,act) k = match p.pat_desc with
| Tpat_any -> (omegas size,act)::k
| Tpat_tuple ps -> (ps,act)::k
| Tpat_or (p1,p2,_) ->
    super_flatten size (p1,act) (super_flatten size (p2,act) k)
| _ -> raise Cannot_flatten
      

let for_multiple_match loc args pat_act_list _partial =  
  let repr = None in
  try
    let pat_exits = split vars_pat pat_act_list in
    let pss,defs =  List.split pat_exits in
    let pss = List.fold_right (super_flatten (List.length args)) pss [] in
    let args =
      List.map
	(fun lam -> match lam with
	| Lvar v -> Alias,v,lam
	| lam -> Strict,Ident.create "m",lam)
	args in
    let num_fail = next_raise_count () in

    (* Perform strict let binding, for the sake of semantics *)
    let xs = List.map (fun (_,v,_) -> Alias,v,Lvar v) args in
    let lam = compile_match_out repr xs pss (Lstaticraise (num_fail,[])) in
    let lam = add_defs lam defs in
    let lam = Lstaticcatch (lam,(num_fail,[]), partial_function loc ()) in
    List.fold_right
      (fun (str,v,ev) lam -> Lambda.bind str v ev lam)
      args lam
  with
  | Cannot_flatten ->
      let arg =  (Lprim(Pmakeblock(0, Immutable), args)) in
      compile_matching loc repr (partial_function loc) arg pat_act_list _partial

(************************)

let for_tupled_function loc xs pats_act_list  _partial =
  let pats_exits = split vars_pats pats_act_list in
  let pss,defs =  List.split pats_exits
  and num_fail = next_raise_count ()
  and xs = List.map (fun x -> Alias,x,Lvar x) xs in
  let lam = compile_match_out None xs pss (Lstaticraise (num_fail,[])) in
  let lam =  add_defs lam defs in
  Lstaticcatch (lam,(num_fail,[]),partial_function loc ())
	  
end

let compile_matching,  for_multiple_match,  for_tupled_function =
  if Matchcommon.share then
    let module M = Make(Share) in
    M.compile_matching,  M.for_multiple_match,  M.for_tupled_function
  else
    let module M = Make(OutLambda) in
    M.compile_matching,  M.for_multiple_match,  M.for_tupled_function
