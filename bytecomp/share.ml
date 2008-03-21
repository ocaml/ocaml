(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(********************************)
(* Out module with Hash-Consing *)
(********************************)

open Lambda

type out = int

(*******************************)
(* Hash consing infrastructure *)
(*******************************)

type key =
  | Final of lambda
  | Bind of let_kind * Ident.t * lambda * out
  | Choose of Ident.t * (Discr.discr * out) list * out option
  | Patch of  out * lambda (* Delayed patch for guards *)

type node =
    { mutable refs : int ;
      fv : IdentSet.t ;
      me : key ; }

let t_mem = Hashtbl.create 101
and t_node = Extarray.create ()
(* This table maps access path to (unique) variables *)
let t_ids = Hashtbl.create 101


let reset_state () =
  Hashtbl.clear t_mem ;
  Hashtbl.clear t_ids ;
  ()

(* DEBUG *)

open Printf 

let rec iter_dump dump chan = function
  | [] -> ()
  | [x] -> dump chan x
  | x::xs -> fprintf chan "%a, %a" dump x (iter_dump dump) xs
      


let dump_node chan k cell =
  fprintf chan "%02d:<%d> " k cell.refs ;
  match cell.me with
  | Final lam ->
      let ppf = Format.formatter_of_out_channel chan in
      Format.fprintf ppf "%a@." Printlambda.lambda lam
  | Bind (_,x,_,idx) ->
      fprintf chan "Bind %s in %i\n" (Ident.unique_name x) idx
  | Choose (_,cls,d) ->
      fprintf chan "Choose {" ;
      iter_dump  (fun chan (_,idx) -> fprintf chan "%d" idx) chan cls ;
      begin match d with
      | Some idx -> fprintf chan ",%d}\n" idx
      | None -> fprintf chan "}\n"
      end
  | Patch (i,lam) ->
      let ppf = Format.formatter_of_out_channel chan in
      Format.fprintf ppf "Patch {%i %a}@." i Printlambda.lambda lam


(******************)
(* Free variables *)
(******************)

let node_fv idx = (Extarray.get t_node idx).fv

(* Compute them *)

let comp_fv node = match node with
| Final e -> Lambda.free_variables e
| Bind (_, x, ex, idx) ->
    IdentSet.union
      (Lambda.free_variables ex)
      (IdentSet.remove x (node_fv idx))
| Choose (x,cls,d) ->
    IdentSet.add x
      (List.fold_right
	 (fun (_,idx) k -> IdentSet.union (node_fv idx) k)
	 cls
	 (match d with
	 | None -> IdentSet.empty
	 | Some idx -> node_fv idx))
| Patch (idx,e) ->
    IdentSet.union (node_fv idx) (Lambda.free_variables e)


(* Hash-Consing constructors *)


let share_node key =
  let idx = 
    try Hashtbl.find t_mem key
    with Not_found ->
      let idx =
	Extarray.emit t_node
	  {refs = 0 ; fv=comp_fv key ; me = key } in
      Hashtbl.add t_mem key idx ;
      idx in
(*
  prerr_string "NODE: " ;
  dump_node stderr idx (Extarray.get t_node idx) ; flush stderr ;
*)
  idx

let final lam = share_node (Final lam)

(* Remove useless bindinds now, it cannot hurt *)
let share_bind str x ex idx = match ex with
| Lvar y when Ident.same x y -> idx
| _ ->
    if IdentSet.mem x (node_fv idx) then
      share_node (Bind (str,x,ex,idx))
    else
      idx

let alias x y idx = share_bind Alias x (Lvar y) idx

let bind = share_bind

(* lam will always be a field access on another variable *)
let share_id lam =
  try Hashtbl.find t_ids lam
  with Not_found ->
    let id = Ident.create "f" in
    Hashtbl.add t_ids lam id ;
    id

let field_ids d x = Discr.field_ids share_id d x

let switch x cls d = match cls,d with
| ([],Some idx)
| ([_,idx],None) -> idx
| _,_ -> share_node (Choose (x,cls,d))

(*********************)
(* Alter final nodes *)
(*********************)

let rec is_guarded idx = match (Extarray.get t_node idx).me with
| Final e -> Lambda.is_guarded e
| Bind (_,_,_,idx) -> is_guarded idx
| Patch _| Choose _ -> assert false

let rec patch_guarded patch idx = match (Extarray.get t_node idx).me with
| Final e -> share_node (Patch (patch,e))
| Bind (str,x,ex,idx) -> share_bind str x ex (patch_guarded patch idx)
| Patch _| Choose _ -> assert false

(* Hum... *)
let event_branch _repr idx = idx

(*********************)
(* Back to lambda... *)
(*********************)

let to_lambda idx =
  let t_node = Extarray.trim t_node in
  reset_state () ;


(* Set ref counts, much safer  in a separate pass,
   also returns the set of all bound vars *)
  let rec set_refs idx =
    let cell = t_node.(idx) in
    if cell.refs > 0 then begin
      cell.refs <- cell.refs + 1 ;
      IdentSet.empty
    end else begin
      cell.refs <- 1 ;
      match cell.me with
      | Final _ -> IdentSet.empty
      | Bind (_,x,_,idx) -> IdentSet.add x (set_refs idx)
      | Patch (i,e) -> set_refs i
      | Choose (x,cls,d) ->
          List.fold_right
	    (fun (_,idx) bv -> IdentSet.union (set_refs idx) bv)	      
	    cls
            (match d with
            | None -> IdentSet.empty
            | Some idx -> set_refs idx)
    end in

  let all_bounds = set_refs idx in

  if Matchcommon.verbose > 2 then begin
    Printf.eprintf "All bound:" ;
    IdentSet.iter (fun v -> Printf.eprintf " %s" (Ident.unique_name v))
      all_bounds ;
    prerr_endline ""
  end ;

(***************************************************)
(* Put handlers at appropriate places,             *)
(* ie, as deep as possible with all exits included *)
(* Deep handlers minimize static raise number of   *)
(* arguments.                                      *)
(***************************************************)

(*
  let E be a share expression

   .... E ... E .....

  is to be changed into
   catch ... (exit i args1) .... (exit i args2)
   with (i vars) E

*)

  let idx_to_e = Hashtbl.create 17 (* from node indices to exit numbers *)
  and handlers = Hashtbl.create 17 (* from node indices to to handlers
	                              to be inserted above them *)
  and exits = Hashtbl.create 17 in (* from indices to exit numbers + formal args *)
  
  
  let register_exit idx =
    try
      let _ = Hashtbl.find idx_to_e idx in ()
    with
    | Not_found ->
        let e = next_raise_count () in
        Hashtbl.add idx_to_e idx e

  and get_exit idx =
    try Hashtbl.find idx_to_e idx with Not_found -> assert false in

  let get_handlers idx =
    try Hashtbl.find handlers idx
    with Not_found -> [] in

  let register_handler idx e args idx_e =
    let args = IdentSet.elements args in
    Hashtbl.add exits idx_e (e,args) ;
    Hashtbl.replace handlers idx ((e,args,idx_e)::get_handlers idx) in


(*
  e1, e2 below are lists (node index X number of occurences of idx)
  merge detects when seen references of idx are all here
  (cf. now below)
*)

  let rec merge e1 e2 = match e1,e2 with
  | [],e | e,[] -> e,[]
  | ((x1,v1) as c1::r1), ((x2,v2) as c2::r2) ->
      if x1 < x2 then
	let kont,now = merge r1 e2 in
	c1::kont,now
      else if x2 < x1 then
	let kont,now = merge e1 r2 in
	c2::kont,now
      else
	let v = v1+v2 in
	if v = t_node.(x1).refs then begin
          let kont, now = merge r1 r2 in
          kont, x1::now
	end else
          let kont, now = merge r1 r2 in
          (x1,v)::kont,now in

(*
  This series of functions builds two tables indexed by nodes (indices)
    1. For exits
    2. For handlers
*)

  let rec put_handlers bound idx =
    let cell = t_node.(idx) in
    if cell.refs > 1 then begin
 (* This node will be replaced by an exit *)
      register_exit idx ;
      [idx,1]
    end else begin
      do_put bound idx cell.me
    end

  and do_put bound idx = function
    | Final _ -> []
    | Bind (_,x,_,idx) ->
        put_handlers (IdentSet.add x bound) idx
    | Choose (_,cls,d) ->
        let r,now = 
          List.fold_left
            (fun (r,now) (_,idx) ->
              let kont,lnow = merge r (put_handlers bound idx) in
              kont,lnow@now)
            (match d with
            | None -> [],[]
            | Some idx -> put_handlers bound idx,[])
            cls in
        dodo_put bound idx r now
    | Patch (idx,e) -> put_handlers bound idx


(* Insert the given list of nodes as handlers here *)
  and dodo_put bound idx r = function
    | [] -> r
    | idx_e::rem ->
        let e = get_exit idx_e
        and cell = t_node.(idx_e) in
	(* Those variables need to be given as arguments
           by exits *)
        let args = IdentSet.diff (IdentSet.inter all_bounds cell.fv) bound in
        register_handler idx e args idx_e ;
	(* Node idx_e is scanned now, in its final context
           Notice
             1. All its free vars are bound
             2. we may have to insert  additional handler,
                given that scanning idx_e may produce
                enough exits *)
        let r,now = merge (do_put cell.fv idx_e cell.me) r in
        dodo_put bound idx (dodo_put bound idx r now) rem in

  let _tops = put_handlers IdentSet.empty  idx in


  if Matchcommon.verbose > 1 then begin
    prerr_endline "** Sharing **" ;
    for k = 0 to Array.length t_node-1 do
      dump_node stderr k t_node.(k)
    done ;
    if _tops <> [] then begin
      prerr_string "Tops:" ;
      List.iter
	(fun (idx,c) -> eprintf " <%i,%i>" idx c) _tops ;
    end
      
  end ;

  assert (_tops = []) ;

  let rec do_share idx =
    let cell = t_node.(idx) in
    if cell.refs = 1 then
      share_handlers (share_node cell) idx
    else
      let e,args =
        try Hashtbl.find exits idx with Not_found -> assert false in
      Lstaticraise
        (e, List.map (fun v -> Lvar v) args)

  and share_handlers r idx =
    let hs = get_handlers idx in
    List.fold_right
      (fun (e,xs,idx_e) r ->
        Lstaticcatch
          (r,(e,xs),
           share_handlers
             (share_node (t_node.(idx_e)))
             idx_e))
      hs r

  and share_node cell = match cell.me with
  | Final e -> e
  | Bind (str,x,ex,idx) -> Lambda.bind str x ex (do_share idx)
  | Choose (v,cls,d) ->
      Discr.switch
	v
        (List.map (fun (c,idx) -> c, do_share idx) cls)
        (match d with
        | None -> None
        | Some idx -> Some (do_share idx))
  | Patch (i1,lam2) ->
      let lam1 =  do_share i1 in
      try
	Lambda.patch_guarded lam1 lam2
      with e ->
	Format.eprintf "%a@." Printlambda.lambda lam1;
	Format.eprintf "%a@." Printlambda.lambda lam2 ;
	raise e in
  
  do_share idx
