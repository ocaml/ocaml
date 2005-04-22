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
open Ctype
open Etype
open Longident
open Typedtree

let print_kelem ppf kelem =
  Format.fprintf ppf "{@[ktype= %a;@ kdepend= %a@]}@."
    Printtyp.type_scheme kelem.ktype
    (fun ppf -> 
      function 
	| None -> Format.fprintf ppf "None"
	| Some t -> Printtyp.type_scheme ppf t) kelem.kdepend

                  (**********************************************)
                  (*              Anti-unification              *)
                  (**********************************************)

(* anti_unify receives type scheme t1 and t2, and returns their 
   least general antiunifier. The levels of the newly created 
   type variables have the generic level.
*)

let anti_unify env t1 t2 =
  let cache = ref [] in
  let add_and_return t1 t2 t =
    cache := ((t1,t2), t) :: !cache;
    t
  in
  let rec sub t1 t2 = 
    if t1 == t2 then t1 else
    let t1 = repr t1
    and t2 = repr t2 in
    if t1 == t2 then t1 else
    try
      let rec search = function
  	| [] -> raise Not_found
  	| ((t1',t2'),t) :: cs ->
  	    if equal env false [t1;t2] [t1';t2'] then t
  	    else search cs
      in
      search !cache
    with
    | Not_found ->
        match t1.desc, t2.desc with
  	| Tarrow (l1, t11, t12, c1), Tarrow (l2, t21, t22, c2) 
	  when l1 = l2 ->
	    (* FIXME: we ignore commutativity *)
  	    newty (Tarrow(l1, sub t11 t21, sub t12 t22, c1 ))
  	| Ttuple tl1, Ttuple tl2 when List.length tl1 = List.length tl2 ->
  	    newty (Ttuple( List.map2 sub tl1 tl2))
  	| Tconstr (p1, tl1, _), Tconstr (p2, tl2, _) when Path.same p1 p2 ->
  	    (* abbrev? *)
  	    newty (Tconstr (p1, List.map2 sub tl1 tl2, ref Mnil (* ??? *) ))
	| Tlink _, _ | _, Tlink _ -> assert false
	| Toverload _, _ | _, Toverload _ -> assert false 
	| Tkonst ([],_), _ | _, Tkonst ([], _) -> assert false
	| Tkonst (k,t1), _ -> sub t1 t2
	| _, Tkonst (k,t2) -> sub t1 t2
  	| _, _ -> 
  	    add_and_return t1 t2 (newgenvar ())
  in
  correct_levels (sub t1 t2) (* FIX the type levels *)

(* Anti-unification of set of types *)
let anti_unify_types env types = 
  List.fold_right (fun typ st ->
    anti_unify env typ st) (List.tl types) (List.hd types)

(*
let anti_unify_types env types = 
  let t = anti_unify_types env types in
  Format.fprintf Format.err_formatter "AUNIF: %a@." 
    Printtyp.type_scheme t;
  t
*)

let _ = Ctype.anti_unify_types_ref := anti_unify_types

(* normalize konst_type *)
(* here, konstraints are normalized: konstraints unrelated to the body are
   removed *)

let normalize_konst_type konst ty =
  let tvars = type_variables [ty] in
  List.filter (fun kelem -> 
    let kvars = type_variables_of_konst_elem kelem in
    let exist = 
      List.exists (fun tv -> List.memq tv kvars) tvars
    in
	      if not exist then prerr_endline "ELIM";
	      exist) konst

exception Not_generic_primitive_type
let type_of_generic_primitive_compilation env konst ty =
  let konst = 
    List.map (fun kelem -> 
      if kelem.ktype.desc <> Tvar || kelem.kdepend <> None then
	raise Not_generic_primitive_type;
      kelem.ktype) konst
  in
  let obj_path, _ = Env.lookup_type (Ldot (Lident "Obj", "t")) env in
  let type_path, _ = Env.lookup_type (Ldot (Lident "Rtype", "type_expr")) env 
  in 
  let obj_t = Ctype.newty (Tconstr (obj_path, [], ref Mnil)) in
  let type_t = Ctype.newty (Tconstr (type_path, [], ref Mnil)) in
  begin_def ();
  let ty', konst' = 
    let ts = Ctype.instance_list (ty :: konst) in
    List.hd ts, List.tl ts
  in
  List.iter (fun v -> Ctype.unify env v obj_t) konst';
  let type_abst = 
    match konst' with
    | [] -> assert false
    | [ke] -> type_t
    | _ -> Ctype.newty (Ttuple (List.map (fun _ -> type_t) konst'))
  in
  let ty'' = Ctype.newty (Tarrow ("", type_abst, ty', Cok)) in
  end_def ();
  generalize ty'';
  ty''

(* OLD
(* type abstraction *)
let type_abstraction konst t =
  let t = repr t in
  let konst = normalize_konst_type konst t in
  List.map (fun kelem -> kelem.ktype)
    (List.filter (fun kelem -> kelem.kdepend = None) konst)

let type_abstraction_of_value vdesc =
  match vdesc.val_type.desc with
  | Tkonst (konst, ty) -> type_abstraction konst ty
  | _ -> []
*)

(* association from type variables to identifiers *)

module TVAR = struct
  type t = type_expr * Ident.t
  let equal (t1,_) (t2,_) = t1 == t2
  let hash (t,_) = t.id
end

let ident_of_type_variable, find_ident_of_type_variable = 
  let dummyid = Ident.create "dummy" in
  let module TBL = Weak.Make(TVAR) in
  let tbl = TBL.create 127 in
  let find_ident_of_type_variable tvar =
    let data = tvar, dummyid in
    let _, id = TBL.find tbl data in 
    id
  in
  (fun tvar ->
    assert (tvar.desc = Tvar);
    try
      find_ident_of_type_variable tvar
    with
    | Not_found ->
	let name = "*t" ^ string_of_int tvar.id ^ "*" in
	let id = Ident.create name in
	TBL.add tbl (tvar, id);
	id),
  find_ident_of_type_variable

let rec print_flow ppf = function
  | Ftype t -> Format.fprintf ppf "@[<2><<%a>>@]" Printtyp.type_scheme t
  | Fkonst frec -> print_flow_record ppf frec
  | Foverload (n,f) -> 
      Format.fprintf ppf "@[<1>[%d | @[%a@]]@]"	
	n 
	print_flow f
  | Floop _ -> Format.fprintf ppf "LOOP"

and print_flow_record ppf frec =
  let print_list p sep ppf l =
    let rec f = function
      | [] -> ()
      | x::xs ->
	  Format.fprintf ppf "@[%a@]" p x;
	  sep ppf;
	  f xs
    in
    f l
  in
  Format.fprintf ppf "@[{ @[%a@] }@]" 
    (print_list print_flow_record_elem 
       (fun ppf -> Format.fprintf ppf ";@ "))
    frec

and print_flow_record_elem ppf (kelem, flow) =
  (* FIXME: kelem.ktype is already instansiated.
     Not sure the ordering is preserved correctly... *)
  Format.fprintf ppf "@[<2>(@[%a@]) ===>@ @[%a@]@]" 
    (fun ppf kelem ->
      match kelem.kdepend with
      | None -> Printtyp.type_scheme ppf kelem.ktype
      | Some t -> 
	  Format.fprintf ppf "@[<2>%a <@ %a@]" 
	    Printtyp.type_scheme kelem.ktype
	    Printtyp.type_scheme t) kelem
    print_flow flow

let rec assoc_cache env t scm = function
  | [] -> raise Not_found
  | ((t',scm'), flowref)::cs ->
      if Etype.compare_types scm scm' = 0 &&
	Ctype.equal env false [t] [t'] then
	Floop flowref
      else assoc_cache env t scm cs

let create_ptr () = ref (Fkonst []) (* a dummy value *)
let add_cache t scm ptr cache = ((t,scm),ptr) :: cache
let make_loop ptr flow = ptr := flow

exception Not_instance of type_expr * type_expr

let debug = try ignore (Sys.getenv "GCAML_RESOLUTION"); true with _ -> false

let rec is_instance cache env t scm =
  if debug then  Format.eprintf "@[{{ @[<2>is_instance:@ @[<2>%a@ ?<?@ %a@]@ " 
      Printtyp.type_scheme t
      Printtyp.type_scheme scm;
  try
    let result = is_instance_ cache env t scm in
    if debug then  Format.eprintf "@]}}@]@,";
    result
  with
  | e -> 
      if debug then Format.eprintf "@]ERROR }}@]@,";
      raise e
  
and is_instance_ cache env t scm =
  let scm = repr scm in
  try 
    (* returned flowref is still a dummy *) 
    (* if the cache hits, we make a loop *)
    let result = cache, assoc_cache env t scm cache in
    if debug then Format.eprintf "Cache found";
    result
  with Not_found ->

  let shot = snapshot () in
  try
    match scm.desc with
    | Toverload odesc ->
        let rec search pos = function
  	  | [] -> raise (Unify [])
  	  | typ::typs -> 
  	      try
		let ptr = create_ptr () in
		let cache' = add_cache t scm ptr cache in 
		let cache'', subflow = is_instance cache' env t typ in
		let flow = Foverload (pos, subflow) in
		make_loop ptr flow;
		cache'', flow
	      with
	      | Unify _ | Not_instance _ -> search (pos+1) typs
	in
	search 0 odesc.over_cases
    | Tkonst (k,typ) -> resolve_tkonst cache env t scm k typ
    | _ -> 
	let typ = instance scm in
	unify env t typ; cache, Fkonst []
  with
  | Unify _ | Not_instance _ ->
      backtrack shot;
      raise (Not_instance (t,scm))

and resolve_tkonst cache env t scm k typ = (* scm.desc = Tkonst(k,typ) *)
  if debug then  Format.eprintf "@[{{ @[<2>resolve_tkonst:@ @[<2>%a@ ?<?@ %a@]@ " 
      Printtyp.type_scheme t
      Printtyp.type_scheme scm;
  try
    let result = resolve_tkonst_ cache env t scm k typ in
    if debug then  Format.eprintf "@]}}@]@,";
    result
  with
  | e -> 
      if debug then  Format.eprintf "@]ERROR }}@]@,";
      raise e
    
and resolve_tkonst_ cache env t scm k typ =
  (* scm's level may be broken. (I do not know why...) *)
  let scm = Ctype.correct_levels scm in
  if debug then Format.eprintf "@[<2>before =>@ @[%a@ // @[%a@]@]@]@ "
    Printtyp.type_expr t
    Printtyp.type_scheme scm;
  let scm' = instance scm in
  let k', typ' =
    match (repr scm').desc with
    | Tkonst (k',typ') -> k', typ'
    | _ -> assert false
  in
  if debug then Format.eprintf "@[<2>instance =>@ @[%a@ // @[%a@]@]@]@ "
    Printtyp.type_expr t
    Printtyp.type_scheme scm';
  unify env t typ';
  if debug then Format.eprintf "@[<2>UNIFICATION =>@ @[%a@ // @[%a@]@]@]@ "
    Printtyp.type_expr typ'
    Printtyp.type_scheme scm';
  let ptr = create_ptr () in
  let cache' = add_cache t scm ptr cache in
  let cache'', subflows = resolve_konstraint cache' env k' in
  let flow = Fkonst subflows in
(* TOO ADVANCED
	let flow = 
	  match flow with
	  | Fkonst [_,Ftype t] -> Ftype t
	  | _ -> flow
	in
*)
  make_loop ptr flow;
  cache'', flow

and resolve_konstraint cache env konst =
  if debug && konst <> [] then begin
    Format.eprintf 
      "@[{{ @[Resolve konstraint set:@ @[<2>{{@ %a@ @]}}@ "
      (Gdebug.print_list 
         (fun ppf ke ->
	   match ke.kdepend with
	   | Some t ->
	       Format.fprintf ppf "@[%a <?< %a@]"   
		 Gdebug.print_type_scheme ke.ktype
		 Gdebug.print_type_scheme t
	   | None ->
	       Format.fprintf ppf "@[%a (RT)@]"   
		 Gdebug.print_type_scheme ke.ktype)
         (fun ppf -> Format.fprintf ppf "@ &&@ "))
      konst;
  end;
  try
    let result = resolve_konstraint_ cache env konst in
    if debug && konst <> [] then Format.eprintf "@]}}@]";
    result
  with e -> 
    if debug && konst <> [] then Format.eprintf "@] ERROR }}@]";
    raise e

and resolve_konstraint_ cache env konst =
  List.fold_right (fun kelem (cache, flows) ->
(* FIXME: to avoid problems around shared flow production, 
   we do not share cache between sub-konstraints *)
(*    
   match kelem.kdepend with
   | None -> cache, (kelem, Ftype kelem.ktype) :: flows
   | Some kdepend -> 
       let cache', subflow = is_instance cache env kelem.ktype kdepend in
       cache', (kelem, subflow) :: flows) konst (cache, [])
*)
    match kelem.kdepend with
    | None -> 
	cache, (kelem, Ftype kelem.ktype) :: flows
    | Some kdepend -> 
	let cache', subflow = is_instance cache env kelem.ktype kdepend in
	cache, (kelem, subflow) :: flows) konst (cache, [])

let is_instance env t scm = snd (is_instance [] env t scm)
let resolve_konstraint env konst = snd (resolve_konstraint [] env konst)

let make_toverload env typs =      
  let aunif = anti_unify_types env typs in
  Btype.newgenty (Toverload {over_aunif=aunif; over_cases= typs})
      
let index_of_flow_record env kelem t =
  normalize_type t;
  match t.desc with
  | Tkonst (konst,_) ->
      let rec f pos = function
	| [] -> raise Not_found 
	| ke::ks ->
	    let krepr k = 
	      { ktype= Ctype.repr k.ktype;
		kdepend= 
		  match k.kdepend with
		  | Some t -> Some (Ctype.repr t)
		  | None -> None }
	    in
	    let keq k1 k2 =
	      k1 == k2 ||
	      (let k1 = krepr k1
	       and k2 = krepr k2  in
	       k1 == k2 ||
	       (k1.ktype == k2.ktype &&
		match k1.kdepend, k2.kdepend with
		| Some t1, Some t2 -> t1 == t2
		| None, None -> true
		| _ -> false))
	    in
	    if 
	      keq kelem ke ||
	      (Ctype.equal env false [kelem.ktype] [ke.ktype] &&
	       match kelem.kdepend, ke.kdepend with
	       | None, None -> true
	       | Some t1, Some t2 when Etype.compare_types t1 t2 = 0 -> true
	       | _ -> false)
	    then pos 
	    else f (pos+1) ks
      in
      f 0 konst
  | _ -> assert false

    
    
