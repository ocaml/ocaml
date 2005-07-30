(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml + CDuce                    *)
(*                                                                     *)
(*            Alain Frisch, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Translation from typed abstract syntax to lambda terms,
   for the CDuce extension *)

open Misc
open Asttypes
open Primitive
open Path
open Types
open Typedtree
open Typeopt
open Lambda

module S = Cduce_types.Serial

let get_ext exp_ty =
  match Ctype.repr exp_ty with
    | {desc = Text { ext_const = Some t }} -> t
    | _ -> assert false

let get_ext_type exp =
  let exp_ty =
    Ctype.expand_head exp.exp_env (Ctype.correct_levels exp.exp_type) in
  get_ext exp_ty

let builtin_id f = 
  try
    transl_path (fst (Env.lookup_value (Longident.parse f) Env.empty))
  with Not_found -> 
    Printf.eprintf "Cannot find builtin %s\n" f; flush stderr;
    assert false


let builtin f args =
  Lapply (builtin_id f, args)

let value_absent () =
  try
    match Env.lookup_constructor (Longident.parse "Cduce_types.Value.Absent") 
      Env.empty with
	| { cstr_tag = Cstr_constant n } -> Lconst (Const_pointer n)
	| _ -> assert false
  with Not_found -> assert false
  

let chunk = ref None
let chunk_used = ref false
let pchunk = ref None
let rec_defs = ref []
let globals = ref []

module PathHash = 
  Hashtbl.Make(struct
		 type t = Path.t
		 let equal = Path.same
		 let hash x = Hashtbl.hash (Path.name x)
	       end)


let from_ml_funs = PathHash.create 17
let to_ml_funs = PathHash.create 17

let enter () =
  match !chunk with
    | None -> 
	chunk := Some (Ident.create "cduce_chunk"); 
	pchunk := Some (S.P.init ());
	rec_defs := [];
	PathHash.clear from_ml_funs;
	PathHash.clear to_ml_funs
    | Some _ -> assert false

let get_chunk () =
  match !chunk with
    | None -> assert false
    | Some c -> chunk_used := true; Lvar c

let get_pchunk () =
  match !pchunk with
    | None -> assert false
    | Some c -> c

let leave e = match !chunk,!pchunk with
  | Some c, Some p ->
      let e = if !rec_defs == [] then e else Lletrec (!rec_defs,e) in
      let e = 
	List.fold_left (fun e (x,d) -> Llet (StrictOpt, x, d ,e)) e !globals in
      let e = 
	if !chunk_used then
	  let s = S.P.mk p in
	  chunk_used := false;
	  Llet (Strict, c, 
		builtin "Cduce_types.Serial.G.mk" 
		  [ Lconst (Const_base (Const_string s)) ], 
		e)
	else
	  e
      in
      globals := []; rec_defs := []; chunk := None; pchunk := None;
      PathHash.clear from_ml_funs;
      PathHash.clear to_ml_funs;
      e
  | _ -> assert false
      

let global v =
  let i = S.P.put (get_pchunk ()) v in
  let x = Ident.create "cduce_data" in
  globals := (x, Lprim (Pfield i, [ get_chunk () ])) :: !globals;
  Lvar x
      
module CT = Cduce_types.Types

let transl_ext_match transl_exp partial arg arg_ty bl = 
  let disp,rhs = Cduce_types.Patterns.Compile.make_branches arg_ty
    (List.map (fun (p,binds,e) -> (p,(binds,e))) bl) in

  let result = Ident.create "result" in
  let bindings = Ident.create "bindings" in

  let map_rhs = function
    | Cduce_types.Auto_pat.Fail -> value_absent ()
    | Cduce_types.Auto_pat.Match (arity,(binds,e)) ->
	assert(arity = List.length binds);
	let binds = 
	  List.sort (fun (x1,_) (x2,_) -> Cduce_types.Ident.Id.compare x1 x2) 
	    binds in
	let pos = ref (-1) in
	List.fold_left
	  (fun e (_,id) -> 
	     incr pos;
	     Llet (StrictOpt,id, Lprim(Pfield !pos, [Lvar bindings]),e))
	  (transl_exp e) binds
  in	
	

  let switch =
    match Array.length rhs with
      | 0 ->
	  Lconst (Const_pointer 0)
      | 1 ->
	  map_rhs rhs.(0)
      | n ->
	  Lswitch 
	    (Lprim(Pfield 0, [Lvar result]),
	     { sw_numconsts = n;
	       sw_consts = 
		 Array.to_list (Array.mapi (fun i x -> (i,map_rhs x)) rhs);
	       sw_numblocks = 0;
	       sw_blocks = [];
	       sw_failaction = None }) in
  Llet (Alias,result,
	builtin "Cduce_types.Run_dispatch.run_dispatcher" [ global disp; arg ],
	Llet (Alias,bindings, Lprim(Pfield 1, [Lvar result]), switch))

let transl_label lab =
  Cduce_types.Ident.Label.mk 
    (Cduce_types.Ns.empty,Cduce_types.Encodings.Utf8.mk_latin1 lab)

let transl_record labels fields =
  let labels = global (Array.of_list labels) in
  let fields = Lprim (Pmakeblock(0, Immutable), fields) in
  builtin "Cduce_types.Value.mk_record" [ labels; fields ] 


let mk_atom s = 
  let t = Cduce_types.Encodings.Utf8.mk_latin1 s in
  Cduce_types.Atoms.V.mk (Cduce_types.Ns.empty,t)

let ml_constr t args =
  let t = global (Cduce_types.Value.Atom (mk_atom t)) in
  match args with
    | [] ->
	t
    | _ ->
	builtin "Cduce_types.Value.ocaml2cduce_constr" 
	  [ t; Lprim (Pmakeblock(0, Immutable), args) ]

let switch e consts blocks =
  let sw = {
    sw_numconsts = List.length consts;
    sw_consts = consts;
    sw_numblocks = List.length blocks;
    sw_blocks = blocks;
    sw_failaction = None 
  } in
  Lswitch (e, sw)

let switch_block e = function
  | [] -> Lconst (Const_pointer 0)
  | blocks ->
      let sw = {
	sw_numconsts = 1;
	sw_consts = [];
	sw_numblocks = List.length blocks;
	sw_blocks = blocks;
	sw_failaction = Some (Lconst (Const_pointer 0))
      } in
      Lswitch (e, sw)

(* TODO: decision tree *)
let dispatch_variant v consts blocks =
  let consts =
    match consts with
      | [] -> None
      | (_,l)::tl ->
	  Some (List.fold_left 
		  (fun e (i,l) -> Lifthenelse 
		     (Lprim(Poffsetint (-i), [v]), e, l))
		  l tl)
  and blocks =
    match blocks with
      | [] -> None
      | (_,l)::tl ->
	  let tag = Ident.create "variant" in
	  Some (Llet 
		  (Alias, tag, Lprim(Pfield 0, [v]),
		   List.fold_left 
		     (fun e (i,l) -> 
			Lifthenelse (Lprim(Poffsetint (-i), [Lvar tag]), e, l))
		     l tl))
  in
  match consts,blocks with
    | Some x, None | None, Some x -> x
    | Some x, Some y -> Lifthenelse (Lprim(Pisint,[v]), x, y)
    | None, None -> assert false

let rec mapi f i = function
  | hd::tl -> (f i hd)::(mapi f (succ i) tl)
  | [] -> []

let swap = List.map (fun (v,(co,cn)) -> (v,(cn,co)))

let rec transl_from_ml args env t e =
  let t = Ctype.repr (Ctype.expand_head env t) in
  if List.mem_assq t args then 
    match List.assq t args with
      | Some i, _ ->  Lapply (Lvar i, [ e ])
      | None, _ -> assert false
  else match t.desc with
    | Tconstr (p,_,_) when Path.same p Predef.path_unit ->
	builtin_id "Cduce_types.Value.nil"
    | Tconstr (p,_,_) when Path.same p Predef.path_int ->
	builtin "Cduce_types.Value.ocaml2cduce_int" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_int32 ->
	builtin "Cduce_types.Value.ocaml2cduce_int32" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_int64 ->
	builtin "Cduce_types.Value.ocaml2cduce_int64" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_char ->
	builtin "Cduce_types.Value.ocaml2cduce_char" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_string ->
	builtin "Cduce_types.Value.ocaml2cduce_string" [ e ]
    | Tconstr (p,[t],_) when Path.same p Predef.path_list ->
	builtin "Cduce_types.Value.ocaml2cduce_list" 
	  [ transl_from_ml_fun args env t; e ]
    | Tconstr (p,[t],_) when Path.same p Predef.path_array ->
	builtin "Cduce_types.Value.ocaml2cduce_array" 
	  [ transl_from_ml_fun args env t; e ]
    | Tconstr (p,_,_) when Path.name p = "Cduce_types.Value.t" ->
	e
    | Ttuple tl ->
	let v = Ident.create "v" in
	let rec aux n = function
	  | [] -> assert false
	  | [hd] -> transl_from_ml args env hd (Lprim(Pfield n, [Lvar v]))
	  | hd::tl -> builtin "Cduce_types.Value.pair"
	      [transl_from_ml args env hd (Lprim(Pfield n, [Lvar v]));
	       aux (succ n) tl]
	in
	Llet (Strict,v,e,aux 0 tl)
    | Text _ ->
	e
    | Tconstr (p,a,_) ->
	let pn = Path.name p in
	let f = 
	  try PathHash.find from_ml_funs p
	  with Not_found ->
	    let n = Ident.create ("from_ml_"^pn) in
	    PathHash.add from_ml_funs p (Lvar n);
	    let l = transl_from_ml_decl env p in
	    rec_defs := (n, l) :: !rec_defs;
	    Lvar n
	in
	let decl = try Env.find_type p env with Not_found -> assert false in
	let e = 
	  List.fold_right2
	    (fun (co,cn,_) arg accu ->
	       let accu = 
		 if cn then transl_to_ml_fun (swap args) env arg :: accu 
		 else accu in
	       let accu = 
		 if co then transl_from_ml_fun args env arg :: accu 
		 else accu in
	       accu)
	    decl.type_variance
	    a
	    [e] 
	in
	Lapply (f, e)
(*    | Tarrow (_,t1,t2,_) ->
	builtin "Cduce_types.Value.ocaml2cduce_fun" 
	  [ transl_to_ml_fun (swap args) env t1;
	    transl_from_ml_fun args env t2;
	    e ] *)
    | Tvariant rd ->
	let fields = Ctype.get_fields rd in
	let v = Ident.create "v" in
	let conv t = transl_from_ml args env t (Lprim(Pfield 1, [Lvar v])) in
	let cstr (consts,blocks) = function 
	  | (c,None) ->
	      (Btype.hash_variant c, ml_constr c []) :: consts, blocks
	  | (c,Some t) ->
	      consts, (Btype.hash_variant c, ml_constr c [conv t]) :: blocks in
	let (consts,blocks) = List.fold_left cstr ([],[]) fields in
	Llet (Strict, v, e,
	      dispatch_variant (Lvar v) consts blocks)
    | _ -> 
	assert false

and transl_from_ml_fun args env t =
  let v = Ident.create "v" in
  Lfunction (Curried, [v], transl_from_ml args env t (Lvar v))

and transl_from_ml_decl env p =
  let v = Ident.create "v" in
  let decl = try Env.find_type p env with Not_found -> assert false in
  let args = List.map (fun _ -> Ctype.newvar ()) decl.type_params in
  let funs =
    List.map2 (fun v (co,cn,_) ->
		 (v,
		  ((if co then Some (Ident.create "from_ml_arg") else None),
		  (if cn then Some (Ident.create "to_ml_arg") else None))))
      args decl.type_variance in
  let inst t =
    let ps,t = Ctype.instance_parameterized_type decl.type_params t  in
    List.iter2 (Ctype.unify env) ps args;
    t in
  let arg ?(repr=Record_regular) i t = 
    let i = 
      match repr with Record_regular -> Pfield i 
	| Record_float -> Pfloatfield i in
    transl_from_ml funs env (inst t) (Lprim (i,[Lvar v])) in
  let e = 
  match decl.type_kind, decl.type_manifest with
    | Type_abstract, None ->
	builtin "Cduce_types.Value.abstract" [
	  Lconst (Const_base (Const_string (Path.name p)));
	  Lvar v
	]
    | Type_variant (cstrs,priv), _ ->
	let cstr (consts,blocks) = function 
	  | (c,{ cstr_tag = Cstr_constant n }) ->
	      (n, ml_constr c []) :: consts, blocks
	  | (c,{ cstr_tag = Cstr_block n; cstr_args = args }) ->
	      consts, (n, ml_constr c (mapi arg 0 args)) :: blocks
	  | _ -> assert false in
	let cstrs = Datarepr.constructor_descrs (Ctype.newvar()) cstrs priv in
	let (consts,blocks) = List.fold_left cstr ([],[]) cstrs in
	switch (Lvar v) consts blocks
    | Type_record (fields,repr,priv), _ ->
	let fields= Datarepr.label_descrs (Ctype.newvar ()) fields repr priv in
	let labels = List.map (fun (lab,_) -> transl_label lab) fields in
	let fields = mapi 
	  (fun i (_,l) -> assert (l.lbl_pos = i); arg ~repr i l.lbl_arg) 0 
	  fields in
	transl_record labels fields
    | _ -> assert false
  in
  List.fold_right 
    (fun (_,(co,cn)) e -> 
       let e = 
	 match cn with Some a -> Lfunction (Curried, [a], e) | None -> e in
       let e = 
	 match co with Some a -> Lfunction (Curried, [a], e) | None -> e in
       e)
    funs
    (Lfunction (Curried, [v], e))

and transl_to_ml args env t e =
  let t = Ctype.repr (Ctype.expand_head env t) in
  if List.mem_assq t args then 
    match List.assq t args with
      | Some i, _ ->  Lapply (Lvar i, [ e ])
      | None, _ -> assert false
  else match t.desc with
    | Tconstr (p,_,_) when Path.same p Predef.path_unit ->
	Lconst (Const_pointer 0)
    | Tconstr (p,_,_) when Path.same p Predef.path_int ->
	builtin "Cduce_types.Value.cduce2ocaml_int" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_int32 ->
	builtin "Cduce_types.Value.cduce2ocaml_int32" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_int64 ->
	builtin "Cduce_types.Value.cduce2ocaml_int64" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_char ->
	builtin "Cduce_types.Value.cduce2ocaml_char" [ e ]
    | Tconstr (p,_,_) when Path.same p Predef.path_string ->
	builtin "Cduce_types.Value.cduce2ocaml_string" [ e ]
    | Tconstr (p,[t],_) when Path.same p Predef.path_list ->
	builtin "Cduce_types.Value.cduce2ocaml_list" 
	  [ transl_to_ml_fun args env t; e ]
    | Tconstr (p,[t],_) when Path.same p Predef.path_array ->
	builtin "Cduce_types.Value.cduce2ocaml_array" 
	  [ transl_to_ml_fun args env t; e ]
    | Tconstr (p,_,_) when Path.name p = "Cduce_types.Value.t" ->
	e
    | Ttuple tl ->
	let v = Ident.create "v" in
	let rec aux accu v = function
	  | [] -> assert false
	  | [hd] -> 
	      let accu = List.rev (transl_to_ml args env hd v :: accu) 
	      in
	      Lprim (Pmakeblock(0, Immutable), accu)
	  | hd::tl -> 
	      let p = Ident.create "pair" in
	      Llet (Strict,p,builtin "Cduce_types.Value.get_pair" [v],
		   aux 
		     (transl_to_ml args env hd (Lprim (Pfield 0, [Lvar p]))
			:: accu)
		     (Lprim (Pfield 1, [Lvar p]))
		     tl)
	in
	aux [] e tl
    | Text _ ->
	e
    | Tconstr (p,a,_) ->
	let pn = Path.name p in
	let f = 
	  try PathHash.find to_ml_funs p
	  with Not_found ->
	    let n = Ident.create ("to_ml_"^pn) in
	    PathHash.add to_ml_funs p (Lvar n);
	    let l = transl_to_ml_decl env p in
	    rec_defs := (n, l) :: !rec_defs;
	    Lvar n
	in
	let decl = try Env.find_type p env with Not_found -> assert false in
	let e = 
	  List.fold_right2
	    (fun (co,cn,_) arg accu ->
	       let accu = 
		 if cn then transl_from_ml_fun (swap args) env arg :: accu 
		 else accu in
	       let accu = 
		 if co then transl_to_ml_fun args env arg :: accu 
		 else accu in
	       accu)
	    decl.type_variance
	    a
	    [e] 
	in
	Lapply (f, e)
(*    | Tarrow (_,t1,t2,_) ->
	builtin "Cduce_types.Value.cduce2ocaml_fun" 
	  [ transl_from_ml_fun (swap args) env t1;
	    transl_to_ml_fun args env t2;
	    e ] *)
    | Tvariant rd ->
	let fields = Ctype.get_fields rd in
	let v = Ident.create "v" in
	let interm = Ident.create "interm" in
	let conv t = 
	  let newval = 
	    transl_to_ml args env t (Lprim(Pfield 1, [Lvar interm])) in
	  Lprim (Psetfield (1,true), [Lvar interm; newval]) in
	let cstr map = function 
	  | (c,None) -> map
	  | (c,Some t) -> (Btype.hash_variant c, conv t) :: map in
	let map = List.fold_left cstr [] fields in
	let transl = List.map 
	  (fun (c,_) ->
	     let a = mk_atom c in
	     (Cduce_types.Atoms.atom a, Btype.hash_variant c)) fields in
	let transl = Cduce_types.Atoms.mk_map transl in
	Llet (Strict, interm,
	      builtin "Cduce_types.Value.cduce2ocaml_variant"
		[ global transl; e ],
	      Lsequence (dispatch_variant (Lvar interm) 
			   [(0,Lconst(Const_pointer 0))]
			   map, Lvar interm))
    | _ -> assert false

and transl_to_ml_fun args env t =
  let v = Ident.create "v" in
  Lfunction (Curried, [v], transl_to_ml args env t (Lvar v))

and transl_to_ml_decl env p =
  let v = Ident.create "v" in
  let interm = Ident.create "interm" in
  let decl = try Env.find_type p env with Not_found -> assert false in
  let args = List.map (fun _ -> Ctype.newvar ()) decl.type_params in
  let funs =
    List.map2 (fun v (co,cn,_) ->
		 (v,
		  ((if co then Some (Ident.create "to_ml_arg") else None),
		  (if cn then Some (Ident.create "from_ml_arg") else None))))
      args decl.type_variance in
  let inst t =
    let ps,t = Ctype.instance_parameterized_type decl.type_params t  in
    List.iter2 (Ctype.unify env) ps args;
    t in
  let arg i t =
    let newval = 
      transl_to_ml funs env (inst t) (Lprim (Pfield i,[Lvar interm])) in
    Lprim (Psetfield (i,true), [Lvar interm; newval]) in
  let e = 
  match decl.type_kind, decl.type_manifest with
    | Type_abstract, None ->
	builtin "Cduce_types.Value.get_abstract" [ Lvar v ]
    | (Type_variant (_,Private) | Type_record (_,_,Private)), _ ->
	builtin "Pervasives.failwith"
	  [ Lconst (Const_base (Const_string (Path.name p))) ]
    | Type_variant (cstrs,priv), _ ->
	let cstr map = function 
	  | (c,{ cstr_tag = Cstr_constant n }) ->
	      map
	  | (c,{ cstr_tag = Cstr_block n; cstr_args = args }) ->
	      let action = 
		match mapi arg 0 args with
		  | hd::tl -> 
		      List.fold_left (fun x y -> Lsequence (x,y)) hd tl
		  | _ -> assert false in
	      (n, action) :: map
	  | _ -> assert false in
	let cstrs = Datarepr.constructor_descrs (Ctype.newvar()) cstrs priv in
	let map = List.fold_left cstr [] cstrs in
	let transl = 
	  List.map (
	    function (c,{ cstr_tag = Cstr_constant n | Cstr_block n}) -> 
	      (Cduce_types.Atoms.atom (mk_atom c), n)
	      | _ -> assert false) cstrs in
	let transl = Cduce_types.Atoms.mk_map transl in
	Llet (Strict, interm,
	      builtin "Cduce_types.Value.cduce2ocaml_constr"
		[ global transl; Lvar v ],
	      Lsequence (switch_block (Lvar interm) map, Lvar interm))
    | Type_record (fields,repr,priv), _ ->
	let fields= Datarepr.label_descrs (Ctype.newvar ()) fields repr priv in
	let prim = match repr with
	  | Record_regular -> 
	      let mut = List.exists (fun (_,l) -> l.lbl_mut=Mutable) fields in
	      Pmakeblock (0, if mut then Mutable else Immutable)
	  | Record_float -> Pmakearray Pfloatarray in
	let fields = 
	  List.map (fun (lab,l) -> 
		      let v = builtin "Cduce_types.Value.get_field" [
			Lvar v;
			Lconst(Const_pointer (Cduce_types.Upool.int 
						(transl_label lab)))
		      ] in
		      transl_to_ml funs env (inst l.lbl_arg) v
		   ) fields in
	Lprim (prim, fields)
    | _ -> assert false
  in
  List.fold_right 
    (fun (_,(co,cn)) e -> 
       let e = 
	 match cn with Some a -> Lfunction (Curried, [a], e) | None -> e in
       let e = 
	 match co with Some a -> Lfunction (Curried, [a], e) | None -> e in
       e)
    funs
    (Lfunction (Curried, [v], e))

let transl_ext transl_exp env typ = function
  | Textexp_cst c ->
      global (Cduce_types.Value.const c)

  | Textexp_match (arg,bl) ->
      transl_ext_match transl_exp false (transl_exp arg) (get_ext_type arg) bl

  | Textexp_map (arg,bl) -> 
      let elem = Ident.create "elem" in
      let f =
	Lfunction 
	  (Curried, [elem], 
	   transl_ext_match transl_exp false (Lvar elem) 
	     (Cduce_types.Sequence.approx (get_ext_type arg)) bl) in
      builtin "Cduce_types.Value.transform" [ f; transl_exp arg ]
	 
  | Textexp_xmap (arg,bl,ty_arg) ->
      let elem = Ident.create "elem" in
      let f =
	Lfunction 
	  (Curried, [elem], 
	   transl_ext_match transl_exp true (Lvar elem) (get_ext ty_arg) bl) in
      builtin "Cduce_types.Value.xtransform" [ f; transl_exp arg ]

  | Textexp_op (op,args) ->
      builtin ("Cduce_types.Value."^op) (List.map transl_exp args)

  | Textexp_record fl ->
      let labels,fields = List.split (Cduce_types.Ident.LabelMap.get fl) in
      transl_record labels (List.map transl_exp fields)

  | Textexp_removefield (e,l) ->
      builtin "Cduce_types.Value.remove_field" [
	Lconst(Const_pointer (Cduce_types.Upool.int l));
	transl_exp e
      ]

  | Textexp_namespace e ->
      transl_exp e

  | Textexp_from_ml e ->
      transl_from_ml 
	[] e.exp_env (Ctype.correct_levels e.exp_type) (transl_exp e)

  | Textexp_to_ml e ->
      transl_to_ml [] env (Ctype.correct_levels typ) (transl_exp e)

  | Textexp_check e ->
      let typ_e = get_ext_type e in
      let typ_res = get_ext typ in
      let e = transl_exp e in
      if Cduce_types.Types.subtype typ_e typ_res then e 
      else
	let d = Cduce_types.Patterns.Compile.make_checker typ_e typ_res in
	builtin "Cduce_types.Explain.check_failure" [ global d; e ]
      
