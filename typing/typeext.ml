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

(* Typechecking for the CDuce extension *)

open Parsetree
open Types
open Btype
open Ctype

let anyext = { ext_const = None; ext_lb = []; ext_atoms = [] }
let anyext_var = newextvar anyext

module CT = Cduce_types.Types
module SEQ = Cduce_types.Sequence
module U = Cduce_types.Encodings.Utf8
module Ns = Cduce_types.Ns
module CEnv = Cduce_types.Ident.Env
module IdSet = Cduce_types.Ident.IdSet
module Patterns = Cduce_types.Patterns
module LabelMap = Cduce_types.Ident.LabelMap
module Label = Cduce_types.Ident.Label

let real_type_expect = ref (fun ?in_function _ _ _ -> assert false)
let transl_simple_type = ref None

let solve_env = ref Env.empty
let real_repr t = repr (expand_head !solve_env t)

type t = CT.t

let id x = Cduce_types.Ident.ident (Cduce_types.Ns.empty, U.mk_latin1 x)

type cannot_translate =
  | TUnregularRecursion
  | TUnguardedRecursion
  | TOpen
  | TArrow
  | TOther
  

type error = 
  | UnboundNamespacePrefix of string
  | DuplicatedLabel
  | CaptureNotAllowed of string
  | UnguardedRecursion
  | NotXmlType
  | EmptyType
  | Cyclic
  | NotSubtype of t * t
  | CannotTranslateML of Types.type_expr * cannot_translate
  | InvalidChar
  | PatError of string

exception Error of Location.t * error

let register_exttypes () =
  let types = Env.exttypes real_repr !solve_env in
  List.iter 
    (fun (p,ty) ->
       CT.Print.register_global
	 ""
	 (Cduce_types.Ns.empty,
	  Cduce_types.Encodings.Utf8.mk_latin1 p)
	 ty) types
  
let report_error ppf = function
  | UnboundNamespacePrefix pr ->
      Format.fprintf ppf "Unbound namespace prefix %s" pr
  | DuplicatedLabel ->
      Format.fprintf ppf "The same label appears several times in this record"
  | CaptureNotAllowed s ->
      Format.fprintf ppf "Capture variable not allowed in types: %s" s
  | UnguardedRecursion ->
      Format.fprintf ppf "Unguarded recursion"
  | NotXmlType ->
      Format.fprintf ppf "Not an XML type"
  | EmptyType ->
      Format.fprintf ppf "This definition yields an empty type"
  | NotSubtype (t1,t2) ->
      register_exttypes ();
      Format.fprintf ppf "Subtyping failed %a <= %a@."
	CT.Print.print t1
	CT.Print.print t2;
      Format.fprintf ppf "Sample:@.%a"
        Cduce_types.Sample.print
	(Cduce_types.Sample.get (CT.diff t1 t2))
  | Cyclic ->
      Format.fprintf ppf "Cycle detected: cannot type-check"
  | CannotTranslateML (t,e) ->
      Format.fprintf ppf "Cannot translate ML type: %a"
	Printtyp.type_expr t;
      (match e with
	 | TOpen -> Format.fprintf ppf "@.Type with variables."
	 | TArrow -> Format.fprintf ppf "@.Arrow type."
	 | TUnguardedRecursion -> Format.fprintf ppf "@.Unguarded recursion."
	 | TUnregularRecursion -> Format.fprintf ppf "@.Unregular recursion."
	 | TOther -> ())
  | InvalidChar ->
      Format.fprintf ppf "Invalid character literal"
  | PatError s ->
      Format.fprintf ppf "%s" s

let error loc err = raise (Error (loc,err))

let noloc = Location.none

(* Built-in types *)

let builtin_types = [ 
  "Int" , CT.Int.any;
  "Char" , CT.Char.any;
  "Any" , CT.any;
  "Empty" , CT.empty;
  "Atom" , CT.Atom.any;
  "String" , SEQ.string;
  "Latin1" , SEQ.string_latin1;
  "Int32" , (CT.interval Cduce_types.Intervals.int32);
  "Int64" , (CT.interval Cduce_types.Intervals.int64);
  "Arrow",  CT.Arrow.any;
  "Latin1Char", SEQ.char_latin1;
]

let built_ins =
  List.fold_left (
    fun accu (name,t) ->
      CT.Print.register_global ""
	(Ns.empty, U.mk_latin1 name) t;
      CEnv.add (id name) t accu
  ) CEnv.empty builtin_types

(* Solver *)

let to_eval = ref []


let check loc t ub =
  if CT.subtype t ub then t
  else error loc (NotSubtype (t,ub))

let compute_atom loc ub = function
  | { ext_atom_v = Atom_computed t } -> 
      check loc t ub
  | { ext_atom_v = Atom_computing } -> 
      error loc Cyclic
  | { ext_atom_v = Atom_start } as a -> 
      a.ext_atom_v <- Atom_computing; 
      let t = a.ext_atom_def ub in a.ext_atom_v <- Atom_computed t; 
      check loc t ub

let var_atoms v e ub =
  let atoms = ref [] and vs = ref [v] and consts = ref CT.empty in
  let rec aux e =  atoms := e.ext_atoms @ !atoms; List.iter aux2 e.ext_lb
  and aux2 (loc,v) =
    let v = real_repr v in
    if (List.memq v !vs) then () 
    else (vs := v :: !vs; match v.desc with
	    | Text { ext_const = Some t } -> 
		consts := CT.cup !consts (check loc t ub)
	    | Text e -> aux e
	    | _ -> assert false)
  in
  aux e;
  !atoms,!consts
  
let real_compute_var v e ub =
  let atoms,csts = var_atoms v e ub in
  List.fold_left 
    (fun accu (loc,at) -> CT.cup accu (compute_atom loc ub at)) 
    csts atoms

let compute_var loc v ub =
  let v = real_repr v in
  match v.desc with
    | Text { ext_const = Some t } -> check loc t ub
    | Text ({ ext_const = None } as e) -> 
	let t = real_compute_var v e ub in 
	v.desc <- Text { anyext with ext_const = Some t };
	t
    | _ -> assert false

let check_const v =
  match v.desc with
    | Text ({ ext_const = Some t } as e) ->
	ignore (real_compute_var v e t);
	v.desc <- Text { anyext with ext_const = Some t }
    | _ -> ()


let solve env =
  solve_env := env;
  try
    let vars = List.map real_repr (allextvars ()) in
    List.iter check_const vars;
    List.iter (fun v -> ignore (compute_var Location.none v CT.any)) vars;
    List.iter (fun z -> z ()) !to_eval;
    register_exttypes ()
  with exn -> to_eval := []; raise exn


(* From ML types to CDuce types *)

let rec seq = function
  | [] -> SEQ.nil_node
  | hd::tl -> CT.cons (CT.times hd (seq tl))
  
let ml_constr c p =
  let c = Cduce_types.Atoms.V.mk (Ns.empty,U.mk_latin1 c) in
  let c = CT.atom (Cduce_types.Atoms.atom c) in
  match p with
    | [] -> c
    | p ->
	let p = seq (CT.cons (CT.record_fields (false,LabelMap.empty)) :: p) in
	CT.xml (CT.cons c) p

type from_ml_env = { loc : Location.t;
		     tys : (type_expr * CT.Node.t) list;
		     seen : type_expr list;
		     constrs : (Path.t * (type_expr list) * CT.Node.t) list }

exception CannotTranslate of cannot_translate

let rec typ_from_ml env t =
  let t = real_repr t in
  if List.mem_assq t env.tys then List.assq t env.tys
  else if List.memq t env.seen then raise (CannotTranslate TUnguardedRecursion)
  else let env = { env with seen = t :: env.seen } in
  match t.desc with
    | Tconstr (p,_,_) when Path.same p Predef.path_unit ->
	SEQ.nil_node
    | Tconstr (p,_,_) when Path.same p Predef.path_int ->
	CT.cons (CT.Int.any)
    | Tconstr (p,_,_) when Path.same p Predef.path_int32 ->
	CT.cons (CT.interval Cduce_types.Intervals.int32)
    | Tconstr (p,_,_) when Path.same p Predef.path_int64 ->
	CT.cons (CT.interval Cduce_types.Intervals.int64)
    | Tconstr (p,_,_) when Path.same p Predef.path_char ->
	CT.cons SEQ.char_latin1
    | Tconstr (p,_,_) when Path.same p Predef.path_string ->
	CT.cons SEQ.string_latin1
    | Tconstr (p,[t],_) when Path.same p Predef.path_list ->
	SEQ.star_node (typ_from_ml env t)
    | Tconstr (p,[t],_) when Path.same p Predef.path_array ->
	SEQ.star_node (typ_from_ml env t)
    | Tconstr (p,_,_) when Path.name p = "Cduce_types.Value.t" ->
	CT.any_node
    | Tconstr (p,_,_) when Path.same p Predef.path_exn ->
	raise (CannotTranslate TOther)
    | Ttuple tl ->
	CT.cons (CT.tuple (List.map (typ_from_ml env) tl))
    | Text _ ->
	CT.cons (compute_var env.loc t CT.any)
    | Tconstr (p,args,_) ->
	typ_from_ml_constr env p args
(*    | Tarrow (_,t1,t2,_) ->
	CT.cons (CT.arrow (typ_from_ml env t1) (typ_from_ml env t2)) *)
    | Tarrow _ ->
	raise (CannotTranslate TArrow)
    | Tvariant rd ->
	let rd = Btype.row_repr rd in
	if not rd.row_closed then raise (CannotTranslate TOpen);
	let fields = Ctype.get_fields rd in
	CT.cons 
	  (List.fold_left
	     (fun accu (lab,f) -> 
		CT.cup accu (ml_constr lab (match f with None -> []
					      | Some t -> [typ_from_ml env t]))
	     ) CT.empty (Ctype.get_fields rd))
    | Tvar ->
	raise (CannotTranslate TOpen)
    | _ -> 
	raise (CannotTranslate TOther)

and typ_from_ml_constr env p args =
  let args = List.map real_repr args in
  let ty_args = List.map (typ_from_ml env) args in
  let env = { env with tys = (List.combine args ty_args) @ env.tys } in
  try 
    let (_,args',n) = 
      List.find (fun (p',_,_) -> Path.same p p') env.constrs in
    List.iter2 (Ctype.unify !solve_env) args args';
    n
  with Ctype.Unify _ -> raise (CannotTranslate TUnregularRecursion) | Not_found -> 
    let n = CT.make () in
    let env = { env with constrs = (p,args,n) :: env.constrs } in
    let decl = 
      try Env.find_type p !solve_env
      with Not_found -> raise (CannotTranslate TOther) in
    let inst t =
      let ps,t = Ctype.instance_parameterized_type decl.type_params t  in
      List.iter2 (Ctype.unify !solve_env) ps args;
      t in
    let t : CT.t = 
      match decl.type_kind, decl.type_manifest with
	  (* 	       | Type_abstract, Some t -> typ_from_ml env (inst t) *)
	| Type_abstract, None when Path.same p Predef.path_float ->
	    CT.abstract (CT.Abstract.atom (Path.name p))
	| Type_variant (cstrs,_), _ ->
	    List.fold_left
	      (fun accu (c,p) ->
		 let p = List.map inst p in
		 let p = List.map (typ_from_ml env) p in
		 let t = ml_constr c p in
		 CT.cup accu t)
	      CT.empty cstrs
	| Type_record (fields,_,_), _ ->
	    let fields =
	      LabelMap.from_list (fun _ _ -> assert false)
		(
		  List.map
		    (fun (lab,_,t) ->
		       let t = typ_from_ml env (inst t) in
		       (Label.mk (Ns.empty,U.mk_latin1 lab), t)
		    ) fields) in
	    CT.record_fields (false,fields)
	| _ -> raise (CannotTranslate TOther) in
    CT.define n t;
    n
      
let typ_from_ml loc t = 
  try CT.descr (typ_from_ml { loc = loc; tys = []; seen =  []; constrs = [] }  t)
  with CannotTranslate e -> error loc (CannotTranslateML (t,e))

(*********************)


let parse_ns env loc pr =
  try Env.find_namespace pr env
  with Cduce_types.Ns.UnknownPrefix ns ->
    error loc (UnboundNamespacePrefix (U.to_string ns))

let parse_atom env loc (pr,l) =
  let ns = parse_ns env loc pr in
  Cduce_types.Atoms.V.mk (ns,l)
 
let transl_label env loc (pr,l) =
  let ns = if (pr = "") then Ns.empty else parse_ns env loc pr in
  Cduce_types.Ident.Label.mk (ns,l)

let transl_record (env : Env.t) loc f r :
    'a Cduce_types.Ident.label_map =
  let r = List.map (fun (q,x) -> (transl_label env loc q, f x)) r in
  LabelMap.from_list (fun _ _ ->  error loc DuplicatedLabel) r

let rec transl_const env c = match c.pextcst_desc with
  | Pextcst_pair (c1,c2) -> CT.Pair (transl_const env c1, transl_const env c2)
  | Pextcst_xml (c1,c2,c3) -> 
      CT.Xml (transl_const env c1, 
	      CT.Pair (transl_const env c2,transl_const env c3))
  | Pextcst_record f -> 
      CT.Record (transl_record env c.pextcst_loc (transl_const env) f)
  | Pextcst_atom q -> CT.Atom (parse_atom env c.pextcst_loc q)
  | Pextcst_int i -> CT.Integer i
  | Pextcst_char s -> 
      let idx = U.start_index s in
      let (i,idx) = U.next s idx in
      if not (U.equal_index idx (U.end_index s)) then 
	error c.pextcst_loc InvalidChar;
      CT.Char (Cduce_types.Chars.V.mk_int i)
  | Pextcst_string s -> CT.String (U.start_index s,U.end_index s,s,SEQ.nil_cst)
  | Pextcst_intern c -> c

(* Eliminate Recursion, propagate Sequence Capture Variables *)

open Cduce_types.Typepat

(* This environment is used in phase (1) to eliminate recursion *)

type penv = {
  penv_tenv : Env.t;
  penv_derec : node CEnv.t;
}

let penv tenv = { penv_tenv = tenv; penv_derec = CEnv.empty }

let all_delayed = ref []
let clean_on_err () = all_delayed := []
let delayed loc = let s = mk_delayed () in all_delayed := (loc,s) :: !all_delayed; s
let check_one_delayed (loc,p) = if not (check_wf p) then error loc UnguardedRecursion
let check_delayed () = 
  let l = !all_delayed in all_delayed := []; List.iter check_one_delayed l

let pcdata =
  Pext_star (Pext_elem { pext_loc = Location.none;
			 pext_desc = Pext_cst CT.Char.any })

let rec derecurs env p = match p.pext_desc with
  | Pext_name v -> derecurs_var env p.pext_loc v
  | Pext_recurs (p,b) -> derecurs (fst (derecurs_def env b)) p
  | Pext_cst t -> mk_type t
  | Pext_ns ns -> mk_type 
      (CT.atom (Cduce_types.Atoms.any_in_ns (parse_ns env.penv_tenv p.pext_loc ns)))
  | Pext_or (p1,p2) -> mk_or (derecurs env p1) (derecurs env p2)
  | Pext_and (p1,p2) -> mk_and (derecurs env p1) (derecurs env p2)
  | Pext_diff (p1,p2) -> mk_diff (derecurs env p1) (derecurs env p2)
  | Pext_prod (p1,p2) -> mk_prod (derecurs env p1) (derecurs env p2)
  | Pext_xml (p1,p2) -> mk_xml (derecurs env p1) (derecurs env p2)
  | Pext_arrow (p1,p2) -> mk_arrow (derecurs env p1) (derecurs env p2) 
  | Pext_optional p -> mk_optional (derecurs env p)
  | Pext_record (o,r) -> 
      let aux = function
	| (p,Some e) -> (derecurs env p, Some (derecurs env e))
	| (p,None) -> derecurs env p, None in
      mk_record o (transl_record env.penv_tenv p.pext_loc aux r)
  | Pext_bind (x,c) -> 
      mk_constant (id x) (transl_const env.penv_tenv c)
  | Pext_constant c -> mk_type (CT.constant (transl_const env.penv_tenv c))
  | Pext_regexp r -> rexp (derecurs_regexp env r) 
  | Pext_from_ml t ->
      (match !transl_simple_type with
	 | None -> assert false
	 | Some f ->
	     mk_type (
	       typ_from_ml p.pext_loc
		 (f env.penv_tenv true t)))
  | Pext_concat (p1,p2) ->  mk_concat (derecurs env p1) (derecurs env p2)
  | Pext_merge (p1,p2) -> mk_merge (derecurs env p1) (derecurs env p2)

and derecurs_regexp env = function
  | Pext_epsilon -> mk_epsilon
  | Pext_elem { pext_desc = Pext_name (Longident.Lident "PCDATA") } ->
      derecurs_regexp env pcdata
  | Pext_elem p -> mk_elem (derecurs env p)
  | Pext_guard p -> mk_guard (derecurs env p)
  | Pext_seq (p1,p2) -> mk_seq (derecurs_regexp env p1) (derecurs_regexp env p2)
  | Pext_alt (p1,p2) -> mk_alt (derecurs_regexp env p1) (derecurs_regexp env p2)
  | Pext_star p -> mk_star (derecurs_regexp env p)
  | Pext_weakstar p -> mk_weakstar (derecurs_regexp env p) 
  | Pext_capture (x,p) -> mk_seqcapt (id x) (derecurs_regexp env p)

and derecurs_var env loc v =
  match v with
    | Longident.Lident x ->
	let id = id x in
	(try
	   try mk_type (CEnv.find id built_ins)
	   with Not_found ->
	     try CEnv.find id env.penv_derec
	     with Not_found -> derecurs_var' env loc v
	 with Not_found -> mk_capture id)
    | _ -> 
	try derecurs_var' env loc v
	with Not_found -> error loc NotXmlType

and derecurs_var' env loc v = 
  let (p,d) = Env.lookup_type v env.penv_tenv in
  match d.Types.type_manifest with
     | Some ty ->
	 let ty = repr (expand_head env.penv_tenv ty) in
	 (match ty.desc with
	    | Types.Text { Types.ext_const = Some t } -> mk_type t
	    | Types.Text _ when !extmode -> mk_type CT.any (* ??? *) 
	    | _ -> 
(*		Format.fprintf Format.std_formatter
		  "Type:%a@.extmode:%b@."
		  Printtyp.raw_type_expr ty !extmode; *)
		error loc NotXmlType)
     | _ -> error loc NotXmlType
	
and derecurs_def env b =
  let b = List.map 
    (fun (v,p) -> (id v,p,delayed p.pext_loc)) b in
  let n = 
    List.fold_left (fun env (v,p,s) -> CEnv.add v s env) env.penv_derec b in
  let env = { env with penv_derec = n } in
  List.iter (fun (v,p,s) -> link s (derecurs env p)) b;
  env, b

let derec penv p =
  let d = derecurs penv p in
  elim_concats ();
  check_delayed ();
  internalize d;
  d

let check_no_fv loc n =
  match peek_fv n with
    | None -> ()
    | Some x -> error loc (CaptureNotAllowed (Cduce_types.Ident.to_string x))

let wrap loc f x =
  try f x
  with Patterns.Error s -> clean_on_err (); error loc (PatError s)
    | exn -> clean_on_err (); raise exn


let transl_ext_type_node env p =
  solve_env := env;
(*  assert (not !extmode); *)
  wrap p.pext_loc (fun p ->
	  let d = derec (penv env) p in
	  check_no_fv p.pext_loc d;
	  typ_node d) p

    
let transl_ext_pat env p = 
  solve_env := env;
(*  assert (not !extmode); *)
  wrap p.pext_loc (fun p ->
		     let d = derec (penv env) p in
		     pat_node d) p


let transl_ext_type env p =
  if !extmode then anyext_var
  else
    let t = CT.descr (transl_ext_type_node env p) in
    Btype.newextvar 
      { Types.ext_const = Some t; Types.ext_atoms = []; Types.ext_lb = [] }

let transl_type_decl env b =
  if !extmode then
    List.map (fun _ -> CT.any) b
  else (
  solve_env := env;
  (* Mmmh, if t is already defined, type t = {{ {: t :} }} will be
     accepted *)

  let _,b' = derecurs_def (penv env) b in
  elim_concats ();
  check_delayed ();
  let aux loc d = internalize d; check_no_fv loc d; typ d in
  let ts = 
    List.map
      (fun (v',p,d) -> 
	 let t = aux p.pext_loc d in
	 if (p.pext_loc <> noloc) && (CT.is_empty t) then error p.pext_loc EmptyType;
	 t) b' in
  List.iter2 
    (fun (v,_) t -> 
       CT.Print.register_global "" (Ns.empty, U.mk_latin1 v) t) 
    b ts;
  ts)

let transl_type_decl env =
  wrap noloc (transl_type_decl env)

let transl_branches env b =
  let t,l = 
    List.fold_left 
      (fun (acc,bl) (p,e) -> 
	 let loc = p.pext_loc in
	 let p = transl_ext_pat env p in
	 let a = CT.descr (Patterns.accept p) in
	 (CT.cup acc a),(CT.diff a acc,loc,p,e)::bl
      ) (CT.empty,[]) b
  in
  t, List.rev l






(*****************************************************************************)
(* TODO: return a lazy type to avoid computing it when not necessary *)


let exptypes = ref []

let register_ext_annot env s e =
  match (repr (expand_head env e.Typedtree.exp_type)).desc with
    | Tvar -> exptypes := (s,e) :: !exptypes
    | Text _ -> s.Parsetree.pexp_ext <- true
    | _ -> ()

let flush_ext_annot env =
  List.iter 
    (fun (s,e) ->
       match (repr (expand_head env e.Typedtree.exp_type)).desc with
	 | Text _ -> s.Parsetree.pexp_ext <- true
	 | _ -> ()
    ) !exptypes;
  exptypes := []





let force_ext env loc t =
  let a = newextvar anyext in
  unify env a t

let pair_subtype loc =
  let a = newextvar anyext in
  let b = newextvar { anyext with ext_lb = [loc,a] } in
  (a,b)
  
let ext_subtype env loc t =
  let (a,b) = pair_subtype loc in
  unify env t b;
  a
  
let ext_supertype env loc t =   
  let (a,b) = pair_subtype loc in
  unify env t a;
  b

let ext_cst env loc c =
  if !extmode then anyext_var
  else newextvar { ext_const = Some (CT.constant c); 
		   ext_lb = []; ext_atoms = [] }

let atom loc f =
  newextvar { anyext with ext_atoms = 
      [ loc, { ext_atom_v = Atom_start; ext_atom_def = f } ] }

let ext_ub env loc ub e =
  if !extmode then anyext_var 
  else atom loc (fun _ -> compute_var loc e ub)


let ext_from_ml env loc t =
  if !extmode then anyext_var 
  else atom loc (fun _ -> typ_from_ml loc t)

let ext_to_ml env loc ml ext =
  if !extmode then ()
  else ignore (atom loc (fun _ -> compute_var loc ext (typ_from_ml loc ml)))

let ext_int2 f env loc e1 e2 =
  if !extmode then anyext_var 
  else atom loc 
    (fun ub ->
       let t1 = compute_var loc e1 CT.Int.any in
       let t2 = compute_var loc e2 CT.Int.any in
       CT.interval
	 (f (CT.Int.get t1) (CT.Int.get t2)))

let ext_merge env loc e1 e2 =
  if !extmode then anyext_var 
  else atom loc 
    (fun ub ->
       let t1 = compute_var loc e1 CT.Record.any in
       let t2 = compute_var loc e2 CT.Record.any in
       CT.Record.merge t1 t2)

let add_values =
  List.fold_left
    (fun env (x,v) -> Env.add_value x { val_type = v; val_kind = Val_reg } env)

let ext_branch env loc t p a =
  let module P = Cduce_types.Patterns in
  let fv = P.fv p in
  let ids = 
    List.map (fun x -> x, Ident.create (Cduce_types.Ident.to_string x)) fv in
  let vars = 
    if !extmode then List.map (fun (_,x) -> x, anyext_var) ids
    else
      let z = lazy (
	let ta = compute_var loc a CT.any in
	let ta = CT.cap t ta in
	if (CT.is_empty ta) && (loc != Location.none) then
	  Location.prerr_warning loc Warnings.Unused_match;
	P.filter ta p 
      ) in
      to_eval := (fun () -> ignore (Lazy.force z)) :: !to_eval; 
      (* to get a warning for unused
	 branch without a capture variable *)
      List.map
	(fun (x,id) ->
	   id, 
	   atom loc (fun _ -> CT.descr (Cduce_types.Ident.IdMap.assoc x (Lazy.force z))))
	ids
  in
  ids, add_values env vars

let ext_pair env loc e1 e2 =
  if !extmode then anyext_var
  else atom loc
    (fun ub ->
       let t = CT.Product.get ub in
       let t1 = compute_var loc e1 (CT.Product.pi1 t) in
       let t2 = compute_var loc e2 (CT.Product.pi2 t) in
       CT.times (CT.cons t1) (CT.cons t2))

let ext_record (env : Env.t) loc fl =
  if !extmode then anyext_var
  else atom loc
    (fun ub ->
       let fl = 
	 LabelMap.map 
	   (fun e -> CT.cons (compute_var loc e CT.any))
	   fl in
       CT.record_fields (false,fl))

let ext_concat env loc e1 e2 =
  if !extmode then anyext_var
  else atom loc
    (fun ub ->
       let t1 = compute_var loc e1 SEQ.any in
       let t2 = compute_var loc e2 SEQ.any in
       SEQ.concat t1 t2)

let ext_xml env loc e1 e2 e3 =
  if !extmode then anyext_var
  else atom loc
    (fun ub ->
       let t1 = compute_var loc e1 CT.any in
       let t2 = compute_var loc e2 CT.any in
       let t3 = compute_var loc e3 CT.any in
       CT.xml
	 (CT.cons t1) (CT.cons (CT.times (CT.cons t2) (CT.cons t3))))

let ext_apply env loc e1 e2 =
  if !extmode then anyext_var
  else atom loc
    (fun ub ->
       let module A = CT.Arrow in
       let t1 = compute_var loc e1 A.any in
       let t1 = A.get t1 in
       let dom = A.domain t1 in
       let t2 = compute_var loc e2 (A.domain t1)  in
       A.apply t1 t2)

let ext_map env loc e0 e1 brs =
  if !extmode then (unify env e1 anyext_var; anyext_var)
  else 
    let z = lazy (
      let t0 = compute_var loc e0 CT.any in
      let tl,f = SEQ.map_mono t0 in
      let u = List.fold_left CT.cup CT.empty tl in
      (u,(tl,f))
    ) in
    unify env e1 (atom loc (fun ub -> fst (Lazy.force z)));
    atom loc 
      (fun ub ->
	 let (tl,f) = snd (Lazy.force z) in
	 let aux t = 
	   List.fold_left
	     (fun accu (s,e) ->
		if (CT.disjoint t s) then accu
		else CT.cup accu (compute_var loc e SEQ.any))
	     CT.empty brs
	 in
	 SEQ.flatten (f (List.map aux tl))
      )

let ext_xmap env loc domain e0 e1 brs =
  if !extmode then (unify env e1 anyext_var; anyext_var)
  else 
    let z = lazy (
      let t0 = compute_var loc e0 CT.any in
      let u, tl,f = SEQ.map_tree_mono domain t0 in
      (u,(tl,f))
    ) in
    unify env e1 (atom loc (fun ub -> fst (Lazy.force z)));
    atom loc 
      (fun ub ->
	 let (tl,f) = snd (Lazy.force z) in
	 let aux t = 
	   List.fold_left
	     (fun accu (s,e) ->
		if (CT.disjoint t s) then accu
		else CT.cup accu (compute_var loc e SEQ.any))
	     CT.empty brs
	 in
	 f (List.map aux tl)
      )
     
let ext_removefield env loc e l =
  if !extmode then anyext_var 
  else atom loc 
    (fun ub ->
       let t = compute_var loc e CT.Record.any in
       CT.Record.remove_field t l)

let ext_op env loc op tl = match op,tl with
  | "concat",[t1;t2] -> ext_concat env loc t1 t2
  | "add",[t1;t2] -> ext_int2 Cduce_types.Intervals.add env loc t1 t2
  | "merge",[t1;t2] -> ext_merge env loc t1 t2
  | "mul",[t1;t2] -> ext_int2 Cduce_types.Intervals.mul env loc t1 t2
  | "sub",[t1;t2] -> ext_int2 Cduce_types.Intervals.sub env loc t1 t2
  | "div",[t1;t2] -> ext_int2 Cduce_types.Intervals.div env loc t1 t2
  | "modulo",[t1;t2] -> ext_int2 Cduce_types.Intervals.modulo env loc t1 t2
  | "pair",[t1;t2] -> ext_pair env loc t1 t2
  | "xml",[t1;t2;t3] -> ext_xml env loc t1 t2 t3
(*  | "apply",[t1;t2] -> ext_apply env loc t1 t2 *)
  | _ -> assert false

  



open Typedtree

let type_expect ?in_function env sexp ty_expected =
  if !extmode then 
    let e = !real_type_expect ?in_function env sexp ty_expected in
    register_ext_annot env sexp e; 
    e
  else
    if sexp.pexp_ext 
    then 
      let t = ext_subtype env sexp.pexp_loc ty_expected in
      let e = !real_type_expect ?in_function env sexp t in
      { e with exp_type = ty_expected }
    else
      !real_type_expect ?in_function env sexp ty_expected


let type_ext env e =
  type_expect env e 
    (newextvar { ext_const = None; ext_lb = []; ext_atoms = [] })

let type_expression env loc = function
  | Pextexp_cst cst ->
      let cst = transl_const env cst in
      Textexp_cst cst, 
      ext_cst env loc cst
  | Pextexp_match (e0,bl) ->
      let e0 = type_ext env e0 in
      let res = newvar () in

      let (acc,bl) = transl_branches env bl in
      let t0 = ext_ub env e0.exp_loc acc e0.exp_type in
      let brs = List.map
	(fun (t,loc,p,e) ->
	   let binds,newenv = ext_branch env loc t p t0 in
	   let e = type_expect newenv e res in
	   (p,binds,e)) bl in
      Textexp_match (e0,brs), 
      res
  | Pextexp_map (e0,bl) ->
      let e0 = type_ext env e0 in
      let (acc,bl) = transl_branches env bl in
      let acc = Cduce_types.Sequence.star acc in
      let t0 = ext_ub env e0.exp_loc acc e0.exp_type in
      let t1 = newvar () in
      let brs = List.map
	(fun (t,loc,p,e) ->
	   let binds,newenv = ext_branch env loc t p t1 in
	   let e = type_ext newenv e in
	   (p,binds,e)) bl in
      let tres = 
	ext_map env loc t0 t1
	  (List.map2 (fun (t,_,_,_) (_,_,e) -> (t,e.exp_type)) bl brs) in
      Textexp_map (e0,brs), 
      tres
  | Pextexp_xmap (e0,bl) ->
      let e0 = type_ext env e0 in
      let (acc,bl) = transl_branches env bl in
      let t0 = ext_ub env e0.exp_loc Cduce_types.Sequence.any_xtransformable 
	e0.exp_type in
      let t1 = newvar () in
      let brs = List.map
	(fun (t,loc,p,e) ->
	   let binds,newenv = ext_branch env loc t p t1 in
	   let e = type_ext newenv e in
	   (p,binds,e)) bl in
      let tres = 
	ext_xmap env loc acc t0 t1
	  (List.map2 (fun (t,_,_,_) (_,_,e) -> (t,e.exp_type)) bl brs) in
      Textexp_xmap (e0,brs,t1), 
      tres
  | Pextexp_op (op,el) ->
      let el = List.map (type_ext env) el in
      Textexp_op (op,el), 
      ext_op env loc op (List.map (fun e -> e.exp_type) el)
  | Pextexp_record fl ->
      let fl = transl_record env loc (type_ext env) fl in
      Textexp_record fl,
      ext_record env loc (LabelMap.map (fun e -> e.exp_type) fl)
  | Pextexp_removefield (e,l) ->
      let e = type_ext env e in
      let l = transl_label env loc l in
      Textexp_removefield (e,l),
      ext_removefield env loc e.exp_type l
  | Pextexp_namespace (pr,ns,e) ->
      let e = type_expect (Env.add_namespace pr ns env) e (newvar ()) in
      Textexp_namespace e,
      e.exp_type
  | Pextexp_from_ml e ->
      let t = new_global_var () in (* Prevent generalization *)
      let e = type_expect env e t in
      let t = ext_from_ml env loc t in
      Textexp_from_ml e, t
  | Pextexp_to_ml e ->
      let t = new_global_var () in  (* Prevent generalization *)
      let e = type_ext env e in
      ext_to_ml env loc t e.exp_type;
      Textexp_to_ml e, t
  | Pextexp_check (e,t) ->
      let e = type_ext env e in
      let t = transl_ext_type env t in
      Textexp_check e,
      t
      

let annot env sexp e = 
  if !extmode then (register_ext_annot env sexp e; e)
  else (
    if sexp.pexp_ext 
    then { e with exp_type = ext_supertype env e.exp_loc e.exp_type }
    else e
  )

let subtype_loc = ref Location.none

let () =
  Ctype.ext_subtype := 
    (fun env t -> 
       if !extmode then anyext_var else ext_subtype env !subtype_loc t);
  Ctype.ext_supertype := 
    (fun env t -> 
       if !extmode then anyext_var else ext_supertype env !subtype_loc t)
