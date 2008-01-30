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

(* Converting type expression to rtype *)

open Location
open Longident
open Asttypes
open Parsetree
open Path
open Types

type error = 
    Unsupported 
  | Cannot_have_full_path 
  | Multiply_bound_type_variable

exception Error of Location.t * error

(* type of rtype must be got dynamically, 
   since Rtype is not available in the compilation of stdlib *)
let get_rtype_path () =
  try
    fst (Env.lookup_type (Ldot (Lident "Rtype", "type_expr")) Env.empty)
  with
  | Not_found ->
      Misc.fatal_error ("Primitive type Rtype.type_expr not found.")

let get_rtype_type () =
  Btype.newgenty (Tconstr (get_rtype_path (), [], ref Mnil))

(* type declaration of rtype *)
let get_rtype_type_declaration env =
  let rtypedecl_path = 
    if not !Clflags.nobuiltintypes then
      try
	fst (Env.lookup_type (Ldot (Lident "Rtype", "type_declaration")) env)
      with
      | Not_found ->
	  Misc.fatal_error ("Primitive type Rtype.type_declaration not found.")
      else
	try
	  fst (Env.lookup_type (Lident "type_declaration") env)
	with
	| Not_found ->
	  Misc.fatal_error ("Primitive type type_declaration not found.")
  in
  Btype.newgenty (Tconstr (rtypedecl_path, [], ref Mnil))

(* Longidents *)

let cstr name = Ldot (Lident "Rtype", name)
let cstr_path name = Ldot (Ldot (Lident "Rtype", "Path"), name)

(* Patterns *)

let make_pat loc desc = { ppat_desc= desc; ppat_loc= loc }

let make_pat_construct loc lid args =
  let args =
    match args with
    | [] -> None
    | [x] -> Some x
    | _ -> Some (make_pat loc (Ppat_tuple args))
  in
  make_pat loc (Ppat_construct (lid, args, false))

let rec mktailpat = function
    [] -> make_pat Location.none (Ppat_construct(Lident "[]", None, false))
  | p1 :: pl ->
      let pat_pl = mktailpat pl in
      let l = {loc_start = p1.ppat_loc.loc_start;
               loc_end = pat_pl.ppat_loc.loc_end;
               loc_ghost = true}
      in
      let arg = make_pat l (Ppat_tuple [p1; pat_pl]) in
      make_pat l (Ppat_construct(Lident "::", Some arg, false))

let pattern_of_longident trans loc lid = 
  (* first of all, we have to translate the longident to a path! *)
  let path = 
    try trans lid with Not_found ->
      raise (Typetexp.Error(loc, Typetexp.Unbound_type_constructor lid))
  in
  let rec pattern_of_path = function
    | Pident id ->
	make_pat_construct loc (cstr_path "Pident")
	  [make_pat loc (Ppat_constant (Const_string (Ident.name id)));
	   make_pat loc (Ppat_constant (Const_int (Ident.stamp id)))]
    | Pdot (path, str, n) ->
	make_pat_construct loc (cstr_path "Pdot")
	  [pattern_of_path path;
	   make_pat loc (Ppat_constant (Const_string str));
	   make_pat loc (Ppat_constant (Const_int n))]
    | Papply (p1, p2) ->
	make_pat_construct loc (cstr_path "Papply")
	  [pattern_of_path p1; pattern_of_path p2]
  in
  pattern_of_path path
      
(* find non linear pattern type variables *)
let non_linear_vars t =
  let tbl = Hashtbl.create 17 in
  let rec scan_rec t =
    match t.ptyp_desc with
    | Ptyp_lident _ -> ()
    | Ptyp_var n -> 
	begin
	  let cntr = 
	    try Hashtbl.find tbl n with 
	    | Not_found ->
		let cntr = ref 0 in
		Hashtbl.add tbl n cntr;
		cntr
	  in
	  incr cntr
	end
    | Ptyp_arrow (_,t1,t2) -> scan_rec t1; scan_rec t2
    | Ptyp_tuple ts -> List.iter scan_rec ts
    | Ptyp_constr (_,ts) -> List.iter scan_rec ts
    | _ -> raise (Error (t.ptyp_loc, Unsupported))
  in
  scan_rec t;
  Hashtbl.fold (fun n cntr st -> if !cntr > 1 then n :: st else st) tbl []

let get_pattern_id =
  let cntr = ref 0 in
  fun () -> incr cntr; !cntr

type non_linear_type_variable_info = {
    varinfo_name : string; (* orignal name *)
    varinfo_variables : string list (* pattern variables for occurrences *)
  }

type implicit_when = Parsetree.expression * Parsetree.expression

(* This function compiles data type pattern to run-time type patterns *)
(*
let pattern_of_type non_linear transl_longident t =
  let pattern_id = get_pattern_id () in
  let make_name name id = Printf.sprintf "%s*%d-%d" name id pattern_id  in
  let make_var name =
    { pexp_desc= Pexp_ident (Lident name);
      pexp_loc= Location.none }
  in
  let non_linear_tvars = non_linear_vars t in
  let non_linear_check_tbl = Hashtbl.create 17 in
  let tvars = ref [] in
  let rec pattern_of_type t =
    match t.ptyp_desc with
    | Ptyp_lident lid ->
        begin match lid with
        | Lident name -> make_pat t.ptyp_loc (Ppat_var name)
        | _ -> raise (Error (t.ptyp_loc, Cannot_have_full_path))
        end
    | _ ->
        let pat = 
	  make_pat t.ptyp_loc 
 	    (Ppat_record [cstr "desc", pattern_of_desc t.ptyp_loc t.ptyp_desc])
	in
	match t.ptyp_desc with
	| Ptyp_var n when non_linear && List.mem n non_linear_tvars ->
	    (* ({desc= Ptyp_var n} as nx) *)
	    let name =
	      try
		let pexp, cntr, checks = Hashtbl.find non_linear_check_tbl n in
		incr cntr;
		let name = make_name ("'" ^ n) !cntr in
		checks := (pexp, make_var name) :: !checks;
		name
	      with
	      | Not_found ->
		let name = make_name ("'" ^ n) 0 in
		Hashtbl.add non_linear_check_tbl n 
		  (make_var name, ref 0, ref []);
		name
	    in
	    make_pat t.ptyp_loc (Ppat_alias (pat, name))
	| _ -> pat
      
  and pattern_of_desc loc = function
    | Ptyp_var n -> 
	if not non_linear && List.mem n !tvars then 
	  raise (Error (loc, Multiply_bound_type_variable))
	else begin
	  tvars := n :: !tvars;
          make_pat_construct loc (cstr "Tvar") []
	end
    | Ptyp_arrow (l, t1, t2) -> 
        make_pat_construct loc (cstr "Tarrow")
  	  [ make_pat loc (Ppat_constant (Const_string l));
  	    pattern_of_type t1;
  	    pattern_of_type t2 ]
    | Ptyp_tuple ts ->
        make_pat_construct loc (cstr "Ttuple") 
 	  [ mktailpat (List.map pattern_of_type ts) ]
    | Ptyp_constr (lid, ts) ->
        make_pat_construct loc (cstr "Tconstr")
  	  [ make_pat loc (Ppat_tuple 
 			    [ pattern_of_longident transl_longident loc lid;
 			      make_pat loc Ppat_any ]);
  	    mktailpat (List.map pattern_of_type ts)]
    | Ptyp_lident _ -> assert false
    | _ -> raise (Error (loc, Unsupported))
  in
  let pat = pattern_of_type t in
  let implicit_whens = 
    Hashtbl.fold (fun n (_, cntr, checks) st -> !checks @ st) 
      non_linear_check_tbl []
  in
  pat, implicit_whens
*)
    
(* Here, we compile to data types are compiled to a pattern variable.
   But, in addition to this, we have a (==) check of type declarations *)
let pattern_of_type non_linear transl_longident t =
  let pattern_id = get_pattern_id () in
  let make_name name id = Printf.sprintf "%s*%d-%d" name id pattern_id  in
  let make_var loc name =
    { pexp_desc= Pexp_ident (Lident name);
      pexp_loc= loc }
  in
  let non_linear_tvars = non_linear_vars t in
  let non_linear_check_tbl = Hashtbl.create 17 in
  let tvars = ref [] in

  let cntr_decl = ref 0 in
  let checks_decl = ref [] in

  let rec pattern_of_type t =
    match t.ptyp_desc with
    | Ptyp_lident lid ->
        begin match lid with
        | Lident name -> make_pat t.ptyp_loc (Ppat_var name)
        | _ -> raise (Error (t.ptyp_loc, Cannot_have_full_path))
        end
    | _ ->
        let pat = 
	  make_pat t.ptyp_loc 
 	    (Ppat_record [cstr "desc", pattern_of_desc t.ptyp_loc t.ptyp_desc])
	in
	match t.ptyp_desc with
	| Ptyp_var n when non_linear && List.mem n non_linear_tvars ->
	    (* ({desc= Ptyp_var n} as nx) *)
	    let name =
	      try
		let pexp, cntr, checks = Hashtbl.find non_linear_check_tbl n in
		incr cntr;
		let name = make_name ("'" ^ n) !cntr in
		checks := (pexp, make_var t.ptyp_loc name) :: !checks;
		name
	      with
	      | Not_found ->
		let name = make_name ("'" ^ n) 0 in
		Hashtbl.add non_linear_check_tbl n 
		  (make_var t.ptyp_loc name, ref 0, ref []);
		name
	    in
	    make_pat t.ptyp_loc (Ppat_alias (pat, name))
	| _ -> pat
      
  and pattern_of_desc loc = function
    | Ptyp_var n -> 
	if not non_linear && List.mem n !tvars then 
	  raise (Error (loc, Multiply_bound_type_variable))
	else begin
	  tvars := n :: !tvars;
          make_pat_construct loc (cstr "Tvar") []
	end
    | Ptyp_arrow (l, t1, t2) -> 
        make_pat_construct loc (cstr "Tarrow")
  	  [ make_pat loc (Ppat_constant (Const_string l));
  	    pattern_of_type t1;
  	    pattern_of_type t2 ]
    | Ptyp_tuple ts ->
        make_pat_construct loc (cstr "Ttuple") 
 	  [ mktailpat (List.map pattern_of_type ts) ]
    | Ptyp_constr (lid, ts) ->
	let name = make_name "decl" !cntr_decl in
	let v = make_var loc name in
	incr cntr_decl;
	checks_decl := (v, {pexp_desc= Pexp_typedecl lid; 
			    pexp_loc= Location.none }) :: !checks_decl;
        make_pat_construct loc (cstr "Tconstr")
  	  [ make_pat loc (Ppat_tuple 
 			    [ make_pat loc Ppat_any; (* ignore path *)
			      make_pat loc (Ppat_var name) ]);
  	    mktailpat (List.map pattern_of_type ts)]
    | Ptyp_lident _ -> assert false
    | _ -> raise (Error (loc, Unsupported))
  in
  let pat = pattern_of_type t in
  let implicit_whens = 
    Hashtbl.fold (fun n (_, cntr, checks) st -> !checks @ st) 
      non_linear_check_tbl !checks_decl
  in
  pat, implicit_whens
    
(* rtype -> type_expr *)

let recover_ident (name,pos) =
  Ident.create_with_stamp name pos

let rec recover_path = function
  | Rtype.Path.Pident id -> Pident (recover_ident id)
  | Rtype.Path.Pdot (p, name, pos) -> Pdot (recover_path p, name, pos)
  | Rtype.Path.Papply (p1,p2) -> Papply (recover_path p1, recover_path p2)

let rec recover_longident = function
  | Rtype.Path.Pident (name,_) -> Lident name
  | Rtype.Path.Pdot (p, name, pos) -> Ldot (recover_longident p, name)
  | Rtype.Path.Papply (p1,p2) -> 
      Lapply (recover_longident p1, recover_longident p2)

let recover_type_expr t =
  let cache = ref [] in 
  let rec recover_type_expr t = 
    try List.assq t !cache with Not_found ->
      let recovered = Btype.newgenty (recover_type_desc t.Rtype.desc) in
      cache := (t, recovered) :: !cache;
      recovered
  and recover_type_desc = function
    | Rtype.Tvar ->
	Tvar
    | Rtype.Tarrow (l,t1,t2) -> 
	Tarrow (l, recover_type_expr t1, recover_type_expr t2, Cunknown)
    | Rtype.Ttuple ts ->
	Ttuple (List.map recover_type_expr ts)
    | Rtype.Tconstr((p,_), ts) ->
	Tconstr(recover_path p, List.map recover_type_expr ts, ref Mnil)
  in
  recover_type_expr t

(* type declarations *)

let cached_runtime_type_tbl = 
  ref ([] : (Types.type_expr * Rtype.type_expr) list)
let dummy_runtime_type_declaration_tbl = 
  ref ([] : (Rtype.type_declaration * Path.t) list)
let dummy_cntr = ref 0

let reset_tables () =
  cached_runtime_type_tbl := [];
  dummy_runtime_type_declaration_tbl := [];
  dummy_cntr := 0

let register_dummy_runtime_type_declaration p =
  (* empty type declaration for compilation hacks *)
  (* since Rtype.type_declaration is unmutable, we must explicitly
     create a different copy the following for each dummy entrie *)
  let dummy_runtime_type_declaration () = 
    incr dummy_cntr;
    { Rtype.type_name= "dummy";
      Rtype.type_params = [];
      Rtype.type_arity= !dummy_cntr; 
        (* we need this to make each instance different! *)
      Rtype.type_kind= Rtype.Type_abstract;
      Rtype.type_manifest= None;
      Rtype.type_variance= [];
      Rtype.type_defined_with= []
    }
  in
  let dummy = dummy_runtime_type_declaration () in
  dummy_runtime_type_declaration_tbl :=
    (dummy, p) :: !dummy_runtime_type_declaration_tbl;
  dummy

let rec runtime_path = function
  | Pident id -> Rtype.Path.Pident (Ident.name id, Ident.stamp id)
  | Pdot (p, n, i) -> Rtype.Path.Pdot (runtime_path p, n, i)
  | Papply (p1,p2) -> Rtype.Path.Papply (runtime_path p1, runtime_path p2)

let rec runtime_type_expr t =
  match t.desc with
  | Tlink t -> runtime_type_expr t
  | _ ->
      try
	List.assq t !cached_runtime_type_tbl
      with
      | Not_found ->
	  try
	    let rt = { Rtype.desc= runtime_type_desc t.desc } in
	    cached_runtime_type_tbl := (t, rt) :: !cached_runtime_type_tbl;
	    rt
	  with
	  | (Error (loc, Unsupported) as e) ->
	      (* FIXME *)
	      Location.prerr_warning Location.none
		(Warnings.Gcaml_related "unsupported data type for runtime_type_declaration"); 
	      Format.fprintf Format.err_formatter "%a@."
		Printtyp.type_expr t;
	      raise e

and runtime_type_desc = function
  | Tvar -> Rtype.Tvar
  | Tarrow (l,t1,t2,_) -> 
      Rtype.Tarrow (l, runtime_type_expr t1, runtime_type_expr t2)
  | Ttuple ts -> Rtype.Ttuple (List.map runtime_type_expr ts)
  | Tconstr (p, ts, _) ->
      let dummy = register_dummy_runtime_type_declaration p in
      Rtype.Tconstr ((runtime_path p, Rtype.Box dummy), 
		     List.map runtime_type_expr ts)
  | Tpath p -> Rtype.Tvar
  | _ -> raise (Error (Location.none, Unsupported))

let runtime_type_exprs ts =
  reset_tables ();
  let rts = List.map runtime_type_expr ts in 
  rts, !cached_runtime_type_tbl, !dummy_runtime_type_declaration_tbl

let runtime_private_flag = function
  | Private -> Rtype.Private
  | Public -> Rtype.Public

let runtime_record_representation = function
  | Record_regular -> Rtype.Record_regular
  | Record_float -> Rtype.Record_float

let runtime_mutable_flag = function
  | Immutable -> Rtype.Immutable
  | Mutable -> Rtype.Mutable

let runtime_type_kind = function
  | Type_abstract -> Rtype.Type_abstract
  | Type_variant (stss, p) -> 
      Rtype.Type_variant 
	(List.map (fun (s,ts) -> s, List.map runtime_type_expr ts) stss,
	 runtime_private_flag p)
  | Type_record (lmts, repl, p) ->
      Rtype.Type_record 
	(List.map (fun (l,m,t) -> 
	  l, runtime_mutable_flag m, runtime_type_expr t) lmts,
	 runtime_record_representation repl,
	 runtime_private_flag p)

let runtime_type_declaration name recdefs td =
  reset_tables ();
  let defined_with = 
    List.map (fun id ->
      let p = Pident id in
      let dummy = register_dummy_runtime_type_declaration p in
      runtime_path p, Rtype.Box dummy) recdefs
  in
  try
    let manifest =
      match td.type_manifest with 
      | None -> None
      | Some m -> Some (runtime_type_expr m)
    in
    let rdecl = 
      { Rtype.type_name= name;
	Rtype.type_params= List.map runtime_type_expr td.type_params;
  	Rtype.type_arity= td.type_arity;
  	  Rtype.type_kind= runtime_type_kind td.type_kind;
  	Rtype.type_manifest= manifest;
  	Rtype.type_variance= td.type_variance;
	Rtype.type_defined_with= defined_with }
    in
    (* note that rdecl must be computed before we return the content of 
       the reference dummy_runtime_type_declaration *)
    rdecl, !cached_runtime_type_tbl, !dummy_runtime_type_declaration_tbl
  with
  | Error (_, Unsupported) ->
      { Rtype.type_name= name;
	Rtype.type_params= List.map runtime_type_expr td.type_params;
  	Rtype.type_arity= td.type_arity;
  	Rtype.type_kind= Rtype.Type_abstract;
  	Rtype.type_manifest= None;
  	Rtype.type_variance= td.type_variance;
	Rtype.type_defined_with= defined_with },
        [], []

(* tools *)

let path_is_in_scope env rp =
  let lident = recover_longident rp in
(* let _path = recover_path rp in *)
  try
    let path_found, _ = Env.lookup_type lident env in
    let rpath_found = runtime_path path_found in

    let rec path_same p1 p2 =
      match p1, p2 with
      | (Rtype.Path.Pident id1, Rtype.Path.Pident id2) -> id1 = id2
      | (Rtype.Path.Pdot(p1, s1, pos1), Rtype.Path.Pdot(p2, s2, pos2)) -> s1 = s2 && path_same p1 p2
      | (Rtype.Path.Papply(fun1, arg1), Rtype.Path.Papply(fun2, arg2)) ->
	  path_same fun1 fun2 && path_same arg1 arg2
      | (_, _) -> false
    in
    path_same rp rpath_found
  with
  | Not_found -> false

(* Error reporting *)

open Format

let report_error ppf = function
  | Cannot_have_full_path ->
      fprintf ppf "This expression is not permitted inside run time type pattern"
  | Multiply_bound_type_variable ->
      fprintf ppf "This type variable is bound several times in this matching"
  | Unsupported ->
      fprintf ppf "This type construction is not yet supported in run time types"
