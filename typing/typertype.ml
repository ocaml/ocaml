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

(* rtype type *)

let get_rtype_type () =
  (* must be got dynamically, since Rtype is not available in the compilation
     of stdlib *)
  let rtype_path = 
    try
      fst (Env.lookup_type (Ldot (Lident "Rtype", "type_expr")) Env.empty)
    with
    | Not_found ->
	Misc.fatal_error ("Primitive type Rtype.type_expr not found.")
  in
  Btype.newgenty (Tconstr (rtype_path, [], ref Mnil))

let get_rtype_type_declaration () =
  (* must be got dynamically, since Rtype is not available in the compilation
     of stdlib *)
  let rtypedecl_path = 
    try
      fst (Env.lookup_type (Ldot (Lident "Rtype", "type_declaration")) Env.empty)
    with
    | Not_found ->
	Misc.fatal_error ("Primitive type Rtype.type_declaration not found.")
  in
  Btype.newgenty (Tconstr (rtypedecl_path, [], ref Mnil))

(* Longidents *)

let some = Lident "Some"
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
  let path = trans lid in
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
      
let pattern_of_type transl_longident t =
  let tvars = ref [] in
  let rec pattern_of_type t =
    match t.ptyp_desc with
    | Ptyp_lident lid ->
        begin match lid with
        | Lident name -> make_pat t.ptyp_loc (Ppat_var name)
        | _ -> raise (Error (t.ptyp_loc, Cannot_have_full_path))
        end
    | _ ->
        make_pat t.ptyp_loc 
 	 (Ppat_record [cstr "desc", pattern_of_desc t.ptyp_loc t.ptyp_desc])
      
  and pattern_of_desc loc = function
    | Ptyp_var n -> 
	if List.mem n !tvars then
	  (* as normal pattern variables, a type var cannot appear more than 
	     once in a pattern *)
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
  pattern_of_type t

    
(**
(* Value *)

let make_exp loc desc = { pexp_desc= desc; pexp_loc= loc }

let make_exp_construct loc lid args =
  let args =
    match args with
    | [] -> None
    | [x] -> Some x
    | _ -> Some (make_exp loc (Pexp_tuple args))
  in
  make_exp loc (Pexp_construct (lid, args, false))

let rec mktailexp = function
    [] -> make_exp Location.none (Pexp_construct(Lident "[]", None, false))
  | e1 :: el ->
      let exp_el = mktailexp el in
      let l = {loc_start = e1.pexp_loc.loc_start;
               loc_end = exp_el.pexp_loc.loc_end;
               loc_ghost = true}
      in
      let arg = make_exp l (Pexp_tuple [e1; exp_el]) in
      make_exp l (Pexp_construct(Lident "::", Some arg, false))

let value_of_longident trans loc lid = 
  (* first of all, we have to translate the longident to a path! *)
  let path = trans lid in
  let rec value_of_path = function
    | Pident id ->
	make_exp_construct loc (cstr_path "Pident")
	  [make_exp loc (Pexp_constant (Const_string (Ident.name id)));
	   make_exp loc (Pexp_constant (Const_int (Ident.stamp id)))]
    | Pdot (path, str, n) ->
	make_exp_construct loc (cstr_path "Pdot")
	  [value_of_path path;
	   make_exp loc (Pexp_constant (Const_string str));
	   make_exp loc (Pexp_constant (Const_int n))]
    | Papply (p1, p2) ->
	make_exp_construct loc (cstr_path "Papply")
	  [value_of_path p1; value_of_path p2]
  in
  value_of_path path

let value_of_type transl_longident t =
  let make_desc loc vdesc =
    make_exp loc (Pexp_record ([cstr "desc", vdesc], None))
  in
  let var_occurrences = ref [] in
  let var_conversions = ref [] in
  let add_var loc n =
    try
      let num = List.assoc n !var_occurrences in incr num
    with
    | Not_found ->
	let name = "'" ^ n in
	var_occurrences := (n, ref 1) :: !var_occurrences;
	var_conversions :=
	  (n, (make_desc loc (make_exp_construct loc (cstr "Tvar") []),
	       name)) :: !var_conversions
  in
  let rec scan_vars t =
    match t.ptyp_desc with
    | Ptyp_lident _ -> ()
    | Ptyp_var n -> add_var t.ptyp_loc n
    | Ptyp_arrow (_,t1,t2) -> scan_vars t1; scan_vars t2
    | Ptyp_tuple ts -> List.iter scan_vars ts
    | Ptyp_constr (_,ts) -> List.iter scan_vars ts
    | _ -> raise (Error (t.ptyp_loc, Unsupported))
  in
  let rec value_of_type t =
    match t.ptyp_desc with
    | Ptyp_lident lid -> make_exp t.ptyp_loc (Pexp_ident lid)
    | Ptyp_var n ->
	begin try
	  let num = !(List.assoc n !var_occurrences) in
	  let pexp, lid = List.assoc n !var_conversions in
	  if num = 1 then pexp 
	  else make_exp t.ptyp_loc (Pexp_ident (Lident lid))
	with
	| Not_found -> assert false
	end
    | _ ->
	make_desc t.ptyp_loc (value_of_desc t.ptyp_loc t.ptyp_desc)
     
  and value_of_desc loc = function
    | Ptyp_var n -> assert false 
    | Ptyp_arrow (l, t1, t2) -> 
	make_exp_construct loc (cstr "Tarrow")
 	  [ make_exp loc (Pexp_constant (Const_string l));
 	    value_of_type t1;
 	    value_of_type t2 ]
    | Ptyp_tuple ts ->
	make_exp_construct loc (cstr "Ttuple")
	  [ mktailexp (List.map value_of_type ts) ]
    | Ptyp_constr (lid, ts) ->
	make_exp_construct loc (cstr "Tconstr")
 	  [make_exp loc (Pexp_tuple
			   [ value_of_longident transl_longident loc lid;
			     make_exp loc (Pexp_ident lid) ]);
	                     (* lid may be fake_idents! *)
 	   mktailexp (List.map value_of_type ts)]
    | Ptyp_lident _ -> assert false
    | _ -> raise (Error (loc, Unsupported))
  in
  scan_vars t;
  let defs = 
    List.fold_right (fun (name,num) st ->
      if !num > 1 then
	let pexp, lid = 
	  try
	    List.assoc name !var_conversions
	  with
	  | Not_found -> assert false
	in
	(make_pat Location.none (Ppat_var lid), pexp) :: st
      else st) !var_occurrences []
  in
  make_exp t.ptyp_loc (Pexp_let (Nonrecursive, defs, value_of_type t))

(* type_expr -> core_type *)

(* We first translate the type_exprs to Parsetree.core_type, 
   in order to reuse the conversion of core_types to run time 
   type parse trees, which is defined in Typertype.
 
   It may sound silly, but the good point is that thus I do not 
   need to write almost the same algorithm twice. In addition,
   passing the result through the type checker, I can be sure of
   the correctness of my conversion. 
*)
 
let make_type desc = { ptyp_desc= desc; ptyp_loc= Location.none } 

(* Generally speaking, path => longident may not be one-to-one mapping!
   We DO create a longident of a path here, just because Ttypertype
   needs longidents instead of paths. We remember this fake conversion
   and later recover the original idents. *)
 
let to_fake_longident path = 
  let rec name = function
    | Pident id -> Ident.unique_name id
    | Pdot (p, n, i) -> 
	Printf.sprintf "%s.%s_%d" (name p) n i
    | Papply (p1, p2) -> 
	Printf.sprintf "%s(%s)" (name p1) (name p2)
  in
  Lident ("*fake*" ^ name path)

let to_core_types vartbl typs =
  let path_lid_tbl = ref [] in
  let var_conv_tbl = ref [] in
  let var_cntr = ref 0 in
  let type_abstractions = ref [] in
  let rec to_core typ = 
    match typ.desc with
    | Tvar ->
	(* retrieve the original type variable, in order to recover
	   the linkage to the generalization *)
	begin try
	  let t = List.assq typ vartbl in 
	  let id = Etype.find_ident_of_type_variable t in
	  (* Here, the mapping between longident and identifer is 
	     one to one. *)
	  let lid = Lident (Ident.name id) in
	  type_abstractions := id :: !type_abstractions;
  	  make_type (Ptyp_lident lid)
	with Not_found ->
	  try
	    List.assq typ !var_conv_tbl
	  with
	  | Not_found ->
	      let name = "v" ^ string_of_int !var_cntr in
	      incr var_cntr;
	      make_type (Ptyp_var name)
	end
    | Tlink t -> to_core t
    | _ -> make_type (to_core_desc typ.desc)

  and to_core_desc = function
    | Tvar | Tlink _ -> assert false
    | Tarrow (l, t1, t2, _) -> Ptyp_arrow (l, to_core t1, to_core t2)
    | Ttuple ts -> Ptyp_tuple (List.map to_core ts)
    | Tconstr (p, ts, _) ->
	(* Here paths are converted back to longident. The conversions
	   are registered and returned with the final result. *)
	let lid = 
	  try List.assoc p !path_lid_tbl with Not_found ->
	    let lid = to_fake_longident p in
	    path_lid_tbl := (p, lid) :: !path_lid_tbl;
	    lid
	in
	Ptyp_constr (lid, List.map to_core ts)

    | Tobject (_,_)
    | Tfield (_,_,_,_)
    | Tnil
    | Tvariant _
    | Tunivar
    | Tpoly (_,_) -> raise (Error (Location.none, Unsupported))
	
    | Tkonst (_,_)
    | Tsubst _
    | Tpath _ -> assert false
  in
  let core_types = List.map to_core typs in
  core_types, !path_lid_tbl, !type_abstractions
***)

(* rtype -> type_expr *)

let recover_ident (name,pos) =
  Ident.create_with_stamp name pos

let rec recover_path = function
  | Rtype.Path.Pident id -> Pident (recover_ident id)
  | Rtype.Path.Pdot (p, name, pos) -> Pdot (recover_path p, name, pos)
  | Rtype.Path.Papply (p1,p2) -> Papply (recover_path p1, recover_path p2)

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

(* empty type declaration for compilation hacks *)
(* since Rtype.type_declaration is unmutable, we must explicitly
   create a different copy the following for each dummy entrie *)
let dummy_runtime_type_declaration () = 
  incr dummy_cntr;
  { Rtype.type_params = [];
    Rtype.type_arity= !dummy_cntr; 
      (* we need this to make each instance different! *)
    Rtype.type_kind= Rtype.Type_abstract;
    Rtype.type_manifest= None;
    Rtype.type_variance= [] }

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
	  let rt = { Rtype.desc= runtime_type_desc t.desc } in
	  cached_runtime_type_tbl := (t, rt) :: !cached_runtime_type_tbl;
	  rt

and runtime_type_desc = function
  | Tvar -> Rtype.Tvar
  | Tarrow (l,t1,t2,_) -> 
      Rtype.Tarrow (l, runtime_type_expr t1, runtime_type_expr t2)
  | Ttuple ts -> Rtype.Ttuple (List.map runtime_type_expr ts)
  | Tconstr (p, ts, _) ->
      let dummy = dummy_runtime_type_declaration () in
      dummy_runtime_type_declaration_tbl :=
	(dummy, p) :: !dummy_runtime_type_declaration_tbl;
      Rtype.Tconstr ((runtime_path p, dummy), 
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

let runtime_type_declaration td =
  reset_tables ();
  let rdecl = 
    { Rtype.type_params= List.map runtime_type_expr td.type_params;
      Rtype.type_arity= td.type_arity;
      Rtype.type_kind= runtime_type_kind td.type_kind;
      Rtype.type_manifest= 
        (match td.type_manifest with 
	| None -> None
	| Some m -> Some (runtime_type_expr m));
      Rtype.type_variance= td.type_variance }
  in
  (* note that rdecl is computed before we return the content of the reference
   dummy_runtime_type_declaration *)
  rdecl, !cached_runtime_type_tbl, !dummy_runtime_type_declaration_tbl

(* Error reporting *)

open Format

let report_error ppf = function
  | Cannot_have_full_path ->
      fprintf ppf "This expression is not permitted inside run time type pattern"
  | Multiply_bound_type_variable ->
      fprintf ppf "This type variable is bound several times in this matching"
  | Unsupported ->
      fprintf ppf "This type construction is not yet supported in run time types"
      
