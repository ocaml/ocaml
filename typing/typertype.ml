(* Converting type expression to rtype *)

open Location
open Longident
open Asttypes
open Parsetree
open Path

type error = Unsupported | Cannot_have_full_path
exception Error of Location.t * error

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
  let path = trans lid in
  let rec pattern_of_path = function
    | Pident id ->
	make_pat_construct loc (cstr_path "Pident")
	  [make_pat loc (Ppat_constant (Const_string (Ident.unique_name id)))]
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
       make_pat_construct loc (cstr "Tvar") [] (*FIXME*)
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
 	[pattern_of_longident transl_longident loc lid;
 	 mktailpat (List.map pattern_of_type ts)]
   | Ptyp_lident _ -> assert false
   | _ -> raise (Error (loc, Unsupported))
 in
 pattern_of_type t

    
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
	  [make_exp loc (Pexp_constant (Const_string (Ident.unique_name id)))]
	  (* FIXME : unify with Transltype.ident_of *)
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
    | Ptyp_var n -> 
	make_exp_construct loc (cstr "Tvar") [] (*FIXME*)
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
 	  [value_of_longident transl_longident loc lid;
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
