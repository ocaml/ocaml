open Path
open Types
open Ctype

exception Unsupported_type_constructor

let ident_of = Ident.unique_name
    
let rec rpath_of = function
  | Pident id -> Rtype.Path.Pident (ident_of id)
  | Pdot (p, n, pos) -> Rtype.Path.Pdot (rpath_of p, n, pos)
  | Papply (p1, p2) -> Rtype.Path.Papply (rpath_of p1, rpath_of p2)

let rtype_of t =
  let tbl = ref [] in
  let rec sub t = 
    match t.desc with
    | Tvar ->
	begin try List.assq t !tbl with Not_found ->
	  let tv = Rtype.mk_type Rtype.Tvar in
	  tbl := (t,tv) :: !tbl;
	  tv
	end
    | Tarrow (l, t1, t2, com) ->
	Rtype.mk_type (Rtype.Tarrow (l, sub t1, sub t2))
    | Ttuple ts ->
	Rtype.mk_type (Rtype.Ttuple (List.map sub ts))
    | Tconstr (p, args, ab_memo_ref) ->
	Rtype.mk_type (Rtype.Tconstr (rpath_of p, List.map sub args))
    | Tlink t -> sub t
    | _ -> raise Unsupported_type_constructor
(*
    | Tobject of type_expr * (Path.t * type_expr list) option ref
    | Tfield of string * field_kind * type_expr * type_expr
    | Tnil
    | Tsubst of type_expr         (* for copying *)
    | Tvariant of row_desc
    | Tunivar
    | Tpoly of type_expr * type_expr list
*)
  in
  sub t

open Lambda
open Asttypes

(*
open Rtype      (* types are from Rtype *)
open Rtype.Path (* paths are from Rtype *)
*)

(* the same thing is defined in translcore *)
exception Not_constant
let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

(* different from Translcore.transl_list *)
let rec transl_list f = function
    [] -> Lconst (Const_pointer 0)
  | x :: xs ->
      let ll = [f x; transl_list f xs] in
      try
	Lconst (Const_block (0, List.map extract_constant ll))
      with
	_ -> Lprim (Pmakeblock (0, Immutable), ll)

let make_block n ll =
  try
    Lconst(Const_block(n, List.map extract_constant ll))
  with
  | Not_constant ->
      Lprim(Pmakeblock(n, Immutable), ll)

(*
 * let rec transl_runtime_path = function
 *   | Pident s -> 
 *       Const_block (0, [Const_base (Const_string s)])
 *   | Pdot (p,n,x) ->
 *       Const_block (1, [transl_runtime_path p; 
 * 		       Const_base (Const_string n); Const_base (Const_int x)])
 *   | Papply (p1,p2) -> 
 *       Const_block (2, [transl_runtime_path p1; transl_runtime_path p2])
 * 
 * (* FIXME: no type variable sharing is considered *)
 * let rec transl_runtime_type d =
 *   Lprim(Pmakeblock(0, Mutable), [ transl_runtime_type_desc d.desc ])
 * 
 * and transl_runtime_type_desc = function
 *   | Tvar -> 
 *       Lconst(Const_pointer 0)
 *   | Tarrow(l,t1,t2) -> 
 *       make_block 0 [Lconst(Const_base (Const_string l)); 
 * 		    transl_runtime_type t1; 
 * 		    transl_runtime_type t2]
 *   | Ttuple ts -> make_block 1 (List.map transl_runtime_type ts)
 *   | Tconstr (p, ts) ->
 *       make_block 2 [Lconst (transl_runtime_path p); transl_list transl_runtime_type ts]
 *)

(* translation to compiled rtype directly from type_expr *)

open Types

let rec transl_rpath_of_path = function
  | Pident id -> 
      Const_block (0, [Const_base (Const_string (ident_of id))])
  | Pdot (p,n,x) ->
      Const_block (1, [transl_rpath_of_path p; 
		       Const_base (Const_string n); Const_base (Const_int x)])
  | Papply (p1,p2) -> 
      Const_block (2, [transl_rpath_of_path p1; transl_rpath_of_path p2])

let transl_type_expr vartbl t =
  (* occurrence number table *)
  let occurrence_tbl =
    let tbl = ref [] in
    let rec count_type t =
      let t = repr t in
      try
	List.assq t !tbl := true;
      with
      | Not_found ->
	  tbl := (t, ref false) :: !tbl;
	  match t.desc with
	  | Tvar -> ()
	  | Tarrow(l,t1,t2,_) -> count_type t1; count_type t2
	  | Ttuple ts | Tconstr (_, ts, _) -> List.iter count_type ts
	  | _ -> raise Unsupported_type_constructor
    in
    count_type t;
    !tbl
  in
  (* ident table for multiple occurs *)
  let ident_tbl =
    List.fold_right (fun (t,mult) st ->
      if !mult then begin
	let name = "*t" ^ string_of_int t.id ^ "*" in
	(t, Ident.create name) :: st
      end else st) occurrence_tbl []
  in

  let rec transl_type_expr t =
    (* if it is registered as multi-occurred, use ident *)
    let t = repr t in
    try
      Lambda.transl_path (Path.Pident (List.assq t ident_tbl))
    with
    | Not_found -> transl_top_type_expr t

  and transl_top_type_expr t =
    let t = repr t in
    try
      match t.desc with
      | Tvar ->
	  (* retrieve the original type variable, in order to recover
	     the linkage to the generalization *)
	  let t = try List.assq t vartbl with Not_found -> t in
  	  Lambda.transl_path 
	    (Path.Pident (Etype.find_ident_of_type_variable t))
      | _ -> raise Not_found
    with
    | Not_found -> 
        Lprim(Pmakeblock(0, Mutable), [ transl_type_desc t.desc ])

  and transl_type_desc = function
    | Tvar -> Lconst(Const_pointer 0)
    | Tarrow(l,t1,t2,_) -> 
        make_block 0 [Lconst(Const_base (Const_string l)); 
  		    transl_type_expr t1; 
  		    transl_type_expr t2]
    | Ttuple ts -> make_block 1 (List.map transl_type_expr ts)
    | Tconstr (p, ts, _) ->
        make_block 2 [Lconst (transl_rpath_of_path p); 
  		    transl_list transl_type_expr ts]
    | _ -> raise Unsupported_type_constructor
  in
  List.fold_right (fun (t,id) st ->
    Llet(Alias, id, transl_top_type_expr t, st))
    ident_tbl (transl_top_type_expr t)
      
let transl_type_exprs vartbl = List.map (transl_type_expr vartbl)
