open Rtype
open Path
open Lambda
open Asttypes
open Types
open Longident
open Misc
open Outcometree

let dbg = try ignore (Sys.getenv "DEBUG_TRANSLTYPE"); true with _ -> false

(************************ these are defined in transcore.ml but not exported *)

exception Not_constant 

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant
;;

let rec transl_list f = function
    [] -> Lconst (Const_pointer 0)
  | x :: xs ->
      let ll = [f x; transl_list f xs] in
      try
	Lconst (Const_block (0, List.map extract_constant ll))
      with
	_ -> Lprim (Pmakeblock (0, Immutable), ll)
;;

(*/********************** these are defined in transcore.ml but not exported *)

let rpath_of_predefined_type id =
  RPident (Ident.name id, Ident.stamp id)
;;

let rec rpath_of_path = function
  | Pident i -> RPident (Ident.name i, Ident.stamp i)
  | Pdot (p,n,x) -> RPdot (rpath_of_path p, n, x)
  | Papply (p1,p2) -> RPapply (rpath_of_path p1, rpath_of_path p2)
;;

let rec transl_rpath = function
    RPident (i,s) -> 
      Const_block (0, [Const_base (Const_string i);
		       Const_base (Const_int s)])
  | RPdot (p,n,x) -> 
      Const_block (1, [transl_rpath p; 
		       Const_base (Const_string n); 
		       Const_base (Const_int x)])
  | RPapply (p1,p2) -> 
      Const_block (2, [transl_rpath p1; 
		       transl_rpath p2])
;;

let transl_rpath_of_ident modname id =
  (* RPdot (modid, Ident.name id, Ident.stamp id) *)
  Lprim( Pmakeblock (1, Immutable), [ 
	modname;
	Lconst (Const_base (Const_string (Ident.name id)));
	Lconst (Const_base (Const_int (Ident.stamp id))) ] )
;;

let transl_rpath_of_path p = 
  transl_rpath (rpath_of_path p)
;;

(* We have a type expression, compile the runtime representation for it *)
(* env is needed if we want to expand it *)
 
let transl_rtype_of_type (* env *) t =
  if dbg then begin
    Format.print_string "transl_rtype ";
    Printtyp.type_expr Format.std_formatter t;
    Format.print_newline ();
  end;
  let varcnt = ref 1 (* must begin with 1 *)
  and vartbl = ref [] in
  
  let rec sub t =
    let new_type_var t = 
      (* RTvar (!varcnt) *)
      let rt = 
	Lconst (Const_block (0, [Const_base (Const_int !varcnt)])) 
      in
      (* cache it *)
      vartbl := (t, rt) :: !vartbl;
      incr varcnt;
      rt
    in

    let any_type_var t = (* for pattern creation *) 
      (* RTvar (-!varcnt) *)
      let rt = 
	Lconst (Const_block (0, [Const_base (Const_int (- !varcnt))]))
      in
      (* cache it *)
      vartbl := (t, rt) :: !vartbl;
      incr varcnt;
      rt
    in

    let t = Ctype.repr t in
    match t.desc with
    | Tvar -> (* BUG?: normal tvar cannot appear here... *)
	(try List.assq t !vartbl with Not_found -> new_type_var t)
(* Disabled currently...
    | Tany -> (* FOR COERCE, WE DO A DIRTY HACK *)
	any_type_var t
*)
    | Tarrow (l,f,t,_) -> (* what we should do with commutable ? *)
	(* RTarrow (l, sub f, sub t) *)
	begin
	  let ll = [Lconst (Const_base (Const_string l)); sub f; sub t] in
	  try
	    Lconst (Const_block (1, List.map extract_constant ll))
	  with
	    _ -> Lprim(Pmakeblock(1, Immutable), ll)
	end
    | Ttuple tl -> 
	(* RTtuple (List.map sub tl) *)
	begin
	  let ll = [transl_list sub tl] in
	  try
	    Lconst (Const_block (2, List.map extract_constant ll))
	  with
	    _ -> Lprim(Pmakeblock(2, Immutable), ll)
	end
    | Tconstr (p,tl,abbrev) -> 

(******************************************************** expansion is disabled
	(* try to expand *)
	begin
	  try
	    let t' = Ctype.expand_abbrev env p tl abbrev t.level in
	    sub t'
	  with Ctype.Cannot_expand ->
            let ll = [ Lconst (transl_rpath_of_path env p); transl_list sub tl ]
	    in
	    try
	      Lconst (Const_block (3, List.map extract_constant ll))
		with
	      _ -> Lprim(Pmakeblock(3, Immutable), ll)
	end
******************************************************************************)

	begin
	  let rp = transl_rpath_of_path p in
	  let ll = [ Lconst rp; transl_list sub tl ]
	  in
	  try
	    Lconst (Const_block (3, List.map extract_constant ll))
	  with
	    _ -> Lprim(Pmakeblock(3, Immutable), ll)
	end
		
    | Tlink t -> sub t
    | Tobject _ 
    | Tfield _
    | Tnil 
    | Tvariant _ 
    | Tsubst _ ->
	raise (Failure "Actually, I do not know about object type system") 
  in
  sub t

(* rtype -> type_expr *)

let type_expr_of_rtype rt =
  let tvars = ref [] in
  let rec path_of_rpath = function
      RPident (name,stamp) -> Pident (Ident.create_with_stamp name stamp)
    | RPdot (RPident ("*toplevel*",0), s, x) ->
	Pident (Ident.create_with_stamp s x)
    | RPdot (rp, s, x) -> Pdot (path_of_rpath rp, s, x)
    | RPapply (p1,p2) -> Papply (path_of_rpath p1, path_of_rpath p2)
  in
  let rec sub = function
    | RTvar x -> 
	begin 
	  try List.assoc x !tvars with Not_found ->
	    let tv = Btype.newgenty Tvar in
	    tvars := (x,tv) :: !tvars;
	    tv
	end
    | RTarrow (l,f,t) ->
	Btype.newgenty (Tarrow (l, sub f, sub t, Cunknown))
    | RTtuple tls ->
	Btype.newgenty (Ttuple (List.map sub tls))
    | RTconstr (rp, args) ->
	Btype.newgenty (Tconstr (path_of_rpath rp, 
				 List.map sub args, ref Mnil))
  in
  sub rt
;;

(* dynamic/coerce primitive compilaiton are stored in stdlib/rtype.ml *)
let rtype_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "Rtype", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

