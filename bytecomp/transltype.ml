open Rtype
open Path
open Lambda
open Asttypes
open Types
open Longident
open Misc
open Outcometree
open Btype
open Ctype

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

let rec run_ident_of_path = function
  | Pident i -> Ride_ident (Ident.name i, Ident.stamp i)
  | Pdot (p,n,x) -> Ride_dot (run_ident_of_path p, n, x)
  | Papply (p1,p2) -> Ride_apply (run_ident_of_path p1, 
				  run_ident_of_path p2)
;;

let rec transl_run_ident = function
  | Ride_ident (s,i) -> 
      Const_block (0, [Const_base (Const_string s);
		       Const_base (Const_int i)])
  | Ride_dot (p,n,x) -> 
      Const_block (1, [transl_run_ident p; 
		       Const_base (Const_string n); 
		       Const_base (Const_int x)])
  | Ride_apply (p1,p2) -> 
      Const_block (2, [transl_run_ident p1; 
		       transl_run_ident p2])
;;

let transl_run_ident_of_path p = transl_run_ident (run_ident_of_path p)
;;

let rec tree_of_run_ident = function
  | Ride_ident (s,i) -> 
      Oide_ident s
  | Ride_dot (ri,s,i) -> 
      Oide_dot (tree_of_run_ident ri, s)
  | Ride_apply (r1,r2) -> 
      Oide_apply(tree_of_run_ident r1, tree_of_run_ident r2)
;;	

(* We have a type expression, compile the runtime representation for it *)
 
let rec transl_run_type = function
  | Rtyp_var i -> Lconst (Const_block (0, [Const_base (Const_int i)]))
  | Rtyp_arrow (l,t1,t2) ->
      begin
	let ll = [ Lconst (Const_base (Const_string l)); 
		   transl_run_type t1; transl_run_type t2] in
	try
	  Lconst (Const_block (1, List.map extract_constant ll))
	with
	  _ -> Lprim(Pmakeblock(1, Immutable), ll)
      end
  | Rtyp_tuple rts ->
      begin
	let ll = [transl_list transl_run_type rts] in
	try
	  Lconst (Const_block (2, List.map extract_constant ll))
	with
	  _ -> Lprim(Pmakeblock(2, Immutable), ll)
      end
  | Rtyp_constr ((rp,digest), tl) ->
      begin
	let rpcomp = transl_run_ident rp in
	let digestcomp = Const_base (Const_string digest) in
	let rpdigest = Const_block (0, [rpcomp; digestcomp]) in
	let ll = [ Lconst rpdigest; transl_list transl_run_type tl ]
	in
	try
	  Lconst (Const_block (3, List.map extract_constant ll))
	with
	  _ -> Lprim(Pmakeblock(3, Immutable), ll)
      end
;;

let snames = ref []
let sname_counter = ref 0
let sreset_names () = snames := []; sname_counter := 0
let snew_name () =
  let name =
    if !sname_counter < 26
    then String.make 1 (Char.chr(97 + !sname_counter)) 
    else String.make 1 (Char.chr(97 + !sname_counter mod 26)) ^
           string_of_int(!sname_counter / 26) in
  incr sname_counter;
  name
;;
let sname_of_type t =
  try List.assq t !snames with Not_found ->
    let name = snew_name () in
    snames := (t, name) :: !snames;
    name
;;

let tree_of_val_type conv t = 
  let rec aux = function
    | Rtyp_var x ->
	Otyp_var (false, sname_of_type x)
    | Rtyp_arrow (l,t1,t2) ->
	Otyp_arrow (l, aux t1, aux t2)
    | Rtyp_tuple tls ->
	Otyp_tuple (List.map aux tls)
    | Rtyp_constr (p, tls) ->
	Otyp_constr (conv p, List.map aux tls)
  in
  sreset_names ();
  aux t
;;

let tree_of_run_type = tree_of_val_type (fun (ri,_) -> tree_of_run_ident ri)
;;

(* dynamic/coerce primitive compilaiton are stored in stdlib/rtype.ml *)
let rtype_prim name =
  try
    transl_path
      (fst (Env.lookup_value (Ldot (Lident "Rtype", name)) Env.empty))
  with Not_found ->
    fatal_error ("Primitive " ^ name ^ " not found.")

(* Print a type expression *)

let names = ref ([] : (type_expr * int) list)
let name_counter = ref 0

let reset_names () = names := []; name_counter := 0

let new_name () =
  let name = !name_counter in
  incr name_counter;
  name

let name_of_type t =
  try List.assq t !names with Not_found ->
    let name = new_name () in
    names := (t, name) :: !names;
    name

let check_name_of_type t = ignore(name_of_type t)

let visited_objects = ref ([] : type_expr list)
let aliased = ref ([] : type_expr list)

let is_aliased ty = List.memq ty !aliased
let add_alias ty =
  if not (is_aliased ty) then aliased := ty :: !aliased

let proxy ty =
  let ty = repr ty in
  match ty.desc with
  | Tvariant row -> Btype.row_more row
  | _ -> ty

let namable_row row =
  row.row_name <> None &&
  List.for_all
    (fun (_, f) ->
       match row_field_repr f with
       | Reither(c, l, _, _) ->
           row.row_closed && if c then l = [] else List.length l = 1
       | _ -> true)
    row.row_fields

let rec mark_loops_rec visited ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.memq px visited then add_alias px else
    let visited = px :: visited in
    match ty.desc with
    | Tvar -> ()
    | Tarrow(_, ty1, ty2, _) ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Ttuple tyl -> List.iter (mark_loops_rec visited) tyl
    | Tconstr(_, tyl, _) ->
        List.iter (mark_loops_rec visited) tyl
    | Tvariant row ->
        let row = row_repr row in
        if List.memq px !visited_objects then add_alias px else
         begin
          if not (static_row row) then
            visited_objects := px :: !visited_objects;
          match row.row_name with
          | Some(p, tyl) when namable_row row ->
              List.iter (mark_loops_rec visited) tyl
          | _ ->
              iter_row (mark_loops_rec visited) {row with row_bound = []}
         end
    | Tobject (fi, nm) ->
        if List.memq px !visited_objects then add_alias px else
         begin
          if opened_object ty then
            visited_objects := px :: !visited_objects;
          begin match !nm with
          | None ->
              mark_loops_rec visited fi
          | Some (_, l) ->
              List.iter (mark_loops_rec visited) l
          end
        end
    | Tfield(_, kind, ty1, ty2) when field_kind_repr kind = Fpresent ->
        mark_loops_rec visited ty1; mark_loops_rec visited ty2
    | Tfield(_, _, _, ty2) ->
        mark_loops_rec visited ty2
    | Tnil -> ()
    | Tsubst ty -> mark_loops_rec visited ty
    | Tlink _ -> fatal_error "Transltype.mark_loops_rec (2)"

let mark_loops ty =
  normalize_type Env.empty ty;
  mark_loops_rec [] ty;;

let reset_loop_marks () =
  visited_objects := []; aliased := []

let reset () =
  reset_names (); reset_loop_marks ()

let reset_and_mark_loops ty =
  reset (); mark_loops ty;;

let reset_and_mark_loops_list tyl =
 reset (); List.iter mark_loops tyl;;

(* Disabled in classic mode when printing an unification error *)
let print_labels = ref true

type 'a val_type_decl_desc = 
  | Rdecl_abstract
  | Rdecl_manifest of 'a val_type * 'a val_type_decl_desc
  | Rdecl_sum of (string * 'a val_type list) list
  | Rdecl_record of (string * bool * 'a val_type) list
;;

type 'a val_type_declaration =
    string * (int * (bool * bool)) list * 'a val_type_decl_desc * 
      ('a val_type * 'a val_type) list
;;

let rec val_type_of_typexp of_path ty =
  let ty = repr ty in
  let px = proxy ty in
  if List.mem_assq px !names then
   Rtyp_var (name_of_type px) else

  let pr_typ () =
   (match ty.desc with
    | Tvar ->
        Rtyp_var (name_of_type ty)
    | Tarrow(l, ty1, ty2, _) ->
        let pr_arrow l ty1 ty2 =
          let lab =
            if l <> "" || is_optional l then l else ""
          in
          let t1 =
            if is_optional l then
              match (repr ty1).desc with
              | Tconstr(path, [ty], _)
                when Path.same path Predef.path_option ->
                  val_type_of_typexp of_path ty
              | _ -> raise (Failure "<hidden>")
            else val_type_of_typexp of_path ty1 in
          Rtyp_arrow (lab, t1, val_type_of_typexp of_path ty2) in
        pr_arrow l ty1 ty2
    | Ttuple tyl ->
        Rtyp_tuple (val_types_of_typlist of_path tyl)
    | Tconstr(p, tyl, abbrev) ->
        Rtyp_constr (of_path p, val_types_of_typlist of_path tyl)
(*
    | Tvariant row ->
        let row = row_repr row in
        let fields =
          if row.row_closed then
            List.filter (fun (_, f) -> row_field_repr f <> Rabsent)
              row.row_fields
          else row.row_fields in
        let present =
          List.filter
            (fun (_, f) ->
               match row_field_repr f with
               | Rpresent _ -> true
               | _ -> false)
            fields in
        let all_present = List.length present = List.length fields in
        begin match row.row_name with
        | Some(p, tyl) when namable_row row ->
            let id = tree_of_path p in
            let args = val_types_of_typlist of_path sch tyl in
            if row.row_closed && all_present then
              Otyp_constr (id, args)
            else
              let non_gen = is_non_gen sch px in
              let tags =
                if all_present then None else Some (List.map fst present) in
              Otyp_variant (non_gen, Ovar_name(tree_of_path p, args),
                            row.row_closed, tags)
        | _ ->
            let non_gen =
              not (row.row_closed && all_present) && is_non_gen sch px in
            let fields = List.map (tree_of_row_field sch) fields in
            let tags =
              if all_present then None else Some (List.map fst present) in
            Otyp_variant (non_gen, Ovar_fields fields, row.row_closed, tags)
        end
    | Tobject (fi, nm) ->
        tree_of_typobject sch ty fi nm
*)
    | Tsubst ty ->
        val_type_of_typexp of_path ty
      |	Tvariant _ | Tobject (_,_) | Tlink _ | Tnil | Tfield _ ->
        fatal_error "Transltype.val_type_of_typexp"
   ) in
  if is_aliased px then begin
    raise (Failure "alias type is not supported")
    (* check_name_of_type px;
       Otyp_alias (pr_typ (), name_of_type px) *)
  end
  else pr_typ ()

(*
and tree_of_row_field sch (l, f) =
  match row_field_repr f with
  | Rpresent None | Reither(true, [], _, _) -> (l, false, [])
  | Rpresent(Some ty) -> (l, false, [val_type_of_typexp sch ty])
  | Reither(c, tyl, _, _) ->
      if c (* contradiction: un constructeur constant qui a un argument *)
      then (l, true, val_types_of_typlist of_path sch tyl)
      else (l, false, val_types_of_typlist of_path sch tyl)
  | Rabsent -> (l, false, [] (* une erreur, en fait *))
*)

and val_types_of_typlist of_path = function
  | [] -> []
  | ty :: tyl ->
      let tr = val_type_of_typexp of_path ty in
      tr :: val_types_of_typlist of_path tyl

(*
and tree_of_typobject sch ty fi nm =
  begin match !nm with
  | None ->
      let pr_fields fi =
        let (fields, rest) = flatten_fields fi in
        let present_fields =
          List.fold_right
            (fun (n, k, t) l ->
               match field_kind_repr k with
               | Fpresent -> (n, t) :: l
               | _ -> l)
            fields [] in
        let sorted_fields =
          Sort.list (fun (n, _) (n', _) -> n <= n') present_fields in
        tree_of_typfields sch rest sorted_fields in
      let (fields, rest) = pr_fields fi in
      Otyp_object (fields, rest)
  | Some (p, {desc = Tvar} :: tyl) ->
      let non_gen = is_non_gen sch ty in
      let args = val_types_of_typlist of_path sch tyl in
      Otyp_class (non_gen, tree_of_path p, args)
  | _ ->
      fatal_error "Transltype.tree_of_typobject"
  end

and tree_of_typfields sch rest = function
  | [] ->
      let rest =
        match rest.desc with
        | Tvar -> Some (is_non_gen sch rest)
        | Tnil -> None
        | _ -> fatal_error "typfields (1)"
      in
      ([], rest)
  | (s, t) :: l ->
      let field = (s, val_type_of_typexp sch t) in
      let (fields, rest) = tree_of_typfields sch rest l in
      (field :: fields, rest)
*)
;;

let val_type_of_type_scheme of_path ty = reset_and_mark_loops ty; val_type_of_typexp of_path ty

let rec val_of_constraints of_path params =
  List.fold_right
    (fun ty list ->
       let ty' = unalias ty in
       if ty != ty' then
         let tr = val_type_of_typexp of_path ty in
         (tr, val_type_of_typexp of_path ty') :: list
       else list)
    params []

and filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        let ty = repr ty in
        if List.memq ty tyl then Btype.newgenty (Tsubst ty) :: tyl
        else ty :: tyl)
      [] tyl
  in List.rev params

and val_of_type_declaration of_path id decl =

  reset();

  let params = filter_params decl.type_params in

  aliased := params @ !aliased;
  List.iter mark_loops params;
  List.iter check_name_of_type (List.map proxy params);
  begin match decl.type_manifest with
  | None -> ()
  | Some ty -> mark_loops ty
  end;
  begin match decl.type_kind with
  | Type_abstract -> ()
  | Type_variant [] -> ()
  | Type_variant cstrs ->
      List.iter (fun (_, args) -> List.iter mark_loops args) cstrs
  | Type_record(l, rep) ->
      List.iter (fun (_, _, ty) -> mark_loops ty) l
  end;

  let type_param =
    function
    | Rtyp_var (id) -> id
    | _ -> (-1) (* ??? *)
  in
  let type_defined decl =
    if decl.type_kind = Type_abstract && decl.type_manifest = None
       && List.exists (fun x -> x <> (true, true)) decl.type_variance then
      (Ident.name id,
       List.combine
         (List.map (fun ty -> type_param (val_type_of_typexp of_path ty)) params)
         decl.type_variance)
    else
      let ty =
        val_type_of_typexp run_ident_of_path
          (Btype.newgenty (Tconstr(Pident id, params, ref Mnil)))
      in
      match ty with
      | Rtyp_constr (_, tyl) ->
          (Ident.name id, List.map (fun ty -> (type_param ty, (true, true))) tyl)
      | _ -> assert false
  in
  let val_type_of_manifest decl ty1 =
    match decl.type_manifest with
    | None -> ty1
    | Some ty -> Rdecl_manifest (val_type_of_typexp of_path ty, ty1)
  in
  let (name, args) = type_defined decl in
  let constraints = val_of_constraints of_path params in
  let ty =
    match decl.type_kind with
    | Type_abstract ->
        begin match decl.type_manifest with
        | None -> Rdecl_abstract
        | Some ty -> Rdecl_manifest (val_type_of_typexp of_path ty, Rdecl_abstract)
        end
    | Type_variant cstrs ->
        val_type_of_manifest decl (Rdecl_sum (List.map (val_type_of_constructor of_path) cstrs))
    | Type_record(lbls, rep) ->
        val_type_of_manifest decl (Rdecl_record (List.map (val_type_of_label of_path) lbls))
  in
  (name, args, ty, constraints)

and val_type_of_constructor of_path (name, args) =
  (name, val_types_of_typlist of_path args)

and val_type_of_label of_path (name, mut, arg) =
  (name, mut = Mutable, val_type_of_typexp of_path arg)
;;


let detect_mutual_recursives get_subnodes start =
  let loop = ref [start] in
  let add lst set =
    List.iter (fun x -> if not (List.mem x !set) then set := x :: !set) lst
  in
  let rec aux chain node =
    let chain' = node :: chain in
    if List.mem node !loop then 
      add chain' loop (* loop with start *)
    else if List.mem node chain then 
      () (* another loop, but start is not there *)
    else List.iter (aux chain') (get_subnodes node)
  in
  List.iter (aux []) (get_subnodes start);
  !loop
;;

let get_ident_of_path = function
  | Pident i -> i
  | Pdot (_,s,i) -> Ident.create_with_stamp s i
  | _ -> assert false
;;

let extract_used_paths_and_decls env path =
  let path_decls = ref [] in

  let paths_of_type typ = 
    let acc = ref [] in
    let rec aux acc = function
      | Rtyp_var _ -> ()
      | Rtyp_arrow (_,t1,t2) -> aux acc t1; aux acc t2
      | Rtyp_tuple tls -> List.iter (aux acc) tls 
      | Rtyp_constr (p,args) ->
	  if not (List.mem p !acc) then acc := p :: !acc;
	  List.iter (aux acc) args
    in
    aux acc typ;
    !acc
  in

  let paths_of_decl (name, args, decl, constraints) =
    let acc = ref [] in
    let add set lst =
      List.iter (fun x -> if not (List.mem x !set) then set := x :: !set) lst
    in
    List.iter (fun (t1,t2) ->
      add acc (paths_of_type t1);
      add acc (paths_of_type t2)) constraints;
    let rec aux acc = function
      | Rdecl_abstract -> ()
      | Rdecl_manifest (typ, Rdecl_abstract) -> 
	  add acc (paths_of_type typ)
      | Rdecl_manifest (_, decl) -> aux  acc decl
      | Rdecl_sum vars -> 
	  List.iter (fun (_,typs) -> List.iter (fun typ -> 
	    add acc (paths_of_type typ)) typs) vars
      | Rdecl_record ltyps ->
	  List.iter (fun (_,_,typ) -> 
	    add acc (paths_of_type typ)) ltyps
    in
    aux acc decl;
    !acc
  in

  let paths_of_datatype path =
(*
Format.fprintf Format.err_formatter "Paths of %a ..." Printtyp.path path;
Format.pp_print_newline Format.err_formatter ();
*)
    let vdecl =
      try
	let decl = 
	  Env.find_type path env 
	in
	val_of_type_declaration (fun x -> x) (get_ident_of_path path) decl
      with
      |	Not_found ->
(*
Format.fprintf Format.err_formatter "Definition of %a is not available..." Printtyp.path path;
Format.pp_print_newline Format.err_formatter ();
*)
	  Ident.name (get_ident_of_path path), [], Rdecl_abstract, []
    in
    if not (List.mem_assoc path !path_decls) then
      path_decls := (path, vdecl) :: !path_decls;
    let paths = paths_of_decl vdecl in
(*
Format.fprintf Format.err_formatter "Paths of %a done" Printtyp.path path;
Format.pp_print_newline Format.err_formatter ();
*)
    paths
  in
  
  let recursives = detect_mutual_recursives paths_of_datatype path in
  !path_decls, recursives
;; 

type occurrence =
  | Occ_Recursive of string
  | Occ_Digest of string

let tree_of_stype = 
  tree_of_val_type 
    (function Occ_Recursive s -> Oide_ident s
            | Occ_Digest s -> Oide_ident (Rtype.string_of_digest s));;

let rec tree_of_type_decl_desc = function
  | Rdecl_abstract -> Otyp_abstract
  | Rdecl_manifest (t, Rdecl_abstract) -> 
      Otyp_manifest (tree_of_stype t, Otyp_abstract)
  | Rdecl_manifest (_, decl) ->
      tree_of_type_decl_desc decl
  | Rdecl_sum consts ->
      Otyp_sum (List.map (fun (s,args) -> s, List.map tree_of_stype args)
		  consts)
  | Rdecl_record labls ->
      Otyp_record (List.map (fun (l,m,arg) ->
	l,m, tree_of_stype arg) labls )
;;

let rec tree_of_type_declaration (name, args, desc, constraints) =
  name, (List.map (fun (i,bs) -> sname_of_type i, bs) args), 
  tree_of_type_decl_desc desc, 
  (List.map (fun (t1,t2) -> tree_of_stype t1, tree_of_stype t2) constraints)
;;

type digest =
  | Abstract
  | Digest of string

let digest_cache = Hashtbl.create 31;;

let rec type_digest env path =
  try 
    let digest = Hashtbl.find digest_cache path in
    digest
  with Not_found ->
    let path_decls, recursives = extract_used_paths_and_decls env path in
    if List.mem path Predef.builtin_abstract_types then begin
      Digest (Ident.name (get_ident_of_path path))
    end else begin
      Format.printf "TYPE DIGEST of %a\n" Printtyp.path path;
      Format.printf "used paths: ";
      List.iter (fun (p,_) ->
  	Format.printf "%a " Printtyp.path p) path_decls;
      Format.print_newline ();
      Format.printf "recursives: ";
      List.iter (fun p ->
  	Format.printf "%a " Printtyp.path p) recursives;
      Format.print_newline ();
  
      try
	(* if one of datatype is abstract, we give up *)
	List.iter (fun (p, (_,_,decl_desc,_)) ->  
	  if decl_desc = Rdecl_abstract &&
	     not (List.mem p Predef.builtin_abstract_types)
	  then raise Exit) path_decls;

  	let rec replace_type = function
  	  | Rtyp_var i -> Rtyp_var i
  	  | Rtyp_arrow (l,t1,t2) -> Rtyp_arrow(l,replace_type t1, replace_type t2)
  	  | Rtyp_tuple tls -> Rtyp_tuple (List.map replace_type tls)
  	  | Rtyp_constr (p,tls) ->
  	      let p' =
  		if List.mem p recursives then 
  		  Occ_Recursive (Ident.name (get_ident_of_path p))
  		else begin 
		  match type_digest env p with
		  | Digest s -> Occ_Digest s
		  | _ -> assert false
		end
  	      in 
  	      Rtyp_constr (p', List.map replace_type tls)
  	in
  	let rec replace_decl = function
  	  | Rdecl_abstract -> Rdecl_abstract
  	  | Rdecl_manifest (t,Rdecl_abstract) -> 
  	      Rdecl_manifest (replace_type t, Rdecl_abstract)
  	  | Rdecl_manifest (_,decl) -> replace_decl decl
  	  | Rdecl_sum consts ->
  	      Rdecl_sum (List.map (fun (c,args) ->
  		c, List.map replace_type args) consts)
  	  | Rdecl_record labls ->
  	      Rdecl_record (List.map (fun (l,m,arg) ->
  		l,m,replace_type arg) labls)
  	in
  	let rec replace_variance_of_args args =
	  (* we believe that variance never affects value representations *)
	  List.map (fun (s,_) -> s,(true, true)) args
	in
  	let related_path_decls = 
  	  List.map (fun p -> 
  	    let (name, args, decl, constraints) = List.assoc p path_decls in
  	    p, (name, replace_variance_of_args args, replace_decl decl, 
  		(List.map (fun (t1,t2) ->
  		  replace_type t1, replace_type t2) constraints))) recursives 
  	in
  	(* we suppose that we have no duplicated names 
  	   (it is a normal assumption) *)
  	let sorted_related_path_decls =
  	  List.sort (fun (_,(s1,_,_,_)) (_,(s2,_,_,_)) -> compare s1 s2)
  	    related_path_decls
  	in
  	Format.fprintf Format.err_formatter "RELATED TYPE DECLS";
  	Format.pp_print_newline Format.err_formatter ();
  	List.iter (fun (p, decl) ->
  	  !Printtyp.outcome_sig_item Format.err_formatter
  	    (Osig_type [tree_of_type_declaration decl]);
  	  Format.pp_print_newline Format.err_formatter ();
  	    ) sorted_related_path_decls;
    
  	let raw_digest = 
  	  Digest.string (Marshal.to_string (List.map snd sorted_related_path_decls) [Marshal.No_sharing])
  	in
  	List.iter (fun (path,(name,_,_,_)) ->
  	  Hashtbl.add digest_cache path (Digest (name^"_"^raw_digest)))
	    sorted_related_path_decls; 
	Hashtbl.find digest_cache path
      with
      |	Exit -> (* some type is abstract... *)
  	  List.iter (fun path ->
  	    Hashtbl.add digest_cache path Abstract) recursives;
	  Hashtbl.find digest_cache path
    end
;;

type error = Contains_abstract_type of type_expr * Path.t
exception Error of error

let run_type_of_typexp env ty =
  val_type_of_typexp (fun p -> 
    let ri = run_ident_of_path p
    and digest = type_digest env p in
    match digest with
    | Digest d -> ri, d
    | _ -> raise (Error (Contains_abstract_type (ty, p)))) ty
;;

let transl_run_type_of_typexp env ty =
  reset ();
  transl_run_type (run_type_of_typexp env ty)
;;
