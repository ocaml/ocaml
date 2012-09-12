open Types
open Typedtree
open Path
open Format

let should_trace = true
(* try ignore(Sys.getenv "-trace"); true
   with Not_found -> false *)

let trace msg fn arg = if should_trace 
  then (printf "%s:\n" msg; fn arg; print_newline())

let mtrace msg = if should_trace
    then printf "%s\n" msg

let rec print_list f xs = match xs with
 | [] -> print_string " **list\n"
 | x::l -> f x; print_list f l

exception Not_bound_var

type validity = Valid | Invalid | Unknown | HighFailure | Timeout

(* 1: constructor true; 0: constructor false; 2: other constructor *)

type bool_info = Ptrue | Pfalse | Pothers

(* functions related expressions and patterns *)

let rec add_pat_expr_list_to_tbl xs tbl = match xs with
  | [] -> tbl
  | (p,e)::l -> begin match p.pat_desc with
    | Tpat_var (id) -> let new_tbl = Tbl.add (Pident id) e tbl in
                       add_pat_expr_list_to_tbl l new_tbl
    | _ -> add_pat_expr_list_to_tbl l tbl
    end 

let rec pattern_to_expression p = 
  let desc = match p.pat_desc with
| Tpat_var(id) -> 
    let vd = {val_type = p.pat_type; val_kind = Val_reg} in
    Texp_ident(Pident id, vd)
| Tpat_constant (c) -> Texp_constant (c)
| Tpat_construct(path, cdesc, ps) -> 
    Texp_construct(path, cdesc, List.map pattern_to_expression ps)
| Tpat_tuple(ps) -> 
    Texp_tuple(List.map pattern_to_expression ps)
| _ -> failwith "escSyn.ml : pattern is not handled"
 in {exp_desc = desc;
     exp_loc  = p.pat_loc;
     exp_type = p.pat_type;
     exp_env  = p.pat_env}

let is_pattern_true pat = match pat.pat_desc with
| Tpat_construct (path, cdesc, ps) -> 
  if path = Predef.path_bool 
  then if cdesc.cstr_tag = Cstr_constant 1
       then Ptrue
       else Pfalse
  else Pothers
| _ -> Pothers

let is_expression_true exp = match exp.exp_desc with
| Texp_construct (path, cdesc, es) -> 
    if path = Predef.path_bool
	then if cdesc.cstr_tag = Cstr_constant 1
	    then Ptrue
	    else Pfalse
    else Pothers
| _ -> Pothers

let is_expression_bool exp = let ty = exp.exp_type in
match (Ctype.repr ty).desc with
| Tconstr (path, es, abbr) -> path = Predef.path_bool
| _ -> false

let rec getop path = match path with
| Pident (id) -> Ident.name id
| Pdot (t, str, i) -> str
| Papply (t1, t2) -> getop t2

let get_primitive_op exp = match exp.exp_desc with
 | Texp_ident(path, vd) -> getop path
 | _ -> "nop"

let is_expression_prop exp = match exp.exp_desc with 
| Texp_apply(e0, args) -> begin match e0.exp_desc with
  | Texp_ident(path, vd) -> begin match getop path with
      | ">" | ">=" | "=" | "<" | "<=" -> true
      | _ -> false
	    end 
  | Texp_apply(op, args1) -> begin match op.exp_desc with
    | Texp_ident(path, vd) -> begin match getop path with
      | ">" | ">=" | "=" | "<" | "<=" -> true
      | _ -> false
	    end 
    | _ -> false
   end
  | _ -> false
 end
| _ ->  false

let is_expression_primitiveop exp = match exp.exp_desc with
| Texp_ident (path, vd) -> begin match vd.val_kind with
  | Val_prim _ -> true
  | _ -> false
 end
| _ -> false

let rec is_expression_tv exp = match exp.exp_desc with
| Texp_constant _ -> true
| Texp_ident _ -> true
| Texp_construct (_, _, es) -> List.for_all is_expression_tv es
| Texp_tuple (es) -> List.for_all is_expression_tv es
| _ -> false

let rec is_expression_tvalue exp = is_expression_tv exp ||
 (match (Ctype.repr exp.exp_type).desc with
   | Tarrow _ -> true
   | _ -> false)

let rec is_expression_argable exp = match exp.exp_desc with
| Texp_ident _ | Texp_constant _ -> true
| Texp_apply (e, args) -> is_expression_argable e && 
    List.for_all (fun (eopt, _) -> match eopt with
    | Some a -> is_expression_argable a
    | None -> false) args
| Texp_construct (path, cdesc, es) -> 
    List.for_all is_expression_argable es
| Texp_tuple (es) -> 
    List.for_all is_expression_argable es
| _ -> false

let exn_to_bad exp = 
  let new_desc = match exp.exp_desc with
  (* | Texp_ident (path, value_desc) -> 
      if (Path.name path) = "Pervasives.failwith"
      then Texp_bad (Callee (exp.exp_loc, path))
      else exp.exp_desc *)
  | Texp_apply(e1,args) ->
     begin match e1.exp_desc with
     | Texp_ident(path, value_desc) when (Path.name path) = "Pervasives.failwith" ->
         Texp_bad (Callee (exp.exp_loc, path))
     | _ ->   
      let ty = exp.exp_type in
      begin match ty.desc with
      | Tconstr(path, _, _) when path = Predef.path_exn ->
         Texp_bad (Callee (exp.exp_loc, path))
      | _ -> exp.exp_desc
      end
     end
  | others -> others
  in {exp with exp_desc = new_desc}

let pre_processing exp =  map_expression exn_to_bad exp

let rec add x d t = match t with
| [] -> [(x,d)]
| ((i,v)::l) -> if Ident.unique_name x = Ident.unique_name i
                 then (x,d)::l
                 else (i,v)::(add x d l)

let rec find x t = match t with
| [] -> raise Not_bound_var
| ((i,v)::l) -> if Ident.unique_name x = Ident.unique_name i
                 then v
                 else find x l

let rec memIdent x t = match t with
   | [] -> false
   | i::l -> if Ident.unique_name x = Ident.unique_name i
              then true 
              else memIdent x l
  
let rec memPath p ps = match ps with
         | [] -> false
         | x::l -> begin match (p,x) with
                   | (Pident id1, Pident id2) -> 
		     if Ident.name id1 = Ident.name id2 then true
		     else memPath p l
		   | _ -> memPath p l
                   end 

let rec intersection xs ys = match xs with
  | [] -> []
  | x::l -> if memPath x ys then x::(intersection l ys)
            else intersection l ys

let rec rename_path_ident gamma path = match path with
 | Pident id ->  begin try Pident (find id gamma)
  	         with Not_bound_var -> path 
                 end
 | Pdot (p, s, pos) -> Pdot (rename_path_ident gamma p, s, pos)
 | Papply (p1, p2) -> Papply(p1, rename_path_ident gamma p2)

let rec rename_path_id boundvars gamma path = match path with
 | Pident id ->  if memIdent id boundvars
                 then path
                 else begin try Pident (find id gamma)
  	                    with Not_bound_var -> path 
                      end
 | Pdot (p, s, pos) -> Pdot (rename_path_ident gamma p, s, pos)
 | Papply (p1, p2) -> Papply(p1, rename_path_ident gamma p2)

(* subst replace free variables id1 in expr by id2 *)
let rec subst bound_vars gamma expr = match expr.exp_desc with
 | Texp_ident (path, vd) -> 
     {expr with exp_desc = Texp_ident (rename_path_id [] gamma path, vd)}
 | Texp_constant (c) -> expr
 | Texp_let (rec_flag, pat_expr_list, e2) -> 
     let bvars     = let_bound_idents pat_expr_list in
     let p_e_list  = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) gamma e)) 
	                   pat_expr_list in
     let new_e2 = subst bound_vars gamma e2 in
     {expr with exp_desc = Texp_let (rec_flag, p_e_list, new_e2)}
 | Texp_function (pat_expr_list, ptial) -> 
     let bvars     = let_bound_idents pat_expr_list in
     let p_e_list  = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) gamma e)) 
	                   pat_expr_list in
     {expr with exp_desc = Texp_function (p_e_list, ptial)}
 | Texp_apply (e1, es) ->
     let new_e1 = subst bound_vars gamma e1 in
     let new_es = List.map (fun (e_opt, optl) -> match e_opt with
                            | Some e -> (Some (subst bound_vars gamma e), optl)
                            | None -> (None, optl)) es
     in {expr with exp_desc = Texp_apply (new_e1, new_es)}
 | Texp_match (e0, pat_expr_list, ptial) -> 
     let new_e0   = subst bound_vars gamma e0 in
     let bvars    = let_bound_idents pat_expr_list in
     let p_e_list = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) gamma e)) 
 	                     pat_expr_list in
     {expr with exp_desc = Texp_match (new_e0, p_e_list, ptial)} 
 | Texp_try (e1, pat_expr_list) -> 
     let new_e1   = subst bound_vars gamma e1 in
     let bvars    = let_bound_idents pat_expr_list in
     let p_e_list = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) gamma e)) 
 	                     pat_expr_list in
     {expr with exp_desc = Texp_try (new_e1, p_e_list)}
 | Texp_tuple (es) -> 
     let new_es = List.map (subst bound_vars gamma) es in
     {expr with exp_desc = Texp_tuple new_es}
 | Texp_construct (path, cdesc, es) -> 
     let new_es = List.map (subst bound_vars gamma) es in
     {expr with exp_desc = Texp_construct (path, cdesc, new_es)}
 | Texp_variant (lbl, e_opt) -> 
     let new_e = match e_opt with
     | Some e -> Some (subst bound_vars gamma e)
     | None -> None
     in {expr with exp_desc = Texp_variant (lbl, new_e)}
 | Texp_record (ldesc_expr_list, e_opt) ->
     let l_e_list = List.map (fun (p,e) -> (p, subst bound_vars gamma e)) 
 	                     ldesc_expr_list in
     {expr with exp_desc = Texp_record (l_e_list, match e_opt with
     | Some e -> Some (subst bound_vars gamma e)
     | None -> None)}
 | Texp_field (e, ldesc) -> 
     let new_e = subst bound_vars gamma e in
     {expr with exp_desc = Texp_field (new_e, ldesc)}
 | Texp_setfield (e1, ldesc, e2) -> 
     let new_e1 = subst bound_vars gamma e1 in
     let new_e2 = subst bound_vars gamma e2 in
     {expr with exp_desc = Texp_setfield (new_e1, ldesc, new_e2)}
 | Texp_array (es) -> 
     let new_es = List.map (subst bound_vars gamma) es in
     {expr with exp_desc = Texp_tuple new_es}
 | Texp_ifthenelse (e0, e1, e2_opt) -> 
     let new_e0 = subst bound_vars gamma e0 in
     let new_e1 = subst bound_vars gamma e1 in
     let new_e2 = match e2_opt with
     | Some e -> Some (subst bound_vars gamma e)
     | None -> None 
     in {expr with exp_desc = Texp_ifthenelse (new_e0, new_e1, new_e2)}    
 | Texp_sequence (e1, e2) -> 
     let new_e1 = subst bound_vars gamma e1 in
     let new_e2 = subst bound_vars gamma e2 in
     {expr with exp_desc = Texp_sequence (new_e1, new_e2)}    
 | Texp_while (e1, e2) -> 
     let new_e1 = subst bound_vars gamma e1 in
     let new_e2 = subst bound_vars gamma e2 in
     {expr with exp_desc = Texp_while (new_e1, new_e2)}    
 | Texp_for (id, e1, e2, dflag, e3) ->
     let new_e1 = subst bound_vars gamma e1 in
     let new_e2 = subst bound_vars gamma e2 in
     let new_e3 = subst bound_vars gamma e3 in
     {expr with exp_desc = Texp_for (id, new_e1, new_e2, dflag, new_e3)}    
  | Texp_when (e1, e2) -> 
     let new_e1 = subst bound_vars gamma e1 in
     let new_e2 = subst bound_vars gamma e2 in
     {expr with exp_desc = Texp_when (new_e1, new_e2)}    
  | Texp_send (e, meth) -> 
     let new_e = subst bound_vars gamma e in
     {expr with exp_desc = Texp_send (new_e, meth)}    
  | Texp_new _ -> expr
  | Texp_instvar _ -> expr
  | Texp_setinstvar (path1, path2, e) -> 
     let new_e = subst bound_vars gamma e in
     {expr with exp_desc = Texp_setinstvar(path1, path2, new_e)}
  | Texp_override (path1, path_expr_list) -> 
     let p_e_list = List.map (fun (p,e) -> (p, subst bound_vars gamma e)) 
 	                      path_expr_list in
     {expr with exp_desc = Texp_override (path1, p_e_list)}
  | Texp_letmodule (id, modexpr, e) -> 
      let new_e = subst bound_vars gamma e in
      {expr with exp_desc = Texp_letmodule (id, modexpr, new_e)}
  | Texp_assert (e) -> 
      let new_e = subst bound_vars gamma e in
      {expr with exp_desc = Texp_assert(new_e)}
  | Texp_assertfalse -> expr
  | Texp_lazy (e) -> 
      let new_e = subst bound_vars gamma e in
      {expr with exp_desc = Texp_assert(new_e)}
  | Texp_object _ -> expr
  | Texp_pack _ -> expr
  | Texp_local_contract (c, e) -> 
      let new_e = subst bound_vars gamma e in
      {expr with exp_desc = Texp_local_contract(c, new_e)}
  | Texp_contract (c, e1, e2, e3) -> 
      let new_e1 = subst bound_vars gamma e1 in
      let new_e2 = subst bound_vars gamma e2 in
      let new_e3 = subst bound_vars gamma e3 in
      {expr with exp_desc = Texp_contract (c, new_e1, new_e2, new_e3)}    
  | Texp_bad _ -> expr
  | Texp_unr _ -> expr
  | Texp_Lambda (ids, e) -> 
      let new_e = subst bound_vars gamma e in
      {expr with exp_desc = Texp_Lambda (ids, new_e)}
  | Texp_App (e1, es) -> 
      let new_e1 = subst bound_vars gamma e1 in
      let new_es = List.map (subst bound_vars gamma) es in
      {expr with exp_desc = Texp_App(new_e1, new_es)}
  | Texp_raise (e) -> expr

let rec rename_pattern gamma pat = match pat.pat_desc with
| Tpat_var(id) -> 
     {pat with pat_desc = try Tpat_var (find id gamma)
                 with Not_found -> Tpat_var(id)}
   
| d -> {pat with pat_desc = map_pattern_desc (rename_pattern gamma) d}

let rec rename_boundvars gamma expr = match expr.exp_desc with
 | Texp_ident (path, vd) ->
     let new_id = rename_path_ident gamma path in
     {expr with exp_desc = Texp_ident (new_id, vd)}
 | Texp_constant (c) -> expr
 | Texp_let (rec_flag, pat_expr_list, e2) -> 
     let p_e_list  = List.map (fun (p,e) -> (p, rename_boundvars gamma e)) 
	                   pat_expr_list in
     let new_e2 = rename_boundvars gamma e2 in
     {expr with exp_desc = Texp_let (rec_flag, p_e_list, new_e2)}
 | Texp_function (pat_expr_list, ptial) ->      
     let p_e_list  = List.map (fun (p,e) -> 
       match p.pat_desc with
       | Tpat_var(id) -> 
	   let new_id = Ident.rename id in
	   let new_gamma = add id new_id gamma in
	   ({p with pat_desc = Tpat_var(new_id)}, rename_boundvars new_gamma e)
       | Tpat_construct(path, cdesc, ps) -> 
	  let bvars = let_bound_idents pat_expr_list in
	  let new_gamma = List.fold_right (fun i default -> 
		    add i (Ident.rename i) default) bvars gamma in
	  let new_ps = List.map (rename_pattern new_gamma) ps in
          ({p with pat_desc = Tpat_construct(path, cdesc, new_ps)},
            rename_boundvars new_gamma e)
       | _ -> (p, e))
	                   pat_expr_list in
     {expr with exp_desc = Texp_function (p_e_list, ptial)}
 | Texp_apply (e1, es) ->
     let new_e1 = rename_boundvars gamma e1 in
     let new_es = List.map (fun (e_opt, optl) -> match e_opt with
                            | Some e -> (Some (rename_boundvars gamma e), optl)
                            | None -> (None, optl)) es
     in {expr with exp_desc = Texp_apply (new_e1, new_es)}
 | Texp_match (e0, pat_expr_list, ptial) -> 
     let new_e0   = rename_boundvars gamma e0 in
     let p_e_list = List.map (fun (p,e) -> 
       match p.pat_desc with
       | Tpat_var(id) -> 
	   let new_id = Ident.rename id in
	   let new_gamma = add id new_id gamma in
	   ({p with pat_desc = Tpat_var(new_id)}, rename_boundvars new_gamma e)
       | Tpat_construct(path, cdesc, ps) -> 
	  let bvars = List.fold_right (fun pi default -> 
	                       default@(pat_bound_idents pi)) ps [] in
	  let newbvars = List.map Ident.rename bvars in
          let blist = List.combine bvars newbvars in
	  let new_gamma = List.fold_right (fun (i,ni) default ->
	                   add i ni default) blist gamma in
	  let new_ps = List.map (rename_pattern new_gamma) ps in
          ({p with pat_desc = Tpat_construct(path, cdesc, new_ps)},
            rename_boundvars new_gamma e)
       | Tpat_tuple(ps) -> 
	  let bvars = List.fold_right (fun pi default -> 
	                       default@(pat_bound_idents pi)) ps [] in
	  let newbvars = List.map Ident.rename bvars in
          let blist = List.combine bvars newbvars in
	  let new_gamma = List.fold_right (fun (i,ni) default ->
	                   add i ni default) blist gamma in
	  let new_ps = List.map (rename_pattern new_gamma) ps in
          ({p with pat_desc = Tpat_tuple(new_ps)},
            rename_boundvars new_gamma e)
       | _ -> (p,e))
 	                     pat_expr_list in
     {expr with exp_desc = Texp_match (new_e0, p_e_list, ptial)} 
 | Texp_try (e1, pat_expr_list) -> 
     let new_e1   = rename_boundvars gamma e1 in
     let p_e_list = List.map (fun (p,e) -> (p, rename_boundvars gamma e)) 
 	                     pat_expr_list in
     {expr with exp_desc = Texp_try (new_e1, p_e_list)}
 | Texp_tuple (es) -> 
     let new_es = List.map (rename_boundvars gamma) es in
     {expr with exp_desc = Texp_tuple new_es}
 | Texp_construct (path, cdesc, es) -> 
     let new_es = List.map (rename_boundvars gamma) es in
     {expr with exp_desc = Texp_construct (path, cdesc, new_es)}
 | Texp_variant (lbl, e_opt) -> 
     let new_e = match e_opt with
     | Some e -> Some (rename_boundvars gamma e)
     | None -> None
     in {expr with exp_desc = Texp_variant (lbl, new_e)}
 | Texp_record (ldesc_expr_list, e_opt) ->
     let l_e_list = List.map (fun (p,e) -> (p, rename_boundvars gamma e)) 
 	                     ldesc_expr_list in
     {expr with exp_desc = Texp_record (l_e_list, match e_opt with
     | Some e -> Some (rename_boundvars gamma e)
     | None -> None)}
 | Texp_field (e, ldesc) -> 
     let new_e = rename_boundvars gamma e in
     {expr with exp_desc = Texp_field (new_e, ldesc)}
 | Texp_setfield (e1, ldesc, e2) -> 
     let new_e1 = rename_boundvars gamma e1 in
     let new_e2 = rename_boundvars gamma e2 in
     {expr with exp_desc = Texp_setfield (new_e1, ldesc, new_e2)}
 | Texp_array (es) -> 
     let new_es = List.map (rename_boundvars gamma) es in
     {expr with exp_desc = Texp_tuple new_es}
 | Texp_ifthenelse (e0, e1, e2_opt) -> 
     let new_e0 = rename_boundvars gamma e0 in
     let new_e1 = rename_boundvars gamma e1 in
     let new_e2 = match e2_opt with
     | Some e -> Some (rename_boundvars gamma e)
     | None -> None 
     in {expr with exp_desc = Texp_ifthenelse (new_e0, new_e1, new_e2)}    
 | Texp_sequence (e1, e2) -> 
     let new_e1 = rename_boundvars gamma e1 in
     let new_e2 = rename_boundvars gamma e2 in
     {expr with exp_desc = Texp_sequence (new_e1, new_e2)}    
 | Texp_while (e1, e2) -> 
     let new_e1 = rename_boundvars gamma e1 in
     let new_e2 = rename_boundvars gamma e2 in
     {expr with exp_desc = Texp_while (new_e1, new_e2)}    
 | Texp_for (id, e1, e2, dflag, e3) ->
     let new_e1 = rename_boundvars gamma e1 in
     let new_e2 = rename_boundvars gamma e2 in
     let new_e3 = rename_boundvars gamma e3 in
     {expr with exp_desc = Texp_for (id, new_e1, new_e2, dflag, new_e3)}    
  | Texp_when (e1, e2) -> 
     let new_e1 = rename_boundvars gamma e1 in
     let new_e2 = rename_boundvars gamma e2 in
     {expr with exp_desc = Texp_when (new_e1, new_e2)}    
  | Texp_send (e, meth) -> 
     let new_e = rename_boundvars gamma e in
     {expr with exp_desc = Texp_send (new_e, meth)}    
  | Texp_new _ -> expr
  | Texp_instvar _ -> expr
  | Texp_setinstvar (path1, path2, e) -> 
     let new_e = rename_boundvars gamma e in
     {expr with exp_desc = Texp_setinstvar(path1, path2, new_e)}
  | Texp_override (path1, path_expr_list) -> 
     let p_e_list = List.map (fun (p,e) -> (p, rename_boundvars gamma e)) 
 	                      path_expr_list in
     {expr with exp_desc = Texp_override (path1, p_e_list)}
  | Texp_letmodule (id, modexpr, e) -> 
      let new_e = rename_boundvars gamma e in
      {expr with exp_desc = Texp_letmodule (id, modexpr, new_e)}
  | Texp_assert (e) -> 
      let new_e = rename_boundvars gamma e in
      {expr with exp_desc = Texp_assert(new_e)}
  | Texp_assertfalse -> expr
  | Texp_lazy (e) -> 
      let new_e = rename_boundvars gamma e in
      {expr with exp_desc = Texp_assert(new_e)}
  | Texp_object _ -> expr
  | Texp_pack _ -> expr
  | Texp_local_contract (c, e) -> 
      let new_e = rename_boundvars gamma e in
      {expr with exp_desc = Texp_local_contract(c, new_e)}
  | Texp_contract (c, e1, e2, e3) -> 
      let new_e1 = rename_boundvars gamma e1 in
      let new_e2 = rename_boundvars gamma e2 in
      let new_e3 = rename_boundvars gamma e3 in
      {expr with exp_desc = Texp_contract (c, new_e1, new_e2, new_e3)}    
  | Texp_bad _ -> expr
  | Texp_unr _ -> expr
  | Texp_Lambda (ids, e) -> 
      let new_e = rename_boundvars gamma e in
      {expr with exp_desc = Texp_Lambda (ids, new_e)}
  | Texp_App (e1, es) -> 
      let new_e1 = rename_boundvars gamma e1 in
      let new_es = List.map (rename_boundvars gamma) es in
      {expr with exp_desc = Texp_App(new_e1, new_es)}
  | Texp_raise (e) -> expr


       
