open Types
open Typedtree
open Path

type validity = Valid | Invalid | Unknown | HighFailure | Timeout

(* 1: constructor true; 0: constructor false; 2: other constructor *)

type bool_info = Ptrue | Pfalse | Pothers

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
| Pident (id) -> Ident.unique_name id
| Pdot (t, str, i) -> str
| Papply (t1, t2) -> getop t2

let is_expression_prop exp = match exp.exp_desc with 
| Texp_apply(e0, args) -> begin match e0.exp_desc with
  | Texp_ident(path, vd) ->  begin match getop path with
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
| _ -> false

let rec is_expression_argable exp = match exp.exp_desc with
| Texp_ident _ | Texp_constant _ -> true
| Texp_apply (e, args) -> is_expression_argable e && 
    List.for_all (fun (eopt, _) -> match eopt with
    | Some a -> is_expression_argable a
    | None -> false) args
| _ -> false
