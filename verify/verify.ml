open Asttypes
open Path
open Types
open Typedtree
open EscSyn
open ThmEnv
open ToErgosrc
open Esc
open Format

type error =
    Illegal_tuple_expr
  | Not_pat_var

exception Error of Location.t * error

(* naxu's temp file starts. *)

let fmt_contract_declaration ppf (cdecl:Types.contract_declaration) = 
  !(Oprint.out_contract_declaration []) ppf cdecl

(* naxu's temp file ends. *)

(* Translate wrapped expression e |><| c to unwrapped expression 
val transl_core_contract: 
   Typedtree.contract_declaration list ->         % contract decls in this module
   ('a * Types.contract_declaration) Ident.tbl -> % contract decls in opened modules
   Typedtree.core_contract ->                     % c
   Typedtree.expression ->                        % e
   Typedtree.expression ->                        % caller
   Typedtree.expression ->                        % callee
   Typedtree.expression 
*)

let rec transl_core_contract env cntr e callee caller =
  let cty  = e.exp_type in
  let mkpat desc ty = { pat_desc =  desc; 
                        pat_loc  = e.exp_loc;
                        pat_type = ty;
                        pat_env  = e.exp_env } in
  let mkexp ed ty = {e with exp_desc = ed ; exp_type = ty} in
  let mkident i ty = Texp_ident (Pident i, {val_type = ty;
                                            val_kind = Val_reg}) in
  (* below is the generate Contract_exn pattern *)
  let cexn_path = Predef.path_contract_failure in
  let cexn_ctag = Cstr_exception(cexn_path) in
  let dummy_type_expr = { desc = Tvar; level = 0; id =0 } in
  let cexn_cdesc = { cstr_res = Predef.type_exn;
                     cstr_args = [dummy_type_expr];
                     cstr_arity = 1;
                     cstr_tag = cexn_ctag;
                     cstr_consts = -1;
                     cstr_nonconsts = -1;
                     cstr_private = Asttypes.Public } in
  let iexp i ty = Texp_ident (Pident i, 
                   {val_type = ty; val_kind = Val_reg}) in 
  let ids = (Ident.create_idents "pn" 4) in
  let bls = [Predef.type_string; Predef.type_int; 
             Predef.type_int; Predef.type_string] in
  let pvars = List.map (fun (i,t) -> mkpat (Tpat_var i) t) 
               (List.combine ids bls) in
  let evars = List.map (fun (i,t) -> mkexp (iexp i t) t)
               (List.combine ids bls) in
  let dummy_tuple = { desc = Ttuple bls; level = 0; id =0 } in
  let cpat = Tpat_construct(cexn_path, cexn_cdesc, 
                 [mkpat (Tpat_tuple pvars) dummy_tuple]) in
  let contract_exn_pat = mkpat cpat Predef.type_exn in
  (* contract_exn_pat is the Contract_exn pattern *)
  let ce:expression_desc = match cntr.contract_desc with
     | Tctr_pred (x, p, exnop) -> 
       (* e |>r1,r2<| {x | p} =  let x = e in if p then x else r1 
          This forces evaluation of e, that is, if e diverges, RHS diverges;
          if e crashes, RHS crashes.
          Moreover, wrap free-variables in p with the variable's contract if any.
	  This detects non-crash-free contracts.
       *) 
        begin match is_expression_true p with
        | Ptrue ->  e.exp_desc
        | _ ->
        let new_x = Ident.create "x" in
        let vd = { val_type = e.exp_type; val_kind = Val_reg } in
	let xe = { e with exp_desc = Texp_ident(Pident new_x, vd) } in
        (* let wrapped_p = contract_id_in_expr env p in 
        let expanded_p = deep_transl_contract env wrapped_p in
        let cond = Texp_ifthenelse (subst [] [(x, new_x)] p, xe, Some callee) in 
        *)
        let cond = Texp_ifthenelse (subst [] [(x, new_x)] p, xe, Some callee) in 
	Texp_let (Nonrecursive, [(mkpat (Tpat_var new_x) cty, e)], mkexp cond cty) 
        end
        (* e |>r1,r2<| try {x | p} with [Ei -> booli] 
          if (try let x = e in p
              with Contract_exn (f,row,col,bl) -> raise Contract_exn (f,row,col,bl)
                    | Ei .. -> booli
                    | _ -> false)
          then e else r1
          This forces evaluation of e, that is, if e diverges, RHS diverges;
          However, if e throws an exception, we still test p[e/x], which may 
          allow certain exceptions. If p[e/x] throws exceptions, we treat it
          as false, thus raise exception r1.
        *)
        (*
        begin match exnop with
        | None -> 
        let xe = Texp_ident (Pident x, {val_type = cty; val_kind = Val_reg}) in
        let cond = Texp_ifthenelse (p, mkexp xe cty, Some callee) in
	Texp_let (Nonrecursive, [(mkpat (Tpat_var x) cty, e)], mkexp cond cty) 
        | Some exns -> 
	let letexp = Texp_let (Nonrecursive, [(mkpat (Tpat_var x) cty, e)], p) in
        let ctr_exn = Texp_construct(cexn_path, cexn_cdesc, 
                       [mkexp (Texp_tuple evars) dummy_tuple]) in
        let raise_ctr_exn = Texp_raise (mkexp ctr_exn Predef.type_exn) in 
        let contract_exn_branch = (contract_exn_pat,
                                   mkexp raise_ctr_exn Predef.type_bool) in
        let branches = (contract_exn_branch :: exns) @     
                    [(mkpat Tpat_any Predef.type_exn,     (* the -> false branch *)
                     mkexp (Texp_constant(Const_int 0)) Predef.type_bool)] in
	let trycond = Texp_try(mkexp letexp Predef.type_bool, branches) in
        Texp_ifthenelse (mkexp trycond Predef.type_bool, e, Some callee) 
        end
        *)
     | Tctr_arrow (xop, c1, c2) -> 
      (* indy version:
         e |>r1,r2<| x:c1 -> c2 = 
	   \v. (e (v |>r2,r1<| c1)) |>r1,r2<| c2[(v |>r2,r1<| c1)/x]
      *)      
         let c1_type = c1.contract_type in
         let c2_type = c2.contract_type in
         let res = match xop with
         | Some x -> 
             let v = Ident.create "v" in
	     let xvar = mkexp (mkident v c1_type) c1_type in
             let v_c1 = transl_core_contract env c1 xvar caller callee in
	     let resarg = Texp_apply (e, [(Some v_c1, Required)]) in
              (* e.g. x:({a | a > 0} -> {b | true}) -> {c | x 0 > c} 
                 we want to blame the x in {c | x 0 > c}  *)	     
	     let new_callee = match callee.exp_desc with
             | Texp_bad (Caller (loc, _, _)) 
	     | Texp_bad (Callee (loc, _)) ->
	       { callee with exp_desc =
			 Texp_bad (Callee (c2.contract_loc, Pident x))}
             | Texp_unr (Caller (loc, _, _)) | Texp_unr (Callee (loc, _)) -> 
	       { callee with exp_desc =
				Texp_unr (Callee (c2.contract_loc, Pident x))}
             | _ -> callee in 
             let new_caller = match caller.exp_desc with
             | Texp_bad (Caller (loc, _, _)) | Texp_bad (Callee (loc, _)) -> 
		 { caller with exp_desc =
		   Texp_bad (Caller (c2.contract_loc,
				     Some (ThmEnv.contract_name env),
				     Pident x))}
             | Texp_unr (Caller (loc, _, _)) | Texp_unr (Callee (loc, _)) -> 
		 { caller with exp_desc =
		   Texp_unr (Caller (c2.contract_loc, 
				     Some (ThmEnv.contract_name env),
				     Pident x))}
             | _ -> caller in
	     
	     let env2 = add_dep_contracts env v c1 in
          (*   let v_c1_indy = transl_core_contract env2 c1 xvar caller callee in
	     let c2subst = subst_contract x v_c1_indy c2 in 
            
	     let resfun = transl_core_contract env c2subst 
                              (mkexp resarg c2_type) callee caller in
          *)
	     let c2subst = subst_contract x xvar c2 in 
	     let resfun = transl_core_contract env2 c2subst 
                               (mkexp resarg c2_type) new_callee new_caller in
	     Texp_function ([(mkpat (Tpat_var v) c1_type, resfun)], Partial)
         | None -> 
	     let v = Ident.create "v" in
             let xvar = mkexp (mkident v c1_type) c1_type in
             let c1x = transl_core_contract env c1 xvar caller callee in
   	     let resarg = Texp_apply (e, [(Some c1x, Required)]) in
             let resfun = transl_core_contract env c2 
		                               (mkexp resarg c2_type) callee caller in
             Texp_function ([(mkpat (Tpat_var v) c1_type, resfun)], Partial)
         in res
     | Tctr_tuple cs -> 
      (* e |>r1,r2<| (x:c1, c2) = match e with 
                           (x1, x2) -> (x1 |>r1,r2<| c1, 
                                        x2 |>r1,r2<| c2[(x1 |>r2,r1<| c1)/x])  *)
       begin 	
	  let new_ids =  (Ident.create_idents "x" (List.length cs))  in
          let typs = match (Ctype.repr cty).desc with
                         | Ttuple ts -> ts
                         | _ -> raise(Error(e.exp_loc,Illegal_tuple_expr)) in
          let (ps, es) = List.split (List.map (fun (i, t) -> 
                                let vd = {val_type = t; val_kind = Val_reg} in
				let exp = { exp_desc = Texp_ident (Pident i, vd);
					    exp_loc = e.exp_loc;
                                            exp_type = t;
					    exp_env = e.exp_env
					  } in
		   	        (mkpat (Tpat_var i) t, exp))
                            (List.combine new_ids typs)) in
          (* similar to dependent function contract, the substitution
            c2[(x1 |>r2,r1<| c1)/x] is done in transmod.ml *)
          let ces = List.map (fun ((vo,c), ei) -> 
                             transl_core_contract env c ei callee caller)
                             (List.combine cs es) in
          let newpat = { pat_desc = Tpat_tuple ps;
                         pat_loc = e.exp_loc;
                         pat_type = cty;
                         pat_env = e.exp_env } in
          Texp_match (e, [(newpat, mkexp (Texp_tuple ces) cty)], Total)
       end
     | Tctr_constr (p1, cdesc, cs) -> 
        (* this is constructor contract, which is the general case of tuple contract 
           e |>r1,r2<| K ci = match e with 
           | K x1 .. xn -> K (x1 |>r1,r2<| c1, .. ,
                              xn |>r1,r2<| cn [(xi |>r1,r2<| ci)/xi] )
           | _ -> r1 
        *)
        let ids = Ident.create_idents "px" (List.length cs) in
        let xi = List.map (fun (i,t) -> mkpat (Tpat_var i) t)
                          (List.combine ids cdesc.cstr_args) in
        let xi_rhs = List.map (fun (i,t) -> 
                          let vd = {val_type = t; val_kind = Val_reg} in
                          mkexp (Texp_ident (Pident i,vd)) t)
                          (List.combine ids cdesc.cstr_args) in
        (* similar to dependent function contract, the substitution
	   cn [(xi |>r1,r2<| ci)/xi] is done in transmod.ml *)
        let kxi = (mkpat (Tpat_construct (p1, cdesc, xi)) cdesc.cstr_res, 
                   mkexp (Texp_construct (p1, cdesc,  
                      List.map (fun ((vo, c), e) -> transl_core_contract env c e callee caller) 
                      (List.combine cs xi_rhs))) cdesc.cstr_res) in     
        Texp_match (e, [kxi; (mkpat Tpat_any cdesc.cstr_res, callee)], Total)
     | Tctr_and (c1, c2) -> 
         (* e |>r1,r2<| c1 and c2 = (e |>r1,r2<| c1) |>r1,r2<| c2 *)
         let res = transl_core_contract env c2 (transl_core_contract env c1 e callee caller) callee caller in
         res.exp_desc
     | Tctr_or (c1, c2) -> 
         (* e |>r1,r2<| c1 or c2 
             = try e |>r1,r2<| c1 with
                Contract_exn1 -> try e |>r1,r2<| c2 with
                                     Contract_exn2 -> raise Contract_exn2
                                   | _ -> e
              | _ -> e
         *)
         let e1 = transl_core_contract env c1 e callee caller in
         let e2 = transl_core_contract env c2 e callee caller in
         let ctr_exn = Texp_construct(cexn_path, cexn_cdesc, 
                       [mkexp (Texp_tuple evars) dummy_tuple]) in
         let raise_ctr_exn = Texp_raise (mkexp ctr_exn Predef.type_exn) in 
         let contract_exn_branch = (contract_exn_pat,
                                   mkexp raise_ctr_exn Predef.type_bool) in
         let branches2 = [contract_exn_branch; 
                          (mkpat Tpat_any Predef.type_exn, e)] in 
         let sndtry = mkexp (Texp_try(e2, branches2)) cty in
         let branches1 = [(contract_exn_pat, sndtry);
                         (mkpat Tpat_any Predef.type_exn, e)] in
         Texp_try(e1, branches1)      
     | Tctr_typconstr (p, cs) -> 
       (* TO BE DONE. this could be a disjunctive contract. *)
        e.exp_desc 
     | Tctr_var v -> e.exp_desc
       (* TO BE DONE
         let c = lookup for v
         let ce = transl_core_contract env c e callee caller in
         ce.exp_desc
       *)
     | Tctr_poly (vs, c) -> 
       (* e |>r1,r2<| 'a. c = /\'a. e |>r1,r2<| c *)
         let ce = transl_core_contract env c e callee caller in
         ce.exp_desc
  in mkexp ce cty

(* Given x:t1 -> t2, the subst_contract computes t2[(v |><| t1)/x] *)
and subst_contract v e cntr = 
  let mkpat var = { pat_desc = Tpat_var var; 
                    pat_loc  = e.exp_loc;
                    pat_type = e.exp_type;
                    pat_env  = e.exp_env } in
  let sc = match cntr.contract_desc with
             Tctr_pred (x, p, exnop) -> 
               (* {x | p} [e/v]   is expressed as  {x | let v = e in p} *) 
               let subst_p = Texp_let (Nonrecursive, [(mkpat v, e)], p) in
               Tctr_pred (x, { e with exp_desc = subst_p }, exnop) 
           | Tctr_arrow (xop, c1, c2) -> 
               let sc1 = subst_contract v e c1 in
               let sc2 = subst_contract v e c2 in
               Tctr_arrow (xop, sc1, sc2)
           | Tctr_tuple cs -> 
               Tctr_tuple (List.map (fun (vo,c) -> (vo, subst_contract v e c)) cs)
           | Tctr_constr (i, cdesc, cs) -> 
               Tctr_constr (i, cdesc, 
                 List.map (fun (vo, c) -> (vo, subst_contract v e c)) cs)
           | Tctr_and (c1, c2) -> 
	       let sc1 = subst_contract v e c1 in
               let sc2 = subst_contract v e c2 in
               Tctr_and (sc1, sc2)
           | Tctr_or (c1, c2) -> 
	       let sc1 = subst_contract v e c1 in
               let sc2 = subst_contract v e c2 in
               Tctr_or (sc1, sc2)
           | Tctr_typconstr (i, cs) -> 
               Tctr_typconstr (i, List.map (subst_contract v e) cs)
	   | Tctr_var v -> Tctr_var v
           | Tctr_poly (vs, c) -> Tctr_poly (vs, subst_contract v e c)
  in { cntr with contract_desc = sc }

(* deep_transl_contract takes e and expands all ei |><| ci in e *)
and deep_transl_contract env expr = 
      Typedtree.map_expression (fun ei -> match ei.exp_desc with
          | Texp_contract (c, e, r1, r2) -> 
	      begin match e.exp_desc with
	      | Texp_ident (path, vdesc) ->
		  let env2 = update_contract_name env path in
		  transl_core_contract env2 c e r1 r2
	      | _ -> transl_core_contract env c e r1 r2
              end
          | _ -> ei) expr
   
(* fetch_contract takes a function name and lookup its contract in the
set of contract declarations. *)
and fetch_contract_by_pattern p contract_decls = 
   List.find (fun x -> match p.pat_desc with
            | Tpat_var (i) -> begin
                       match x.ttopctr_id with
                       | Pident (j) -> j = i
    		       | _ -> false 
                     end       
            | _ -> false) contract_decls
      
and fetch_contract_by_path p contract_decls = 
  List.find (fun x -> match p with 
   | Pident (i) -> begin 
                    match x.ttopctr_id with
                    | Pident (j) -> j = i
                    | _ -> false
                   end
   | _ -> false) contract_decls

and find_in_ident_tbl path = 
 function
    Ident.Empty ->
      raise Not_found
  | Ident.Node(l, k, r, _) ->
      let (p, cdecl) = (Ident.tbl_data) k in
      let c = compare (Path.name path) (Path.name p) in
      if c = 0 then
        (Ident.tbl_data) k
      else
        find_in_ident_tbl path (if c < 0 then l else r)

(* local_fun_contracts contains x:t1 in contract x:t1 -> t2.
   contract_decls contains contract declaration in the current module
       contract f = {...} -> {...}
   opened_contracts contains contracts from open ... and 
   contracts exported from nested modules.
*)
and wrap_id_with_contract env expr = 
  let depContracts        = dep_contracts env in
  let cdecls              = contract_decls env in
  let opened_contracts    = opened_contract_decls env in
  let caller_path         = name env in
  let contracted_exp_desc = match expr.exp_desc with
       | Texp_ident (callee_path, value_desc) -> 
         let bl_caller = Caller (expr.exp_loc, Some caller_path, callee_path) in 
         let bl_callee = Callee (expr.exp_loc, callee_path) in     
         begin
          try  (* lookup for dependent contract first *)
           let id = match callee_path with
                     | Pident(i) -> i
                     | _ -> raise Not_found
           in
           let c = Ident.find_same id depContracts in
           requiresC c expr bl_caller bl_callee
          with Not_found -> 
          try (* lookup for contracts in current module *)
           let c = fetch_contract_by_path callee_path cdecls in
           requiresC c.ttopctr_desc expr bl_caller bl_callee
          with Not_found -> 
	  try (* lookup for contracts in opened modules *)
	   let (p, c) = find_in_ident_tbl callee_path opened_contracts in 
             requiresC (core_contract_from_iface c.Types.ttopctr_desc) 
		        expr bl_caller bl_callee 
 	  with Not_found -> 	                
	    expr.exp_desc
         end
       | others -> others
  in { expr with exp_desc = contracted_exp_desc }

(* contract_id_in_expr wrapped all functions called in expression with its contract
if it has any contract. E.g. f x = ..f (x - 1) ... g x
 becomes f x = ..(f <| C_f) (x - 1) ... (g <| C_g) x
val contract_id_in_expr :
           Typedtree.contract_declaration list ->
           ('a * Types.contract_declaration) Ident.tbl ->
           Path.t -> Typedtree.expression -> Typedtree.expression
*)
and contract_id_in_expr env expr = 
    map_expression (wrap_id_with_contract env) expr    

(* contract_id_in_contract wrapped all functions called in a contract with its
contract if it has any conract. E.g. 
contract h = {x | not (null x)} -> {y | true}
contract f = {x | true)} -> {y | h x < y}

becomes f = {x | true)} -> {y | (h <| C_h) x < y}
This checking for contracts makes sure that we have crash-free contracts.
It is good for dynamic contract checking and 
essential for static contract checking for functions in a program. 

val contract_id_in_contract :
          ThmEnv.t -> Typedtree.core_contract -> Typedtree.core_contract

 -- this is only used in translcore.ml for local contracts
 -- glocal contract is done at verify/transl_core_contract
*)
and contract_id_in_contract env c = 
  let new_desc = match c.contract_desc with
	  | Tctr_pred (id, e, exnopt) -> 
              let ce = contract_id_in_expr env e in
              let expanded_ce = deep_transl_contract env ce in
              Tctr_pred (id, expanded_ce, exnopt)
          | Tctr_arrow (idopt, c1, c2) -> 
	      let new_c1 = contract_id_in_contract env c1 in              
	      let new_c2 = contract_id_in_contract env c2 in
	      Tctr_arrow (idopt, new_c1, new_c2)
          | Tctr_tuple cs -> 
              let rec sub_dep senv xs = begin match xs with
                | [] -> []
                | (vo, c)::l -> 
                  let new_c = contract_id_in_contract env c in
                  let senv2 = match vo with
		  | None -> senv
                  | Some id -> update_name env (Pident id) in
                  (vo, new_c) :: sub_dep senv2 l
                end
             in Tctr_tuple (sub_dep env cs)
	  | Tctr_constr (i, cdesc, cs) -> 
              let rec sub_dep xs = begin match xs with
                | [] -> []
                | (vo, c)::l -> 
                  let new_c = contract_id_in_contract env c
                  in (vo, new_c) :: sub_dep l 
                end
             in Tctr_constr (i, cdesc, sub_dep cs)
          | Tctr_and (c1, c2) -> 
               let new_c1 = contract_id_in_contract env c1 in
               let new_c2 = contract_id_in_contract env c2 in
               Tctr_and (new_c1, new_c2)
          | Tctr_or (c1, c2) -> 
               let new_c1 = contract_id_in_contract env c1 in
               let new_c2 = contract_id_in_contract env c2 in
               Tctr_or (new_c1, new_c2)
          | Tctr_typconstr (i, cs) -> 
               Tctr_typconstr (i, List.map (contract_id_in_contract env) cs)
          | Tctr_var v -> Tctr_var v
          | Tctr_poly (vs, c) -> Tctr_poly (vs, contract_id_in_contract env c)
  in {c with contract_desc = new_desc}


let rec remove_path_list xs ys = match xs with
  | [] -> []
  | x::l -> if memPath x ys then remove_path_list l ys
            else x::(remove_path_list l ys)

let rec remove_redundant_path_name ps acc = match ps with
  | [] -> acc
  | x::l -> if not(memPath x acc) then remove_redundant_path_name l (x::acc)
            else remove_redundant_path_name l acc

let rec get_top_id_in_expression tops e = match e.exp_desc with 
  | Texp_ident (path, vd) ->
       if memPath path tops then [path] else [] 
  | Texp_let(_, pat_expr_list, expr) -> 
       (List.flatten (List.map (fun (_,ei) -> get_top_id_in_expression tops ei)
                              pat_expr_list))@(get_top_id_in_expression tops expr)
  | Texp_function (pat_expr_list, _) -> 
       List.flatten (List.map (fun (_,ei) -> get_top_id_in_expression tops ei)
                              pat_expr_list)
  | Texp_apply (e1, eopt_optl_list) -> 
       (get_top_id_in_expression tops e1)@ 
        (List.flatten (List.map (fun (eopt,_) -> 
	             match eopt with
                    | None -> []
		    | Some ei -> get_top_id_in_expression tops ei)
               eopt_optl_list))
  | Texp_match (e0, pat_expr_list, _) ->
       (get_top_id_in_expression tops e0)@  
        (List.flatten (List.map (fun (_, ei) -> get_top_id_in_expression tops ei)
               pat_expr_list))
  | Texp_try(e0, pat_expr_list) ->
       (get_top_id_in_expression tops e0)@  
        (List.flatten (List.map (fun (_, ei) -> get_top_id_in_expression tops ei)
               pat_expr_list))       
  | Texp_ifthenelse (e0, e1, e2opt) -> 
       (get_top_id_in_expression tops e0)@(get_top_id_in_expression tops e1)@    
       (match e2opt with 
        | None -> []
	| Some e2 -> get_top_id_in_expression tops e2)
  | Texp_construct(_,_, es) -> 
       List.flatten (List.map (fun ei -> get_top_id_in_expression tops ei) es)
  | Texp_tuple(es) -> 
       List.flatten (List.map (fun ei -> get_top_id_in_expression tops ei) es)
  | Texp_variant(_, eopt) -> begin match eopt with
                             | None -> []
			     | Some a -> get_top_id_in_expression tops a
			     end
  | Texp_record(lbl_expr_list, eopt) -> 
       (List.flatten (List.map (fun (_,ei) -> get_top_id_in_expression tops ei) 
                      lbl_expr_list))@
       (match eopt with 
        | None -> []
	| Some a -> get_top_id_in_expression tops a)
  | Texp_field(e0,_) -> 
       get_top_id_in_expression tops e0
  | Texp_setfield (e1,_,e2) ->
       (get_top_id_in_expression tops e1)@(get_top_id_in_expression tops e2)
  | Texp_array (es) -> 
       List.flatten (List.map (fun ei -> get_top_id_in_expression tops ei) es)
  | Texp_sequence(e1, e2) -> 
       (get_top_id_in_expression tops e1)@(get_top_id_in_expression tops e2)
  | Texp_while(e1,e2) -> 
       (get_top_id_in_expression tops e1)@(get_top_id_in_expression tops e2)
  | Texp_for(_,e1,e2,_,e3) ->
       (get_top_id_in_expression tops e1)@(get_top_id_in_expression tops e2)@
       (get_top_id_in_expression tops e3)
  | Texp_when(e1,e2) -> 
       (get_top_id_in_expression tops e1)@(get_top_id_in_expression tops e2)
  | Texp_send(e1,_) -> get_top_id_in_expression tops e1
  | Texp_setinstvar(_,_,e1) -> get_top_id_in_expression tops e1
  | Texp_override(_, path_expr_list) -> 
       List.flatten (List.map (fun (_,ei) -> get_top_id_in_expression tops ei) 
                    path_expr_list)
  | Texp_letmodule(_,_,e1) -> get_top_id_in_expression tops e1
  | Texp_assert(e1) -> get_top_id_in_expression tops e1
  | Texp_lazy(e1) -> get_top_id_in_expression tops e1
  | _ -> []


let rec get_top_id_in_contract tops t acc = match t.contract_desc with 
  | Tctr_pred (id, e, exnopt) -> 
     remove_redundant_path_name (get_top_id_in_expression tops e) acc
  | Tctr_arrow (idopt, t1, t2) -> 
     let names1 = get_top_id_in_contract tops t1 acc in
     let names2 = get_top_id_in_contract tops t2 (acc@names1) in
     names1@names2
  | Tctr_tuple(idopt_t_list) -> 
     remove_redundant_path_name (List.flatten (List.map (fun (a,t) -> 
     				get_top_id_in_contract tops t acc) idopt_t_list)) []
  | _ -> []

(* val get_top_id_in_contract : contract_declaration -> Path.t list *)
let get_top_id_in_cdecl tops cdecl = get_top_id_in_contract tops cdecl.ttopctr_desc []


(* val get_top_id_in_contracts : contract_declaration list -> Path.t list *)
let rec get_top_id_in_contracts tops cdecls = match cdecls with
  | [] -> []
  | h::t -> let names = get_top_id_in_contracts tops t in
            remove_redundant_path_name (get_top_id_in_cdecl tops h) names

(* val transl_str_contracts : core_contract list -> 
                              (Path.t, contract_declaration) Tbl.t ->
                              Typedtree.structure -> 
                              Typedtree.structure       

The contract flag: 0: dynamic, 1: static, 2: hybrid *)

let rec transl_str_contracts contract_flag env strs = 
 let contract_decls = ThmEnv.contract_decls env in
 let contract_wrapping senv (pat, expr) =   
     let (fpath, fid) =  match pat.pat_desc with
                       | Tpat_var id -> (Pident id, id)
                       | others -> raise(Error(pat.pat_loc, Not_pat_var))
     in
     (* wrapping free-variables in function body with its contract, the caller
        is thus the current function name *)
     let cexpr = contract_id_in_expr senv expr in  
     let expanded_cexpr = deep_transl_contract senv cexpr in 
     if contract_flag = 0 then (* dynamic contract checking only *)    
       (pat, expanded_cexpr)
     else if contract_flag = 1 || contract_flag = -1 then 
          (* static contract checking only *)
       let mkexp desc = {expr with exp_desc = desc} in
       try
         let c  = fetch_contract_by_pattern pat contract_decls in
         let bl_callee = Callee (cexpr.exp_loc, fpath) in
         let ce = ensuresC c.ttopctr_desc cexpr bl_callee in
         let _ = static_contract_checking senv contract_flag
                      (fid, deep_transl_contract senv (mkexp ce))  in
         (pat, expr)  
       with Not_found -> 
         let _ = static_contract_checking senv contract_flag
          	     (fid, expanded_cexpr) in
     	 (pat, expr)
     else (* contract_flag is 2, which is hybrid (i.e. static followed by dynamic)
             contract checking. *)
       let mkexp desc = {expr with exp_desc = desc} in
       try
         let c  = fetch_contract_by_pattern pat contract_decls in
         let bl_callee = Callee (cexpr.exp_loc, fpath) in
         let ce = ensuresC c.ttopctr_desc cexpr bl_callee in
         let (residual_exp, validity) = static_contract_checking senv contract_flag
                                  (fid, deep_transl_contract senv (mkexp ce)) in
         match validity with
	 | Valid -> (pat, expr)
         | _ -> (pat, residual_exp)
       with Not_found -> 
         let (residual_exp, validity) = static_contract_checking senv contract_flag
                                     (fid, expanded_cexpr) in
         match validity with
	 | Valid -> (pat, expr)
         | _ -> (pat, residual_exp)
  in
  let t_decls = ThmEnv.type_decls env in
  match strs with
  | [] -> []
  | Tstr_axiom(decl) :: rem -> 
      let new_env = add_tasks env [mlaxiom_to_smtaxiom t_decls decl] in
      transl_str_contracts contract_flag new_env rem
  | Tstr_value(rec_flag, pat_expr_list) :: rem ->
   (* We lookup tstr_contracts to see there is any contract for this function. 
      If there is one, we fetch the contract and wrap the function body with it.
      For each function called in the body of this function including a 
      recursive call, if the function has a contract, we also fetch the contract 
      and wrap the function call with its contract. For example,
      let f = fun x -> ... f (x - 1)  + g x ...
        becomes
      let f = fun x -> ... (f <| C_f) (x - 1)  
   	                 + (g <| C_g) x ...
    *)       
     let rec result_type t = match (Ctype.repr t).desc with
          | Tarrow(_, t1, t2, _) -> result_type t2
          | others -> Ctype.repr t in
     let is_result_type_unit t = match (result_type t).desc with
        | Tconstr(p,_, _) -> Path.name p = "unit"
        | _ -> false in
     let pre_processed_list = 
        List.map (fun (p,e) -> (p, pre_processing e)) pat_expr_list in
     let defs_go_to_env =        
        List.filter (fun (p,e) -> not(is_result_type_unit e.exp_type))  
	 pre_processed_list in
     let env0 = 
       try  (* add current function definition to env *)       
	 List.fold_right (fun (p,e) default -> 
	     match p.pat_desc with
	     | Tpat_var (id) ->
		 begin match rec_flag with
		 | Nonrecursive -> extend_nonrec_env default id e
                 | Recursive -> extend_rec_env default id e
		 | Default -> default
                 end
	     | _ -> default) defs_go_to_env env
       with ToErgosrc.Error(loc,err) -> 
	 ToErgosrc.report_error std_formatter err; env
     in
     let ys = List.flatten (List.map (fun (p,e) -> get_top_ids p) pat_expr_list) in
     (* add top ids to env *)
     let env1 = add_top_defs env0 ys in	 
     let top_ids_so_far = top_defs env1 in
     let used_in_contracts = get_top_id_in_contracts top_ids_so_far contract_decls in 
  (* let top_ids_not_in_contracts = remove_path_list top_ids_so_far used_in_contracts in 
  *)
     let rec required_dfns = 
         List.flatten (List.map (fun p -> match p with
	          | Pident id -> begin try let e = try lookup_nonrec_env id env 
		                        with Not_found -> lookup_rec_env id env in
 	                	let p = { pat_desc = Tpat_var (id);
				          pat_loc  = e.exp_loc;
				          pat_type = e.exp_type;
				          pat_env  = e.exp_env } in
			        [(p,e)]
				with Not_found -> [] end
		  | _ -> []) (intersection top_ids_so_far 
		              (used_in_contracts@(descendants env)))) in
     (* add logic declare to env *)
     let logics = List.fold_right (fun (p,_) default -> 
                     (bound_vars_to_logic p)@default) pat_expr_list [] in 
     let env2 = add_tasks env1 logics in
     let axioms = def_to_axioms t_decls required_dfns in
     let env3 = add_tasks env2 axioms in
(*     print_list (fmt_contract_declaration Format.std_formatter) 
                (List.map contract_declaration_to_iface (ThmEnv.contract_decls env3));
*)
     let wrapped_pat_expr_list = 
       List.map (fun (p,e) -> 
	     let current_env = match p.pat_desc with
	     | Tpat_var (id) -> update_name env3 (Pident id)
             | _ -> env3 in 
             contract_wrapping current_env (p, e)) pre_processed_list in
     let contract_rem = transl_str_contracts contract_flag env2 rem in
     (Tstr_value(rec_flag, wrapped_pat_expr_list))::contract_rem
 | (Tstr_type id_tdecl_list) :: rem -> 
     (* For static contract checking, we convert type declaration to 
        alt-ergo type and logic declarations. For example,
        type 'a list = Nil | Cons of 'a * 'a list
          becomes
        type 'a list
        logic Nil : 'a list
        logic Cons : 'a , 'a list -> 'a list 
     *)
    if contract_flag = 1 or contract_flag = 2
    then let new_env = 
	 add_tasks env (List.flatten (List.map 
					(type_to_typelogic Location.none) 
                                        id_tdecl_list)) in
            transl_str_contracts contract_flag new_env rem
    else transl_str_contracts contract_flag env rem
 | (Tstr_module(id, mexpr) :: rem) -> 
    let new_module_expr_desc = 
         begin
          match mexpr.mod_desc with
          | (Tmod_structure str) -> 
              Tmod_structure (transl_str_contracts contract_flag env str)
          | others ->  others 
         end 
    in 
    let new_str_module = Tstr_module (id, 
                                  { mod_desc = new_module_expr_desc;
                                    mod_loc  = mexpr.mod_loc;
                                    mod_type = mexpr.mod_type;
                                    mod_env  = mexpr.mod_env }) in
    let contract_rem = transl_str_contracts contract_flag env rem in
    new_str_module :: contract_rem
 | (other_str :: rem) -> other_str :: (transl_str_contracts contract_flag env rem)


(* Function trasl_contracts does a typedtree-to-typedtree transformation that 
transforms contract declarations away i.e. embed it into the definitions. 
Tstr_contract(ds) contains contracts declared in the current module while
Tstr_opened_contracts(t) contains imported contracts.
Function transl_contracts is called in driver/compile.ml

The flag info: 0: dynamic, 1: static, 2: hybrid  *)

and get_top_ids p = match p.pat_desc with
		      | Tpat_var(id) -> [Pident id]
		      | Tpat_tuple(ps) -> List.flatten (List.map get_top_ids ps)
		      | _ -> []

and aux_transl_contracts contract_flag (str, cc) = 
  let rec extract_contracts xs = 
    match xs with
    | [] -> ([], Ident.empty, [], [], Tbl.empty)		 
    | (Tstr_mty_contracts(t) :: rem) ->
        let (current_contracts, mty_opened_contracts, type_decls, top_ids, top_funs) = 
	  extract_contracts rem in 
        (current_contracts, Ident.merge t mty_opened_contracts, type_decls, top_ids, top_funs)
    | (Tstr_opened_contracts(t) :: rem) ->
	let (current_contracts, mty_opened_contracts, type_decls, top_ids, top_funs) = 
	  extract_contracts rem in
	(current_contracts, Ident.merge t mty_opened_contracts, type_decls, top_ids, top_funs) 
    | ((Tstr_contract (ds)) :: rem) -> 
	let (current_contracts, mty_opened_contracts, type_decls, top_ids, top_funs) = 
	  extract_contracts rem in	       
	(ds@current_contracts, mty_opened_contracts, type_decls, top_ids, top_funs)
    | ((Tstr_type(ts)) :: rem) -> 
	let (current_contracts, mty_opened_contracts, type_decls, top_ids, top_funs) = 
	  extract_contracts rem in	       	      
	(current_contracts, mty_opened_contracts, ts@type_decls, top_ids, top_funs)
    | ((Tstr_value(_,pat_expr_list)) :: rem) ->
	let (current_contracts, mty_opened_contracts, type_decls, top_ids, top_funs) = 
	  extract_contracts rem in	       	      
	(current_contracts, mty_opened_contracts, type_decls, 
	   top_ids@(List.flatten (List.map (fun (p,e) -> get_top_ids p) pat_expr_list)), 
	   add_pat_expr_list_to_tbl pat_expr_list top_funs)
    | (_::rem) -> extract_contracts rem
  in
  let (tstr_contracts, mty_opened_contracts, type_decls, top_ids, top_funs) = 
    extract_contracts str in
  (* let checked_tstr_contracts = List.map (fun c -> 
                 let senv = update_name env c.ttopctr_id in 
                 let new_c_desc = contract_id_in_contract senv c.ttopctr_desc in
                 {c with ttopctr_desc = new_c_desc}) tstr_contracts in *)
  let t_decls = List.map (fun (a,b) -> (Ident.unique_name a, b)) type_decls in
  let used_in_contracts = get_top_id_in_contracts top_ids tstr_contracts in 
  let top_ids_not_in_contracts = remove_path_list top_ids used_in_contracts in 
  let rec required_dfns xs = match xs with 
         | [] -> []
         | path::l -> 
     (* ids_called are ids called in the definition of functions used in contracts *)
	      let ids_called = try let e = Tbl.find path top_funs in
	                           get_top_id_in_expression top_ids_not_in_contracts e 
                               with Not_found -> [] in
 	      ids_called@(required_dfns l)
        in
  let env = initEnv tstr_contracts mty_opened_contracts t_decls (required_dfns used_in_contracts) in
  (transl_str_contracts contract_flag env str, cc)

and transl_contracts contract_flag (str, cc) = 
  try
  let time0 = Unix.gettimeofday () in 
  let (a,b) = aux_transl_contracts contract_flag (str, cc) in
  let time1 = Unix.gettimeofday () in 
  printf "Static contract checking time: %f secs@." (time1 -. time0);   
  (a,b)  
  with _ -> 
      (str, cc)

let report_error ppf = function
  | Illegal_tuple_expr -> 
      fprintf ppf
        "This expression is not of type tuple"
  | Not_pat_var -> 
      fprintf ppf
	"Not a pattern variable"




