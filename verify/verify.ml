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

let rec rename_path_ident bvars path id1 id2 = match path with
 | Pident id -> if not(List.mem id bvars) && id = id1
                then Pident id2
                else path
 | Pdot (p, s, pos) -> Pdot (rename_path_ident bvars p id1 id2, s, pos)
 | Papply (p1, p2) -> Papply(p1, rename_path_ident bvars p2 id1 id2)

(* subst replace free variables id1 in expr by id2 *)
let rec subst bound_vars id1 id2 expr = match expr.exp_desc with
 | Texp_ident (path, vd) -> 
     {expr with exp_desc = Texp_ident (rename_path_ident bound_vars path id1 id2, vd)}
 | Texp_constant (c) -> expr
 | Texp_let (rec_flag, pat_expr_list, e2) -> 
     let bvars     = let_bound_idents pat_expr_list in
     let p_e_list  = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) id1 id2 e)) 
	                   pat_expr_list in
     let new_e2 = subst bound_vars id1 id2 e2 in
     {expr with exp_desc = Texp_let (rec_flag, p_e_list, new_e2)}
 | Texp_function (pat_expr_list, ptial) -> 
     let bvars     = let_bound_idents pat_expr_list in
     let p_e_list  = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) id1 id2 e)) 
	                   pat_expr_list in
     {expr with exp_desc = Texp_function (p_e_list, ptial)}
 | Texp_apply (e1, es) ->
     let new_e1 = subst bound_vars id1 id2 e1 in
     let new_es = List.map (fun (e_opt, optl) -> match e_opt with
                            | Some e -> (Some (subst bound_vars id1 id2 e), optl)
                            | None -> (None, optl)) es
     in {expr with exp_desc = Texp_apply (new_e1, new_es)}
 | Texp_match (e0, pat_expr_list, ptial) -> 
     let new_e0   = subst bound_vars id1 id2 e0 in
     let bvars    = let_bound_idents pat_expr_list in
     let p_e_list = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) id1 id2 e)) 
 	                     pat_expr_list in
     {expr with exp_desc = Texp_match (new_e0, p_e_list, ptial)} 
 | Texp_try (e1, pat_expr_list) -> 
     let new_e1   = subst bound_vars id1 id2 e1 in
     let bvars    = let_bound_idents pat_expr_list in
     let p_e_list = List.map (fun (p,e) -> (p, subst (bound_vars@bvars) id1 id2 e)) 
 	                     pat_expr_list in
     {expr with exp_desc = Texp_try (new_e1, p_e_list)}
 | Texp_tuple (es) -> 
     let new_es = List.map (subst bound_vars id1 id2) es in
     {expr with exp_desc = Texp_tuple new_es}
 | Texp_construct (path, cdesc, es) -> 
     let new_es = List.map (subst bound_vars id1 id2) es in
     {expr with exp_desc = Texp_construct (path, cdesc, new_es)}
 | Texp_variant (lbl, e_opt) -> 
     let new_e = match e_opt with
     | Some e -> Some (subst bound_vars id1 id2 e)
     | None -> None
     in {expr with exp_desc = Texp_variant (lbl, new_e)}
 | Texp_record (ldesc_expr_list, e_opt) ->
     let l_e_list = List.map (fun (p,e) -> (p, subst bound_vars id1 id2 e)) 
 	                     ldesc_expr_list in
     {expr with exp_desc = Texp_record (l_e_list, match e_opt with
     | Some e -> Some (subst bound_vars id1 id2 e)
     | None -> None)}
 | Texp_field (e, ldesc) -> 
     let new_e = subst bound_vars id1 id2 e in
     {expr with exp_desc = Texp_field (new_e, ldesc)}
 | Texp_setfield (e1, ldesc, e2) -> 
     let new_e1 = subst bound_vars id1 id2 e1 in
     let new_e2 = subst bound_vars id1 id2 e2 in
     {expr with exp_desc = Texp_setfield (new_e1, ldesc, new_e2)}
 | Texp_array (es) -> 
     let new_es = List.map (subst bound_vars id1 id2) es in
     {expr with exp_desc = Texp_tuple new_es}
 | Texp_ifthenelse (e0, e1, e2_opt) -> 
     let new_e0 = subst bound_vars id1 id2 e0 in
     let new_e1 = subst bound_vars id1 id2 e1 in
     let new_e2 = match e2_opt with
     | Some e -> Some (subst bound_vars id1 id2 e)
     | None -> None 
     in {expr with exp_desc = Texp_ifthenelse (new_e0, new_e1, new_e2)}    
 | Texp_sequence (e1, e2) -> 
     let new_e1 = subst bound_vars id1 id2 e1 in
     let new_e2 = subst bound_vars id1 id2 e2 in
     {expr with exp_desc = Texp_sequence (new_e1, new_e2)}    
 | Texp_while (e1, e2) -> 
     let new_e1 = subst bound_vars id1 id2 e1 in
     let new_e2 = subst bound_vars id1 id2 e2 in
     {expr with exp_desc = Texp_while (new_e1, new_e2)}    
 | Texp_for (id, e1, e2, dflag, e3) ->
     let new_e1 = subst bound_vars id1 id2 e1 in
     let new_e2 = subst bound_vars id1 id2 e2 in
     let new_e3 = subst bound_vars id1 id2 e3 in
     {expr with exp_desc = Texp_for (id, new_e1, new_e2, dflag, new_e3)}    
  | Texp_when (e1, e2) -> 
     let new_e1 = subst bound_vars id1 id2 e1 in
     let new_e2 = subst bound_vars id1 id2 e2 in
     {expr with exp_desc = Texp_when (new_e1, new_e2)}    
  | Texp_send (e, meth) -> 
     let new_e = subst bound_vars id1 id2 e in
     {expr with exp_desc = Texp_send (new_e, meth)}    
  | Texp_new _ -> expr
  | Texp_instvar _ -> expr
  | Texp_setinstvar (path1, path2, e) -> 
     let new_e = subst bound_vars id1 id2 e in
     {expr with exp_desc = Texp_setinstvar(path1, path2, new_e)}
  | Texp_override (path1, path_expr_list) -> 
     let p_e_list = List.map (fun (p,e) -> (p, subst bound_vars id1 id2 e)) 
 	                      path_expr_list in
     {expr with exp_desc = Texp_override (path1, p_e_list)}
  | Texp_letmodule (id, modexpr, e) -> 
      let new_e = subst bound_vars id1 id2 e in
      {expr with exp_desc = Texp_letmodule (id, modexpr, new_e)}
  | Texp_assert (e) -> 
      let new_e = subst bound_vars id1 id2 e in
      {expr with exp_desc = Texp_assert(new_e)}
  | Texp_assertfalse -> expr
  | Texp_lazy (e) -> 
      let new_e = subst bound_vars id1 id2 e in
      {expr with exp_desc = Texp_assert(new_e)}
  | Texp_object _ -> expr
  | Texp_pack _ -> expr
  | Texp_local_contract (c, e) -> 
      let new_e = subst bound_vars id1 id2 e in
      {expr with exp_desc = Texp_local_contract(c, new_e)}
  | Texp_contract (c, e1, e2, e3) -> 
      let new_e1 = subst bound_vars id1 id2 e1 in
      let new_e2 = subst bound_vars id1 id2 e2 in
      let new_e3 = subst bound_vars id1 id2 e3 in
      {expr with exp_desc = Texp_contract (c, new_e1, new_e2, new_e3)}    
  | Texp_bad _ -> expr
  | Texp_unr _ -> expr
  | Texp_Lambda (ids, e) -> 
      let new_e = subst bound_vars id1 id2 e in
      {expr with exp_desc = Texp_Lambda (ids, new_e)}
  | Texp_App (e1, es) -> 
      let new_e1 = subst bound_vars id1 id2 e1 in
      let new_es = List.map (subst bound_vars id1 id2) es in
      {expr with exp_desc = Texp_App(new_e1, new_es)}
  | Texp_raise (e) -> expr
      

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
        let new_x = Ident.create "x" in
        let vd = { val_type = e.exp_type; val_kind = Val_reg } in
	let xe = { e with exp_desc = Texp_ident(Pident new_x, vd) } in
        let wrapped_p = contract_id_in_expr env p in 
        let expanded_p = deep_transl_contract env wrapped_p in
        let cond = Texp_ifthenelse (subst [] x new_x expanded_p, xe, Some callee) in 
	Texp_let (Nonrecursive, [(mkpat (Tpat_var new_x) cty, e)], mkexp cond cty) 
       
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
             let v_c1_indy = transl_core_contract env c1 xvar caller callee in
	     let c2subst = subst_contract x v_c1_indy c2 in 
	     let resfun = transl_core_contract env c2subst 
		                               (mkexp resarg c2_type) callee caller in
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
              transl_core_contract env c e r1 r2
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
  let cdecls = contract_decls env in
  let opened_contracts = opened_contract_decls env in
  let caller_path = name env in
  let contracted_exp_desc = match expr.exp_desc with
       | Texp_ident (callee_path, value_desc) -> 
         let bl_caller = Caller (expr.exp_loc, Some caller_path, callee_path) in 
         let bl_callee = Callee (expr.exp_loc, callee_path) in     
         begin
          (* try  lookup for dependent contract first 
           let id = match callee_path with
                     | Pident(i) -> i
                     | _ -> raise Not_found
           in
           let c = Ident.find_same id local_fun_contracts in
           requiresC c expr bl_caller bl_callee
          with Not_found -> *)
          try (* lookup for contracts in current module *)
           let c = fetch_contract_by_path callee_path cdecls in
           requiresC c.ttopctr_desc expr bl_caller bl_callee
          with Not_found -> 
	  try (* lookup for contracts in opened modules *)
	   let (p, c) = find_in_ident_tbl callee_path opened_contracts in 
             requiresC (core_contract_from_iface c.Types.ttopctr_desc) 
		        expr bl_caller bl_callee 
 	  with Not_found -> 	    
             (* Format.fprintf Format.std_formatter "%s" (name callee_path); *)
	    (* convert exception to Texp_bad *)
            if cmpPath_byname callee_path Predef.path_exn = 0
	    then Texp_bad (Callee (expr.exp_loc, callee_path))
	    else expr.exp_desc
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
*)
and contract_id_in_contract env c = 
  let new_desc = match c.contract_desc with
	  | Tctr_pred (id, e, exnop) -> 
              let ce = contract_id_in_expr env e in
              let expanded_ce = deep_transl_contract env ce in
              Tctr_pred (id, expanded_ce, exnop)
          | Tctr_arrow (idopt, c1, c2) -> 
	      let new_c1 = contract_id_in_contract env c1 in
	      let new_c2 = contract_id_in_contract env c2 in
              Tctr_arrow (idopt, new_c1, new_c2)
          | Tctr_tuple cs -> 
              let rec sub_dep xs = begin match xs with
                | [] -> []
                | (vo, c)::l -> 
                  let new_c = contract_id_in_contract env c 
                  in (vo, new_c) :: sub_dep l
                end
             in Tctr_tuple (sub_dep cs)
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
     let cexpr = contract_id_in_expr env expr in  
     let expanded_cexpr = deep_transl_contract env cexpr in 
     if contract_flag = 0 then (* dynamic contract checking only *)    
       (pat, expanded_cexpr)
     else if contract_flag = 1 then (* static contract checking only *)
       let mkexp desc = {expr with exp_desc = desc} in
       try
         let c  = fetch_contract_by_pattern pat contract_decls in
         let bl_callee = Callee (cexpr.exp_loc, fpath) in
         let ce = ensuresC c.ttopctr_desc cexpr bl_callee in
         let _ = static_contract_checking senv 
                      (fid, deep_transl_contract env (mkexp ce))  in
         (pat, expr)  
       with Not_found -> 
         let _ = static_contract_checking senv (fid, expanded_cexpr) in
     	 (pat, expr)
     else (* contract_flag is 2, which is hybrid (i.e. static followed by dynamic)
             contract checking. *)
       let mkexp desc = {expr with exp_desc = desc} in
       try
         let c  = fetch_contract_by_pattern pat contract_decls in
         let bl_callee = Callee (cexpr.exp_loc, fpath) in
         let ce = ensuresC c.ttopctr_desc cexpr bl_callee in
         let (residual_exp, validity) = static_contract_checking senv 
                                  (fid, deep_transl_contract senv (mkexp ce)) in
         match validity with
	 | Valid -> (pat, expr)
         | _ -> (pat, residual_exp)
       with Not_found -> 
         let (residual_exp, validity) = static_contract_checking senv 
                                     (fid, expanded_cexpr) in
         match validity with
	 | Valid -> (pat, expr)
         | _ -> (pat, residual_exp)
  in
  match strs with
   [] -> []
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
     let module_env = try let ts = add_tasks env (def_to_axioms pat_expr_list) in
                          ts
                      with ToErgosrc.Error(loc,err) -> 
			ToErgosrc.report_error std_formatter err; env 
     in
     let wrapped_pat_expr_list = 
       List.map (contract_wrapping module_env) pat_expr_list in
     let contract_rem = transl_str_contracts contract_flag module_env rem in
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
       then let new_env = add_tasks env (List.flatten (List.map (type_to_typelogic Location.none) 
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

and aux_transl_contracts contract_flag (str, cc) = 
  let rec extract_contracts xs = 
        match xs with
           | [] -> ([], Ident.empty)		 
           | (Tstr_mty_contracts(t) :: rem) ->
               let (current_contracts, mty_opened_contracts) = extract_contracts rem in 
               (current_contracts, Ident.merge t mty_opened_contracts)
	   | (Tstr_opened_contracts(t) :: rem) ->
	       let (current_contracts, mty_opened_contracts) = extract_contracts rem in
	       (current_contracts, Ident.merge t mty_opened_contracts) 
           | ((Tstr_contract (ds)) :: rem) -> 
	       let (current_contracts, mty_opened_contracts) = extract_contracts rem in	       
	       (ds@current_contracts, mty_opened_contracts)
           | (_::rem) -> extract_contracts rem
  in
  let (tstr_contracts, mty_opened_contracts) = extract_contracts str in
  (* let checked_tstr_contracts = List.map (fun c -> 
                 let new_c_desc = contract_id_in_contract 
                                    tstr_contracts 
				    mty_opened_contracts 
                                    c.ttopctr_id
                                    c.ttopctr_desc in
                 {c with ttopctr_desc = new_c_desc}) tstr_contracts in *)
  let env = initEnv tstr_contracts mty_opened_contracts in
  (transl_str_contracts contract_flag env str, cc)

and transl_contracts contract_flag (str, cc) = 
  let time0 = Sys.time () in 
  let (a,b) = aux_transl_contracts contract_flag (str, cc) in
  let time1 = Sys.time () in 
  printf "Static contract checking time: %f secs@." (time1 -. time0);   
  (a,b)  

let report_error ppf = function
  | Illegal_tuple_expr -> 
      fprintf ppf
        "This expression is not of type tuple"
  | Not_pat_var -> 
      fprintf ppf
	"Not a pattern variable"




