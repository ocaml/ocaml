open Symbol
open Abstract_identifiers
open Flambda

let compilation_unit_id = "unit"

let compilation_unit =
  Compilation_unit.create compilation_unit_id
    (linkage_name "test")

let other_compilation_unit =
  Compilation_unit.create "other"
    (linkage_name "other_test")

let new_var name =
  Variable.create ~current_compilation_unit:compilation_unit name

let new_var_other_unit name =
  Variable.create ~current_compilation_unit:other_compilation_unit name

let nid ?name () = ExprId.create ?name ()

let flet var def body =
  Flet(Not_assigned,var,def,body,
       nid ~name:(Format.asprintf "let %a" Variable.print var) ())

let flets defs body =
  List.fold_right (fun (var, def) body -> flet var def body) defs body

let fvar var =
  Fvar(var,
       nid ~name:(Format.asprintf "var %a" Variable.print var) ())

let int i = Fconst(Fconst_base(Asttypes.Const_int i),nid ())

let fbool b =
  let v =
    if true
    then 1 else 0 in
  Fconst(Fconst_pointer v,nid ())

let rec fseq = function
  | [] -> assert false
  | [e] -> e
  | h::t -> Fsequence(h,fseq t,nid ())

let ffor v lo hi body =
  Ffor(v,lo,hi,Asttypes.Upto,body,nid ())

let fwhile cond body =
  Fwhile(cond,body,nid ())

let ftry body v handler =
  Ftrywith(body,v,handler,nid ())

let fcatch exn vars body handler =
  Fstaticcatch(exn,vars,body,handler,nid ())

let fstaticraise exn args =
  Fstaticraise(exn, args, nid ())

let fassign v exp =
  Fassign(v, exp, nid ())

let fprim p l =
  Fprim(p, l, Debuginfo.none, nid ())

let fadd e1 e2 =
  fprim Lambda.Paddint [e1;e2]

let int_equal e1 e2 =
  fprim (Lambda.Pintcomp (Lambda.Ceq)) [e1;e2]

let int_leq e1 e2 =
  fprim (Lambda.Pintcomp (Lambda.Cle)) [e1;e2]

let tuple l =
  fprim (Lambda.Pmakeblock(0,Asttypes.Immutable)) l

let atuple l =
  fprim (Lambda.Pmakeblock(0,Asttypes.Immutable)) (List.map fvar l)

let ffield i t =
  fprim (Lambda.Pfield i) [t]

let afield i t =
  fprim (Lambda.Pfield i) [fvar t]

let fccall l =
  let open Primitive in
  let prim =
    { prim_name = "test";
      prim_arity = 1;
      prim_alloc = true;
      prim_native_name = "c_test";
      prim_native_float = false } in
  fprim (Lambda.Pccall prim) l

let fun_decl params fv body =
  { stub = false;
    params;
    free_variables = VarSet.of_list (params @ fv);
    body;
    dbg = Debuginfo.none }

let fun_decls lst fv =
  let functs = List.map (fun (var,_,_) -> var) lst in
  let funs =
    List.fold_left (fun map (var,params,body) ->
        let decl = fun_decl params (fv@functs) body in
        VarMap.add var decl map)
      VarMap.empty lst in
  { ident = FunId.create compilation_unit;
    funs;
    compilation_unit }

let fclosure lst fv =
  let fv_var = List.map fst fv in
  Fclosure
    ({ cl_fun = fun_decls lst fv_var;
       cl_free_var = VarMap.of_list fv;
       cl_specialised_arg = VarMap.empty },
     nid ())

let ffunction fu_closure fu_fun =
  Ffunction({ fu_closure; fu_fun; fu_relative_to = None }, nid ())

let afunction fu_closure fu_fun =
  ffunction (fvar fu_closure) fu_fun

let ffun_fclos ?(fv=[]) f args body =
  ffunction
    (fclosure [f, args, body] fv)
    (Closure_function.wrap f)

let ffun_fclos' ?(fv=[]) args body =
  let f = new_var "f" in
  ffunction
    (fclosure [f, args, body] fv)
    (Closure_function.wrap f)

let afun_fclos' ?(fv=[]) args body =
  let f = new_var "f" in
  let clos = new_var "clos" in
  flet clos (fclosure [f, args, body] fv)
    (afunction clos (Closure_function.wrap f))

let fun_decl' params body =
  { stub = false; params; body;
    free_variables = Flambdaiter.free_variables body;
    dbg = Debuginfo.none }

let fun_decls' lst =
  let funs =
    List.fold_left (fun map (var,params,body) ->
        let decl = fun_decl' params body in
        VarMap.add var decl map)
      VarMap.empty lst in
  { ident = FunId.create compilation_unit;
    funs;
    compilation_unit }

let fapply ?(kind=Indirect) f args =
  Fapply({
      ap_function = f;
      ap_arg = args;
      ap_dbg = Debuginfo.none;
      ap_kind = kind},nid ())

let aapply ?kind f args =
  fapply ?kind (fvar f) (List.map fvar args)

let fif cond ifso ifnot =
  Fifthenelse(cond,ifso,ifnot,nid ())

type env =
  { var : Variable.t VarMap.t }

let empty_env =
  { var = VarMap.empty }

let add_var v v' env =
  { var = VarMap.add v v' env.var }

let add_var_list l1 l2 env =
  List.fold_right2 add_var l1 l2 env

let equal_var env v v' =
  try Variable.equal (VarMap.find v env.var) v'
  with Not_found -> false

let equal_let_kind k1 k2 = match k1, k2 with
  | Assigned, Assigned
  | Not_assigned, Not_assigned -> true
  | (Assigned | Not_assigned), (Assigned | Not_assigned) -> false

let add_same_var_set env s =
  VarSet.fold (fun v env -> add_var v v env) s env

let equal_set env s1 s2 =
  let img =
    VarSet.fold (fun v acc -> VarSet.add (VarMap.find v env.var) acc) s1 VarSet.empty in
  VarSet.equal img s2 &&
  VarSet.cardinal s1 = VarSet.cardinal s2

let rec equal env t1 t2 = match t1, t2 with
  | Fvar(v1, _), Fvar(v2, _) -> equal_var env v1 v2
  | Fsymbol(s1,_), Fsymbol(s2,_) -> Symbol.equal s1 s2
  | Fconst (c1, _), Fconst (c2, _) -> c1 = c2


  | Flet (k1, v1, def1, body1, _), Flet (k2, v2, def2, body2, _) ->
      equal_let_kind k1 k2 &&
      equal env def1 def2 &&
      equal (add_var v1 v2 env) body1 body2

  | Fprim (p1, args1, _, _), Fprim (p2, args2, _, _) ->
      p1 = p2 &&
      equal_list env args1 args2

  | Fifthenelse (cond1, ifso1, ifnot1, _), Fifthenelse (cond2, ifso2, ifnot2, _) ->
      equal env cond1 cond2 &&
      equal env ifso1 ifso2 &&
      equal env ifnot1 ifnot2

  | Fwhile (a1, b1, _), Fwhile (a2, b2, _) ->
      equal env a1 a2 && equal env b1 b2

  | Ffor(v1, a1, b1, df1, c1, _), Ffor(v2, a2, b2, df2, c2, _) ->
      equal env a1 a2 &&
      equal env b1 b2 &&
      df1 = df2 &&
      equal (add_var v1 v2 env) c1 c2

  | Fclosure (c1, _), Fclosure (c2, _) ->
      (* could be more general: assumes variables are the same:
         substitution breaks it ! *)
      let env = add_same_var_set env (VarMap.keys c1.cl_fun.funs) in
      let env = add_same_var_set env (VarMap.keys c1.cl_free_var) in

      let same_function f1 f2 =
        if not (List.length f1.params = List.length f2.params)
        then false
        else
          let env = add_var_list f1.params f2.params env in
          f1.stub = f2.stub &&
          equal_set env f1.free_variables f2.free_variables &&
          equal env f1.body f2.body in

      VarMap.equal (equal env) c1.cl_free_var c2.cl_free_var &&
      VarMap.equal Variable.equal c1.cl_specialised_arg c2.cl_specialised_arg &&
      VarMap.equal same_function c1.cl_fun.funs c2.cl_fun.funs

  | Ffunction (fu1, _), Ffunction (fu2, _) ->
      (* assumes same function name, could be more general *)
      equal env fu1.fu_closure fu2.fu_closure &&
      Closure_function.equal fu1.fu_fun fu2.fu_fun &&
      (match fu1.fu_relative_to, fu2.fu_relative_to with
       | None, None -> true
       | Some _, None | None, Some _ -> false
       | Some re1, Some re2 ->
           Closure_function.equal re1 re2)

  | Fapply(a1, _), Fapply(a2, _) ->
      a1.ap_kind = a2.ap_kind &&
      equal env a1.ap_function a2.ap_function &&
      equal_list env a1.ap_arg a2.ap_arg

  | (Fvar _| Fsymbol _| Fconst _| Flet _ | Fprim _ | Fifthenelse _ | Fwhile _
    | Ffor _ | Fclosure _ | Ffunction _ | Fapply _), _ ->
      false

  | (Fassign _ | Fletrec _ | Fvariable_in_closure _
    | Fswitch _ | Fstaticraise _ | Fstaticcatch _
    | Ftrywith _ | Fsequence _
    | Fsend _ | Funreachable _), _ ->
      let e = Format.asprintf "equal: Not implemented %a"
          Printflambda.flambda t1 in
      failwith e

  | Fevent _, _  -> false

and equal_list env l1 l2 =
  List.length l1 = List.length l2 &&
  List.for_all2 (equal env) l1 l2


let equal t1 t2 =
  equal empty_env t1 t2

(* some variables *)

let v = new_var "v"
let f = new_var "f"
let f_func = Closure_function.wrap f
let f' = new_var "f'"
let g = new_var "g"
let g_func = Closure_function.wrap g
let g' = new_var "g'"
let h = new_var "h"
let h_func = Closure_function.wrap h
let h' = new_var "h'"
let fi = new_var "fi"
let fi_func = Closure_function.wrap fi
let fi' = new_var "fi'"
let fj = new_var "fj"
let fj_func = Closure_function.wrap fj
let fj' = new_var "fj'"
let x = new_var "x"
let y = new_var "y"
let z = new_var "z"
let a = new_var "a"
let b = new_var "b"

let fibo = new_var "fibo"
let x_fibo = new_var "x"
let fibo_fun = Closure_function.wrap fibo

let fibonacci =
  let fibo_closure =
    fclosure
      [fibo, [x_fibo],
       fif (int_leq (fvar x_fibo) (int 1))
         (int 1)
         (fadd
            (fapply
               ~kind:(Direct fibo_fun)
               (fvar fibo)
               [fadd (fvar x_fibo) (int (-1))])
            (fapply
               ~kind:(Direct fibo_fun)
               (fvar fibo)
               [fadd (fvar x_fibo) (int (-2))]))]
      []
  in
  ffunction fibo_closure fibo_fun
