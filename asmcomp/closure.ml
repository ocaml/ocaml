(* Introduction of closures, uncurrying, recognition of direct calls *)

open Misc
open Lambda
open Clambda

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> fatal_error "Closure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

let rec uncurry_fun = function
    Lfunction(param, body) ->
      let (params, final_body) = uncurry_fun body in
      (param :: params, final_body)
  | lam -> ([], lam)

let rec build_closure_env env_param pos = function
    [] -> Tbl.empty
  | id :: rem ->
      Tbl.add id (Uprim(Pfield pos, [Uvar env_param])) 
              (build_closure_env env_param (pos+1) rem)

(* Auxiliaries for compiling recursive definitions *)

type fun_analysis =
  { fa_desc: function_description;
    fa_params: Ident.t list;
    fa_body: lambda;
    fa_cenv: (Ident.t, ulambda) Tbl.t;
    fa_clos: ulambda list }

type rec_approximation =
    Rec_function of fun_analysis
  | Rec_other of lambda

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

let rec close fenv cenv = function
    Lvar id ->
      (begin try Tbl.find id cenv with Not_found -> Uvar id end,
       begin try Tbl.find id fenv with Not_found -> Value_unknown end)
  | Lconst cst ->
      (Uconst cst, Value_unknown)
  | Lfunction(param, body) as funct ->
      close_function fenv cenv (Ident.new "fun") funct
  | Lapply(funct, args) ->
      let nargs = List.length args in
      begin match close fenv cenv funct with
        (ufunct, Value_closure(fundesc, approx_res))
        when nargs = fundesc.fun_arity ->
          let uargs = close_list fenv cenv args in
          let app_args = if fundesc.fun_closed then uargs
                                               else uargs @ [ufunct] in
          (Udirect_apply(fundesc.fun_label, app_args),
           approx_res)
      | (ufunct, Value_closure(fundesc, approx_res))
        when nargs > fundesc.fun_arity ->
          let (first_args, rem_args) = split_list fundesc.fun_arity args in
          let ufirst_args = close_list fenv cenv first_args in
          let app_args = if fundesc.fun_closed then ufirst_args
                                               else ufirst_args @ [ufunct] in
          (Ugeneric_apply(Udirect_apply(fundesc.fun_label, app_args),
                          close_list fenv cenv rem_args),
           Value_unknown)
      | (ufunct, _) ->
          (Ugeneric_apply(ufunct, close_list fenv cenv args), Value_unknown)
      end
  | Llet(id, lam, body) ->
      let (ulam, alam) = close_named  fenv cenv id lam in
      let (ubody, abody) = close (Tbl.add id alam fenv) cenv body in
      (Ulet(id, ulam, ubody), abody)
  | Lletrec([id, (Lfunction(_, _) as funct)], body) ->
      let funapp = close_analyze_function_rec1 fenv cenv id funct in
      let (ufunct, approx) =
        close_build_function
          (Tbl.add id (Value_closure(funapp.fa_desc, Value_unknown)) fenv)
          funapp in
      let (ubody, approx) = close (Tbl.add id approx fenv) cenv body in
      (Ulet(id, ufunct, ubody), approx)
  | Lletrec(decls, body) ->
      let rec make_rec_fenv = function
        [] -> (fenv, [])
      | (id, lam) :: rem ->
          let (new_fenv, precomp) = make_rec_fenv rem in
          match lam with
            Lfunction(param, body) ->
              let funapp = close_analyze_function fenv cenv id lam in
              (Tbl.add id (Value_closure(funapp.fa_desc, Value_unknown))
                       new_fenv,
               (id, Rec_function funapp) :: precomp)
          | _ ->
              (new_fenv, (id, Rec_other lam) :: precomp) in
      let (rec_fenv, precomp) = make_rec_fenv decls in
      let rec close_decls = function
        [] -> (fenv, [])
      | (id, pre) :: rem ->
          let (new_fenv, urem) = close_decls rem in
          match pre with
            Rec_function funapp ->
              let (ulam, approx) = close_build_function rec_fenv funapp in
              (Tbl.add id approx new_fenv, (id, ulam) :: urem)
          | Rec_other lam ->
              let (ulam, approx) = close rec_fenv cenv lam in
              (Tbl.add id approx new_fenv, (id, ulam) :: urem) in
      let (body_fenv, udecls) = close_decls precomp in
      let (ubody, approx) = close body_fenv cenv body in
      (Uletrec(udecls, ubody), approx)
  | Lprim(Pgetglobal id, []) ->
      (Uprim(Pgetglobal id, []), Compilenv.global_approx id)
  | Lprim(Psetglobal id, [lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      Compilenv.set_global_approx approx;
      (Uprim(Psetglobal id, [ulam]), Value_unknown)
  | Lprim(Pmakeblock tag, lams) ->
      let (ulams, approxs) = List.split (List.map (close fenv cenv) lams) in
      (Uprim(Pmakeblock tag, ulams), Value_tuple(Array.of_list approxs))
  | Lprim(Pfield n, [lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      (Uprim(Pfield n, [ulam]),
       match approx with
           Value_tuple a when n < Array.length a -> a.(n)
         | _ -> Value_unknown)
  | Lprim(p, args) ->
      (Uprim(p, close_list fenv cenv args), Value_unknown)
  | Lswitch(arg, nconst, consts, nblock, blocks) ->
      let (uarg, _) = close fenv cenv arg in
      let (const_index, const_cases) = close_switch fenv cenv nconst consts in
      let (block_index, block_cases) = close_switch fenv cenv nblock blocks in
      (Uswitch(uarg, const_index, const_cases, block_index, block_cases),
       Value_unknown)
  | Lstaticfail ->
      (Ustaticfail, Value_unknown)
  | Lcatch(body, handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (Ucatch(ubody, uhandler), Value_unknown)
  | Ltrywith(body, id, handler) ->
      let (ubody, _) = close fenv cenv body in
      let (uhandler, _) = close fenv cenv handler in
      (Utrywith(ubody, id, uhandler), Value_unknown)
  | Lifthenelse(arg, ifso, ifnot) ->
      let (uarg, _) = close fenv cenv arg in
      let (uifso, _) = close fenv cenv ifso in
      let (uifnot, _) = close fenv cenv ifnot in
      (Uifthenelse(uarg, uifso, uifnot), Value_unknown)
  | Lsequence(lam1, lam2) ->
      let (ulam1, _) = close fenv cenv lam1 in
      let (ulam2, approx) = close fenv cenv lam2 in
      (Usequence(ulam1, ulam2), approx)
  | Lwhile(cond, body) ->
      let (ucond, _) = close fenv cenv cond in
      let (ubody, _) = close fenv cenv body in
      (Uwhile(ucond, ubody), Value_unknown)
  | Lfor(id, lo, hi, dir, body) ->
      let (ulo, _) = close fenv cenv lo in
      let (uhi, _) = close fenv cenv hi in
      let (ubody, _) = close fenv cenv body in
      (Ufor(id, ulo, uhi, dir, ubody), Value_unknown)
  | Lshared(lam, _) ->
      close fenv cenv lam

and close_list fenv cenv = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close fenv cenv lam in
      ulam :: close_list fenv cenv rem

and close_named fenv cenv id = function
    Lfunction(param, body) as funct ->
      close_function fenv cenv id funct
  | lam ->
      close fenv cenv lam

(* Build a function closure with the given name *)

and close_function fenv cenv id funct =
  close_build_function fenv (close_analyze_function fenv cenv id funct)
  
(* Return preliminary information for a function closure *)

and close_analyze_function fenv cenv id funct =
  let fv = IdentSet.elements(free_variables funct) in
  let label = Compilenv.current_unit_name() ^ "_" ^ Ident.unique_name id in
  let (params, body) = uncurry_fun funct in
  let arity = List.length params in
  let env_param = Ident.new "env" in
  let cenv_body =
    build_closure_env env_param (if arity > 1 then 3 else 2) fv in
  {fa_desc = {fun_label = label; fun_arity = arity; fun_closed = (fv=[])};
   fa_params = params @ [env_param];
   fa_body = body;
   fa_cenv = cenv_body;
   fa_clos = close_list fenv cenv (List.map (fun id -> Lvar id) fv)}

(* Same, but for a simply recursive function. In this case, the closure for
   the function itself is in its environment parameter. *)

and close_analyze_function_rec1 fenv cenv id funct =
  let fv = IdentSet.elements(IdentSet.remove id (free_variables funct)) in
  let label = Compilenv.current_unit_name() ^ "_" ^ Ident.unique_name id in
  let (params, body) = uncurry_fun funct in
  let arity = List.length params in
  let env_param = Ident.new "env" in
  let cenv_body =
    Tbl.add id (Uvar env_param)
      (build_closure_env env_param (if arity > 1 then 3 else 2) fv) in
  (* Even if fv = [], env may be used inside to refer to the functional
     value of the function. Not detected here. *)
  {fa_desc = {fun_label = label; fun_arity = arity; fun_closed = false};
   fa_params = params @ [env_param];
   fa_body = body;
   fa_cenv = cenv_body;
   fa_clos = close_list fenv cenv (List.map (fun id -> Lvar id) fv)}

(* Actually build the function closure based on infos returned by
   [close_analyze_function] *)

and close_build_function fenv funapp =
  (* No need to add [params] to [fenv] since their approximations are
     unknown anyway *)
  let (ubody, approx) = close fenv funapp.fa_cenv funapp.fa_body in
  (Uclosure(funapp.fa_desc.fun_label, funapp.fa_desc.fun_arity,
            funapp.fa_params, ubody, funapp.fa_clos),
   Value_closure(funapp.fa_desc, approx))

(* Close a switch, preserving sharing between cases. *)

and close_switch fenv cenv num_keys cases =
  let index = Array.new num_keys 0 in
  let num_cases = ref 0 and ucases = ref [] in
  if List.length cases < num_keys then begin
    num_cases := 1;
    ucases := [Ustaticfail]
  end;
  List.iter
    (function
        (key, Lshared(lam, r)) ->
          begin match !r with
            None ->
              let (ulam, _) = close fenv cenv lam in
              ucases := ulam :: !ucases;
              index.(key) <- !num_cases;
              r := Some !num_cases;
              incr num_cases
          | Some n ->
              index.(key) <- n
          end
      | (key, lam) ->
          let (ulam, _) = close fenv cenv lam in
          ucases := ulam :: !ucases;
          index.(key) <- !num_cases;
          incr num_cases)
    cases;
  List.iter
    (function
        (key, Lshared(lam, r)) -> r := None
      | (key, lam) -> ())
    cases;
  (index, Array.of_list(List.rev !ucases))

(* The entry point *)

let intro lam =
  let (ulam, approx) = close Tbl.empty Tbl.empty lam in ulam

