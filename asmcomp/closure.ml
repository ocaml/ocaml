(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Introduction of closures, uncurrying, recognition of direct calls *)

open Misc
open Asttypes
open Primitive
open Lambda
open Clambda

(* Auxiliaries for compiling functions *)

let rec split_list n l =
  if n <= 0 then ([], l) else begin
    match l with
      [] -> fatal_error "Closure.split_list"
    | a::l -> let (l1, l2) = split_list (n-1) l in (a::l1, l2)
  end

let rec build_closure_env env_param pos = function
    [] -> Tbl.empty
  | id :: rem ->
      Tbl.add id (Uprim(Pfield pos, [Uvar env_param])) 
              (build_closure_env env_param (pos+1) rem)

(* Check if a variable occurs in a [clambda] term. *)

let occurs_var var u =
  let rec occurs = function
      Uvar v -> v = var
    | Uconst cst -> false
    | Udirect_apply(lbl, args) -> List.exists occurs args
    | Ugeneric_apply(funct, args) -> occurs funct or List.exists occurs args
    | Uclosure(fundecls, clos) -> List.exists occurs clos
    | Uoffset(u, ofs) -> occurs u
    | Ulet(id, def, body) -> occurs def or occurs body
    | Uletrec(decls, body) ->
        List.exists (fun (id, u) -> occurs u) decls or occurs body
    | Uprim(p, args) -> List.exists occurs args
    | Uswitch(arg, s) ->
        occurs arg or occurs_array s.us_cases_consts
                   or occurs_array s.us_cases_blocks
    | Ustaticfail -> false
    | Ucatch(body, hdlr) -> occurs body or occurs hdlr
    | Utrywith(body, exn, hdlr) -> occurs body or occurs hdlr
    | Uifthenelse(cond, ifso, ifnot) ->
        occurs cond or occurs ifso or occurs ifnot
    | Usequence(u1, u2) -> occurs u1 or occurs u2
    | Uwhile(cond, body) -> occurs cond or occurs body
    | Ufor(id, lo, hi, dir, body) -> occurs lo or occurs hi or occurs body
    | Uassign(id, u) -> id = var or occurs u
    | Usend(met, obj, args) -> 
        occurs met or occurs obj or List.exists occurs args
  and occurs_array a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u

(* Determine whether the estimated size of a clambda term is below
   some threshold *)

let prim_size prim args =
  match prim with
    Pidentity -> 0
  | Pgetglobal id -> 1
  | Psetglobal id -> 1
  | Pmakeblock(tag, mut) -> 5 + List.length args
  | Pfield f -> 1
  | Psetfield(f, isptr) -> if isptr then 4 else 1
  | Pfloatfield f -> 1
  | Psetfloatfield f -> 1
  | Pccall p -> (if p.prim_alloc then 10 else 4) + List.length args
  | Praise -> 4
  | Pstringlength -> 5
  | Pstringrefs | Pstringsets -> 6
  | Pmakearray kind -> 5 + List.length args
  | Parraylength kind -> if kind = Pgenarray then 6 else 2
  | Parrayrefu kind -> if kind = Pgenarray then 12 else 2
  | Parraysetu kind -> if kind = Pgenarray then 16 else 4
  | Parrayrefs kind -> if kind = Pgenarray then 18 else 8
  | Parraysets kind -> if kind = Pgenarray then 22 else 10
  | Pbittest -> 3
  | _ -> 2 (* arithmetic and comparisons *)

let lambda_smaller lam threshold =
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam with
      Uvar v -> ()
    | Uconst(Const_base(Const_int _ | Const_char _ | Const_float _) |
             Const_pointer _) -> incr size
    | Uconst _ ->
        raise Exit (* avoid duplication of structured constants *)
    | Udirect_apply(fn, args) ->
        size := !size + 4; lambda_list_size args
    | Ugeneric_apply(fn, args) ->
        size := !size + 6; lambda_size fn; lambda_list_size args
    | Uclosure(defs, vars) ->
        raise Exit (* inlining would duplicate function definitions *)
    | Uoffset(lam, ofs) ->
        incr size; lambda_size lam
    | Ulet(id, lam, body) ->
        lambda_size lam; lambda_size body
    | Uletrec(bindings, body) ->
        raise Exit (* usually too large *)
    | Uprim(prim, args) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Uswitch(lam, cases) ->
        if Array.length cases.us_cases_consts > 0 then size := !size + 5;
        if Array.length cases.us_cases_blocks > 0 then size := !size + 5;
        if cases.us_checked then size := !size + 2;
        lambda_size lam;
        lambda_array_size cases.us_cases_consts;
        lambda_array_size cases.us_cases_blocks
    | Ustaticfail -> ()
    | Ucatch(body, handler) ->
        incr size; lambda_size body; lambda_size handler
    | Utrywith(body, id, handler) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Uifthenelse(cond, ifso, ifnot) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Usequence(lam1, lam2) ->
        lambda_size lam1; lambda_size lam2
    | Uwhile(cond, body) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Ufor(id, low, high, dir, body) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Uassign(id, lam) ->
        incr size;  lambda_size lam
    | Usend(met, obj, args) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
  and lambda_list_size l = List.iter lambda_size l
  and lambda_array_size a = Array.iter lambda_size a in
  try
    lambda_size lam; !size <= threshold
  with Exit ->
    false

(* Simplify primitive operations on integers *)

let make_const_int n = (Uconst(Const_base(Const_int n)), Value_integer n)
let make_const_ptr n = (Uconst(Const_pointer n), Value_unknown)

let simplif_prim p (args, approxs) =
  match approxs with
    [Value_integer x] ->
      begin match p with
        Pidentity -> make_const_int x
      | Pnegint -> make_const_int (-x)
      | Poffsetint y -> make_const_int (x + y)
      | _ -> (Uprim(p, args), Value_unknown)
      end
  | [Value_integer x; Value_integer y] ->
      begin match p with
        Paddint -> make_const_int(x + y)
      | Psubint -> make_const_int(x - y)
      | Pmulint -> make_const_int(x * y)
      | Pdivint when y <> 0 -> make_const_int(x / y)
      | Pmodint when y <> 0 -> make_const_int(x mod y)
      | Pandint -> make_const_int(x land y)
      | Porint -> make_const_int(x lor y)
      | Pxorint -> make_const_int(x lxor y)
      | Plslint -> make_const_int(x lsl y)
      | Plsrint -> make_const_int(x lsr y)
      | Pasrint -> make_const_int(x asr y)
      | Pintcomp cmp ->
          let result = match cmp with
              Ceq -> x = y
            | Cneq -> x <> y
            | Clt -> x < y
            | Cgt -> x > y
            | Cle -> x <= y
            | Cge -> x >= y in
          make_const_ptr(if result then 1 else 0)
      | _ -> (Uprim(p, args), Value_unknown)
      end
  | _ ->
      (Uprim(p, args), Value_unknown)
      
(* Substitute variables in a [ulambda] term and perform
   some more simplifications on integer primitives.
   The variables must not be assigned in the term.
   This is used to substitute "trivial" arguments for parameters
   during inline expansion. *)

let approx_ulam = function
    Uconst(Const_base(Const_int n)) -> Value_integer n
  | Uconst(Const_base(Const_char c)) -> Value_integer(Char.code c)
  | Uconst(Const_pointer n) -> Value_integer n
  | _ -> Value_unknown

let substitute sb ulam =
  let rec subst ulam =
    match ulam with
      Uvar v ->
        begin try Tbl.find v sb with Not_found -> ulam end
    | Uconst cst -> ulam
    | Udirect_apply(lbl, args) -> Udirect_apply(lbl, List.map subst args)
    | Ugeneric_apply(fn, args) -> Ugeneric_apply(subst fn, List.map subst args)
    | Uclosure(defs, env) -> Uclosure(defs, List.map subst env)
    | Uoffset(u, ofs) -> Uoffset(subst u, ofs)
    | Ulet(id, u1, u2) -> Ulet(id, subst u1, subst u2)
    | Uletrec(bindings, body) ->
        Uletrec(List.map (fun (id, u) -> (id, subst u)) bindings, subst body)
    | Uprim(p, args) ->
        let sargs = List.map subst args in
        let (res, _) = simplif_prim p (sargs, List.map approx_ulam sargs) in
        res
    | Uswitch(arg, sw) ->
        Uswitch(subst arg,
                { us_index_consts = sw.us_index_consts;
                  us_cases_consts = Array.map subst sw.us_cases_consts;
                  us_index_blocks = sw.us_index_blocks;
                  us_cases_blocks = Array.map subst sw.us_cases_blocks;
                  us_checked = sw.us_checked })
    | Ustaticfail -> Ustaticfail
    | Ucatch(u1, u2) -> Ucatch(subst u1, subst u2)
    | Utrywith(u1, id, u2) -> Utrywith(subst u1, id, subst u2)
    | Uifthenelse(u1, u2, u3) ->
        begin match subst u1 with
          Uconst(Const_pointer n) -> if n <> 0 then subst u2 else subst u3
        | su1 -> Uifthenelse(su1, subst u2, subst u3)
        end
    | Usequence(u1, u2) -> Usequence(subst u1, subst u2)
    | Uwhile(u1, u2) -> Uwhile(subst u1, subst u2)
    | Ufor(id, u1, u2, dir, u3) -> Ufor(id, subst u1, subst u2, dir, subst u3)
    | Uassign(id, u) -> Uassign(id, subst u)
    | Usend(u1, u2, ul) -> Usend(subst u1, subst u2, List.map subst ul)
  in subst ulam

(* Perform an inline expansion *)

let is_simple_argument = function
    Uvar _ -> true
  | Uconst(Const_base(Const_int _ | Const_char _ | Const_float _)) -> true
  | Uconst(Const_pointer _) -> true
  | _ -> false

let rec bind_params subst params args body =
  match (params, args) with
    ([], []) -> substitute subst body
  | (p1 :: pl, a1 :: al) ->
      if is_simple_argument a1
      then bind_params (Tbl.add p1 a1 subst) pl al body
      else Ulet(p1, a1, bind_params subst pl al body)
  | (_, _) -> assert false

(* Check if a lambda term denoting a function is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure = function
    Lvar v -> true
  | Lprim(Pgetglobal id, _) -> true
  | Lprim(Pfield n, [arg]) -> is_pure arg
  | _ -> false

(* Generate a direct application *)

let direct_apply fundesc funct ufunct uargs =
  let app_args =
    if fundesc.fun_closed then uargs else uargs @ [ufunct] in
  let app =
    match fundesc.fun_inline with
      None -> Udirect_apply(fundesc.fun_label, app_args)
    | Some(params, body) -> bind_params Tbl.empty params app_args body in
  (* If ufunct can contain side-effects or function definitions,
     we must make sure that it is evaluated exactly once.
     If the function is not closed, we evaluate ufunct as part of the
     arguments.
     If the function is closed, we force the evaluation of ufunct first. *)
  if not fundesc.fun_closed || is_pure funct
  then app
  else Usequence(ufunct, app)

(* Add [Value_integer] info to the approximation of an application *)

let strengthen_approx appl approx =
  match approx_ulam appl with
    Value_integer _ as intapprox -> intapprox
  | _ -> approx

(* Maintain the approximation of the global structure being defined *)

let global_approx = ref([||] : value_approximation array)

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

let close_approx_var fenv cenv id =
  let approx = try Tbl.find id fenv with Not_found -> Value_unknown in
  match approx with
    Value_integer n ->
      make_const_int n
  | approx ->
      let subst = try Tbl.find id cenv with Not_found -> Uvar id in
      (subst, approx)

let close_var fenv cenv id =
  let (ulam, app) = close_approx_var fenv cenv id in ulam

let rec close fenv cenv = function
    Lvar id ->
      close_approx_var fenv cenv id
  | Lconst cst ->
      begin match cst with
        Const_base(Const_int n) -> (Uconst cst, Value_integer n)
      | Const_base(Const_char c) -> (Uconst cst, Value_integer(Char.code c))
      | _ -> (Uconst cst, Value_unknown)
      end
  | Lfunction(kind, params, body) as funct ->
      close_one_function fenv cenv (Ident.create "fun") funct
  | Lapply(funct, args) ->
      let nargs = List.length args in
      begin match (close fenv cenv funct, close_list fenv cenv args) with
        ((ufunct, Value_closure(fundesc, approx_res)),
         [Uprim(Pmakeblock(_, _), uargs)])
        when List.length uargs = - fundesc.fun_arity ->
          let app = direct_apply fundesc funct ufunct uargs in
          (app, strengthen_approx app approx_res)
      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when nargs = fundesc.fun_arity ->
          let app = direct_apply fundesc funct ufunct uargs in
          (app, strengthen_approx app approx_res)
      | ((ufunct, Value_closure(fundesc, approx_res)), uargs)
        when fundesc.fun_arity > 0 && nargs > fundesc.fun_arity ->
          let (first_args, rem_args) = split_list fundesc.fun_arity uargs in
          (Ugeneric_apply(direct_apply fundesc funct ufunct first_args,
                          rem_args),
           Value_unknown)
      | ((ufunct, _), uargs) ->
          (Ugeneric_apply(ufunct, uargs), Value_unknown)
      end
  | Lsend(met, obj, args) ->
      let (umet, _) = close fenv cenv met in
      let (uobj, _) = close fenv cenv obj in
      (Usend(umet, uobj, close_list fenv cenv args), Value_unknown)
  | Llet(str, id, lam, body) ->
      let (ulam, alam) = close_named fenv cenv id lam in
      begin match (str, alam) with
        (Variable, _) ->
          let (ubody, abody) = close fenv cenv body in
          (Ulet(id, ulam, ubody), abody)
      | (_, Value_integer n) ->
          close (Tbl.add id alam fenv) cenv body
      | (_, _) ->
          let (ubody, abody) = close (Tbl.add id alam fenv) cenv body in
          (Ulet(id, ulam, ubody), abody)
      end
  | Lletrec(defs, body) ->
      if List.for_all
           (function (id, Lfunction(_, _, _)) -> true | _ -> false)
           defs
      then begin
        (* Simple case: only function definitions *)
        let (clos, infos) = close_functions fenv cenv defs in
        let clos_ident = Ident.create "clos" in
        let fenv_body =
          List.fold_right
            (fun (id, pos, approx) fenv -> Tbl.add id approx fenv)
            infos fenv in
        let (ubody, approx) = close fenv_body cenv body in
        (Ulet(clos_ident, clos,
              List.fold_right
                (fun (id, pos, approx) body ->
                    Ulet(id, Uoffset(Uvar clos_ident, pos), body))
                infos ubody),
         approx)
      end else begin
        (* General case: recursive definition of values *)
        let rec clos_defs = function
          [] -> ([], fenv)
        | (id, lam) :: rem ->
            let (udefs, fenv_body) = clos_defs rem in
            let (ulam, approx) = close fenv cenv lam in
            ((id, ulam) :: udefs, Tbl.add id approx fenv_body) in
        let (udefs, fenv_body) = clos_defs defs in
        let (ubody, approx) = close fenv_body cenv body in
        (Uletrec(udefs, ubody), approx)
      end
  | Lprim(Pgetglobal id, []) ->
      begin match Compilenv.global_approx id with
        Value_integer n -> make_const_int n
      | app -> (Uprim(Pgetglobal id, []), app)
      end
  | Lprim(Pmakeblock(tag, mut) as prim, lams) ->
      let (ulams, approxs) = List.split (List.map (close fenv cenv) lams) in
      (Uprim(prim, ulams),
       begin match mut with
           Immutable -> Value_tuple(Array.of_list approxs)
         | Mutable -> Value_unknown
       end)
  | Lprim(Pfield n, [lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      let fieldapprox =
        match approx with
          Value_tuple a when n < Array.length a -> a.(n)
        | _ -> Value_unknown in
      (Uprim(Pfield n, [ulam]), fieldapprox)
  | Lprim(Psetfield(n, _), [Lprim(Pgetglobal id, []); lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      (!global_approx).(n) <- approx;
      (Uprim(Psetfield(n, false), [Uprim(Pgetglobal id, []); ulam]),
       Value_unknown)
  | Lprim(p, args) ->
      simplif_prim p (close_list_approx fenv cenv args)
  | Lswitch(arg, sw) ->
      let (uarg, _) = close fenv cenv arg in
      let (const_index, const_cases) =
        close_switch fenv cenv sw.sw_numconsts sw.sw_consts in
      let (block_index, block_cases) =
        close_switch fenv cenv sw.sw_numblocks sw.sw_blocks in
      (Uswitch(uarg, 
               {us_index_consts = const_index;
                us_cases_consts = const_cases;
                us_index_blocks = block_index;
                us_cases_blocks = block_cases;
                us_checked = sw.sw_checked}),
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
  | Lassign(id, lam) ->
      let (ulam, _) = close fenv cenv lam in
      (Uassign(id, ulam), Value_unknown)
  | Levent _ | Lifused _ -> assert false

and close_list fenv cenv = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close fenv cenv lam in
      ulam :: close_list fenv cenv rem

and close_list_approx fenv cenv = function
    [] -> ([], [])
  | lam :: rem ->
      let (ulam, approx) = close fenv cenv lam in
      let (ulams, approxs) = close_list_approx fenv cenv rem in
      (ulam :: ulams, approx :: approxs)

and close_named fenv cenv id = function
    Lfunction(kind, params, body) as funct ->
      close_one_function fenv cenv id funct
  | lam ->
      close fenv cenv lam

(* Build a shared closure for a set of mutually recursive functions *)

and close_functions fenv cenv fun_defs =
  (* Determine the free variables of the functions *)
  let fv =
    IdentSet.elements (free_variables (Lletrec(fun_defs, lambda_unit))) in
  (* Build the function descriptors for the functions.
     Initially all functions are assumed not to need their environment
     parameter. *)
  let uncurried_defs =
    List.map
      (function
          (id, (Lfunction(kind, params, body) as def)) ->
            let label =
              Compilenv.current_unit_name() ^ "_" ^ Ident.unique_name id in
            let arity = List.length params in
            let fundesc =
              {fun_label = label;
               fun_arity = (if kind = Tupled then -arity else arity);
               fun_closed = true;
               fun_inline = None } in
            (id, params, body, fundesc)
        | (_, _) -> fatal_error "Closure.close_functions")
      fun_defs in
  (* Build an approximate fenv for compiling the functions *)
  let fenv_rec =
    List.fold_right
      (fun (id, params, body, fundesc) fenv ->
        Tbl.add id (Value_closure(fundesc, Value_unknown)) fenv)
      uncurried_defs fenv in
  (* Determine the offsets of each function's closure in the shared block *)
  let env_pos = ref (-1) in
  let clos_offsets =
    List.map
      (fun (id, params, body, fundesc) ->
        let pos = !env_pos + 1 in
        env_pos := !env_pos + 1 + (if fundesc.fun_arity <> 1 then 3 else 2);
        pos)
      uncurried_defs in
  let fv_pos = !env_pos in
  (* This reference will be set to false if the hypothesis that a function
     does not use its environment parameter is invalidated. *)
  let useless_env = ref true in
  (* Translate each function definition *)
  let clos_fundef (id, params, body, fundesc) env_pos =
    let env_param = Ident.create "env" in
    let cenv_fv =
      build_closure_env env_param (fv_pos - env_pos) fv in
    let cenv_body =
      List.fold_right2
        (fun (id, params, arity, body) pos env ->
          Tbl.add id (Uoffset(Uvar env_param, pos - env_pos)) env)
        uncurried_defs clos_offsets cenv_fv in
    let (ubody, approx) = close fenv_rec cenv_body body in
    if !useless_env & occurs_var env_param ubody then useless_env := false;
    let fun_params = if !useless_env then params else params @ [env_param] in
    ((fundesc.fun_label, fundesc.fun_arity, fun_params, ubody),
     (id, env_pos, Value_closure(fundesc, approx))) in
  (* Translate all function definitions. *)
  let clos_info_list = 
    let cl = List.map2 clos_fundef uncurried_defs clos_offsets in
    (* If the hypothesis that the environment parameters are useless has been
       invalidated, then set [fun_closed] to false in all descriptions and
       recompile *)
    if !useless_env then cl else begin
      List.iter
        (fun (id, params, body, fundesc) -> fundesc.fun_closed <- false)
        uncurried_defs;
      List.map2 clos_fundef uncurried_defs clos_offsets
    end in
  (* Return the Uclosure node and the list of all identifiers defined,
     with offsets and approximations. *)
  let (clos, infos) = List.split clos_info_list in
  (Uclosure(clos, List.map (close_var fenv cenv) fv), infos)

(* Same, for one non-recursive function *)

and close_one_function fenv cenv id funct =
  match close_functions fenv cenv [id, funct] with
      ((Uclosure([_, _, params, body], _) as clos),
       [_, _, (Value_closure(fundesc, _) as approx)]) ->
        (* See if the function can be inlined *)
        if lambda_smaller body (!Clflags.inline_threshold + List.length params)
        then fundesc.fun_inline <- Some(params, body);
        (clos, approx)
    | _ -> fatal_error "Closure.close_one_function"

(* Close a switch *)

and close_switch fenv cenv num_keys cases =
  let index = Array.create num_keys 0 in
  let ucases = ref []
  and num_cases = ref 0 in
  if List.length cases < num_keys then begin
    num_cases := 1;
    ucases := [Ustaticfail]
  end;
  List.iter
    (function (key, lam) ->
        let (ulam, _) = close fenv cenv lam in
        ucases := ulam :: !ucases;
        index.(key) <- !num_cases;
        incr num_cases)
    cases;
  (index, Array.of_list(List.rev !ucases))

(* The entry point *)

let intro size lam =
  global_approx := Array.create size Value_unknown;
  let (ulam, approx) = close Tbl.empty Tbl.empty lam in
  Compilenv.set_global_approx(Value_tuple !global_approx);
  global_approx := [||];
  ulam
