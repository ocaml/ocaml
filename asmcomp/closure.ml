(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Introduction of closures, uncurrying, recognition of direct calls *)

open Misc
open Asttypes
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
    | Usend(met, obj, args) -> List.exists occurs (met::obj::args)
  and occurs_array a =
    try
      for i = 0 to Array.length a - 1 do
        if occurs a.(i) then raise Exit
      done;
      false
    with Exit ->
      true
  in occurs u

(* Check if a lambda term denoting a function is ``pure'',
   that is without side-effects *and* not containing function definitions *)

let rec is_pure = function
    Lvar v -> true
  | Lprim(Pgetglobal id, _) -> true
  | Lprim(Pfield n, [arg]) -> is_pure arg
  | _ -> false

(* Maintain the approximation of the global structure being defined *)

let global_approx = ref([||] : value_approximation array)

(* Uncurry an expression and explicitate closures.
   Also return the approximation of the expression.
   The approximation environment [fenv] maps idents to approximations.
   Idents not bound in [fenv] approximate to [Value_unknown].
   The closure environment [cenv] maps idents to [ulambda] terms.
   It is used to substitute environment accesses for free identifiers. *)

let close_var cenv id =
  try Tbl.find id cenv with Not_found -> Uvar id

let approx_var fenv id =
  try Tbl.find id fenv with Not_found -> Value_unknown 

let rec close fenv cenv = function
    Lvar id ->
      (close_var cenv id, approx_var fenv id)
  | Lconst cst ->
      (Uconst cst, Value_unknown)
  | Lfunction(params, body) as funct ->
      close_one_function fenv cenv (Ident.create "fun") funct
  | Lapply(funct, args) ->
      let nargs = List.length args in
      begin match close fenv cenv funct with
        (ufunct, Value_closure(fundesc, approx_res))
        when nargs = fundesc.fun_arity ->
          let uargs = close_list fenv cenv args in
          let app_args = if fundesc.fun_closed then uargs
                                               else uargs @ [ufunct] in
          let app = Udirect_apply(fundesc.fun_label, app_args) in
          ((if is_pure funct then app else Usequence(ufunct, app)),
           approx_res)
      | (ufunct, Value_closure(fundesc, approx_res))
        when nargs > fundesc.fun_arity ->
          let (first_args, rem_args) = split_list fundesc.fun_arity args in
          let ufirst_args = close_list fenv cenv first_args in
          let app_args = if fundesc.fun_closed then ufirst_args
                                               else ufirst_args @ [ufunct] in
          let app =
            Ugeneric_apply(Udirect_apply(fundesc.fun_label, app_args),
                           close_list fenv cenv rem_args) in
          ((if is_pure funct then app else Usequence(ufunct, app)),
           Value_unknown)
      | (ufunct, _) ->
          (Ugeneric_apply(ufunct, close_list fenv cenv args), Value_unknown)
      end
  | Lsend(met, obj, args) ->
      let (umet, _) = close fenv cenv met in
      let (uobj, _) = close fenv cenv obj in
      (Usend(umet, uobj, close_list fenv cenv args), Value_unknown)
  | Llet(str, id, lam, body) ->
      let (ulam, alam) = close_named fenv cenv id lam in
      let (ubody, abody) = close (Tbl.add id alam fenv) cenv body in
      (Ulet(id, ulam, ubody), abody)
  | Lletrec(defs, body) ->
      if List.for_all
           (function (id, Lfunction(_, _)) -> true | _ -> false)
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
      (Uprim(Pgetglobal id, []), Compilenv.global_approx id)
  | Lprim(Pmakeblock(tag, mut) as prim, lams) ->
      let (ulams, approxs) = List.split (List.map (close fenv cenv) lams) in
      (Uprim(prim, ulams),
       begin match mut with
           Immutable -> Value_tuple(Array.of_list approxs)
         | Mutable -> Value_unknown
       end)
  | Lprim(Pfield n, [lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      (Uprim(Pfield n, [ulam]),
       match approx with
           Value_tuple a when n < Array.length a -> a.(n)
         | _ -> Value_unknown)
  | Lprim(Psetfield(n, _), [Lprim(Pgetglobal id, []); lam]) ->
      let (ulam, approx) = close fenv cenv lam in
      (!global_approx).(n) <- approx;
      (Uprim(Psetfield(n, false), [Uprim(Pgetglobal id, []); ulam]),
       Value_unknown)
  | Lprim(p, args) ->
      (Uprim(p, close_list fenv cenv args), Value_unknown)
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

and close_list fenv cenv = function
    [] -> []
  | lam :: rem ->
      let (ulam, _) = close fenv cenv lam in
      ulam :: close_list fenv cenv rem

and close_named fenv cenv id = function
    Lfunction(params, body) as funct ->
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
          (id, (Lfunction(params, body) as def)) ->
            let label =
              Compilenv.current_unit_name() ^ "_" ^ Ident.unique_name id in
            let fundesc =
              {fun_label = label;
               fun_arity = List.length params;
               fun_closed = true } in
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
        env_pos := !env_pos + 1 + (if fundesc.fun_arity > 1 then 3 else 2);
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
  (Uclosure(clos, List.map (close_var cenv) fv), infos)

(* Same, for one function *)

and close_one_function fenv cenv id funct =
  match close_functions fenv cenv [id, funct] with
      (clos, (id, pos, approx) :: _) -> (clos, approx)
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
