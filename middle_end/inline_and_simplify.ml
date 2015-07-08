(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module B = Inlining_cost.Benefit
module E = Inlining_env
module R = Inlining_result

(* Two kinds of information are propagated during inlining and
   simplification:
   - [E.t] "environments", top-down, usually called "env";
   - [R.t] "results", bottom-up approximately following the
     evaluation order, usually called "r".
   Along with the results come rewritten Flambda terms.

   In general the pattern is to do a subset of these steps:
   * recursive call of loop on the arguments with the original
     environment:
       [let new_arg, r = loop env r arg]
   * generate fresh new identifiers (if subst.active is true) and
     add the substitution to the environment:
       [let new_id, env = add_variable id env]
   * associate in the environment the approximation of values to
     identifiers:
       [let env = E.add_approx id (R.approx r) env]
   * recursive call of loop on the body of the expression, using
     the new environment
   * mark used variables:
       [let r = use_var r id]
   * remove variable related bottom up information:
       [let r = exit_scope r id in]
   * rebuild the expression according to the information about
     its content.
   * associate its description to the returned value:
       [ret r approx]
   * replace the returned expression by a contant or a direct variable
     access (when possible):
       [simplify_using_approx_and_env env r expr approx]
 *)

let ret = R.set_approx

let simplify_named_using_approx r lam approx =
  let lam, _summary, approx = A.simplify_named approx lam in
  lam, R.set_approx r approx

let simplify_using_approx_and_env env r original_lam approx =
  let lam, summary, approx =
    A.simplify_using_env approx ~is_present_in_env:(E.present env) original_lam
  in
  let r =
    let r = ret r approx in
    match summary with
    | Replaced_term_by_variable var ->
      R.map_benefit (R.use_var r var) (B.remove_code original_lam)
    | Replaced_term_by_constant -> R.map_benefit r (B.remove_code original_lam)
    | Replaced_term_by_symbol (* CR mshinwell: should we do something here? *)
    | Nothing_done -> r
  in
  lam, r

let simplify_named_using_approx_and_env env r original_named approx =
  let named, summary, approx =
    A.simplify_named_using_env approx ~is_present_in_env:(E.present env)
      original_named
  in
  let r =
    let r = ret r approx in
    match summary with
    | Replaced_term_by_variable var ->
      R.map_benefit (R.use_var r var) (B.remove_code_named original_named)
    | Replaced_term_by_constant ->
      R.map_benefit r (B.remove_code_named original_named)
    | Replaced_term_by_symbol
    | Nothing_done -> r
  in
  named, r

(* This adds only the minimal set of approximations to the closures.
   It is not strictly necessary to have this restriction, but it helps
   to catch potential substitution bugs. *)
let populate_closure_approximations
      ~(function_decl : Flambda.function_declaration)
      ~(free_vars : (_ * A.t) Variable.Map.t)
      ~(parameter_approximations : A.t Variable.Map.t)
      ~set_of_closures_env =
  (* Add approximations of used free variables *)
  let env =
    Variable.Map.fold (fun id (_, desc) env ->
       if Variable.Set.mem id function_decl.free_variables
       then E.add_approx id desc env
       else env) free_vars set_of_closures_env in
  (* Add known approximations of function parameters *)
  let env =
    List.fold_left (fun env id ->
       let approx = try Variable.Map.find id parameter_approximations
                    with Not_found -> A.value_unknown in
       E.add_approx id approx env)
      env function_decl.params in
  env

(* Determine whether a given closure ID corresponds directly to a variable
   (bound to a closure) in the given environment.  This happens when the body
   of a [let rec]-bound function refers to another in the same set of closures.
   If we succeed in this process, we can change [Project_closure]
   expressions into [Var] expressions, thus sharing closure projections. *)
let reference_recursive_function_directly env closure_id =
  let closure_id = Closure_id.unwrap closure_id in
  match E.find_opt env closure_id with
  | None -> None
  | Some approx -> Some (Flambda.Expr (Var closure_id), approx)

(* Simplify an expression that takes a set of closures and projects an
   individual closure from it. *)
let simplify_project_closure env r ~(project_closure : Flambda.project_closure)
      : Flambda.named * R.t =
  let set_of_closures_approx = E.find project_closure.set_of_closures env in
  match A.check_approx_for_set_of_closures set_of_closures_approx with
  | Wrong ->
    Misc.fatal_errorf "Wrong approximation when projecting closure: %a"
      Printflambda.project_closure project_closure
  | Unresolved symbol ->
    (* A set of closures coming from another compilation unit, whose .cmx is
       missing; as such, we cannot have rewritten the function and don't
       need to do any freshening. *)
    Project_closure project_closure, ret r (A.value_unresolved symbol)
  | Ok (set_of_closures_var, value_set_of_closures) ->
    let closure_id =
      A.freshen_and_check_closure_id value_set_of_closures
        project_closure.closure_id
    in
    match reference_recursive_function_directly env closure_id with
    | Some (flam, approx) -> flam, ret r approx
    | None ->
      let approx =
        A.value_closure ?set_of_closures_var value_set_of_closures
          closure_id
      in
      Project_closure { project_closure with closure_id }, ret r approx

(* Simplify an expression that, given one closure within some set of
   closures, returns another closure (possibly the same one) within the
   same set. *)
let simplify_move_within_set_of_closures env r
      ~(move_within_set_of_closures : Flambda.move_within_set_of_closures)
      : Flambda.named * R.t =
  let closure_approx = E.find move_within_set_of_closures.closure env in
  match A.check_approx_for_closure closure_approx with
  | Wrong ->
    Misc.fatal_errorf "Wrong approximation when moving within set of \
        closures: %a"
      Printflambda.move_within_set_of_closures move_within_set_of_closures
  | Ok (_value_closure, set_of_closures_var, value_set_of_closures) ->
    let freshen =
      (* CR mshinwell: potentially misleading name---not freshening with new
         names, but with previously fresh names *)
      A.freshen_and_check_closure_id value_set_of_closures
    in
    let move_to = freshen move_within_set_of_closures.move_to in
    match reference_recursive_function_directly env move_to with
    | Some (flam, approx) -> flam, ret r approx
    | None ->
      let start_from = freshen move_within_set_of_closures.start_from in
      if Closure_id.equal start_from move_to then
        (* Moving from one closure to itself is a no-op.  We can return an
           [Var] since we already have a variable bound to the closure. *)
        Expr (Var move_within_set_of_closures.closure), ret r closure_approx
      else
        match set_of_closures_var with
        | Some set_of_closures_var ->
          (* A variable bound to the set of closures is in scope, meaning we
             can rewrite the [Move_within_set_of_closures] to a
             [Project_closure]. *)
          let project_closure : Flambda.project_closure =
            { set_of_closures = set_of_closures_var;
              closure_id = move_to;
            }
          in
          let approx =
            A.value_closure ~set_of_closures_var value_set_of_closures move_to
          in
          Project_closure project_closure, ret r approx
        | None ->
          (* The set of closures is not available in scope, and we have no
             other information by which to simplify the move. *)
          let move_within : Flambda.move_within_set_of_closures =
            { move_within_set_of_closures with start_from; move_to; }
          in
          let approx = A.value_closure value_set_of_closures move_to in
          Move_within_set_of_closures move_within, ret r approx

(* Transform an expression denoting an access to a variable bound in
   a closure.  Variables in the closure ([project_var.closure]) may
   have been freshened since [expr] was constructed; as such, we
   must ensure the same happens to [expr].  The renaming information is
   contained within the approximation deduced from [closure] (as
   such, that approximation *must* identify which closure it is).

   For instance in some imaginary syntax for flambda:

     [let f x =
        let g y ~closure:{a} = a + y in
        let closure = { a = x } in
          g 12 ~closure]

   when [f] is traversed, [g] can be inlined, resulting in the
   expression

     [let f z =
        let g y ~closure:{a} = a + y in
        let closure = { a = x } in
          closure.a + 12]

   [closure.a] being a notation for:

     [Project_var{closure = closure; closure_id = g; var = a}]

   If [f] is inlined later, the resulting code will be

     [let x = ... in
      let g' y' ~closure':{a'} = a' + y' in
      let closure' = { a' = x } in
        closure'.a' + 12]

   in particular the field [a] of the closure has been alpha renamed to [a'].
   This information must be carried from the declaration to the use.

   If the function is declared outside of the alpha renamed part, there is
   no need for renaming in the [Ffunction] and [Project_var].
   This is not usualy the case, except when the closure declaration is a
   symbol.

   What ensures that this information is available at [Project_var]
   point is that those constructions can only be introduced by inlining,
   which requires that same information. For this to still be valid,
   other transformation must avoid transforming the information flow in
   a way that the inline function can't propagate it.
*)
let rec simplify_project_var env r ~(project_var : Flambda.project_var)
      : Flambda.named * R.t =
  let approx = R.approx r in
  let closure = project_var.closure in
  match A.check_approx_for_closure_allowing_unresolved approx with
  | Ok (_value_closure, _set_of_closures_var, value_set_of_closures) ->
    let module F = Freshening.Project_var in
    let freshening = value_set_of_closures.freshening in
    let var = F.apply_var_within_closure freshening project_var.var in
    let closure_id = project_var.closure_id in
    let closure_id' = F.apply_closure_id freshening closure_id in
    assert (Closure_id.equal closure_id closure_id');
    let approx = A.approx_for_bound_var value_set_of_closures var in
    let expr : Flambda.named = Project_var { closure; closure_id; var; } in
    simplify_named_using_approx_and_env env r expr approx
  | Unresolved symbol ->
    (* This value comes from a symbol for which we couldn't find any
       approximation, telling us that names within the closure couldn't
       have been renamed.  So we don't need to change the variable or
       closure ID in the [Project_var] expression. *)
    Project_var { project_var with closure }, ret r (A.value_unresolved symbol)
  | Wrong ->
    (* We must have the correct approximation of the value to ensure
       we take account of all freshenings. *)
    Misc.fatal_errorf "[Project_var] from a value with wrong \
        approximation: %a@.%a@.%a@."
      Printflambda.project_var project_var
      Variable.print closure
      Simple_value_approx.print approx

and sequence env r expr1 expr2 =
  let expr : Flambda.t =
    Let (Immutable, Variable.create "seq", Expr expr1, expr2)
  in
  loop env r expr

and loop env r tree =
  let f, r = loop_direct env r tree in
  let module Backend = (val (E.backend env) : Backend_intf.S) in
  (* CR mshinwell for pchambart: This call to [really_import_approx] is
     kind of confusing; it seems like some kind of catch-all.  What
     exactly is happening here? *)
  f, ret r (Backend.really_import_approx (R.approx r))

and loop_named env r (tree : Flambda.named) : Flambda.named * R.t =
  match tree with
  | Symbol sym ->
    let module Backend = (val (E.backend env) : Backend_intf.S) in
    simplify_named_using_approx r tree (Backend.import_symbol sym)
  | Const cst -> tree, ret r (A.const cst)
  | Set_of_closures set_of_closures ->
    simplify_set_of_closures env r set_of_closures
  | Project_closure project_closure ->
    simplify_project_closure env r ~project_closure
  | Project_var project_var -> simplify_project_var env r ~project_var
  | Move_within_set_of_closures move_within_set_of_closures ->
    simplify_move_within_set_of_closures env r ~move_within_set_of_closures
  | Prim (Pgetglobal id, [], _dbg) ->
    let approx =
      if Ident.is_predef_exn id then A.value_unknown
      else
        let module Backend = (val (E.backend env) : Backend_intf.S) in
        Backend.import_global id
    in
    tree, ret r approx
  | Prim (Pgetglobalfield (id, i), [], _dbg) ->
    let approx =
      if id = Compilation_unit.get_current_id_exn ()
      then R.find_global r ~field_index:i
      else
        let module Backend = (val (E.backend env) : Backend_intf.S) in
        A.get_field (Backend.import_global id) ~field_index:i
    in
    simplify_named_using_approx r tree approx
  | Prim (Psetglobalfield (_, i), [arg], _) ->
    let approx = E.find arg env in
    let r = R.add_global r ~field_index:i ~approx in
    tree, ret r A.value_unknown
  | Prim (Pfield i, [arg], _) as expr ->
    let approx = A.get_field (E.find arg env) ~field_index:i in
    simplify_named_using_approx_and_env env r expr approx
  | Prim ((Psetfield _ | Parraysetu _ | Parraysets _), block::_, dbg) ->
    let block_approx = E.find block env in
    if A.is_certainly_immutable block_approx then begin
      Location.prerr_warning (Debuginfo.to_location dbg)
        Warnings.Assignment_on_non_mutable_value
    end;
    tree, ret r A.value_unknown
  | Prim ((Psequand | Psequor), _, _) ->
    Misc.fatal_error "Psequand and Psequor must be expanded (see handling in \
        closure_conversion.ml)"
  | Prim (p, args, dbg) ->
    let approxs = E.find_list env args in
    let expr, approx, benefit =
      let module Backend = (val (E.backend env) : Backend_intf.S) in
      Simplify_primitives.primitive p (args, approxs) tree dbg
        ~size_int:Backend.size_int ~big_endian:Backend.big_endian
    in
    let r = R.map_benefit r (B.(+) benefit) in
    expr, ret r approx

and loop_direct env r (tree : Flambda.t) : Flambda.t * R.t =
  match tree with
  | Var var ->
    let var = Freshening.apply_variable (E.freshening env) var in
    simplify_using_approx_and_env env r (Fvar var) (E.find var env)
  | Apply apply -> simplify_apply env r ~apply
  | Let (str, id, lam, body) ->
    (* The different cases for rewriting [Let] are, if the original code
       corresponds to [let id = lam in body],
       * [body] with [id] substituted by [lam] when possible (unused or
         constant);
       * [lam; body] when id is not used but [lam] has a side effect;
       * [let id = lam in body] otherwise.
     *)
    let init_used_var = R.used_variables r in
    let lam, r = loop env r lam in
    let id, sb = Freshening.add_variable (E.freshening env) id in
    let env = E.set_freshening sb env in
    let def_used_var = R.used_variables r in
    let body_env =
      match str with
      | Mutable ->
       (* If the variable is mutable, we don't propagate anything about it. *)
       E.clear_approx id env
      | Immutable -> E.add_approx id (R.approx r) env
    in
    (* To distinguish variables used by the body and the declaration,
       [body] is rewritten without the set of used variables from
       the declaration. *)
    let r_body = R.set_used_variables r init_used_var in
    let body, r = loop body_env r_body body in
    let (expr : Flambda.t), r =
      if Variable.Set.mem id (R.used_variables r) then
        Flambda.Let (str, id, lam, body),
          (* if [lam] is kept, add its used variables *)
          R.set_used_variables r
            (Variable.Set.union def_used_var (R.used_variables r))
      (* CR mshinwell for pchambart: This looks like a copy of
         the function called [sequence], above
            pchambart: it almost a copy, but we can't return the
         same 'r' in both cases as in other uses of [sequence].
         In fact in the other cases, it should also avoid preventing
         the elimination of unused variables like here, but it didn't
         seem as important as for the let.
         I should find a nice pattern to allow to do that elsewhere
         without too much syntactic noise. *)
      else if Effect_analysis.no_effects lam then
        let r = R.map_benefit r (B.remove_code lam) in
        body, r
      else
        (* CR mshinwell: check that Pignore is inserted correctly by a later
           pass. *)
        (* Generate a fresh name for increasing legibility of the
           intermediate language (in particular to make it more obvious that
           the variable is unused). *)
        let fresh_var = Variable.create "unused" in
        Flambda.Let (fresh_var, lam, body),
          R.set_used_variables r
            (Variable.Set.union def_used_var (R.used_variables r))
    in
    expr, R.exit_scope r id
  | Let_rec (defs, body) ->
    let defs, sb = Freshening.add_variables (E.freshening env) defs in
    let env = E.set_freshening sb env in
    let def_env =
      List.fold_left (fun env_acc (id, _lam) ->
          E.add_approx id A.value_unknown env_acc)
        env defs
    in
    let defs, body_env, r =
      List.fold_right (fun (id, lam) (defs, env_acc, r) ->
          let lam, r = loop def_env r lam in
          let defs = (id, lam) :: defs in
          let env_acc = E.add_approx id (R.approx r) env_acc in
          defs, env_acc, r)
        defs ([], env, r)
    in
    let body, r = loop body_env r body in
    let r = List.fold_left (fun r (id, _) -> R.exit_scope r id) r defs in
    Let_rec (defs, body), r
  | Static_raise (i, args) ->
    let i = Freshening.apply_static_exception (E.freshening env) i in
    let args, _, r = loop_list env r args in
    let r = R.use_staticfail r i in
    Static_raise (i, args), ret r A.value_bottom
  | Static_catch (i, vars, body, handler) ->
    let i, sb = Freshening.add_static_exception (E.freshening env) i in
    let env = E.set_freshening sb env in
    let body, r = loop env r body in
    if not (Static_exception.Set.mem i (R.used_staticfail r)) then
      (* If the static exception is not used, we can drop the declaration *)
      body, r
    else begin
      match (body : Flambda.t) with
      | Static_raise (j, args, _) when
          Static_exception.equal i
            (Freshening.apply_static_exception (E.freshening env) j) ->
        (* This is usually true, since whe checked that the static
           exception was used.  The only case where it can be false
           is when an argument can raise.  This could be avoided if
           all arguments where guaranteed to be variables. *)
        let handler =
          List.fold_left2 (fun body var arg ->
              Flambda.Let (Immutable, var, arg, body))
            handler vars args
        in
        let r = R.exit_scope_catch r i in
        loop env r handler
      | _ ->
        let vars, sb = Freshening.add_variables' (E.freshening env) vars in
        let env =
          List.fold_left (fun env id -> E.add_approx id A.value_unknown env)
            (E.set_freshening sb env) vars
        in
        let env = E.inside_branch env in
        let handler, r = loop env r handler in
        let r = List.fold_left R.exit_scope r vars in
        let r = R.exit_scope_catch r i in
        Static_catch (i, vars, body, handler), ret r A.value_unknown
    end
  | Try_with (body, id, handler) ->
    let body, r = loop env r body in
    let id, sb = Freshening.add_variable (E.freshening env) id in
    let env = E.add_approx id A.value_unknown (E.set_freshening sb env) in
    let env = E.inside_branch env in
    let handler, r = loop env r handler in
    let r = R.exit_scope r id in
    Try_with (body, id, handler), ret r A.value_unknown
  | If_then_else (arg, ifso, ifnot) ->
    (* When arg is the constant false or true (or something considered
       as true), we can drop the if and replace it by a sequence.
       if arg is not effectful we can also drop it. *)
    let arg, r = loop env r arg in
    begin match (R.approx r).descr with
    | Value_constptr 0 ->
      (* constant false, keep ifnot *)
      let ifnot, r = loop env r ifnot in
      let r = R.map_benefit r B.remove_branch in
      sequence env r arg ifnot, r
    | Value_constptr _ | Value_block _ ->
      (* constant true, keep ifso *)
      let ifso, r = loop env r ifso in
      let r = R.map_benefit r B.remove_branch in
      sequence env r arg ifso, r
    | _ ->
      let env = E.inside_branch env in
      let ifso, r = loop env r ifso in
      let ifso_approx = R.approx r in
      let ifnot, r = loop env r ifnot in
      let ifnot_approx = R.approx r in
      If_then_else (arg, ifso, ifnot),
      ret r (A.meet ifso_approx ifnot_approx)
    end
  | While (cond, body) ->
    let cond, r = loop env r cond in
    let env = E.inside_loop env in
    let body, r = loop env r body in
    While (cond, body), ret r A.value_unknown
  | Send (kind, met, obj, args, dbg) ->
    let met, r = loop env r met in
    let obj, r = loop env r obj in
    let args, _, r = loop_list env r args in
    Send (kind, met, obj, args, dbg), ret r A.value_unknown
  | For (id, lo, hi, dir, body) ->
    let lo, r = loop env r lo in
    let hi, r = loop env r hi in
    let id, sb = Freshening.add_variable (E.freshening env) id in
    let env = E.add_approx id A.value_unknown (E.set_freshening sb env) in
    let env = E.inside_loop env in
    let body, r = loop env r body in
    let r = R.exit_scope r id in
    For (id, lo, hi, dir, body), ret r A.value_unknown
  | Assign (id, lam) ->
    let lam, r = loop env r lam in
    let id = Freshening.apply_variable (E.freshening env) id in
    let r = R.use_var r id in
    Assign (id, lam), ret r A.value_unknown
  | Switch (arg, sw) ->
    (* When arg is known to be a block with a fixed tag or a fixed integer,
       we can drop the switch and replace it by a sequence.
       if arg is not effectful we can also drop it. *)
    let arg, r = loop env r arg in
    let get_failaction () : Flambda.t =
      (* If the switch is applied to a statically-known value that is
         outside of each match case:
         * if there is a default action take that case
         * otherwise this is something that is guaranteed not to
           be reachable by the type checker: for instance
           [type 'a t = Int : int -> int t | Float : float -> float t
            match Int 1 with
            | Int _ -> ...
            | Float f as v ->
                match v with       This match is unreachable
                | Float f -> ...]
       *)
      match sw.failaction with
      | None -> Proved_unreachable
      | Some f -> f
    in
    begin match (R.approx r).descr with
    | Value_int i
    | Value_constptr i ->
      let lam =
        try List.assoc i sw.consts
        with Not_found -> get_failaction ()
      in
      let lam, r = loop env r lam in
      let r = R.map_benefit r B.remove_branch in
      sequence env r arg lam, r
    | Value_block (tag, _) ->
      let tag = Tag.to_int tag in
      let lam =
        try List.assoc tag sw.blocks
        with Not_found -> get_failaction ()
      in
      let lam, r = loop env r lam in
      let r = R.map_benefit r B.remove_branch in
      sequence env r arg lam, r
    | _ ->
      let env = E.inside_branch env in
      let f (i, v) (acc, r) =
        let approx = R.approx r in
        let lam, r = loop env r v in
        ((i, lam)::acc, R.set_approx r (A.meet (R.approx r) approx))
      in
      let r = R.set_approx r A.value_bottom in
      let consts, r = List.fold_right f sw.consts ([], r) in
      let blocks, r = List.fold_right f sw.blocks ([], r) in
      let failaction, r =
        match sw.failaction with
        | None -> None, r
        | Some l ->
          let approx = R.approx r in
          let l, r = loop env r l in
          Some l, R.set_approx r (A.meet (R.approx r) approx)
      in
      let sw = { sw with failaction; consts; blocks; } in
      Switch (arg, sw), r
    end
  | String_switch (arg, sw, def) ->
    let arg, r = loop env r arg in
    let sw, r =
      List.fold_right (fun (str, lam) (sw, r) ->
          let lam, r = loop env r lam in
          (str, lam)::sw, r)
        sw
        ([], r)
    in
    let def, r =
      match def with
      | None -> def, r
      | Some def ->
        let def, r = loop env r def in
        Some def, r
    in
    String_switch (arg, sw, def), ret r A.value_unknown
  | Proved_unreachable -> tree, ret r A.value_bottom

and loop_list env r l = match l with
  | [] -> [], [], r
  | h::t ->
    let t', approxs, r = loop_list env r t in
    let h', r = loop env r h in
    let approxs = (R.approx r) :: approxs in
    if t' == t && h' == h
    then l, approxs, r
    else h' :: t', approxs, r

(* Transforms closure definitions by applying [loop] on the code of every
   one of the set and on the expressions of the free variables.
   If the substitution is activated, alpha renaming also occur on everything
   defined by the set of closures:
   * Variables bound by a closure of the set
   * closure identifiers
   * parameters

   The rewriting occurs in a clean environment without any of the variables
   defined outside reachable.  This helps increase robustness against accidental,
   potentially unsound simplification of variable accesses by
   [simplify_using_approx_and_env].

   The rewriting occurs in an environment filled with:
   * The approximation of the free variables
   * An explicitely unknown approximation for function parameters,
     except for those where it is known to be safe: those present in the
     [specialised_args] set.
   * An approximation for the closures in the set. It contains the code of
     the functions before rewriting.

   The approximation of the currently defined closures is available to
   allow marking recursives calls as direct and in some cases, allow
   inlining of one closure from the set inside another one. For this to
   be correct an alpha renaming is first applied on the expressions by
   [apply_function_decls_and_free_vars].

   For instance when rewriting the declaration

     [let rec f_1 x_1 =
        let y_1 = x_1 + 1 in
        g_1 y_1
      and g_1 z_1 = f_1 (f_1 z_1)]

   When rewriting this function, the first substitution will contain
   some mapping:
   { f_1 -> f_2;
     g_1 -> g_2;
     x_1 -> x_2;
     z_1 -> z_2 }

   And the approximation for the closure will contain

   { f_2:
       fun x_2 ->
         let y_1 = x_2 + 1 in
         g_2 y_1
     g_2:
       fun z_2 -> f_2 (f_2 z_2) }

   Note that no substitution is applied to the let-bound variable [y_1].
   If [f_2] where to be inlined inside [g_2], we known that a new substitution
   will be introduced in the current scope for [y_1] each time.


   If the function where a recursive one comming from another compilation
   unit, the code already went through [Flambdasym] that could have
   replaced the function variable by the symbol identifying the function
   (this occur if the function contains only constants in its closure).
   To handle that case, we first replace those symbols by the original
   variable.
*)
and simplify_set_of_closures original_env r
      (set_of_closures : Flambda.set_of_closures) : Flambda.named * R.t =
  let function_decls =
    let module Backend = (val (E.backend original_env) : Backend_intf.S) in
    (* CR mshinwell: Does this affect
       [reference_recursive_function_directly]? *)
    Freshening.rewrite_recursive_calls_with_symbols (E.freshening original_env)
      set_of_closures.function_decls ~make_closure_symbol:Backend.closure_symbol
  in
  let env = E.increase_closure_depth original_env in
  let free_vars =
    Variable.Map.map (fun external_var ->
        external_var, E.find external_var env)
      set_of_closures.free_vars
  in
  let specialised_args =
    Variable.Map.map (Freshening.apply_variable (E.freshening env))
      set_of_closures.specialised_args
  in
  (* Remove all variable bindings from the environment.  This isn't necessary,
     but allows us to catch bugs where variables escape their scope. *)
  let environment_before_cleaning = env in
  let env = E.local env in
  let free_vars, function_decls, sb, freshening =
    Freshening.apply_function_decls_and_free_vars (E.freshening env) free_vars
      function_decls
  in
  let env = E.set_freshening sb env in
  let specialised_args =
    Variable.Map.map_keys (Freshening.apply_variable (E.freshening env))
      specialised_args
  in
  let parameter_approximations =
    (* Approximations of parameters that are known to always hold the same
       argument throughout the body of the function. *)
    Variable.Map.map_keys (Freshening.apply_variable (E.freshening env))
      (Variable.Map.map (fun id -> E.find id environment_before_cleaning)
        specialised_args)
  in
  let env =
    E.enter_set_of_closures_declaration function_decls.set_of_closures_id env
  in
  (* we use the previous closure for evaluating the functions *)
  let internal_value_set_of_closures : A.value_set_of_closures =
    { function_decls = function_decls;
      bound_vars = Variable.Map.fold (fun id (_, desc) map ->
          Var_within_closure.Map.add (Var_within_closure.wrap id) desc map)
          free_vars Var_within_closure.Map.empty;
      unchanging_params = Variable.Set.empty;
      specialised_args = Variable.Map.keys specialised_args;
      freshening;
    }
  in
  (* Populate the environment with the approximation of each closure.
     This part of the environment is shared between all of the closures in
     the set of closures. *)
  let set_of_closures_env =
    Variable.Map.fold (fun closure _ env ->
        let approx =
          A.value_closure ~closure_var:closure internal_value_set_of_closures
            (Closure_id.wrap closure)
        in
        E.add_approx closure approx env)
      function_decls.funs env
  in
  let simplify_function fid (function_decl : Flambda.function_declaration)
        (funs, used_params, r)
        : Flambda.function_declaration Variable.Map.t * Variable.Set.t * R.t =
    let closure_env =
      populate_closure_approximations ~function_decl ~free_vars
        ~parameter_approximations ~set_of_closures_env
    in
    let body, r =
      E.enter_closure closure_env ~closure_id:(Closure_id.wrap fid)
        ~inline_inside:
          (Inlining_decision.should_inline_inside_declaration function_decl)
        ~where:Transform_set_of_closures_expression
        ~f:(fun body_env -> loop body_env r function_decl.body)
    in
    let used_params =
      (* CR mshinwell for pchambart: The original code seemed to start from
         the existing [used_params] as well.  Why do we need that?  I think
         we might be able to just remove this whole "union used_params" bit. *)
      let is_used_var var = Variable.Set.mem var (R.used_variables r) in
      Variable.Set.union used_params
        (Variable.Set.of_list (List.filter is_used_var function_decl.params))
    in
    let free_variables = Free_variables.calculate body in
    Variable.Map.add fid { function_decl with body; free_variables } funs,
      used_params, R.exit_scope_set r function_decl.free_variables
  in
  let funs, used_params, r =
    Variable.Map.fold simplify_function function_decls.funs
      (Variable.Map.empty, Variable.Set.empty, r)
  in
  let specialised_args =
    (* Remove any specialised arguments whose parameters are unused. *)
    Variable.Map.filter (fun id _ -> Variable.Set.mem id used_params)
      specialised_args
  in
  let r =
    Variable.Map.fold (fun _id' v acc -> R.use_var acc v)
      specialised_args r
  in
  let function_decls = { function_decls with funs } in
  let unchanging_params =
    Invariant_params.unchanging_params_in_recursion function_decls
  in
  let value_set_of_closures : A.value_set_of_closures =
    { internal_value_set_of_closures with
      function_decls; unchanging_params;
    }
  in
  let r = Variable.Map.fold (fun id _ r -> R.exit_scope r id) function_decls.funs r in
  let set_of_closures : Flambda.set_of_closures =
    { function_decls;
      free_vars = Variable.Map.map fst free_vars;
      specialised_args;
    }
  in
  Set_of_closures (set_of_closures),
    ret r (A.value_set_of_closures value_set_of_closures)

(* Transform an flambda function application based on information provided
   by an approximation of the function being applied.

   If the approximation does not identify which closure is being applied, the
   application remains as-is.

   Otherwise, we determine whether the application is actually a full or
   partial application (note that previously it may have appeared as partial,
   but now we may know from the approximation that it is full).  The
   interesting case is that of a full application: we then consider whether
   the function can be inlined.  (See [full_apply], below.)
*)
and simplify_apply env r ~(apply : Flambda.apply) : Flambda.t * R.t =
  let { Flambda. func; args; kind = _; dbg } = apply in
  let func_approx = Env.find func env in
  let args_approxs = List.map (fun arg -> E.find arg env) args in
  let no_transformation () : Flambda.t * R.t =
    Apply ({ func; args; kind = Indirect; dbg }),
      ret r A.value_unknown
  in
  match A.check_approx_for_closure func_approx with
  | Ok (value_closure, _set_of_closures_var, value_set_of_closures) ->
    let closure_id = value_closure.closure_id in
    let clos = value_set_of_closures.function_decls in
    let func_decl =
      try Flambdautils.find_declaration closure_id clos with
      | Not_found ->
        Format.printf "approximation references non-existent closure %a@."
            Closure_id.print closure_id;
        assert false
    in
    let nargs = List.length args in
    let arity = Flambdautils.function_arity func_decl in
    if nargs = arity then
      full_apply env r clos func closure_id func_decl value_set_of_closures
        (args, args_approxs) dbg
    else if nargs > arity then
      let h_args, q_args = Misc.split_at arity args in
      let h_approxs, _q_approxs = Misc.split_at arity args_approxs in
      let expr, r =
        full_apply env r clos func closure_id func_decl value_set_of_closures
          (h_args, h_approxs) dbg
      in
      let func_var = fresh_variable ~name:"full_apply" in
      let expr : Flambda.t =
        Let (Immutable, func_var, expr,
          Apply { func = func_var; args = q_args; kind = Indirect; dbg })
      in
      loop env r expr
    else if nargs > 0 && nargs < arity then
      let partial_fun = partial_apply func closure_id func_decl args dbg in
      loop env r partial_fun
    else
      no_transformation ()
  | Wrong -> no_transformation ()

and full_apply env r clos lhs_of_application closure_id func closure
      args_with_approxs dbg eid =
  Inlining_decision.for_call_site ~env ~r ~clos ~lhs_of_application
    ~fun_id:closure_id ~func ~closure ~args_with_approxs ~dbg ~eid
    ~simplify:loop

and partial_apply funct fun_id (func : Flambda.function_declaration)
      (args : Variable.t list) dbg : Flambda.t =
  let arity = Flambdautils.function_arity func in
  let remaining_args = arity - (List.length args) in
  assert (remaining_args > 0);
  let param_sb =
    List.map (fun id -> Variable.freshen id) func.params
  in
  let applied_args, remaining_args = Misc.map2_head
      (fun arg id' -> id', arg) args param_sb in
  let funct_id = Variable.create "partial_called_fun" in
  let new_fun_id = Variable.create "partial_fun" in
  let expr : Flambda.t =
    Apply ({
      func = funct_id;
      args = param_sb;
      kind = Direct fun_id;
      dbg;
    })
  in
  let closures =
    Flambdautils.make_closure_declaration ~id:new_fun_id
      ~body:expr ~params:remaining_args
  in
  let with_args = List.fold_right (fun (id', arg) expr ->
      Flambda.Let (Immutable, id', Var (arg),
        expr))
    applied_args closures
  in
  Let (Immutable, funct_id, funct, with_args)

(* CR mshinwell for pchambart: Change to a "-dinlining-benefit" option? *)
let debug_benefit =
  try ignore (Sys.getenv "BENEFIT"); true
  with _ -> false

let run ~never_inline ~backend tree =
  let r =
    if never_inline then
      R.set_inlining_threshold (R.create ()) Inlining_cost.Never_inline
    else
      R.create ()
  in
  let stats = !Clflags.inlining_stats in
  if never_inline then Clflags.inlining_stats := false;
  let env = E.empty ~never_inline:false ~backend in
  let result, r = loop env r tree in
  Clflags.inlining_stats := stats;
  if not (Variable.Set.is_empty (R.used_variables r))
  then begin
    Misc.fatal_error (Format.asprintf "remaining variables: %a@.%a@."
      Variable.Set.print (R.used_variables r)
      Printflambda.flambda result)
  end;
  assert (Variable.Set.is_empty (R.used_variables r));
  if not (Static_exception.Set.is_empty (R.used_staticfail r))
  then begin
    Misc.fatal_error (Format.asprintf "remaining static exceptions: %a@.%a@."
      Static_exception.Set.print (R.used_staticfail r)
      Printflambda.flambda result)
  end;
  assert (Static_exception.Set.is_empty (R.used_staticfail r));
  if debug_benefit then
    Format.printf "benefit:@ %a@."
      B.print (R.benefit r);
  result
