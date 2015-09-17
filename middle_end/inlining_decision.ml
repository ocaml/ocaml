(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result
module U = Flambda_utils
module W = Inlining_cost.Whether_sufficient_benefit

let is_probably_a_functor ~env ~args_approxs ~recursive_functions =
  !Clflags.functor_heuristics
    && E.at_toplevel env
    && (not (E.is_inside_branch env))
    && List.for_all A.known args_approxs
    && Variable.Set.is_empty recursive_functions

let should_duplicate_recursive_function env
      ~(function_decl : Flambda.function_declaration)
      ~(function_decls : Flambda.function_declarations)
      ~(value_set_of_closures : A.value_set_of_closures)
      ~args_approxs ~unchanging_params =
  assert (List.length function_decl.params = List.length args_approxs);
  (not (E.inside_set_of_closures_declaration
      function_decls.set_of_closures_id env))
    && (not (Variable.Set.is_empty value_set_of_closures.unchanging_params))
    && Var_within_closure.Map.is_empty
        value_set_of_closures.bound_vars (* closed *)
    && List.exists2 (fun id approx ->
          A.useful approx && Variable.Set.mem id unchanging_params)
        function_decl.params args_approxs

let inline_non_recursive env r ~function_decls ~lhs_of_application
    ~closure_id_being_applied ~(function_decl : Flambda.function_declaration)
    ~only_use_of_function ~no_simplification ~probably_a_functor
    ~(args : Variable.t list) ~simplify
    ~(made_decision : Inlining_stats_types.Decision.t -> unit) =
  let body, r_inlined =
    (* First we construct the code that would result from copying the body of
       the function, without doing any further inlining upon it, to the call
       site. *)
    let r =
      R.set_inlining_threshold (R.clear_benefit r) Inlining_cost.Never_inline
    in
    Inlining_transforms.inline_by_copying_function_body ~env ~r
      ~function_decls ~lhs_of_application ~closure_id_being_applied
      ~function_decl ~args ~simplify
  in
  let keep_inlined_version =
    if function_decl.stub then begin
      made_decision (Inlined (Copying_body Unconditionally));
      true
    end else if only_use_of_function then begin
      made_decision (Inlined (Copying_body Decl_local_to_application));
      true
    end else begin
      let sufficient_benefit =
        W.create ~original:(fst (no_simplification ())) body
          ~probably_a_functor (R.benefit r_inlined)
      in
      let keep_inlined_version = W.evaluate sufficient_benefit in
      let decision : Inlining_stats_types.Decision.t =
        if keep_inlined_version then
          Inlined (Copying_body (Evaluated sufficient_benefit))
        else
          Tried (Copying_body (Evaluated sufficient_benefit))
      in
      made_decision decision;
      keep_inlined_version
    end
  in
  if keep_inlined_version then begin
    (* Inlining the body of the function was sufficiently beneficial that we
       will keep it, replacing the call site.  We continue by allowing
       further inlining within the inlined copy of the body. *)
    let r =
      R.map_benefit r (Inlining_cost.Benefit.(+) (R.benefit r_inlined))
    in
    (* CR mshinwell for pchambart: This [lift_lets] should have a comment. *)
    let body = Lift_code.lift_lets_expr body in
    let env =
      E.note_entering_closure env ~closure_id:closure_id_being_applied
        ~where:Inline_by_copying_function_body
    in
try
    simplify (E.inlining_level_up env) r body
with _exn ->
  Format.eprintf "Exception from simplify (inlining_decision), \
        closure id being applied is %a, term is %a, \
        original function decls %a"
    Closure_id.print closure_id_being_applied
    Flambda.print body
    Flambda.print_function_declarations function_decls;
  Misc.fatal_error "failure"
  end else begin
    (* Inlining the body of the function did not appear sufficiently
       beneficial; however, it may become so if we inline within the body
       first.  We try that next. *)
    let body, r_inlined =
      Inlining_transforms.inline_by_copying_function_body ~env
        ~r:(R.clear_benefit r)
        ~function_decls ~lhs_of_application ~closure_id_being_applied
        ~function_decl ~args ~simplify
    in
    let wsb =
      W.create ~original:(fst (no_simplification ()))
        body ~probably_a_functor (R.benefit r_inlined)
    in
    let keep_inlined_version = W.evaluate wsb in
    let decision : Inlining_stats_types.Decision.t =
      if keep_inlined_version then
        Inlined (Copying_body_with_subfunctions (Evaluated wsb))
      else
        Tried (Copying_body_with_subfunctions (Evaluated wsb))
    in
    made_decision decision;
    if keep_inlined_version then begin
      body, R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
    end
    else begin
      (* r_inlined contains an approximation that may be invalid for the
         untransformed expression: it may reference functions that only
         exists if the body of the function is effectively inlined.
         If the function approximation contained an approximation that
         does not depends on the effective value of its arguments, it
         could be returned instead of [A.value_unknown] *)
      no_simplification ()
    end
  end

let unroll_recursive env r ~max_level ~lhs_of_application
      ~(function_decls : Flambda.function_declarations)
      ~closure_id_being_applied ~function_decl ~args ~simplify
      ~no_simplification
      ~(made_decision : Inlining_stats_types.Decision.t -> unit) =
  let tried_unrolling = ref false in
  let result =
    if E.unrolling_allowed env && E.inlining_level env <= max_level then
      let self_unrolling =
        E.inside_set_of_closures_declaration function_decls.set_of_closures_id
          env
      in
      if self_unrolling then
        (* CR mshinwell for pchambart: Should we really completely
           disallow this?  (Maybe there should be a compiler option?) *)
        None
      else begin
        let env = E.inside_unrolled_function env in
        let body, r_inlined =
          Inlining_transforms.inline_by_copying_function_body ~env
            ~r:(R.clear_benefit r) ~function_decls ~lhs_of_application
            ~closure_id_being_applied ~function_decl ~args ~simplify
        in
        tried_unrolling := true;
        let wsb =
          W.create body ~original:(fst (no_simplification()))
            ~probably_a_functor:false (R.benefit r_inlined)
        in
        let keep_unrolled_version =
          if W.evaluate wsb then begin
            made_decision (Inlined (Unrolled wsb));
            true
          end else begin
            (* No decision is recorded here; we will try another strategy
               below, and then record that we also tried to unroll. *)
            false
          end
        in
        if keep_unrolled_version then
          Some (body,
            R.map_benefit r_inlined
              (Inlining_cost.Benefit.(+) (R.benefit r)))
        else None
      end
    else None
  in
  !tried_unrolling, result

let inline_recursive env r ~max_level ~lhs_of_application
      ~(function_decls : Flambda.function_declarations)
      ~closure_id_being_applied ~function_decl
      ~(value_set_of_closures : Simple_value_approx.value_set_of_closures)
      ~unchanging_params ~args ~args_approxs ~dbg ~simplify ~no_simplification
      ~(made_decision : Inlining_stats_types.Decision.t -> unit) =
  let tried_unrolling, unrolling_result =
    (* First try unrolling the recursive call, if we're allowed to. *)
    unroll_recursive env r ~max_level ~lhs_of_application ~function_decls
      ~closure_id_being_applied ~function_decl ~args ~simplify
      ~no_simplification ~made_decision
  in
  match unrolling_result with
  | Some unrolling_result -> unrolling_result
  | None ->
    (* If unrolling failed, consider duplicating the whole function
       declaration at the call site, specialising parameters whose arguments
       we know. *)
    if should_duplicate_recursive_function env ~function_decls
        ~function_decl ~value_set_of_closures ~args_approxs ~unchanging_params
    then
      let copied_function_declaration =
        Inlining_transforms.inline_by_copying_function_declaration ~env
          ~r:(R.clear_benefit r) ~lhs_of_application
          ~function_decls ~closure_id_being_applied ~function_decl
          ~args ~args_approxs ~unchanging_params
          ~specialised_args:value_set_of_closures.specialised_args ~dbg
          ~simplify
      in
      match copied_function_declaration with
      | Some (expr, r_inlined) ->
        let wsb =
          W.create ~original:(fst (no_simplification ()))
            expr ~probably_a_functor:false (R.benefit r_inlined)
        in
        let keep_inlined_version = W.evaluate wsb in
        let decision : Inlining_stats_types.Decision.t =
          if keep_inlined_version then
            Inlined (Copying_decl (Tried_unrolling tried_unrolling, wsb))
          else
            Tried (Copying_decl (Tried_unrolling tried_unrolling, wsb))
        in
        made_decision decision;
        if keep_inlined_version then
          expr, R.map_benefit r_inlined
            (Inlining_cost.Benefit.(+) (R.benefit r))
        else
          no_simplification ()
      | None -> no_simplification ()
    else begin
      made_decision
        (Did_not_try_copying_decl (Tried_unrolling tried_unrolling));
      no_simplification ()
    end

(* CR mshinwell: We deleted the [only_use_of_function] stuff in [for_call_site],
   which used to identify when the function declaration was only used once,
   within the application expression itself (which is no longer
   expressible).  We should consider handling this case elsewhere and
   setting [stub]. *)

let for_call_site ~env ~r ~(function_decls : Flambda.function_declarations)
      ~lhs_of_application ~closure_id_being_applied
      ~(function_decl : Flambda.function_declaration)
      ~(value_set_of_closures : Simple_value_approx.value_set_of_closures)
      ~args ~args_approxs ~dbg ~simplify =
  if List.length args <> List.length args_approxs then begin
    Misc.fatal_error "Inlining_decision.for_call_site: inconsistent lengths \
        of [args] and [args_approxs]"
  end;
  let made_decision =
    let closure_stack =
      E.inlining_stats_closure_stack (E.note_entering_closure env
          ~closure_id:closure_id_being_applied ~where:Inlining_decision)
    in
    Inlining_stats.record_decision ~closure_stack ~debuginfo:dbg
  in
  let no_simplification () : Flambda.t * R.t =
    (* N.B. This function should not have any side effects; it may be called
       speculatively, and the result discarded. *)
    Apply {
      func = lhs_of_application;
      args;
      kind = Direct closure_id_being_applied;
      dbg
    }, R.set_approx r A.value_unknown
  in
  (* CR mshinwell for pchambart: Mysterious constant.  Turn into a compiler
     option? *)
  let max_level = 3 in
  let unconditionally_inline = function_decl.stub in
  let num_params = List.length function_decl.params in
  let only_use_of_function = false in
  let inlining_threshold = R.inlining_threshold r in
  let fun_var =
    U.find_declaration_variable closure_id_being_applied function_decls
  in
  let recursive_functions =
    Find_recursive_functions.in_function_declarations function_decls
      ~backend:(E.backend env)
  in
  let probably_a_functor =
    is_probably_a_functor ~env ~args_approxs ~recursive_functions
  in
  let recursive = Variable.Set.mem fun_var recursive_functions in
  let fun_cost =
    (* CR mshinwell: should clarify exactly what "unconditionally" means. *)
    if unconditionally_inline || (only_use_of_function && not recursive)
       || probably_a_functor
    then
      inlining_threshold
    else
      Inlining_cost.can_try_inlining function_decl.body inlining_threshold
        ~bonus:num_params
  in
  let expr, r =
    match fun_cost with
    | Never_inline ->
      made_decision Function_obviously_too_large;
      no_simplification ()
    | Can_inline_if_no_larger_than _ when E.never_inline env ->
      (* This case only occurs when examining the body of a stub function
         but not in the context of inlining said function.  As such, there
         is nothing to do here (and no decision to report). *)
      (* CR mshinwell: Below there's a comment saying that we never look
         inside stubs... *)
      no_simplification ()
    | (Can_inline_if_no_larger_than _) as remaining_inlining_threshold ->
      let r = R.set_inlining_threshold r remaining_inlining_threshold in
      let unchanging_params = value_set_of_closures.unchanging_params in
      (* Try inlining if the function is non-recursive and not too far above
         the threshold (or if the function is to be unconditionally
         inlined). *)
      if unconditionally_inline
        || (not recursive && E.inlining_level env <= max_level)
      then
        inline_non_recursive env r ~function_decls ~lhs_of_application
          ~closure_id_being_applied ~function_decl ~made_decision
          ~only_use_of_function ~no_simplification ~probably_a_functor
          ~args ~simplify
      else if E.inlining_level env > max_level then begin
        (* CR mshinwell for pchambart: This branch is taken even if
           [recursive = true].  Is that correct?  It's not the inverse of the
           condition just above. *)
        made_decision (Can_inline_but_tried_nothing (Level_exceeded true));
        no_simplification ()
      end else if recursive then
        inline_recursive env r ~max_level ~lhs_of_application ~function_decls
          ~closure_id_being_applied ~function_decl ~value_set_of_closures
          ~unchanging_params ~args ~args_approxs ~dbg ~simplify
          ~no_simplification ~made_decision
      else begin
        made_decision (Can_inline_but_tried_nothing (Level_exceeded false));
        no_simplification ()
      end
  in
  if E.inlining_level env = 0
  then expr, R.set_inlining_threshold r inlining_threshold
  else expr, r

(* We do not inline inside stubs, which are always inlined at their call site.
   Inlining inside the declaration of a stub could result in more code than
   expected being inlined. *)
(* CR mshinwell for pchambart: maybe we need an example here *)
let should_inline_inside_declaration (decl : Flambda.function_declaration) =
  not decl.stub
