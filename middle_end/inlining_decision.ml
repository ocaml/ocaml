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
      R.set_inlining_threshold (R.reset_benefit r) Inlining_cost.Never_inline
    in
    Inlining_transforms.inline_by_copying_function_body ~env ~r
      ~function_decls ~lhs_of_application ~closure_id_being_applied
      ~function_decl ~args ~simplify
  in
  let keep_inlined_version =
    if function_decl.stub then begin
      made_decision (Inlined (Copying_body Stub));
      true
    end else if only_use_of_function then begin
      made_decision (Inlined (Copying_body Decl_local_to_application));
      true
    end else begin
      let sufficient_benefit =
        W.create ~original:(fst (no_simplification ())) body
          ~branch_depth:(E.branch_depth env)
          ~probably_a_functor
          ~round:(E.round env)
          (R.benefit r_inlined)
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
    let env =
      if not function_decl.stub ||
         (* Stub functions should not prevent other functions
            from being evaluated for inlining *)
         E.inlining_level env = 0
         (* If the function was considered for inlining without considering
            its sub-functions, and it is not below another inlining choice,
            then we are certain that this code will be kept. *)
      then env
      else E.inlining_level_up env
    in
    simplify env r body
  end else begin
    (* Inlining the body of the function did not appear sufficiently
       beneficial; however, it may become so if we inline within the body
       first.  We try that next. *)
    let body, r_inlined =
      Inlining_transforms.inline_by_copying_function_body ~env
        ~r:(R.reset_benefit r)
        ~function_decls ~lhs_of_application ~closure_id_being_applied
        ~function_decl ~args ~simplify
    in
    let wsb =
      W.create ~original:(fst (no_simplification ()))
        ~branch_depth:(E.branch_depth env)
        body ~probably_a_functor ~round:(E.round env)
        (R.benefit r_inlined)
    in
    let keep_inlined_version = W.evaluate wsb in
    let decision : Inlining_stats_types.Decision.t =
      if keep_inlined_version then
        (* CR mshinwell: This "with_subfunctions" name isn't
           descriptive enough. *)
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
         exists if the body of the function is in fact inlined.
         If the function approximation contained an approximation that
         does not depend on the actual values of its arguments, it
         could be returned instead of [A.value_unknown]. *)
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
            ~r:(R.reset_benefit r) ~function_decls ~lhs_of_application
            ~closure_id_being_applied ~function_decl ~args ~simplify
        in
        tried_unrolling := true;
        let wsb =
          W.create body ~original:(fst (no_simplification()))
            ~branch_depth:(E.branch_depth env)
            ~probably_a_functor:false ~round:(E.round env)
            (R.benefit r_inlined)
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

let should_duplicate_recursive_function env
      ~(function_decl : Flambda.function_declaration)
      ~(function_decls : Flambda.function_declarations)
      ~(value_set_of_closures : A.value_set_of_closures)
      ~args_approxs ~(invariant_params : Variable.Set.t Variable.Map.t) =
  assert (List.length function_decl.params = List.length args_approxs);
  !Clflags.inline_recursive_functions
    && (not (E.inside_set_of_closures_declaration
      function_decls.set_of_closures_id env))
    && (not (Variable.Map.is_empty value_set_of_closures.invariant_params))
    && Var_within_closure.Map.is_empty
      value_set_of_closures.bound_vars (* closed *)
    && List.exists2 (fun id approx ->
        A.useful approx && Variable.Map.mem id invariant_params)
      function_decl.params args_approxs

let inline_recursive env r ~max_level ~lhs_of_application
      ~(function_decls : Flambda.function_declarations)
      ~closure_id_being_applied ~function_decl
      ~(value_set_of_closures : Simple_value_approx.value_set_of_closures)
      ~(invariant_params : Variable.Set.t Variable.Map.t)
      ~args ~args_approxs ~dbg ~simplify ~no_simplification
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
        ~function_decl ~value_set_of_closures ~args_approxs ~invariant_params
    then
      let copied_function_declaration =
        Inlining_transforms.inline_by_copying_function_declaration ~env
          ~r:(R.reset_benefit r) ~lhs_of_application
          ~function_decls ~closure_id_being_applied ~function_decl
          ~args ~args_approxs ~invariant_params
          ~specialised_args:value_set_of_closures.specialised_args ~dbg
          ~simplify
      in
      match copied_function_declaration with
      | Some (expr, r_inlined) ->
        let wsb =
          W.create ~original:(fst (no_simplification ()))
            ~branch_depth:(E.branch_depth env)
            expr ~probably_a_functor:false
            ~round:(E.round env)
            (R.benefit r_inlined)
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

let is_probably_a_functor ~env ~args_approxs ~recursive_functions
    ~function_decl =
  !Clflags.functor_heuristics
    && E.at_toplevel env
    && (not (E.is_inside_branch env))
    && ((List.for_all A.known args_approxs) || function_decl.Flambda.is_a_functor)
    && Variable.Set.is_empty (Lazy.force recursive_functions)

let for_call_site ~env ~r ~(function_decls : Flambda.function_declarations)
      ~lhs_of_application ~closure_id_being_applied
      ~(function_decl : Flambda.function_declaration)
      ~(value_set_of_closures : Simple_value_approx.value_set_of_closures)
      ~args ~args_approxs ~dbg ~simplify ~inline_requested =
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
      dbg;
      inline = inline_requested;
    }, R.set_approx r (A.value_unknown Other)
  in
  let max_level =
    match !Clflags.max_inlining_depth with
    | Always max_inlining_depth -> max_inlining_depth
    | Variable by_round ->
      match Ext_types.Int.Map.find (E.round env) by_round with
      | max_inlining_depth -> max_inlining_depth
      | exception Not_found -> Clflags.default_max_inlining_depth
  in
  let unconditionally_inline =
    match (inline_requested : Lambda.inline_attribute) with
    | Always_inline -> true
    | Never_inline ->
      (* CR-someday mshinwell: consider whether there could be better
         behaviour for stubs *)
      false
    | Default_inline -> function_decl.stub
  in
  let num_params = List.length function_decl.params in
  let only_use_of_function = false in
  let inlining_threshold = R.inlining_threshold r in
  let fun_var =
    U.find_declaration_variable closure_id_being_applied function_decls
  in
  let recursive_functions =
    lazy
      (Find_recursive_functions.in_function_declarations function_decls
         ~backend:(E.backend env))
  in
  let probably_a_functor =
    is_probably_a_functor ~env ~args_approxs ~recursive_functions ~function_decl
  in
  let recursive =
    lazy (Variable.Set.mem fun_var (Lazy.force recursive_functions))
  in
  let fun_cost : Inlining_cost.inlining_threshold =
    match (inline_requested : Lambda.inline_attribute) with
    | Never_inline -> Never_inline
    | Always_inline | Default_inline ->
      (* CR mshinwell: should clarify exactly what "unconditionally" means. *)
      if unconditionally_inline
         || (only_use_of_function && not (Lazy.force recursive))
         || probably_a_functor
      then
        inlining_threshold
      else
        Inlining_cost.can_try_inlining function_decl.body inlining_threshold
          ~bonus:num_params
  in
  let expr, r =
    if E.never_inline env then
      (* This case only occurs when examining the body of a stub function
         but not in the context of inlining said function.  As such, there
         is nothing to do here (and no decision to report). *)
      no_simplification ()
    else if fun_cost = Inlining_cost.Never_inline && not function_decl.stub then
      (* CR pchambart: should we also accept unconditionnal inline ?
         It is some kind of user defined stub, but if we restrict to stub
         we are certain that no abusive use of [@@inline] can blow things up *)
      let reason : Inlining_stats_types.Decision.t =
        match inlining_threshold with
        | Never_inline ->
          Function_prevented_from_inlining
        | Can_inline_if_no_larger_than threshold ->
          Function_obviously_too_large threshold
      in
      made_decision reason;
      no_simplification ()
    else
      let remaining_inlining_threshold = fun_cost in
      let r = R.set_inlining_threshold r remaining_inlining_threshold in
      let invariant_params = value_set_of_closures.invariant_params in
      (* Try inlining if the function is non-recursive and not too far above
         the threshold (or if the function is to be unconditionally
         inlined). *)
      (* CR mshinwell for pchambart: I don't understand why this was applying
         inline_non_recursive to recursive functions. *)
      if unconditionally_inline
        || (E.inlining_level env <= max_level && not (Lazy.force recursive))
      then
        inline_non_recursive env r ~function_decls ~lhs_of_application
          ~closure_id_being_applied ~function_decl ~made_decision
          ~only_use_of_function ~no_simplification ~probably_a_functor
          ~args ~simplify
      else if E.inlining_level env > max_level then begin
        made_decision (Can_inline_but_tried_nothing (Level_exceeded true));
        no_simplification ()
      end else if Lazy.force recursive then
        inline_recursive env r ~max_level ~lhs_of_application ~function_decls
          ~closure_id_being_applied ~function_decl ~value_set_of_closures
          ~invariant_params ~args ~args_approxs ~dbg ~simplify
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
