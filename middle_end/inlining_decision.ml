(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

module A = Simple_value_approx
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result
module U = Flambda_utils
module W = Inlining_cost.Whether_sufficient_benefit
module T = Inlining_cost.Threshold

let inline_non_recursive env r ~function_decls ~lhs_of_application
    ~closure_id_being_applied ~(function_decl : Flambda.function_declaration)
    ~value_set_of_closures ~only_use_of_function ~original
    ~(args : Variable.t list) ~size_from_approximation ~simplify
    ~always_inline ~(inline_requested : Lambda.inline_attribute)
    ~(made_decision : Inlining_stats_types.Decision.t -> unit) =
  (* When all of the arguments to the function being inlined are unknown, then
     we cannot materially simplify the function.  As such, we know what the
     benefit of inlining it would be: just removing the call.  In this case
     we may be able to prove the function cannot be inlined without traversing
     its body.
     Note that if the function is sufficiently small, we still have to call
     [simplify], because the body needs freshening before substitution.
  *)
  (* CR-someday mshinwell: (from GPR#8): pchambart writes:

      We may need to think a bit about that. I can't see a lot of meaningful
      examples right now, but there are some cases where some optimisation can
      happen even if we don't know anything about the shape of the arguments.

      For instance

      let f x y = x

      let g x =
        let y = (x,x) in
        f x y
      let f x y =
        if x = y then ... else ...

      let g x = f x x
  *)
  let toplevel = E.at_toplevel env in
  let branch_depth = E.branch_depth env in
  let known_to_have_no_benefit =
    if function_decl.stub || only_use_of_function || always_inline
         || (toplevel && branch_depth = 0) then
      false
    else if A.all_not_useful (E.find_list_exn env args) then
      match size_from_approximation with
      | Some body_size ->
        let wsb =
          let benefit = Inlining_cost.Benefit.zero in
          let benefit = Inlining_cost.Benefit.remove_call benefit in
          let benefit =
            Variable.Set.fold (fun v acc ->
                try
                  let t =
                    Var_within_closure.Map.find (Var_within_closure.wrap v)
                      value_set_of_closures.A.bound_vars
                  in
                  match t.A.var with
                  | Some v ->
                    if (E.mem env v) then Inlining_cost.Benefit.remove_prim acc
                    else acc
                  | None -> acc
                with Not_found -> acc)
              function_decl.free_variables benefit
          in
          W.create_estimate
            ~original_size:Inlining_cost.direct_call_size
            ~new_size:body_size
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_decl.Flambda.is_a_functor
            ~round:(E.round env)
            ~benefit
        in
        if (not (W.evaluate wsb)) then begin
          made_decision (Tried (Copying_body (Evaluated wsb)));
          true
        end else false
      | None ->
        (* The function is definitely too large to inline given that we don't
           have any approximations for its arguments.  Further, the body
           should already have been simplified (inside its declaration), so
           we also expect no gain from the code below that permits inlining
           inside the body. *)
        made_decision (Tried (Copying_body Evaluated_unspecialized));
        true
    else begin
      (* There are useful approximations, so we should simplify. *)
      false
    end
  in
  if known_to_have_no_benefit then begin
    None
  end else begin
    let body, r_inlined =
      (* First we construct the code that would result from copying the body of
         the function, without doing any further inlining upon it, to the call
         site. *)
      let r =
        R.set_inlining_threshold (R.reset_benefit r) (Some T.Never_inline)
      in
      Inlining_transforms.inline_by_copying_function_body ~env ~r
        ~function_decls ~lhs_of_application ~closure_id_being_applied
        ~inline_requested ~function_decl ~args ~simplify
    in
    let num_direct_applications_seen =
      (R.num_direct_applications r_inlined) - (R.num_direct_applications r)
    in
    assert (num_direct_applications_seen >= 0);
    let keep_inlined_version =
      if function_decl.stub then begin
        made_decision (Inlined (Copying_body Stub));
        true
      end else if always_inline then begin
        made_decision (Inlined (Copying_body Unconditionally));
        true
      end else if only_use_of_function then begin
        made_decision (Inlined (Copying_body Decl_local_to_application));
        true
      end else begin
        let sufficient_benefit =
          W.create ~original body
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:function_decl.Flambda.is_a_functor
            ~round:(E.round env)
            ~benefit:(R.benefit r_inlined)
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
        (* The meaning of requesting inlining is that the user ensure
           that the function has a benefit of at least its size. It is not
           added to the benefit exposed by the inlining because the user should
           have taken that into account before annotating the function. *)
        let function_benefit =
          if always_inline then
            Inlining_cost.Benefit.max ~round:(E.round env)
              Inlining_cost.Benefit.(requested_inline ~size_of:body zero)
              (R.benefit r_inlined)
          else
            R.benefit r_inlined
        in
        R.map_benefit r (Inlining_cost.Benefit.(+) function_benefit)
      in
      (* [lift_lets_expr] aims to clean up bindings introduced by the
         inlining. *)
      let body = Lift_code.lift_lets_expr body ~toplevel:true in
      let env =
        E.note_entering_closure env ~closure_id:closure_id_being_applied
          ~where:Inline_by_copying_function_body
      in
      let env =
        if function_decl.stub ||
           (* Stub functions should not prevent other functions
              from being evaluated for inlining *)
           E.inlining_level env = 0
           (* If the function was considered for inlining without considering
              its sub-functions, and it is not below another inlining choice,
              then we are certain that this code will be kept. *)
        then env
        else E.inlining_level_up env
      in
      Some (simplify env r body)
    end else if num_direct_applications_seen < 1 then begin
      (* Inlining the body of the function did not appear sufficiently
         beneficial; however, it may become so if we inline within the body
         first.  We try that next, unless it is known that there are were
         no direct applications in the simplified body computed above, meaning
         no opportunities for inlining. *)
        None
    end else begin
      let body, r_inlined =
        Inlining_transforms.inline_by_copying_function_body ~env
          ~r:(R.reset_benefit r)
          ~function_decls ~lhs_of_application ~closure_id_being_applied
          ~inline_requested ~function_decl ~args ~simplify
      in
      let wsb =
        W.create ~original body
          ~toplevel:(E.at_toplevel env)
          ~branch_depth:(E.branch_depth env)
          ~lifting:function_decl.Flambda.is_a_functor
          ~round:(E.round env)
          ~benefit:(R.benefit r_inlined)
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
        Some (body, R.map_benefit r_inlined
          (Inlining_cost.Benefit.(+) (R.benefit r)))
      end
      else begin
        (* r_inlined contains an approximation that may be invalid for the
           untransformed expression: it may reference functions that only
           exists if the body of the function is in fact inlined.
           If the function approximation contained an approximation that
           does not depend on the actual values of its arguments, it
           could be returned instead of [A.value_unknown]. *)
        None
      end
    end
  end

let unroll_recursive env r ~max_level ~lhs_of_application
      ~(function_decls : Flambda.function_declarations)
      ~closure_id_being_applied ~function_decl ~args ~simplify
      ~original
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
            ~inline_requested:Default_inline
            ~closure_id_being_applied ~function_decl ~args ~simplify
        in
        tried_unrolling := true;
        let wsb =
          W.create body ~original
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:false
            ~round:(E.round env)
            ~benefit:(R.benefit r_inlined)
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
          let r =
            R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
          in
          Some (body, r)
        else None
      end
    else None
  in
  !tried_unrolling, result

let should_duplicate_recursive_function env
      ~(function_decl : Flambda.function_declaration)
      ~(function_decls : Flambda.function_declarations)
      ~(value_set_of_closures : A.value_set_of_closures)
      ~args_approxs =
  assert (List.length function_decl.params = List.length args_approxs);
  !Clflags.inline_recursive_functions
    && (not (E.inside_set_of_closures_declaration
      function_decls.set_of_closures_id env))
    && (not (Variable.Map.is_empty
      (Lazy.force value_set_of_closures.invariant_params)))
    && Var_within_closure.Map.is_empty
      value_set_of_closures.bound_vars (* closed *)
    && List.exists2 (fun id approx ->
        A.useful approx
          && Variable.Map.mem id
            (Lazy.force value_set_of_closures.invariant_params))
      function_decl.params args_approxs

let inline_recursive env r ~max_level ~lhs_of_application
      ~(function_decls : Flambda.function_declarations)
      ~closure_id_being_applied ~function_decl
      ~(value_set_of_closures : Simple_value_approx.value_set_of_closures)
      ~args ~args_approxs ~dbg ~simplify ~original
      ~(made_decision : Inlining_stats_types.Decision.t -> unit) =
  let tried_unrolling, unrolling_result =
    (* First try unrolling the recursive call, if we're allowed to. *)
    unroll_recursive env r ~max_level ~lhs_of_application ~function_decls
      ~closure_id_being_applied ~function_decl ~args ~simplify
      ~original ~made_decision
  in
  match unrolling_result with
  | Some _ -> unrolling_result
  | None ->
    (* If unrolling failed, consider duplicating the whole function
       declaration at the call site, specialising parameters whose arguments
       we know. *)
    if should_duplicate_recursive_function env ~function_decls
        ~function_decl ~value_set_of_closures ~args_approxs
    then
      let copied_function_declaration =
        Inlining_transforms.inline_by_copying_function_declaration ~env
          ~r:(R.reset_benefit r) ~lhs_of_application
          ~function_decls ~closure_id_being_applied ~function_decl
          ~args ~args_approxs
          ~invariant_params:value_set_of_closures.invariant_params
          ~specialised_args:value_set_of_closures.specialised_args ~dbg
          ~simplify
      in
      match copied_function_declaration with
      | Some (expr, r_inlined) ->
        let wsb =
          W.create ~original expr
            ~toplevel:(E.at_toplevel env)
            ~branch_depth:(E.branch_depth env)
            ~lifting:false
            ~round:(E.round env)
            ~benefit:(R.benefit r_inlined)
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
          let r =
            R.map_benefit r_inlined (Inlining_cost.Benefit.(+) (R.benefit r))
          in
          Some (expr, r)
        else
          None
      | None -> None
    else begin
      (* CR lwhite: should include details of why it was not attempted
         in the reason. *)
      made_decision
        (Did_not_try_copying_decl (Tried_unrolling tried_unrolling));
      None
    end

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
  let original =
    Flambda.Apply {
      func = lhs_of_application;
      args;
      kind = Direct closure_id_being_applied;
      dbg;
      inline = inline_requested;
    }
  in
  let original_r =
    R.set_approx (R.seen_direct_application r) (A.value_unknown Other)
  in
  let max_level =
    Clflags.Int_arg_helper.get ~key:(E.round env) !Clflags.max_inlining_depth
  in
  let inline_annotation =
    (* Merge call site annotation and function annotation.
       The call site annotation takes precedence *)
    match (inline_requested : Lambda.inline_attribute) with
    | Default_inline -> function_decl.inline
    | Always_inline | Never_inline -> inline_requested
  in
  let always_inline =
    match (inline_annotation : Lambda.inline_attribute) with
    | Always_inline -> true
    (* CR-someday mshinwell: consider whether there could be better
       behaviour for stubs *)
    | Never_inline | Default_inline -> false
  in
  let is_a_stub = function_decl.stub in
  let num_params = List.length function_decl.params in
  let only_use_of_function = false in
  let raw_inlining_threshold = R.inlining_threshold r in
  let max_inlining_threshold =
    if E.at_toplevel env then
      Inline_and_simplify_aux.initial_inlining_toplevel_threshold
        ~round:(E.round env)
    else
      Inline_and_simplify_aux.initial_inlining_threshold ~round:(E.round env)
  in
  let unthrottled_inlining_threshold =
    match raw_inlining_threshold with
    | None -> max_inlining_threshold
    | Some inlining_threshold -> inlining_threshold
  in
  let inlining_threshold =
    T.min unthrottled_inlining_threshold max_inlining_threshold
  in
  let inlining_threshold_diff =
    T.sub unthrottled_inlining_threshold inlining_threshold
  in
  let fun_var =
    U.find_declaration_variable closure_id_being_applied function_decls
  in
  let recursive_functions =
    lazy
      (Find_recursive_functions.in_function_declarations function_decls
         ~backend:(E.backend env))
  in
  let recursive =
    lazy (Variable.Set.mem fun_var (Lazy.force recursive_functions))
  in
  let fun_cost : Inlining_cost.Threshold.t =
    match (inline_annotation : Lambda.inline_attribute) with
    | Never_inline -> Never_inline
    | Always_inline | Default_inline ->
      if always_inline
        || is_a_stub
        || (only_use_of_function && not (Lazy.force recursive))
      then
        inlining_threshold
      else begin
        Inlining_cost.can_try_inlining function_decl.body inlining_threshold
          ~number_of_arguments:num_params
          (* CR mshinwell: for the moment, this is None, since the
             Inlining_cost code isn't checking sizes up to the max inlining
             threshold---this seems to take too long. *)
          ~size_from_approximation:None
      end
  in
  let simpl =
    if E.never_inline env then
      (* This case only occurs when examining the body of a stub function
         but not in the context of inlining said function.  As such, there
         is nothing to do here (and no decision to report). *)
      None
    else if fun_cost = T.Never_inline && not function_decl.stub then
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
      None
    else
      let remaining_inlining_threshold = fun_cost in
      let r = R.set_inlining_threshold r (Some remaining_inlining_threshold) in
      (* Try inlining if the function is non-recursive and not too far above
         the threshold (or if the function is to be unconditionally
         inlined). *)
      (* CR mshinwell for pchambart: I don't understand why this was applying
         inline_non_recursive to recursive functions. *)
      if is_a_stub
        || (E.inlining_level env < max_level
            (* The classic heuristic completely disables inlining if the
               function is not annotated as to be inlined. *)
            && (always_inline || not !Clflags.classic_inlining)
            && not (Lazy.force recursive))
      then
        let size_from_approximation =
          match
            Variable.Map.find fun_var (Lazy.force value_set_of_closures.size)
          with
          | size -> size
          | exception Not_found ->
            Misc.fatal_errorf "Approximation does not give a size for the \
                function having fun_var %a.  value_set_of_closures: %a"
              Variable.print fun_var
              A.print_value_set_of_closures value_set_of_closures
        in
        inline_non_recursive env r ~function_decls ~lhs_of_application
          ~closure_id_being_applied ~function_decl ~value_set_of_closures
          ~made_decision ~only_use_of_function ~original
          ~inline_requested ~always_inline ~args ~size_from_approximation
          ~simplify
      else if E.inlining_level env >= max_level then begin
        made_decision (Can_inline_but_tried_nothing (Level_exceeded true));
        None
      end else if not !Clflags.classic_inlining && Lazy.force recursive then
        inline_recursive env r ~max_level ~lhs_of_application ~function_decls
          ~closure_id_being_applied ~function_decl ~value_set_of_closures
          ~args ~args_approxs ~dbg ~simplify ~original ~made_decision
      else begin
        made_decision (Can_inline_but_tried_nothing (Level_exceeded false));
        None
      end
  in
  match simpl with
  | None -> original, original_r
  | Some (expr, r) ->
    if E.inlining_level env = 0
    then expr, R.set_inlining_threshold r raw_inlining_threshold
    else expr, R.add_inlining_threshold r inlining_threshold_diff


(* We do not inline inside stubs, which are always inlined at their call site.
   Inlining inside the declaration of a stub could result in more code than
   expected being inlined. *)
(* CR mshinwell for pchambart: maybe we need an example here *)
let should_inline_inside_declaration (decl : Flambda.function_declaration) =
  not decl.stub
