open Abstract_identifiers

module A = Flambdaapprox
module E = Flambda_inline_env
module R = Flambda_inline_result
module U = Flambdautils

let is_probably_a_functor env clos approxs =
  E.at_toplevel env &&
    List.for_all A.known approxs &&
    Variable.Set.is_empty (U.recursive_functions clos)

let should_inline_function_known_to_be_recursive
      ~(func : 'a Flambda.function_declaration)
      ~(clos : 'a Flambda.function_declarations)
      ~env ~(closure : A.value_set_of_closures) ~approxs ~unchanging_params
      ~max_level =
  assert (List.length func.params = List.length approxs);
  (not (E.inside_set_of_closures_declaration clos.ident env))
    && (not (Variable.Set.is_empty closure.unchanging_params))
    && Var_within_closure.Map.is_empty closure.bound_var (* closed *)
    && E.inlining_level env <= max_level
    && List.exists2 (fun id approx ->
          A.useful approx && Variable.Set.mem id unchanging_params)
        func.params approxs

let inlining_decision_for_call_site ~env ~r ~clos ~funct ~fun_id
      ~(func : 'a Flambda.function_declaration)
      ~(closure : Flambdaapprox.value_set_of_closures)
      ~args_with_approxs ~ap_dbg ~eid
      ~inline_by_copying_function_body
      ~inline_by_copying_function_declaration =
  let args, approxs = args_with_approxs in
  let no_transformation () : _ Flambda.t * R.t =
    Fapply ({ap_function = funct; ap_arg = args;
             ap_kind = Direct fun_id; ap_dbg}, eid),
    R.set_approx r A.value_unknown
  in
  let max_level = 3 in
  (* If [unconditionally_inline] is [true], then the function will always be
     inlined, and the strategy used will be that for non-recursive functions.

     The cases where this happens are:
     1. Stub functions for handling tuplified functions (generated during closure
        conversion).
     2. Stub functions for handling default optional arguments (generated in
        bytecomp/simplify.ml).
     3. Functor-like functions, viz. [is_probably_a_functor].

     In the third case, we know from the definition of the [is_probably_a_functor]
     predicate that the function is non-recursive.  In the other two cases, the
     functions may actually be recursive, but not "directly recursive" (where we
     say a function [f] is "directly recursive" if [f] is free in the body of
     [f]).  It would in general be wrong to mark directly recursive functions as
     stubs, even if specific cases work correctly.
  *)
  (* CR mshinwell for mshinwell: finish the comment *)
  let unconditionally_inline = func.stub || is_probably_a_functor env clos approxs in
  let num_params = List.length func.params in
  (* CR pchambart to pchambart: find a better name
     This is true if the function is directly an argument of the
     apply construction. *)
  let direct_apply = match funct with
    | Fclosure ({ fu_closure = Fset_of_closures _ }, _) -> true
    | _ -> false in
  let inline_threshold = R.inline_threshold r in
  let fun_cost =
    if unconditionally_inline || direct_apply then
      (* XCR mshinwell for mshinwell: this comment needs clarification *)
      (* A function is considered for inlining if it does not increase the code
         size too much. This size is verified after effectively duplicating
         and specialising the code in the current context. In that context,
         some local calls can have new opportunity for inlining, for instance.
         [let f g x = g x + 1
          let h x = ...
          let v = f h 1]
         When inlining [f], [g] becomes known and so [h] can be inlined too.
         Inlining only [f] will usualy fit the size constraint and will be
         beneficial. But depending on [h] it can or cannot be beneficial to
         inline it: If [h] is too big, it may be possible to inline it in [f],
         but that may prevent [f] from being inlinable after verification.
         To prevent that, the maximal size increase allowed to [h] is reduced
         by what is consumed by [f].
         In the case of stub functions, we know that the function is small
         enouth and has a high probability of reducing the size of the
         code around it, hence we know that trying to inline it won't prevent
         the surrounding function from being inlined.

         CR pchambart: The case of functors should not be always treated as
           stub functions. It won't often decrease the function size hence
           will probably prevent a function from being inlined, loosing the
           benefit of the potential inlining.
           It may be reasonnable to consider that reavealing an opportunity
           for inlining a functor as sufficient for forced inlining.
         CR pchambart: The heuristic is half broken as the potential local
           inlines are not accumulated. For instance, in the previous example
           if f was [let f g x = g (g x)], if g was just bellow the quota,
           it could considered the two times.
           To correct that, the threshold should be propagated through [r]
           rather than [env]
      *)
      inline_threshold
    else
      Flambdacost.can_try_inlining func.body inline_threshold
        ~bonus:num_params
  in
  let expr, r =
  match fun_cost with
  | Flambdacost.Never_inline -> no_transformation ()
  | Flambdacost.Can_inline _ when E.never_inline env ->
    no_transformation ()
  | (Flambdacost.Can_inline _) as remaining_inline_threshold ->
      let fun_var = U.find_declaration_variable fun_id clos in
      let recursive = Variable.Set.mem fun_var (U.recursive_functions clos) in
      (* CR mshinwell for mshinwell: add comment about stub functions *)
      (* CR mshinwell for pchambart: two variables called [threshold] and
         [inline_threshold] is confusing.
         pchambart: is [remaining_inline_threshold] better ? *)
      let r = R.set_inline_threshold r remaining_inline_threshold in
      let unchanging_params = closure.unchanging_params in
      (* Try inlining if the function is non-recursive and not too far above
         the threshold (or if the function is to be unconditionally inlined). *)
      if unconditionally_inline
        || (not recursive && E.inlining_level env <= max_level)
      then
        let body, r_inlined =
          inline_by_copying_function_body ~env ~r:(R.clear_benefit r) ~clos
            ~lfunc:funct ~fun_id ~func ~args
        in
        (* Now we know how large the inlined version actually is, determine
           whether or not to keep it. *)
        if unconditionally_inline
          || direct_apply
          || Flambdacost.sufficient_benefit_for_inline
               body
               (R.benefit r_inlined)
               inline_threshold
        then
          body, R.map_benefit r_inlined
            (Flambdacost.benefit_union (R.benefit r))
        else
          (* r_inlined contains an approximation that may be invalid for the
             untransformed expression: it may reference functions that only
             exists if the body of the function is effectively inlined.
             If the function approximation contained an approximation that
             does not depends on the effective value of its arguments, it
             could be returned instead of [A.value_unknown] *)
          no_transformation ()
      else if recursive
      then
        let unrolling_result =
          if E.unrolling_allowed env
              && E.inlining_level env <= max_level
          then
            let env = E.inside_unrolled_function env in
            let body, r_inlined =
              inline_by_copying_function_body ~env ~r:(R.clear_benefit r)
                ~clos ~lfunc:funct ~fun_id ~func ~args
            in
            if Flambdacost.sufficient_benefit_for_inline
                body
                (R.benefit r_inlined)
                inline_threshold
            then
              Some (body,
                R.map_benefit r_inlined
                  (Flambdacost.benefit_union (R.benefit r)))
            else None
          else None
        in
        match unrolling_result with
        | Some r -> r
        | None ->
          if should_inline_function_known_to_be_recursive ~func ~clos ~env
              ~closure ~approxs ~unchanging_params ~max_level
          then
            let () =
              if Variable.Map.cardinal clos.funs > 1
              then Format.printf "try inline multi rec %a@."
                  Closure_id.print fun_id
            in
            inline_by_copying_function_declaration ~env ~r ~funct ~clos
              ~fun_id ~func ~args_with_approxs:(args, approxs)
              ~unchanging_params ~specialised_args:closure.specialised_args
              ~ap_dbg ~no_transformation
          else
            no_transformation ()
      else
        no_transformation ()
  in
  if E.inlining_level env = 0
  then expr, R.set_inline_threshold r inline_threshold
  else expr, r
