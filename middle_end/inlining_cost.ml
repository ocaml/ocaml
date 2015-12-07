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

(* Simple approximation of the space cost of a primitive. *)

let prim_size (prim : Lambda.primitive) args =
  match prim with
  | Pidentity -> 0
  | Pgetglobal _ -> 1
  | Psetglobal _ -> 1
  | Pmakeblock _ -> 5 + List.length args
  | Pfield _ -> 1
  | Psetfield (_, isptr, init) ->
    begin match init with
    | Initialization -> 1  (* never causes a write barrier hit *)
    | Assignment ->
      match isptr with
      | Pointer -> 4
      | Immediate -> 1
    end
  | Pfloatfield _ -> 1
  | Psetfloatfield _ -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.Primitive.prim_alloc then 10 else 4) + List.length args
  | Praise _ -> 4
  | Pstringlength -> 5
  | Pstringrefs | Pstringsets -> 6
  | Pmakearray _ -> 5 + List.length args
  | Parraylength Pgenarray -> 6
  | Parraylength _ -> 2
  | Parrayrefu Pgenarray -> 12
  | Parrayrefu _ -> 2
  | Parraysetu Pgenarray -> 16
  | Parraysetu _ -> 4
  | Parrayrefs Pgenarray -> 18
  | Parrayrefs _ -> 8
  | Parraysets Pgenarray -> 22
  | Parraysets _ -> 10
  | Pbittest -> 3
  | Pbigarrayref (_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset (_, ndims, _, _) -> 4 + ndims * 6
  | Psequand | Psequor ->
    Misc.fatal_error "Psequand and Psequor are not allowed in Prim \
        expressions; translate out instead (cf. closure_conversion.ml)"
  (* CR mshinwell: This match must be made exhaustive. *)
  | _ -> 2 (* arithmetic and comparisons *)

(* Simple approximation of the space cost of an Flambda expression. *)

let direct_call_size = 4

let lambda_smaller' lam ~than:threshold =
  let size = ref 0 in
  let rec lambda_size (lam : Flambda.t) =
    if !size > threshold then raise Exit;
    match lam with
    | Var _ -> ()
    | Apply ({ func = _; args = _; kind = direct }) ->
      let call_cost =
        match direct with Indirect -> 6 | Direct _ -> direct_call_size
      in
      size := !size + call_cost
    | Assign _ -> incr size
    | Send _ -> size := !size + 8
    | Proved_unreachable -> ()
    | Let { defining_expr; body; _ } ->
      lambda_named_size defining_expr;
      lambda_size body
    | Let_mutable (_, _, body) -> lambda_size body
    | Let_rec (bindings, body) ->
      List.iter (fun (_, lam) -> lambda_named_size lam) bindings;
      lambda_size body
    | Switch (_, sw) ->
      let aux = function _::_::_ -> size := !size + 5 | _ -> () in
      aux sw.consts; aux sw.blocks;
      List.iter (fun (_, lam) -> lambda_size lam) sw.consts;
      List.iter (fun (_, lam) -> lambda_size lam) sw.blocks
    | String_switch (_, sw, def) ->
      List.iter (fun (_, lam) ->
          size := !size + 2;
          lambda_size lam)
        sw;
      Misc.may lambda_size def
    | Static_raise _ -> ()
    | Static_catch (_, _, body, handler) ->
      incr size; lambda_size body; lambda_size handler
    | Try_with (body, _, handler) ->
      size := !size + 8; lambda_size body; lambda_size handler
    | If_then_else (_, ifso, ifnot) ->
      size := !size + 2;
      lambda_size ifso; lambda_size ifnot
    | While (cond, body) ->
      size := !size + 2; lambda_size cond; lambda_size body
    | For { body; _ } ->
      size := !size + 4; lambda_size body
  and lambda_named_size (named : Flambda.named) =
    if !size > threshold then raise Exit;
    match named with
    | Symbol _ | Read_mutable _ -> ()
    (* CR mshinwell: are these cases correct? *)
    | Const _ | Allocated_const _ -> incr size
    | Read_symbol_field _ -> incr size
    | Set_of_closures ({ function_decls = ffuns }) ->
      Variable.Map.iter (fun _ (ffun : Flambda.function_declaration) ->
          lambda_size ffun.body)
        ffuns.funs
    | Project_closure _ | Project_var _ | Move_within_set_of_closures _ ->
      incr size
    | Prim (prim, args, _) ->
      size := !size + prim_size prim args
    | Expr expr -> lambda_size expr
  in
  try
    lambda_size lam;
    if !size <= threshold then Some !size
    else None
  with Exit ->
    None

let lambda_size lam =
  match lambda_smaller' lam ~than:max_int with
  | Some size ->
      size
  | None ->
      (* There is no way that an expression of size max_int could fit in
         memory. *)
      assert false

type inlining_threshold =
  | Never_inline
  | Can_inline_if_no_larger_than of int

let can_try_inlining lam inlining_threshold ~number_of_arguments
      ~size_from_approximation =
  match inlining_threshold with
  | Never_inline -> Never_inline
  | Can_inline_if_no_larger_than inlining_threshold ->
    let bonus =
      (* removing a call will reduce the size by at least the number
         of arguments *)
      number_of_arguments
    in
    let size =
      let than = inlining_threshold + bonus in
      match size_from_approximation with
      | Some size -> if size <= than then Some size else None
      | None -> lambda_smaller' lam ~than
    in
    match size with
    | None -> Never_inline
    | Some size -> Can_inline_if_no_larger_than (inlining_threshold - size + bonus)

let lambda_smaller lam ~than =
  lambda_smaller' lam ~than <> None

let can_inline lam inlining_threshold ~bonus =
  match inlining_threshold with
  | Never_inline -> false
  | Can_inline_if_no_larger_than inlining_threshold ->
     lambda_smaller
       lam
       ~than:(inlining_threshold + bonus)

let cost (flag : Clflags.Int_arg_helper.parsed) ~default ~round =
  match flag with
  | Always cost -> cost
  | Variable by_round ->
    match Ext_types.Int.Map.find round by_round with
    | cost -> cost
    | exception Not_found -> default

let benefit_factor = 1

module Benefit = struct
  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    (* CR-someday pchambart: branch_benefit : t list; *)
    direct_call_of_indirect : int;
    requested_inline : int;
    (* Benefit to compensate the size of functions marked for inlining *)
  }

  let zero = {
    remove_call = 0;
    remove_alloc = 0;
    remove_prim = 0;
    remove_branch = 0;
    direct_call_of_indirect = 0;
    requested_inline = 0;
  }

  let remove_call t = { t with remove_call = t.remove_call + 1; }
  let remove_alloc t = { t with remove_alloc = t.remove_alloc + 1; }
  let remove_prim t = { t with remove_prim = t.remove_prim + 1; }
  let remove_branch t = { t with remove_branch = t.remove_branch + 1; }
  let direct_call_of_indirect t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }
  let requested_inline t ~size_of =
    let size = lambda_size size_of in
    { t with requested_inline = t.requested_inline + size; }

  let remove_code_helper b (flam : Flambda.t) =
    match flam with
    | Assign _ -> b := remove_prim !b
    | Switch _ | String_switch _ | Static_raise _ | Try_with _
    | If_then_else _ | While _ | For _ -> b := remove_branch !b
    | Apply _ | Send _ -> b := remove_call !b
    | Let _ | Let_mutable _ | Let_rec _ | Proved_unreachable | Var _
    | Static_catch _ -> ()

  let remove_code_helper_named b (named : Flambda.named) =
    match named with
    | Set_of_closures _
    | Prim ((Pmakearray _ | Pmakeblock _ | Pduprecord _), _, _) ->
      b := remove_alloc !b
      (* CR pchambart: should we consider that boxed integer and float
         operations are allocations ? *)
      (* CR mshinwell for pchambart: check closure & const cases carefully *)
    | Prim _ | Project_closure _ | Project_var _
    | Move_within_set_of_closures _ -> b := remove_prim !b
    | Read_symbol_field _ -> () (* CR mshinwell: might be wrong *)
    | Symbol _ | Read_mutable _ | Allocated_const _ | Const _ | Expr _ -> ()

  let remove_code lam b =
    let b = ref b in
    Flambda_iterators.iter_toplevel (remove_code_helper b)
      (remove_code_helper_named b) lam;
    !b

  let remove_code_named lam b =
    let b = ref b in
    Flambda_iterators.iter_named_toplevel (remove_code_helper b)
      (remove_code_helper_named b) lam;
    !b

  let print ppf b =
    Format.fprintf ppf "@[remove_call: %i@ remove_alloc: %i@ \
                        remove_prim: %i@ remove_branc: %i@ \
                        direct: %i@ requested: %i@]"
      b.remove_call
      b.remove_alloc
      b.remove_prim
      b.remove_branch
      b.direct_call_of_indirect
      b.requested_inline

  let evaluate t ~round : int =
    benefit_factor *
      (t.remove_call * (cost !Clflags.inline_call_cost
          ~default:Clflags.default_inline_call_cost ~round)
       + t.remove_alloc * (cost !Clflags.inline_alloc_cost
          ~default:Clflags.default_inline_alloc_cost ~round)
       + t.remove_prim * (cost !Clflags.inline_prim_cost
          ~default:Clflags.default_inline_prim_cost ~round)
       + t.remove_branch * (cost !Clflags.inline_branch_cost
          ~default:Clflags.default_inline_branch_cost ~round)
       + t.direct_call_of_indirect * (cost !Clflags.inline_indirect_cost
          ~default:Clflags.default_inline_indirect_cost ~round))
    + t.requested_inline

  let (+) t1 t2 = {
    remove_call = t1.remove_call + t2.remove_call;
    remove_alloc = t1.remove_alloc + t2.remove_alloc;
    remove_prim = t1.remove_prim + t2.remove_prim;
    remove_branch = t1.remove_branch + t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect + t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline + t2.requested_inline;
  }

  let max ~round t1 t2 =
    let c1 = evaluate ~round t1 in
    let c2 = evaluate ~round t2 in
    if c1 > c2 then t1 else t2

end

module Whether_sufficient_benefit = struct
  type t = {
    round : int;
    benefit : Benefit.t;
    branch_depth : int;
    probably_a_functor : bool;
    original_size : int;
    new_size : int;
    evaluated_benefit : int;
  }

  let create ~original ~branch_depth lam benefit ~probably_a_functor ~round =
    let evaluated_benefit = Benefit.evaluate benefit ~round in
    { round; benefit; branch_depth; probably_a_functor;
      original_size = lambda_size original;
      new_size = lambda_size lam;
      evaluated_benefit;
    }

  let create_given_sizes ~original_size ~branch_depth ~new_size ~benefit
        ~probably_a_functor ~round =
    let evaluated_benefit = Benefit.evaluate benefit ~round in
    { round; benefit; branch_depth; probably_a_functor; original_size;
      new_size; evaluated_benefit;
    }

  let correct_branch_factor f =
    f = f (* is not nan *)
    && f >= 0.

  let evaluate t =
    if t.probably_a_functor then
      true
    else
      (* The estimated benefit is the evaluated benefit times an
         estimation of the probability that the branch does not matter
         for performances (is cold). The probability is very roughtly
         estimated by considering that for every branching the
         sub-expressions has the same [1 / (1 + factor)] probability
         [p] of being cold. Hence the probability for the current
         call to be cold is [p ^ number of nested branch].

         The probability is expressed as [1 / (1 + factor)] rather
         than letting the user directly provide [p], since for every
         positive value of [factor] [p] is in [0, 1]. *)
      let branch_never_taken_estimated_probability =
        let branch_inline_factor =
          match !Clflags.branch_inline_factor with
          | Always branch_inline_factor -> branch_inline_factor
          | Variable by_round ->
            match Ext_types.Int.Map.find t.round by_round with
            | branch_inline_factor -> branch_inline_factor
            | exception Not_found -> Clflags.default_branch_inline_factor
        in
        (* CR pchambart to pchambart: change this assert to a warning *)
        assert(correct_branch_factor branch_inline_factor);
        1. /. (1. +. branch_inline_factor)
      in
      let call_estimated_probability =
        branch_never_taken_estimated_probability ** float t.branch_depth
      in
      let estimated_benefit =
        float t.evaluated_benefit *. call_estimated_probability
      in
      float t.new_size -. estimated_benefit <= float t.original_size


  let to_string t =
      Printf.sprintf "{benefit={call=%d,alloc=%d,prim=%i,branch=%i,indirect=%i,req=%i},\
                      orig_size=%d,new_size=%d,eval_size=%d,eval_benefit=%d,\
                      functor=%b,branch_depth=%d}=%s"
        t.benefit.remove_call
        t.benefit.remove_alloc
        t.benefit.remove_prim
        t.benefit.remove_branch
        t.benefit.direct_call_of_indirect
        t.benefit.requested_inline
        t.original_size
        t.new_size
        (t.original_size - t.new_size)
        t.evaluated_benefit
        t.probably_a_functor
        t.branch_depth
        (if evaluate t then "yes" else "no")
end

let scale_inline_threshold_by = 8

let maximum_interesting_size_of_function_body () =
  (* CR-soon mshinwell for mshinwell: hastily-written comment, to review *)
  (* We may in [Inlining_decision] need to measure the size of functions
     that are below the inlining threshold.  We also need to measure with
     regard to benefit (see [Inlining_decision.inline_non_recursive).  The
     intuition for having a cached size in the second case is as follows.
     If a function's body exceeds some maximum size and its argument
     approximations are unknown (meaning that we cannot materially simplify
     it further), we can infer without examining the function's body that
     it cannot be inlined.  The aim is to speed up [Inlining_decision].

     The "original size" is [Inlining_cost.direct_call_size].
     The "new size" is the size of the function's body; call this body_size.

     To be inlined we need:
       body_size - (evaluated_benefit * call_prob) <= direct_call_size
     i.e.:
       body_size <= direct_call_size + evaluated_benefit*call_prob
     In this case we would be removing a single call:
       evaluated_benefit = benefit_factor * inline_call_cost
     (For [inline_call_cost], we use the maximum this might be across any
     round.)
     Substituting:
       body_size <= direct_call_size
                      + (benefit_factor * inline_call_cost)*call_prob
     The upper bound for the right-hand side is when call_prob = 1.0,
     giving:
       direct_call_size + benefit_factor*inline_call_cost       (**)
     So we should measure all functions at or below this size, but also
     record the size discovered, so we can later re-check (without
     examining the body) when we know [call_prob].
  *)
  let max_cost = ref 0 in
  for round = 0 to !Clflags.simplify_rounds - 1 do
(*
    let inline_threshold =
      (cost !Clflags.inline_threshold
        ~default:Clflags.default_inline_threshold
        ~round)
    in
    let inline_threshold =
      (* Following [can_try_inlining], above. *)
      (* CR-soon mshinwell: try to factor code out *)
      (inline_threshold*scale_inline_threshold_by + rough_max_bonus)
        * can_try_inlining_magic_constant
    in
*)
    let max_size =  (* This is for the (**) case above. *)
      let inline_call_cost =
        cost !Clflags.inline_call_cost
          ~default:Clflags.default_inline_call_cost
          ~round
      in
      direct_call_size + benefit_factor*inline_call_cost
    in
    max_cost := max !max_cost max_size (* (max inline_threshold max_size) *)
  done;
  !max_cost
