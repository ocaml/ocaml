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

module Simplify_sequential_logical_operator (G : sig
  val canonical_absorbing_element : int
  val is_absorbing_element : int -> bool
  val primitive : Lambda.primitive
end) = struct
  module A = Simple_value_approx
  module C = Inlining_cost

  let sequential_op ~arg1 ~(arg1_approx : A.t) ~arg2 ~(arg2_approx : A.t)
        ~dbg ~annot : _ Flambda.t * A.t * C.Benefit.t =
    let arg1_no_effects = Effect_analysis.no_effects arg1 in
    let arg2_no_effects = Effect_analysis.no_effects arg2 in
    let arg2_annot = Flambdautils.data_at_toplevel_node arg2 in
    let module B = C.Benefit in
    let completely_eliminated () : _ Flambda.t * A.t * B.t =
      Fconst (Fconst_pointer G.canonical_absorbing_element, annot),
        A.value_constptr G.canonical_absorbing_element,
        B.remove_branch (B.remove_code arg1 (
          B.remove_code arg2 B.zero))
    in
    match arg1_approx.descr with
    | (Value_int n | Value_constptr n) when G.is_absorbing_element n ->
      if arg1_no_effects then
        completely_eliminated ()
      else
        arg1, arg1_approx, B.remove_branch (B.remove_code arg2 B.zero)
    | (Value_int _ | Value_constptr _) -> (* when not the absorbing element *)
      if arg1_no_effects then
        arg2, arg2_approx, B.remove_branch (B.remove_code arg1 B.zero)
      else
        begin match arg2_approx.descr with
        | (Value_int arg2_val | Value_constptr arg2_val)
            when arg2_no_effects ->
          Fsequence (arg1, Fconst (Fconst_pointer arg2_val, arg2_annot),
              annot), arg2_approx,
            B.remove_branch (B.remove_code arg2 B.zero)
        | _ ->
          Fsequence (arg1, arg2, annot), arg2_approx,
            B.remove_branch B.zero
        end
    | _ ->
      match arg2_approx.descr with
      | (Value_int n | Value_constptr n)
          when G.is_absorbing_element n ->
        begin match arg1_no_effects, arg2_no_effects with
        | true, true -> completely_eliminated ()
        | true, false (* we must run [arg1]: it might short-circuit [arg2] *)
        | false, false ->
          Fprim (G.primitive, [arg1; arg2], dbg, annot),
            A.value_constptr G.canonical_absorbing_element,
              B.zero
        | false, true ->
          Fsequence (arg1,
              Fconst (Fconst_pointer G.canonical_absorbing_element,
                arg2_annot), annot),
            A.value_constptr G.canonical_absorbing_element,
              B.remove_branch (B.remove_code arg2 B.zero)
        end
      | _ ->
        Fprim (G.primitive, [arg1; arg2], dbg, annot),
          A.value_unknown, B.zero
end

module Simplify_and = Simplify_sequential_logical_operator (struct
  let canonical_absorbing_element = 0
  let is_absorbing_element n = (n = 0)
  let primitive = Lambda.Psequand
end)
let sequential_and = Simplify_and.sequential_op

module Simplify_or = Simplify_sequential_logical_operator (struct
  let canonical_absorbing_element = 1
  let is_absorbing_element n = (n <> 0)
  let primitive = Lambda.Psequor
end)
let sequential_or = Simplify_or.sequential_op
