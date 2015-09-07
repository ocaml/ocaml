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

let no_effects_prim (prim : Lambda.primitive) =
  match
    Semantics_of_primitives.for_primitive prim
      ~second_arg_is_definitely_not_zero:false
  with
  | (No_effects | Only_generative_effects), (No_coeffects | Has_coeffects) ->
    true
  | _ -> false

let rec no_effects (flam : Flambda.t) =
  match flam with
  | Var _ -> true
  | Let (_, def, body) -> no_effects_named def && no_effects body
  | Let_mutable (_, _, body) -> no_effects body
  | Let_rec (defs, body) ->
    no_effects body
      && List.for_all (fun (_, def) -> no_effects_named def) defs
  | If_then_else (_, ifso, ifnot) -> no_effects ifso && no_effects ifnot
  | Switch (_, sw) ->
    let aux (_, flam) = no_effects flam in
    List.for_all aux sw.blocks
      && List.for_all aux sw.consts
      && Misc.may_default no_effects sw.failaction true
  | String_switch (_, sw, def) ->
    List.for_all (fun (_, lam) -> no_effects lam) sw
      && Misc.may_default no_effects def true
  | Static_catch (_, _, body, _) | Try_with (body, _, _) ->
    (* If there is a [raise] in [body], the whole [Try_with] may have an
       effect, so there is no need to test the handler. *)
    no_effects body
  | While _ | For _ | Apply _ | Send _ | Assign _ | Static_raise _ -> false
  | Proved_unreachable -> true

and no_effects_named (named : Flambda.named) =
  match named with
  | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
  | Read_symbol_field _
  | Set_of_closures _ | Project_closure _ | Project_var _
  | Move_within_set_of_closures _ -> true
  | Prim (prim, _, _) -> no_effects_prim prim
  | Expr flam -> no_effects flam
