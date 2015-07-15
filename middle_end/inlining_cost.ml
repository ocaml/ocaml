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
  | Psetfield (_, isptr) -> if isptr then 4 else 1
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
  | Pgetglobalfield _ -> 2
  | Psetglobalfield _ -> 2
  | Psequand | Psequor ->
    Misc.fatal_error "Psequand and Psequor are not allowed in Prim \
        expressions; translate out instead (cf. closure_conversion.ml)"
  (* CR mshinwell: This match must be made exhaustive. *)
  | _ -> 2 (* arithmetic and comparisons *)

(* Simple approximation of the space cost of an Flambda expression. *)

let lambda_smaller' lam ~than:threshold =
  let size = ref 0 in
  let rec lambda_size (lam : Flambda.t) =
    if !size > threshold then raise Exit;
    match lam with
    | Var _ -> ()
    | Apply ({ func = _; args = _; kind = direct }) ->
      let call_cost = match direct with Indirect -> 6 | Direct _ -> 4 in
      size := !size + call_cost
    | Assign _ -> incr size
    | Send _ -> size := !size + 8
    | Proved_unreachable -> ()
    | Let (_, _, lam, body) ->
      lambda_named_size lam; lambda_size body
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
    | Static_raise (_, args) -> lambda_list_size args
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
  and lambda_list_size l = List.iter lambda_size l
  and lambda_named_size (named : Flambda.named) =
    if !size > threshold then raise Exit;
    match named with
    | Symbol _ -> ()
    | Const (
        (Const_base (Const_int _ | Const_char _ | Const_float _ |
                     Const_int32 _ | Const_int64 _ | Const_nativeint _)
        | Const_pointer _ | Const_float _
        | Const_float_array _ | Const_immstring _)) -> incr size
    | Const (Const_base ( Const_string _ )) ->
      (* should be moved out by a previous pass: see [List_string] *)
      assert false
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

let lambda_smaller lam ~than =
  lambda_smaller' lam ~than <> None

type inlining_threshold =
  | Never_inline
  | Can_inline_if_no_larger_than of int

let can_try_inlining lam inlining_threshold ~bonus =
  match inlining_threshold with
  | Never_inline -> Never_inline
  | Can_inline_if_no_larger_than inlining_threshold ->
     (* CR mshinwell for pchambart: eliminate magic constant *)
     match lambda_smaller'
             lam
             ~than:((inlining_threshold + bonus) * 4)
     with
     | None -> Never_inline
     | Some size -> Can_inline_if_no_larger_than (inlining_threshold - size)

let can_inline lam inlining_threshold ~bonus =
  match inlining_threshold with
  | Never_inline -> false
  | Can_inline_if_no_larger_than inlining_threshold ->
     lambda_smaller
       lam
       ~than:(inlining_threshold + bonus)

module Benefit = struct
  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    (* CR-someday pchambart: branch_benefit : t list; *)
  }

  let zero = {
    remove_call = 0;
    remove_alloc = 0;
    remove_prim = 0;
    remove_branch = 0;
  }

  let remove_call t = { t with remove_call = t.remove_call + 1; }
  let remove_alloc t = { t with remove_alloc = t.remove_alloc + 1; }
  let remove_prim t = { t with remove_prim = t.remove_prim + 1; }
  let remove_branch t = { t with remove_branch = t.remove_branch + 1; }

  let remove_code_helper b (flam : Flambda.t) =
    match flam with
    | Assign _ -> b := remove_prim !b
    | Switch _ | String_switch _ | Static_raise _ | Try_with _
    | If_then_else _ | While _ | For _ -> b := remove_branch !b
    | Apply _ | Send _ -> b := remove_call !b
    | Let _ | Let_rec _ | Proved_unreachable | Var _ | Static_catch _ -> ()

  let remove_code_helper_named b (named : Flambda.named) =
    match named with
    | Set_of_closures _
    | Prim ((Pmakearray _ | Pmakeblock _ | Pduprecord _), _, _) ->
      b := remove_alloc !b
      (* CR pchambart: should we consider that boxed integer and float
         operations are allocations ? *)
      (* CR mshinwell for pchambart: check closure cases carefully *)
    | Prim _ | Project_closure _ | Project_var _
    | Move_within_set_of_closures _ -> b := remove_prim !b
    | Symbol _ | Const _ | Expr _ -> ()

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
                        remove_prim: %i@ remove_branc: %i@]"
      b.remove_call
      b.remove_alloc
      b.remove_prim
      b.remove_branch

  let benefit_factor = 1

  let evaluate t =
    (* CR mshinwell for pchambart: Shouldn't the "Clflags" variables be
       called "benefit", not "cost"? *)
    benefit_factor *
      (t.remove_call * !Clflags.inline_call_cost
       + t.remove_alloc * !Clflags.inline_alloc_cost
       + t.remove_prim * !Clflags.inline_prim_cost
       + t.remove_branch * !Clflags.inline_branch_cost)

  let (+) t1 t2 = {
    remove_call = t1.remove_call + t2.remove_call;
    remove_alloc = t1.remove_alloc + t2.remove_alloc;
    remove_prim = t1.remove_prim + t2.remove_prim;
    remove_branch = t1.remove_branch + t2.remove_branch;
  }
end

module Whether_sufficient_benefit = struct
  type t = {
    benefit : Benefit.t;
    probably_a_functor : bool;
    original_size : int;
    new_size : int;
    evaluated_benefit : int;
  }

  let create ~original lam benefit ~probably_a_functor =
    match
      lambda_smaller' lam ~than:max_int,
      lambda_smaller' original ~than:max_int
    with
    | Some new_size, Some original_size ->
      let evaluated_benefit = Benefit.evaluate benefit in
      { benefit; probably_a_functor; original_size;
        new_size; evaluated_benefit;
      }
    | _, _ ->
      (* There is no way that an expression of size max_int could fit in
         memory. *)
      assert false

  let evaluate t =
    if t.probably_a_functor then
      true
    else
      t.new_size - t.evaluated_benefit
      <= t.original_size

  let to_string t =
      Printf.sprintf "{benefit={call=%d,alloc=%d,prim=%i,branch=%i},\
                      orig_size=%d,new_size=%d,eval_size=%d,eval_benefit=%d,\
                      functor=%b}=%s"
        t.benefit.remove_call
        t.benefit.remove_alloc
        t.benefit.remove_prim
        t.benefit.remove_branch
        t.original_size
        t.new_size
        (t.original_size - t.new_size)
        t.evaluated_benefit
        t.probably_a_functor
        (if evaluate t then "yes" else "no")
end
