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

open Abstract_identifiers

(* Simple approximation of the space cost of a primitive. *)

let prim_size (prim : Lambda.primitive) args =
  match prim with
  | Pidentity -> 0
  | Pgetglobal id -> 1
  | Psetglobal id -> 1
  | Pmakeblock (tag, mut) -> 5 + List.length args
  | Pfield f -> 1
  | Psetfield (f, isptr) -> if isptr then 4 else 1
  | Pfloatfield f -> 1
  | Psetfloatfield f -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.Primitive.prim_alloc then 10 else 4) + List.length args
  | Praise _ -> 4
  | Pstringlength -> 5
  | Pstringrefs | Pstringsets -> 6
  | Pmakearray kind -> 5 + List.length args
  | Parraylength kind -> if kind = Pgenarray then 6 else 2
  | Parrayrefu kind -> if kind = Pgenarray then 12 else 2
  | Parraysetu kind -> if kind = Pgenarray then 16 else 4
  | Parrayrefs kind -> if kind = Pgenarray then 18 else 8
  | Parraysets kind -> if kind = Pgenarray then 22 else 10
  | Pbittest -> 3
  | Pbigarrayref (_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset (_, ndims, _, _) -> 4 + ndims * 6
  | Pgetglobalfield _ -> 2
  | Psetglobalfield _ -> 2
  | _ -> 2 (* arithmetic and comparisons *)

(* Simple approximation of the space cost of an Flambda expression. *)

let lambda_smaller' lam ~than:threshold =
  let size = ref 0 in
  let rec lambda_size (lam : _ Flambda.t) =
    if !size > threshold then raise Exit;
    match lam with
    | Fvar _ -> ()
    | Fsymbol _ -> ()
    | Fconst (
        (Fconst_base (Const_int _ | Const_char _ | Const_float _ |
                     Const_int32 _ | Const_int64 _ | Const_nativeint _)
        | Fconst_pointer _ | Fconst_float _
        | Fconst_float_array _ | Fconst_immstring _), _) -> incr size
    | Fconst (Fconst_base ( Const_string _ ), _) ->
      assert false
      (* should be moved out by a previous pass: see [List_string] *)
    | Fapply ({ ap_function = fn; ap_arg = args; ap_kind = direct }, _) ->
      let call_cost = match direct with Indirect -> 6 | Direct _ -> 4 in
      size := !size + call_cost; lambda_size fn; lambda_list_size args
    | Fset_of_closures ({ cl_fun = ffuns; cl_free_var = fv }, _) ->
      Variable.Map.iter (fun _ -> lambda_size) fv;
      Variable.Map.iter (fun _ (ffun : _ Flambda.function_declaration) ->
          lambda_size ffun.body)
        ffuns.funs
    | Fclosure ({ fu_closure = lam }, _) ->
      incr size; lambda_size lam
    | Fvar_within_closure ({ vc_closure }, _) ->
      incr size; lambda_size vc_closure
    | Flet (id, _, lam, body, _) ->
      lambda_size lam; lambda_size body
    | Fletrec (bindings, body, _) ->
      List.iter (fun (_, lam) -> lambda_size lam) bindings;
      lambda_size body
    | Fprim (prim, args, _, _) ->
      size := !size + prim_size prim args;
      lambda_list_size args
    | Fswitch (lam, sw, _) ->
      let aux = function _::_::_ -> size := !size + 5 | _ -> () in
      aux sw.fs_consts; aux sw.fs_blocks;
      lambda_size lam;
      List.iter (fun (_, lam) -> lambda_size lam) sw.fs_consts;
      List.iter (fun (_, lam) -> lambda_size lam) sw.fs_blocks
    | Fstringswitch (lam, sw, def, _) ->
      lambda_size lam;
      List.iter (fun (_, lam) ->
          size := !size + 2;
          lambda_size lam)
        sw;
      Misc.may lambda_size def
    | Fstaticraise (_, args, _) -> lambda_list_size args
    | Fstaticcatch (_, _, body, handler, _) ->
      incr size; lambda_size body; lambda_size handler
    | Ftrywith (body, id, handler, _) ->
      size := !size + 8; lambda_size body; lambda_size handler
    | Fifthenelse (cond, ifso, ifnot, _) ->
      size := !size + 2;
      lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Fsequence (lam1, lam2, _) ->
      lambda_size lam1; lambda_size lam2
    | Fwhile (cond, body, _) ->
      size := !size + 2; lambda_size cond; lambda_size body
    | Ffor (id, low, high, dir, body, _) ->
      size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Fassign (id, lam, _) ->
      incr size;  lambda_size lam
    | Fsend (_, met, obj, args, _, _) ->
      size := !size + 8;
      lambda_size met; lambda_size obj; lambda_list_size args
    | Funreachable _ -> ()
  and lambda_list_size l = List.iter lambda_size l in
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

  let remove_code lam b =
    let b = ref b in
    let f (flam : _ Flambda.t) =
      match flam with
      | Fset_of_closures _
      | Fprim ((Pmakearray _ | Pmakeblock _ | Pduprecord _), _, _, _) ->
        b := remove_alloc !b
      | Fprim _ | Fclosure _ | Fvar_within_closure _ | Fassign _ ->
        b := remove_prim !b
      | Fswitch _ | Fstringswitch _ | Fstaticraise _ | Ftrywith _
      | Fifthenelse _ | Fwhile _ | Ffor _ ->
        b := remove_branch !b
      | Fapply _ | Fsend _ ->
        b := remove_call !b
      | Flet _ | Fletrec _ | Funreachable _ | Fsequence _ | Fsymbol _
      | Fvar _ | Fconst _ | Fstaticcatch _ -> ()
    in
    Flambdaiter.iter_toplevel f lam;
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
  type maybe_inline = {
    benefit : Benefit.t;
    probably_a_functor : bool;
    inlining_threshold : inlining_threshold;
    evaluated_size : int;
    evaluated_benefit : int;
    evaluated_threshold : int;
  }

  type t =
    | Do_not_inline
    | Maybe_inline of maybe_inline

  let create ?original lam benefit ~probably_a_functor
        (inlining_threshold : inlining_threshold) =
    match inlining_threshold with
    | Never_inline -> Do_not_inline
    | Can_inline_if_no_larger_than threshold ->
      match lambda_smaller' lam ~than:max_int with
      | None -> Do_not_inline
      | Some evaluated_size ->
        let evaluated_threshold =
          match original with
          | None -> threshold
          | Some original ->
            match lambda_smaller' lam ~than:max_int with
            | None -> threshold
            | Some size -> threshold + evaluated_size
        in
        let evaluated_benefit = Benefit.evaluate benefit in
        Maybe_inline {
          benefit; inlining_threshold; probably_a_functor;
          evaluated_size; evaluated_benefit; evaluated_threshold;
        }

  let evaluate = function
    | Do_not_inline -> false
    | Maybe_inline maybe ->
      if maybe.probably_a_functor then
        true
      else
        maybe.evaluated_size - maybe.evaluated_benefit
            <= maybe.evaluated_threshold

  let to_string t =
    match t with
    | Do_not_inline -> "do-not-inline"
    | Maybe_inline maybe ->
      Printf.sprintf "{benefit={call=%d,alloc=%d,prim=%i,branch=%i},\
                      thresh=%s,eval_size=%d,eval_benefit=%d,\
                      eval_thresh=%d,functor=%b}=%s"
        maybe.benefit.remove_call
        maybe.benefit.remove_alloc
        maybe.benefit.remove_prim
        maybe.benefit.remove_branch
        (match maybe.inlining_threshold with
          | Never_inline -> "N" | Can_inline_if_no_larger_than i -> string_of_int i)
        maybe.evaluated_size
        maybe.evaluated_benefit
        maybe.evaluated_threshold
        maybe.probably_a_functor
        (if evaluate t then "yes" else "no")
end
