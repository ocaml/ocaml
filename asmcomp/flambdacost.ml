(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lambda
open Abstract_identifiers
open Flambda

(* Determine whether the estimated size of a flambda term is below
   some threshold *)

let prim_size prim args =
  match prim with
    Pidentity -> 0
  | Pgetglobal id -> 1
  | Psetglobal id -> 1
  | Pmakeblock(tag, mut) -> 5 + List.length args
  | Pfield f -> 1
  | Psetfield(f, isptr) -> if isptr then 4 else 1
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
  | Pbigarrayref(_, ndims, _, _) -> 4 + ndims * 6
  | Pbigarrayset(_, ndims, _, _) -> 4 + ndims * 6
  | Pgetglobalfield _ -> 2
  | Psetglobalfield _ -> 2
  | _ -> 2 (* arithmetic and comparisons *)

(* Very raw approximation of flambda space cost *)

let lambda_smaller' lam ~than:threshold =
  let open Asttypes in
  let size = ref 0 in
  let rec lambda_size lam =
    if !size > threshold then raise Exit;
    match lam with
      Fvar _ -> ()
    | Fsymbol _ -> ()
    | Fconst(
        (Fconst_base(Const_int _ | Const_char _ | Const_float _ |
                     Const_int32 _ | Const_int64 _ | Const_nativeint _) |
         Fconst_pointer _), _) -> incr size
    | Fconst(
        (Fconst_base( Const_string _ )
        | Fconst_float_array _ | Fconst_immstring _ ) , _) ->
        raise Exit (* Do not duplicate: should be moved out by a previous pass *)
    | Fapply ({ ap_function = fn; ap_arg = args; ap_kind = direct }, _) ->
        let call_cost = match direct with Indirect -> 6 | Direct _ -> 4 in
        size := !size + call_cost; lambda_size fn; lambda_list_size args
    | Fset_of_closures({ cl_fun = ffuns; cl_free_var = fv }, _) ->
        Variable.Map.iter (fun _ -> lambda_size) fv;
        Variable.Map.iter (fun _ ffun -> lambda_size ffun.body) ffuns.funs
    | Fclosure({ fu_closure = lam }, _) ->
        incr size; lambda_size lam
    | Fvariable_in_closure({ vc_closure }, _) ->
        incr size; lambda_size vc_closure
    | Flet(id, _, lam, body, _) ->
        lambda_size lam; lambda_size body
    | Fletrec(bindings, body, _) ->
        List.iter (fun (_,lam) -> lambda_size lam) bindings;
        lambda_size body
    | Fprim(prim, args, _, _) ->
        size := !size + prim_size prim args;
        lambda_list_size args
    | Fswitch(lam, sw, _) ->
        let aux = function _::_::_ -> size := !size + 5 | _ -> () in
        aux sw.fs_consts; aux sw.fs_blocks;
        lambda_size lam;
        List.iter (fun (_,lam) -> lambda_size lam) sw.fs_consts;
        List.iter (fun (_,lam) -> lambda_size lam) sw.fs_blocks
    | Fstringswitch(lam,sw,def,_) ->
        lambda_size lam;
        List.iter (fun (_,lam) ->
            size := !size + 2;
            lambda_size lam)
          sw;
        Misc.may lambda_size def
    | Fstaticraise (_,args,_) -> lambda_list_size args
    | Fstaticcatch(_, _, body, handler, _) ->
        incr size; lambda_size body; lambda_size handler
    | Ftrywith(body, id, handler, _) ->
        size := !size + 8; lambda_size body; lambda_size handler
    | Fifthenelse(cond, ifso, ifnot, _) ->
        size := !size + 2;
        lambda_size cond; lambda_size ifso; lambda_size ifnot
    | Fsequence(lam1, lam2, _) ->
        lambda_size lam1; lambda_size lam2
    | Fwhile(cond, body, _) ->
        size := !size + 2; lambda_size cond; lambda_size body
    | Ffor(id, low, high, dir, body, _) ->
        size := !size + 4; lambda_size low; lambda_size high; lambda_size body
    | Fassign(id, lam, _) ->
        incr size;  lambda_size lam
    | Fsend(_, met, obj, args, _, _) ->
        size := !size + 8;
        lambda_size met; lambda_size obj; lambda_list_size args
    | Funreachable _ -> ()
    | Fevent _ -> assert false
  and lambda_list_size l = List.iter lambda_size l in
  try
    lambda_size lam;
    if !size <= threshold then Some !size
    else None
  with Exit ->
    None

let lambda_smaller lam ~than =
  lambda_smaller' lam ~than <> None

type inline_threshold =
  | Never_inline
  | Can_inline of int

let can_try_inlining lam inline_threshold ~bonus =
  match inline_threshold with
  | Never_inline -> Never_inline
  | Can_inline inline_threshold ->
     match lambda_smaller'
             lam
             ~than:((inline_threshold + bonus) * 2)
     with
     | None -> Never_inline
     | Some size -> Can_inline (inline_threshold - size)

let can_inline lam inline_threshold ~bonus =
  match inline_threshold with
  | Never_inline -> false
  | Can_inline inline_threshold ->
     lambda_smaller
       lam
       ~than:(inline_threshold + bonus)
