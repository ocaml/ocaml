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

(* XCR pchambart: this file is not doing only string lifting anymore, it
   should probably be renamed. It prepares lambda for closure conversion.
   Const_block handling should probably completely disapear from
   flambdagen.
   mshinwell: I've fixed this (although this file is still called
   lift_strings.ml, since it's all it does now).
*)

let rec lift_strings acc (lam : Lambda.lambda)
      : (Ident.t * Lambda.lambda) list * Lambda.lambda =
  match lam with
  | Lvar _ ->
    acc, lam
  | Lconst (Const_base (Const_string _)) ->
    let id = Ident.create "constant_string" in
    (id, lam) :: acc, Lvar id
  | Lconst (Const_base (Const_nativeint _ | Const_char _ | Const_float _ |
                        Const_int32 _ | Const_int64 _ | Const_int _) |
            Const_pointer _ | Const_block _ | Const_float_array _ |
            Const_immstring _) ->
    (* NB. [Const_block] is not expected to contain strings; they should
       have been removed by [Eliminate_const_block], run before this
       pass. *)
    acc, lam
  | Llet (str, id, lam, body) ->
    let acc, lam = lift_strings acc lam in
    let acc, body = lift_strings acc body in
    acc, Llet (str, id, lam, body)
  | Lfunction { kind; params; body; } ->
    let acc, body = lift_strings acc body in
    acc, Lfunction { kind; params; body; }
  | Lapply (funct, args, loc) ->
    let acc, funct = lift_strings acc funct in
    let acc, args = lift_strings_list acc args in
    acc, Lapply (funct, args, loc)
  | Lletrec (defs, body) ->
    let acc, defs = lift_strings_pair_list acc defs in
    let acc, body = lift_strings acc body in
    acc, Lletrec (defs, body)
  | Lsend (kind, met, obj, args, loc) ->
    let acc, met = lift_strings acc met in
    let acc, obj = lift_strings acc obj in
    let acc, args = lift_strings_list acc args in
    acc, Lsend (kind, met, obj, args, loc)
  | Lprim (p, args) ->
    let acc, args = lift_strings_list acc args in
    acc, Lprim (p, args)
  | Lswitch (arg, sw) ->
    let acc, arg = lift_strings acc arg in
    let acc, sw_consts = lift_strings_pair_list acc sw.sw_consts in
    let acc, sw_blocks = lift_strings_pair_list acc sw.sw_blocks in
    let acc, sw_failaction =
      match sw.sw_failaction with
      | None -> acc, None
      | Some failaction ->
        let acc, failaction = lift_strings acc failaction in
        acc, Some failaction 
    in
    acc, Lswitch (arg, { sw with sw_consts; sw_blocks; sw_failaction })
  | Lstringswitch (arg, sw, def) ->
    let acc, arg = lift_strings acc arg in
    let acc, sw = lift_strings_pair_list acc sw in
    let acc, def =
      match def with
      | None -> acc, None
      | Some def ->
        let acc, def = lift_strings acc def in
        acc, Some def
    in
    acc, Lstringswitch (arg, sw, def)
  | Lstaticraise (i, args) ->
    let acc, args = lift_strings_list acc args in
    acc, Lstaticraise (i, args)
  | Lstaticcatch (body, (i, vars), handler) ->
    let acc, body = lift_strings acc body in
    let acc, handler = lift_strings acc handler in
    acc, Lstaticcatch (body, (i, vars), handler)
  | Ltrywith (body, id, handler) ->
    let acc, body = lift_strings acc body in
    let acc, handler = lift_strings acc handler in
    acc, Ltrywith (body, id, handler)
  | Lifthenelse (arg, ifso, ifnot) ->
    let acc, arg = lift_strings acc arg in
    let acc, ifso = lift_strings acc ifso in
    let acc, ifnot = lift_strings acc ifnot in
    acc, Lifthenelse (arg, ifso, ifnot)
  | Lsequence (lam1, lam2) ->
    let acc, lam1 = lift_strings acc lam1 in
    let acc, lam2 = lift_strings acc lam2 in
    acc, Lsequence (lam1, lam2)
  | Lwhile (cond, body) ->
    let acc, cond = lift_strings acc cond in
    let acc, body = lift_strings acc body in
    acc, Lwhile (cond, body)
  | Lfor (id, lo, hi, dir, body) ->
    let acc, lo = lift_strings acc lo in
    let acc, hi = lift_strings acc hi in
    let acc, body = lift_strings acc body in
    acc, Lfor (id, lo, hi, dir, body)
  | Lassign (id, lam) ->
    let acc, lam = lift_strings acc lam in
    acc, Lassign (id, lam)
  | Levent (lam, ev) ->
    let acc, lam = lift_strings acc lam in
    acc, Levent (lam, ev)
  | Lifused _ ->
    assert false

and lift_strings_list acc lams =
  List.fold_right (fun lam (acc, lams) ->
      let acc, lam = lift_strings acc lam in
      acc, lam :: lams)
    lams (acc, [])

and lift_strings_pair_list :
  'a. 'acc -> ('a * Lambda.lambda) list -> 'acc * ('a * Lambda.lambda) list =
  fun acc lams ->
    List.fold_right (fun (v, lam) (acc, lams) ->
        let acc, lam = lift_strings acc lam in
        acc, (v, lam) :: lams)
      lams (acc, [])

let run lam =
  let bindings, lam = lift_strings [] lam in
  List.fold_left (fun lam (id, lam_const) ->
      Lambda.Llet (Strict, id, lam_const, lam))
    lam bindings
