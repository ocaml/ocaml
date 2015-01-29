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

open Abstract_identifiers
open Flambda

let lift_lets tree =
  let rec aux = function
    | Flet(str1,v1,Flet(str2,v2,def2,body2,d2),body1,d1) ->
        Flet(str2,v2,def2,
             aux (Flet(str1,v1,body2,body1,d1)),d2)
    | e -> e in
  Flambdaiter.map aux tree

(** An variable in a closure can either be used by the closure itself
    or by an inlined version of the function. *)
let remove_unused_closure_variables tree =
  let used_variable_within_closure =
    let used = ref Var_within_closure.Set.empty in
    let aux expr = match expr with
      | Fvariable_in_closure({ vc_var },_) ->
         used := Var_within_closure.Set.add vc_var !used
      | e -> ()
    in
    Flambdaiter.iter aux tree;
    !used
  in
  let aux = function
    | Fset_of_closures ({ cl_fun; cl_free_var } as closure, eid) ->
       let all_free_var =
         Variable.Map.fold
           (fun _ { free_variables } acc ->
             Variable.Set.union free_variables acc)
           cl_fun.funs
           Variable.Set.empty in
       let cl_free_var =
         Variable.Map.filter (fun id _ ->
           Variable.Set.mem id all_free_var
             || Var_within_closure.Set.mem (Var_within_closure.wrap id)
                                       used_variable_within_closure)
           cl_free_var in
       Fset_of_closures ({ closure with cl_free_var }, eid)
    | e -> e in
  Flambdaiter.map aux tree

let const_int_expr expr n eid =
  if Flambdaeffects.no_effects expr
  then Flambdaapprox.make_const_int n eid
  else expr, Flambdaapprox.value_int n
let const_ptr_expr expr n eid =
  if Flambdaeffects.no_effects expr
  then Flambdaapprox.make_const_ptr n eid
  else expr, Flambdaapprox.value_constptr n
let const_bool_expr expr b eid =
  const_ptr_expr expr (if b then 1 else 0) eid

let primitive p (args, approxs) expr : 'a flambda * Flambdaapprox.t =
  let open Lambda in
  match p with
  | Pmakeblock(tag, Asttypes.Immutable) ->
      expr, Flambdaapprox.value_block(tag, Array.of_list approxs)
  | _ ->
      let open Flambdaapprox in
      let eid = data_at_toplevel_node expr in
      match descrs approxs with
        [Value_int x] ->
          begin match p with
            Pidentity -> const_int_expr expr x eid
          | Pnegint -> const_int_expr expr (-x) eid
          | Pbswap16 ->
              const_int_expr expr (((x land 0xff) lsl 8) lor
                                   ((x land 0xff00) lsr 8)) eid
          | Poffsetint y -> const_int_expr expr (x + y) eid
          | _ ->
              expr, value_unknown
          end
      | [Value_int x; Value_int y]
      | [Value_constptr x; Value_int y]
      | [Value_int x; Value_constptr y]
      | [Value_constptr x; Value_constptr y] ->
          begin match p with
            Paddint -> const_int_expr expr (x + y) eid
          | Psubint -> const_int_expr expr (x - y) eid
          | Pmulint -> const_int_expr expr (x * y) eid
          | Pdivint when y <> 0 -> const_int_expr expr (x / y) eid
          | Pmodint when y <> 0 -> const_int_expr expr (x mod y) eid
          | Pandint -> const_int_expr expr (x land y) eid
          | Porint -> const_int_expr expr (x lor y) eid
          | Pxorint -> const_int_expr expr (x lxor y) eid
          | Plslint -> const_int_expr expr (x lsl y) eid
          | Plsrint -> const_int_expr expr (x lsr y) eid
          | Pasrint -> const_int_expr expr (x asr y) eid
          | Pintcomp cmp ->
              let result = match cmp with
                  Ceq -> x = y
                | Cneq -> x <> y
                | Clt -> x < y
                | Cgt -> x > y
                | Cle -> x <= y
                | Cge -> x >= y in
              const_bool_expr expr result eid
          | Pisout ->
              const_bool_expr expr (y > x || y < 0) eid
          | Psequand -> const_bool_expr expr (x <> 0 && y <> 0) eid
          | Psequor  -> const_bool_expr expr (x <> 0 || y <> 0) eid
          | _ ->
              expr, value_unknown
          end
      | [Value_constptr x] ->
          begin match p with
            Pidentity -> const_ptr_expr expr x eid
          | Pnot -> const_bool_expr expr (x = 0) eid
          | Pisint -> const_bool_expr expr true eid
          | Pctconst c ->
              begin
                match c with
                | Big_endian -> const_bool_expr expr Arch.big_endian eid
                | Word_size -> const_int_expr expr (8*Arch.size_int) eid
                | Int_size -> const_int_expr expr (8*Arch.size_int - 1) eid
                | Max_wosize -> const_int_expr expr ((1 lsl ((8*Arch.size_int) - 10)) - 1 ) eid
                | Ostype_unix -> const_bool_expr expr (Sys.os_type = "Unix") eid
                | Ostype_win32 -> const_bool_expr expr (Sys.os_type = "Win32") eid
                | Ostype_cygwin -> const_bool_expr expr (Sys.os_type = "Cygwin") eid
              end
          | _ ->
              expr, value_unknown
          end
      | [Value_block _] ->
          begin match p with
          | Pisint -> const_bool_expr expr false eid
          | _ ->
              expr, value_unknown
          end
      | _ ->
          expr, value_unknown
