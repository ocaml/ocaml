(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Longident
open Primitive
open Types
open Typedtree
open Env
open Lambda

let env = lazy begin
  try
    Env.open_pers_signature "Jprims" Env.empty
  with Not_found ->
    fatal_error "module Jprim not found"
end

let transl_name name =
  try
    Env.lookup_value (Lident name) (Lazy.force env)
  with
  | Not_found ->
      fatal_error ("Join primitive: "^name^" not found")


let lambda_exit = lazy (transl_name "exit")
let lambda_create_process = lazy (transl_name "create_process")

let mk_apply f args = match f with
| _,{val_kind=Val_prim p}  -> Lprim (Pccall p,args)
| path,_                   -> Lapply (transl_path path, args)
  

let exit () = mk_apply (Lazy.force lambda_exit) [lambda_unit]
let create_process p =  mk_apply (Lazy.force lambda_create_process) [p]

let do_spawn p =
  create_process (Lfunction (Curried, [], p))

(* Approximation of termination *)

let rec terminates_expr e = match e.exp_desc with
(* Terminates for sure *)
| Texp_ident _ | Texp_constant _ | Texp_function (_,_)
| Texp_variant (_,None) | Texp_spawn _
| Texp_instvar (_,_) | Texp_setinstvar (_, _, _) | Texp_new (_,_)
| Texp_assertfalse
  -> true
(* Recursion *)
| Texp_construct (_,es) | Texp_tuple (es) | Texp_array (es)
  -> List.for_all terminates_expr es
| Texp_let (_, pes,e) | Texp_match (e, pes,_)
| Texp_try (e, pes)
    ->
      terminates_expr e &&
      List.for_all (fun (_,e) -> terminates_expr e) pes
| Texp_variant (_, Some e) | Texp_field (e,_)
| Texp_def (_,e) | Texp_loc (_,e) | Texp_assert (e)
    -> terminates_expr e
| Texp_sequence (e1,e2) | Texp_when (e1,e2) | Texp_setfield (e1,_,e2)
  -> terminates_expr e1 && terminates_expr e2
| Texp_apply ({exp_desc=Texp_ident (_, {val_kind=Val_prim p})}, args) ->
    List.length args <= p.prim_arity &&
    List.for_all (fun (eo,_) -> terminates_option eo) args
| Texp_for (_,e1,e2,_,e3) ->
    terminates_expr e1 && terminates_expr e2 && terminates_expr e3
| Texp_ifthenelse (e1, e2, eo) ->
    terminates_expr e1 && terminates_expr e2 && terminates_option eo
| Texp_record (les,eo) ->
    List.for_all (fun (_,e) -> terminates_expr e) les &&
    terminates_option eo
(* Who knows ? *)
| Texp_letmodule (_,_,_) | Texp_override (_,_)
| Texp_send (_,_) | Texp_while (_,_) | Texp_apply (_,_)
  -> false
(* Process cases *)
| Texp_reply (e, _) -> terminates_expr e
| Texp_par (e1, e2) -> terminates_expr e1 || terminates_expr e2
| Texp_asyncsend (e1, e2) -> terminates_expr e1 && terminates_expr e2
| Texp_null -> true

and terminates_option = function
  | None -> true
  | Some e -> terminates_expr e

let partition_procs procs = List.partition terminates_expr procs

let rec do_as_procs r e = match e.exp_desc with
| Texp_null -> r
| Texp_par (e1,e2) ->
    do_as_procs (do_as_procs r e2) e1
| _ -> e::r

let as_procs e = partition_procs (do_as_procs [] e)

(* Automaton build *)
let get_num names id =
  let {jchannel_id=num} = List.assoc names id in
  id_num

let transl_jpat jpat =
  let _,arg = jpat.jpat_desc in
  args

let transl_jpats jpats =
  List.map transl_jpat jpats

let build_matches {jauto_name=name ; jauto_names=names ; jauto_desc = cls} =
  let r = Array.create (List.length names) [] in

  let rec build_clauses i = function
    | [] -> []
    | {jclause_desc = (jpats,e); jclause_loc=cl_loc}::rem ->
        let nums =
          List.map
            (fun {pat_desc=({jident_desc=id}, _)} -> get_num names id)
            jpats in
        let base_pat =
          List.fold_left
            (fun r num -> r lor (1 lsl num))
            0
            nums in
        List.iter
          (fun num ->
            r.(num) <-
               (base_pat land (lnot (1 lsl num)),i) ::
                r.(num))
          nums ;
        (cl_loc,transl_jpats pats, e)::build_clauses (i+1) rem in

  let guarded = build_clauses 0 in
  (name, Array.map Array.from_list r, Array.from_list guarded)
  
              


              

