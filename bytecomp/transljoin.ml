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
open Asttypes
open Typedtree
open Env
open Lambda

(*
  This first section builds lambda expr needed by jocaml constructs.
  These are calls to functions or primitives defined in the
  Jprims module.
*)

let env = lazy begin
  try
    Env.open_pers_signature "Jprims" Env.empty
  with Not_found ->
    fatal_error "module Jprims not found"
end

let transl_name name =
  try
    Env.lookup_value (Lident name) (Lazy.force env)
  with
  | Not_found ->
      fatal_error ("Join primitive: "^name^" not found")

let mk_lambda name = lazy (transl_name name)
let lambda_exit = mk_lambda "exit"
let lambda_create_location = mk_lambda "create_location"
let lambda_create_process = mk_lambda "create_process"
let lambda_create_process_location = mk_lambda "create_process_location"
let lambda_send_sync = mk_lambda "send_sync"
let lambda_send_async = mk_lambda "send_async"
let lambda_create_automaton = mk_lambda "create_automaton"
let lambda_create_automaton_location = mk_lambda "create_automaton_location"
let lambda_patch_match = mk_lambda "patch_match"
let lambda_patch_guard = mk_lambda "patch_guard"
let lambda_reply_to = mk_lambda "reply_to"

let mk_apply f args = match Lazy.force f with
| _,{val_kind=Val_prim p}  -> Lprim (Pccall p,args)
| path,_                   -> Lapply (transl_path path, args)
  

let exit () = mk_apply lambda_exit [lambda_unit]
let create_location () = mk_apply lambda_create_location [lambda_unit]
let create_process p =  mk_apply lambda_create_process [p]
let create_process_location id_loc p =
  mk_apply lambda_create_process_location [Lvar id_loc ; p]

let send_async auto num arg =
  mk_apply lambda_send_async
    [Lvar auto ; lambda_int  num ; arg]

let send_sync auto num arg =
  mk_apply lambda_send_sync
    [Lvar auto ; lambda_int num; arg]

let create_automaton some_loc nchans nguards = match some_loc with
| None ->
    mk_apply lambda_create_automaton
      [lambda_int nchans ; lambda_int nguards]
| Some id_loc ->
    mk_apply lambda_create_automaton_location
      [Lvar id_loc ; lambda_int nchans ; lambda_int nguards]
    

let patch_match auto i this_match =
  mk_apply lambda_patch_match
    [Lvar auto ; lambda_int i ;
    Lconst (Const_block (0, List.map (fun x -> Const_base (Const_int x)) this_match))]

let patch_guard auto i lam =
  mk_apply lambda_patch_guard
    [Lvar auto ; lambda_int i ; lam]

let reply_to lam1 lam2 =
  mk_apply lambda_reply_to [lam1; lam2]

let do_spawn some_loc p =
  if p = lambda_unit then
    p
  else
    match some_loc with
    | None ->
        create_process (Lfunction (Curried, [], p))
    | Some id_loc ->
        create_process_location id_loc (Lfunction (Curried, [], p))


(*

  All about synchronous threads.

  Synchronous threads are guarded processes, when one of matched names
  at least is synchronous.
    In such case the guarded process is compliled into a function,
    whose result is the answer to a distinguished synchronous name
    (principal name)
*)
let id_lt x y = Ident.name x < Ident.name y

(* Symetrical difference, catches double answers  *)
let rec delta xs ys = match xs, ys with
| [],_  -> ys
| _, [] -> xs
| x::rx, y::ry ->
    if id_lt x y then
      x::delta rx ys
    else if id_lt y x then
      y::delta xs ry
    else (* x=y *)
      delta rx ry

let rec inter xs ys = match xs, ys with
| [],_  -> []
| _, [] -> []
| x::rx, y::ry ->
    if id_lt x y then
      inter rx ys
    else if id_lt y x then
      inter xs ry
    else (* x=y *)
      x::inter rx ry



  
let rec do_principal p = match p.exp_desc with
(* Base cases processes *)
| Texp_asyncsend (_,_) | Texp_exec (_) | Texp_null 
  -> []
| Texp_reply (_, Path.Pident id) -> [id]
(* Recursion *)
| Texp_par (p1, p2) -> delta (do_principal p1) (do_principal p2)
| Texp_let (_,_,p) | Texp_def (_,p) | Texp_loc (_,p)
| Texp_sequence (_,p) | Texp_when (_,p)
  -> do_principal p
| Texp_match (_,(_,p)::cls,_) ->
    List.fold_right
      (fun (_, p) r -> inter (do_principal p) r)
      cls (do_principal p)
| Texp_ifthenelse (_,pifso, Some pifno) ->
    inter (do_principal pifso) (do_principal pifno)
| Texp_ifthenelse (_,_,None) -> []
(* Errors *)
| _ -> assert false

let principal p = match do_principal p with
| x::_ -> Some x
| [] -> None  

(* Once again for finding back parts of princpal threads *)
let rec is_principal id p = match p.exp_desc with
|  Texp_asyncsend (_,_) | Texp_exec (_) | Texp_null 
  -> false
| Texp_reply (_, Path.Pident kont) -> kont=id
| Texp_par (p1, p2) ->
    is_principal id p1 || is_principal id p2
| Texp_let (_,_,p) | Texp_def (_,p) | Texp_loc (_,p)
| Texp_sequence (_,p) | Texp_when (_,p) -> 
    is_principal id p
| Texp_match (_,(_,p)::cls,_) ->
    is_principal id p &&
    List.for_all (fun (_,p) -> is_principal id p) cls
| Texp_ifthenelse (_,pifso, Some pifno) ->
    is_principal id pifso && is_principal id pifno
| Texp_ifthenelse (_,_,None) -> false
| _ -> assert false

(*
  The simple_proc predicates decides whether a new thread is needed
  to execute a process p or not.
  More specifically, the execution of p must terminate and does not
  raise an exception.

  Note :
  -There are connections beetween the answers of
   simple_proc and the way threads are introduced by
   transl_simple_proc and transl_proc in Translcore

  -Interaction of predicate/compilation can be quadratic, I do
   not think it harms on real programs
*)

(*
  simple_pat checks irrefutabililty  for let patterns.

  Idealy one should use some Partial/Total field, but this
  information is lost.. Does not matter much anyway.
*)
let rec simple_pat p = match p.pat_desc with
| Tpat_any | Tpat_var _ -> true
| Tpat_alias (p,_) -> simple_pat p
| Tpat_tuple ps -> List.for_all simple_pat ps
| Tpat_record lps ->  List.for_all (fun (_,p) -> simple_pat p) lps
| Tpat_or (p1,p2,_) -> simple_pat p1 && simple_pat p2
| Tpat_constant _|Tpat_construct (_,_)|Tpat_variant (_,_,_)
| Tpat_array _ -> false

let rec simple_exp e = match e.exp_desc with
(* Mixed cases *)
| Texp_sequence (e1,e2) | Texp_when (e1,e2) ->
    simple_exp e1 && simple_exp e2
| Texp_let (_, pes,e) ->
    List.for_all (fun (pat,e) -> simple_pat pat && simple_exp e) pes &&
    simple_exp e
| Texp_match (e,pes,Total) ->
    simple_exp e &&
    List.for_all (fun (_,e) -> simple_exp e) pes
| Texp_match (_, _, Partial) -> false
| Texp_ifthenelse (e, eifso, eo) ->
    simple_exp e && simple_exp eifso && simple_exp_option eo
| Texp_def (_,e)|Texp_loc(_,e) -> simple_exp e
(* Simple simple expressions *)
| Texp_ident _ | Texp_constant _ | Texp_function (_,_)
| Texp_variant (_,None) 
| Texp_instvar (_,_) | Texp_setinstvar (_, _, _) | Texp_spawn (_)
  -> true
(* Recursion *)
| Texp_construct (_,es) | Texp_tuple (es) | Texp_array (es)
  -> List.for_all simple_exp es
| Texp_variant (_, Some e) | Texp_field (e,_)
    -> simple_exp e
| Texp_setfield (e1,_,e2) -> simple_exp e1 && simple_exp e2
| Texp_apply ({exp_desc=Texp_ident (_, {val_kind=Val_prim p})}, args)
  when p.prim_name <> "%raise" ->
    List.length args <= p.prim_arity &&
    List.for_all (fun (eo,_) -> simple_exp_option eo) args
| Texp_apply (_,_) -> false
| Texp_for (_,e1,e2,_,e3) ->
    simple_exp e1 && simple_exp e2 && simple_exp e3
| Texp_record (les,eo) ->
    List.for_all (fun (_,e) -> simple_exp e) les &&
    simple_exp_option eo
(* Asserts are special *)
| Texp_assert e -> !Clflags.noassert || simple_exp e
| Texp_assertfalse -> !Clflags.noassert
(* Who knows ? *)
| Texp_letmodule (_,_,_) | Texp_override (_,_)
| Texp_send (_,_) | Texp_while (_,_) | Texp_new (_,_)
  -> false
(* Process constructs are errors *)
| _ -> fatal_error "Transljoin.simple_proc"

and simple_exp_option = function
  | None -> true
  | Some e -> simple_exp e

and simple_proc p = match p.exp_desc with
(* Mixed cases *)
| Texp_sequence (e,p) | Texp_when (e,p) ->
    simple_exp e && simple_proc p
| Texp_let (_, pes,e) ->
    List.for_all (fun (pat,e) -> simple_pat pat && simple_exp e) pes &&
    simple_proc e
| Texp_match (e,pps,Total) ->
    simple_exp e &&
    List.for_all (fun (_,p) -> simple_proc e) pps
| Texp_match (_,_,Partial) -> false
| Texp_ifthenelse (e, pifso, Some pifno) ->
    simple_exp e && simple_proc pifso && simple_proc pifno
| Texp_ifthenelse (e, pifso, None) ->
    simple_exp e && simple_proc pifso    
| Texp_def (_,p)|Texp_loc(_,p) -> simple_proc p
(* Process constructs *)
| Texp_reply (e, _) -> simple_exp e
| Texp_par (p1, p2) -> simple_proc p1 || simple_proc p2
| Texp_asyncsend (e1, e2) -> simple_exp e1 && simple_exp e2
| Texp_exec e -> simple_exp e
| Texp_null -> true
(* Plain expressions are errors *)
| _ -> fatal_error "Transljoin.simple_proc"



let partition_procs procs = List.partition simple_proc procs


let rec do_as_procs r e = match e.exp_desc with
| Texp_null -> r
| Texp_par (e1,e2) ->
    do_as_procs (do_as_procs r e2) e1
| _ -> e::r

let rec get_principal id = function
  | [] -> assert false (* one thread must be principal *)
  | p::rem ->
      if is_principal id p then
        p,rem
      else
        let r,rrem = get_principal id rem in
        r,p::rrem

let as_procs sync e =
  let ps = do_as_procs [] e in
  let psync,  ps = match sync with
  | None -> None, ps
  | Some id ->
      let psync, ps = get_principal id ps in
      Some psync, ps in
  let seqs, forks = partition_procs ps in
  psync,
  List.map
    (fun p -> match p.exp_desc with
    | Texp_exec e -> e
    | _ -> p) seqs,
  forks


(*
  This section is for compiling automata.
  Most material is here, other is in Translcore
*)

let get_num names id =
  try
    let {jchannel_id=num} = List.assoc id names in
    num
  with
  | Not_found -> assert false

(* Not so nice way to supress the continuation argument for principal names *)

let transl_jpat sync {jpat_desc=_,arg} = match sync,arg.pat_desc with
| Some id, Tpat_tuple [{pat_desc = Tpat_var oid} ; true_arg]
    when oid = id -> true_arg
| _,_ -> arg
      

let transl_jpats sync jpats = List.map (transl_jpat sync) jpats
         

let build_matches {jauto_name=name ; jauto_names=names ; jauto_desc = cls} =
  let r = Array.create (List.length names) [] in

  let rec build_clauses i = function
    | [] -> []
    | {jclause_desc = (jpats,e); jclause_loc=cl_loc}::rem ->
        (* compute principal name *)
        let sync = principal e in
        (* Sort jpats by channel indexes *)
        let jpats =
          List.sort
            (fun
              {jpat_desc=({jident_desc=id1}, _)}
              {jpat_desc=({jident_desc=id2}, _)} ->
                get_num names id1 - get_num names id2)
            jpats in
        (* collect channel indexes *)
        let nums =
          List.map
            (fun {jpat_desc=({jident_desc=id}, _)} -> get_num names id)
            jpats in
        (* compute bitfield for jpats *)
        let base_pat =
          List.fold_left
            (fun r num -> r lor (1 lsl num))
            0
            nums
        and principal_pat =  match sync with
        | None -> 0
        | Some id -> 1 lsl get_num names id in
          
        (* add automaton entries for jpats *)
        List.iter
          (fun num ->
            r.(num) <-
               i :: principal_pat :: base_pat :: r.(num))
          nums ;
        (cl_loc, sync, transl_jpats sync jpats, e)::build_clauses (i+1) rem in

  let guarded = build_clauses 0 cls in
  for i = 0 to Array.length r-1 do
    r.(i) <- List.rev r.(i)
  done ;
  (name, r, Array.of_list guarded)

              
let build_auto some_loc (name, matches, guards) k =
  let nchannels = Array.length matches
  and nguards = Array.length guards in

  let rec patch_matches i k =
    if i >= nchannels then
      k
    else
      let this_match = matches.(i) in
      Lsequence
        (patch_match name i matches.(i),
         patch_matches (i+1) k) in
        
  Llet
    (Strict, name,
     create_automaton some_loc nchannels nguards,
     patch_matches 0 k)

let build_channels {jauto_name=name ; jauto_names=names} k =
  List.fold_right
    (fun (id, {jchannel_sync=sync ; jchannel_id=num}) k ->
      let jparam = Ident.create "jparam" in
      Llet
        (StrictOpt, id,
         Lfunction
           (Curried,[jparam],
            if sync then
              send_sync name num (Lvar jparam)
            else
              send_async name num (Lvar jparam)),
         k))
    names k

let build_guards comp_fun (name,_,guards) k =
  let nguards = Array.length guards in
  let rec do_rec i =
    if i >= nguards then
      k
    else
      let cl_loc, sync, jpats, e = guards.(i) in
      Lsequence
        (patch_guard name i
           (comp_fun cl_loc sync jpats e),
         do_rec (i+1)) in
  do_rec 0
