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
open Joinmatch

(* DEBUG stuff *)
open Printf

let dump_pat fp jpat =
  let jid,_ = jpat.jpat_desc in
  fprintf fp "%s()" (Ident.unique_name jid.jident_desc)

let dump_list pf fp xs =
  fprintf fp "[" ;
  List.iter (fun x -> fprintf fp "%a; " pf x) xs ;
  fprintf fp "]"

let dump_pats fp jpats = dump_list dump_pat fp jpats

let dump_patss fp xs = dump_list dump_pats fp xs

(*
  This first section builds lambda expr needed by jocaml constructs.
  These are calls to functions or primitives defined in the
  Jprims module.
*)

let get_signature name =
  lazy begin
    try
      Env.open_pers_signature name Env.empty
  with Not_found ->
    fatal_error ("transjoin: module "^name^" not found")
end

let env_join = get_signature "Join"

let transl_name env name =
  try
    Env.lookup_value (Lident name) (Lazy.force env)
  with
  | Not_found ->
      fatal_error ("Join primitive: "^name^" not found")

let mk_lambda env name = lazy (transl_name env name)
let lambda_init_unit_queue = mk_lambda env_join "init_unit_queue"
let lambda_create_process = mk_lambda env_join "create_process"
(* Synchronous sends, two cases only *)
let lambda_send_sync = mk_lambda env_join "send_sync"
and lambda_send_sync_alone = mk_lambda env_join "send_sync_alone"

(* Channel creation *)
let lambda_create_async = mk_lambda env_join "create_async"
and lambda_create_alone = mk_lambda env_join "create_alone"
and lambda_create_sync = mk_lambda env_join "create_sync"
and lambda_create_sync_alone = mk_lambda env_join "create_sync_alone"


(* Asynchronous sends *)
let lambda_send_async = mk_lambda env_join "send_async"
and lambda_tail_send_async = mk_lambda env_join "tail_send_async"

(* Optimized sends *)
let lambda_local_send_async = mk_lambda env_join "local_send_async"
and lambda_local_tail_send_async = mk_lambda env_join "local_tail_send_async"


let lambda_create_automaton = mk_lambda env_join "create_automaton"
let lambda_create_automaton_debug = mk_lambda env_join "create_automaton_debug"
let lambda_wrap_automaton = mk_lambda env_join "wrap_automaton"
let lambda_patch_table = mk_lambda env_join "patch_table"
let lambda_get_queue = mk_lambda env_join "get_queue"
let lambda_reply_to = mk_lambda env_join "reply_to"
let lambda_reply_to_exn = mk_lambda env_join "reply_to_exn"
let lambda_raise_join_exit =  mk_lambda env_join "raise_join_exit"

let mk_apply f args = match Lazy.force f with
| _,{val_kind=Val_prim p}  -> Lprim (Pccall p,args)
| path,_                   -> Lapply (transl_path path, args)
  

let init_unit_queue auto idx =
  mk_apply lambda_init_unit_queue [Lvar auto ; lambda_int idx]

let create_process p =  mk_apply lambda_create_process [p]

let do_send send auto num arg =
 mk_apply send [Lvar auto ; lambda_int  num ; arg]

let create_async auto num =
  mk_apply lambda_create_async [Lvar auto ; lambda_int num]

and create_alone auto  = 
  mk_apply lambda_create_alone [Lvar auto]


and send_async chan arg = mk_apply lambda_send_async [chan ; arg]

and tail_send_async chan arg = mk_apply lambda_tail_send_async [chan ; arg]

and local_send_async auto idx arg =
  mk_apply lambda_local_send_async [Lvar auto ; lambda_int idx ; arg]

and local_tail_send_async auto idx arg =
  mk_apply lambda_local_tail_send_async [Lvar auto ; lambda_int idx ; arg]

let local_tail_send_alone guard arg =  Lapply (Lvar guard, [arg])

let local_send_alone guard arg =
  create_process
    (Lfunction
       (Curried, [Ident.create "_x"], local_tail_send_alone guard arg))

let  create_sync auto num =
  mk_apply lambda_create_sync [Lvar auto ; lambda_int num]

and create_sync_alone guard =
  mk_apply lambda_create_sync_alone [Lvar guard]


let create_automaton some_loc nchans names = match some_loc with
| None ->
   mk_apply lambda_create_automaton_debug
     [lambda_int nchans ; names ]
| Some id_loc -> failwith "NotYet"

let wrap_automaton id = mk_apply lambda_wrap_automaton [Lvar id]

let reply_to lam1 lam2 = mk_apply lambda_reply_to [lam1; lam2]
and reply_to_exn exn kont =
  mk_apply lambda_reply_to_exn [Lvar exn ; Lvar kont]

let raise_join_exit () = mk_apply lambda_raise_join_exit [lambda_unit]

let get_replies sync p =
  let reps = Typejoin.get_replies p in
  match reps, sync with
  | (id,_)::rem, Some oid when Ident.same id oid ->
      true, List.map fst rem
  | _, _ -> false, List.map fst reps

  
let do_spawn some_loc p =
 if p = lambda_unit then
   p
 else
   let param = Ident.create "_x" in
   match some_loc with
   | None ->
       create_process (Lfunction (Curried, [param], p))
   | Some id_loc ->
         assert false

let do_get_queue auto num = mk_apply lambda_get_queue [auto ; lambda_int num]

(*

 All about synchronous threads.

 Synchronous threads are guarded processes, when one of matched names
 at least is synchronous.

 In such case the guarded process is compliled into a function,
 whose result is the answer to a distinguished synchronous name
 (principal name)
*)

(*
let id_lt (x,_) (y,_) = Ident.stamp x < Ident.stamp y


exception MissingLeft of Ident.t
exception MissingRight of Ident.t

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
     let id,loc1 = x
     and _,loc2 = y in
     raise (Error (Double (id, loc1, loc2)))


let rec inter loc xs ys = match xs, ys with
| [],(id,_)::_  -> raise (MissingLeft id)
| (id,_)::_, [] ->  raise (MissingRight id)
| [],[] -> []
| x::rx, y::ry ->
   if id_lt y x then
     raise (MissingLeft (fst y))
   else if id_lt x y then
     raise (MissingRight (fst x))
   else (* x=y *)
     (fst x,loc)::inter loc rx ry

let rec do_principal p = match p.exp_desc with
(* Base cases processes *)
| Texp_asyncsend (_,_) | Texp_null 
 -> []
| Texp_reply (_, id) -> [id, p.exp_loc]
(* Recursion *)
| Texp_par (p1, p2) -> delta (do_principal p1) (do_principal p2)
| Texp_let (_,_,p) | Texp_def (_,p) | Texp_loc (_,p)
| Texp_sequence (_,p) | Texp_when (_,p) -> do_principal p
| Texp_match (_,cls,partial) ->
    let syncs =
      List.map
        (fun (_,p) -> do_principal p, p.exp_loc)
        cls in
    let r =
      begin match syncs with
      | (fst,_)::rem ->
          List.fold_right
            (fun (here, here_loc) r ->
              try
                inter p.exp_loc r here
              with
              | MissingLeft id ->
                  raise (Error (Extra (id, here_loc)))
              | MissingRight id ->
                  raise (Error (Missing (id, here_loc))))
            syncs fst
      | _ -> []
      end in
    begin match r, partial with
    | _::_, Partial -> raise (Error (NonExhaustive p.exp_loc))
    | _ -> r
    end
| Texp_ifthenelse (_,pifso, Some pifno) ->
    begin try
      inter p.exp_loc (do_principal pifso) (do_principal pifno)
    with
    | MissingLeft kid ->
        raise (Error (Missing (kid, pifso.exp_loc)))
    | MissingRight kid ->
        raise (Error (Missing (kid, pifno.exp_loc)))
    end
| Texp_ifthenelse (_,_,None) -> []
(* Errors *)
| _ -> assert false
*)

let principal p = match Typejoin.get_replies p with
| (x,_)::_ -> Some x
| []       -> None  

(* Once again for finding back parts of principal threads *)
let rec is_principal id p = match p.exp_desc with
|  Texp_asyncsend (_,_) | Texp_null 
 -> false
| Texp_reply (_, kont) -> kont=id
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
| Texp_for (_,_,_,_,_) -> false
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
| Texp_letmodule (_,_,_) | Texp_override (_,_) | Texp_lazy (_)
| Texp_send (_,_) | Texp_while (_,_) | Texp_new (_,_) | Texp_try (_,_)
| Texp_object (_, _, _)
  -> false
(* Process constructs are not errors *)
| Texp_reply (_, _)|Texp_par (_, _)|Texp_asyncsend (_, _)
| Texp_null
 -> assert false

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
| Texp_for (_,e1,e2,_,e3) ->
   simple_exp e1 && simple_exp e2 && simple_proc e3
(* Process constructs *)
| Texp_reply (e, _) -> simple_exp e
| Texp_par (p1, p2) -> simple_proc p1 || simple_proc p2
| Texp_asyncsend (e1, e2) -> simple_exp e1 && simple_exp e2
| Texp_null -> true
(* Plain expressions no longer are errors *)
| Texp_spawn _|Texp_object (_, _, _)|Texp_lazy _|Texp_assert _|
  Texp_letmodule (_, _, _)|Texp_override (_, _)|Texp_setinstvar (_, _, _)|
  Texp_instvar (_, _)|Texp_new (_, _)|Texp_send (_, _)|
  Texp_while (_, _)|Texp_array _|
  Texp_setfield (_, _, _)|Texp_field (_, _)|Texp_record (_, _)|
  Texp_variant (_, _)|Texp_construct (_, _)|Texp_tuple _|Texp_try (_, _)|
  Texp_apply (_, _)|Texp_function (_, _)|Texp_constant _|Texp_ident (_, _)|
  Texp_assertfalse
  -> assert false


let do_reply_handler pri kids lam =
  match kids with
  | [] -> lam
  | _  ->
      let param = Ident.create "#exn#" in
      Ltrywith
        (lam,
         param,
         List.fold_right
           (fun kid k -> Lsequence (reply_to_exn param kid, k))
           kids
           (if pri then Lprim (Praise, [Lvar param]) else
           raise_join_exit ()))

let lambda_reply_handler sync p lam =
  let pri, kids = get_replies sync p in
  do_reply_handler pri kids lam

let reply_handler sync p comp_fun e =
(* Find actual continuations to reply to *)
  let pri, kids = get_replies sync p in
  if simple_exp e then
    comp_fun e
  else
    do_reply_handler pri kids (comp_fun e)

  
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
 psync, seqs, forks


(*
 This section is for compiling automata.
 Most material is here, other is in Translcore
*)

let rec get_num_rec id = function
 | [] -> raise Not_found
 | (oid,x)::rem ->
     if id = oid then x else get_num_rec id rem

let dump_idx fp (id, _) = fprintf fp "%s" (Ident.unique_name id)

let get_num msg names id =
 try
   let {jchannel_id=num} = get_num_rec id names in
   num
 with
 | Not_found ->
     fatal_error
       (Printf.sprintf "Transljoin.get_num: %s" (Ident.unique_name id))








(* Intermediate representation of a join-automaton *)
type clause1 =
 { principal : Ident.t option ; (* Principal name, if any *)
   me : Joinmatch.match_clause ; }

type phase1 =
 {name1 : Ident.t ;
  names1 : (Ident.t * Typedtree.joinchannel) list ;
  clauses1 : clause1 array ; } 
(*
 Ident.t * (Ident.t * Typedtree.joinchannel) list *
 (Location.t * Ident.t option * int * int list *
  (Ident.t * bool * int option * Typedtree.pattern) list * Typedtree.expression) array
*)


(*
let build_matches {jauto_name=name ; jauto_names=names ; jauto_desc = cls} =

 let build_clause cl =
   let sync = match cl with
   | Reaction (_, (_, p)) -> principal p
   | Dispatcher (c,_,_,_) ->
       if get_sync names c then Some c else None in
   { principal = sync ; me = cl; } in

 let guarded = Array.map build_clause cls in
 { name1 = name ;  names1 = names ; clauses1 = guarded ; }

*)
let patch_table auto t =
 mk_apply
   lambda_patch_table
   [Lvar auto ; Lprim (Pmakeblock (0,Immutable), t)]

let rec principal_param ipri params nums = match params, nums with
| param::params, num::nums ->
   if num=ipri then param
   else principal_param ipri params nums
| _,_ -> assert false

let names_block names =
  let t = Array.create (List.length names) "" in
  List.iter
    (fun (id, {jchannel_id=i}) -> t.(i) <- Ident.unique_name id )
    names ;
  Lprim
    (Pmakeblock (0, Immutable),
     Array.fold_right
       (fun s r -> lambda_string s::r)
       t [])

let create_auto some_loc
   { jauto_name=(auto_name, wrapped_name); jauto_names = names ; jauto_desc = cls} k =
  let nchans = List.length names in
  Llet
    (Strict, auto_name,
     create_automaton some_loc nchans (names_block names),
     Llet
       (Strict, wrapped_name, wrap_automaton auto_name, k))

let create_channels {jauto_name=(raw_name, name) ; jauto_names=names} k =
  match names with
(*  | [id,{jchannel_sync=sync}] -> (* Fowarder *)
      let lam =
	if sync then
	  create_sync_alone name
	else
	  create_alone name in
      Llet (StrictOpt, id, lam, k)
*)
  | _ ->
      List.fold_right
	(fun (id,
              ({jchannel_sync=sync ; jchannel_id=num} as jc)) k ->
		Llet
		  (StrictOpt, id,
		   begin if sync then
                     create_sync name num
		   else
                     create_async name num
		   end,
		   if
                     Typeopt.is_unit_channel_type
                       jc.jchannel_type
                       jc.jchannel_env
		   then
                     Lsequence (init_unit_queue raw_name num, k)
		   else k))
	names k

let get_queue name names jpat =
 let jid,_ = jpat.jpat_desc in
 let id = jid.jident_desc in
 let i = get_num "(get_queue)" names id in
 let k = jpat.jpat_kont in
 match k with
 | None ->
     None, do_get_queue (Lvar name) i
 | Some kid ->
     let y = Ident.create "_y" in
     Some y, do_get_queue (Lvar name) i



let build_lets bds r =
 List.fold_right
   (fun (oid, lam) r ->
     match oid with
     | None -> r
     | Some y -> Llet (Strict, y, lam, r))
   bds r


let nslots n_names = (n_names + 30) / 31
let major i =  i / 31
and minor i = i mod 31

let build_singleton n_names num =
  if n_names < 32 then
    lambda_int (1 lsl num)
  else
    let nslots = nslots n_names
    and slot = major num
    and idx = minor num in
    let rec do_rec i =
      if i >= nslots then []
      else
        lambda_int
          (if i = slot then (1 lsl idx) else 0)::
        do_rec (i+1) in
    Lprim (Pmakearray Pintarray, do_rec 0)


let build_int_mask names jpats =
  let rec do_rec mask = function
    | [] -> mask
    | jpat::rem ->
        let jid,_ = jpat.jpat_desc in
        let i = get_num "(build_mask)" names jid.jident_desc in
        do_rec (mask lor (1 lsl i)) rem in
  lambda_int (do_rec 0 jpats)

and  build_bv_mask n_names names jpats =
  let nslots = nslots n_names in

  let rec empty i =
    if i <= 0 then []
    else 0::empty (i-1) in

  let rec set_bit slot idx i = function
    | [] -> assert false
    | num::rem ->
        if i = slot then
          num lor (1 lsl idx)::rem
        else
          num::set_bit slot idx (i+1) rem in

  let rec do_rec mask = function
    | [] -> mask
    | jpat::rem ->
        let jid,_ = jpat.jpat_desc in
        let i = get_num "(build_mask)" names jid.jident_desc in        
        do_rec
          (set_bit (major i) (minor i) 0 mask) rem in

  Lprim
    (Pmakearray Pintarray,
     List.map lambda_int (do_rec (empty nslots) jpats))

let build_mask n_names names jpats =
  if n_names < 32 then 
    build_int_mask names jpats
  else
    build_bv_mask n_names names jpats

let rec explode = function
  | [] -> []
  | [xs] -> List.map (fun x -> [x]) xs
  | xs::rem ->
      let rem = explode rem in      
      List.fold_right
        (fun x r ->
          List.fold_right
            (fun xs r -> (x::xs)::r)
            rem r)
        xs
        []




let create_table some_loc auto1 gs r =
  let ngs = Array.length gs
  and name,_ = auto1.jauto_name
  and names = auto1.jauto_names in
  let n_names = List.length names in

  let rec do_guard i =
    if i >= ngs then []
    else
      let g,sync,_ = gs.(i)
      and reac = auto1.jauto_desc.(i) in
      match reac with
      | Dispatcher (sync, c, z, _, _) ->
          let num = get_num "(do_guard, dispatcher)" names c in
          let ipri = if sync then num else -1 in
          let goid = Ident.create "_go" in          
          let lam =
            if sync then
              let y = Ident.create "_y" in
              Lfunction
                (Curried, [goid],
                 Llet
                   (Strict, y, do_get_queue (Lvar name) num,
                    Lapply
                      (Lvar goid,
                       [Lprim (Pfield 0,[Lvar y]) ;                       
                         Lapply
                           (Lvar g,
                            [Lprim (Pfield 1, [Lvar y])])])))
            else
              Lfunction
                (Curried, [goid],
                 Lapply
                   (Lvar goid,
                    [Lvar name ;
                      Lapply (Lvar g, [do_get_queue (Lvar name) num])])) in
          Lprim
            (Pmakeblock (0, Immutable),
             [build_singleton n_names num ; lambda_int ipri; lam])
          ::do_guard (i+1)
      | Reaction (pats, _) ->
          let pats = explode pats in
          let create_reaction jpats r =

            let ipri = match sync with
            | None -> -1
            | Some _ ->
                let rec find_rec = function
                  | [] -> -1
                  | jpat::rem ->
                      if jpat.jpat_kont = sync then
                        let jid,_ = jpat.jpat_desc in
                        get_num "(real_ipri)" names jid.jident_desc
                      else
                        find_rec rem in
                find_rec jpats in
            
            let bds = List.map (get_queue name names) jpats in
            let args =
              List.fold_right2
                (fun bd jpat r -> match bd with
                | None,lam -> lam::r
                | Some y,_ ->
                    let k = jpat.jpat_kont in
                    if k = sync then
                      Lprim (Pfield 1, [Lvar y])::r
                    else
                      Lprim (Pfield 0, [Lvar y])::
                      Lprim (Pfield 1, [Lvar y])::r)
                bds jpats [] in
            let goid = Ident.create "_go" in
            let real_g =
              if ipri < 0 then
                Lfunction
                  (Curried, [goid],
                   build_lets bds
                     (Lapply
                        (Lvar goid, [Lvar name ; Lapply (Lvar g, args)])))
              else
                let pri_kont =
                  let rec find_rec bds jpats = match bds, jpats with
                  | (Some y,_)::bds, jpat::jpats
                    when jpat.jpat_kont = sync ->
                     Lprim (Pfield 0, [Lvar y])
                  | _::bds, _::jpats ->
                      find_rec bds jpats
                  | _, _ -> assert false in
                  find_rec bds jpats in
                Lfunction
                  (Curried, [goid],
                   build_lets bds
                     (Lapply
                        (Lvar goid, [pri_kont ; Lapply (Lvar g, args)]))) in
            Lprim
              (Pmakeblock (0, Immutable),
               [build_mask n_names names jpats ;
                 lambda_int ipri ; real_g])::r in

          List.fold_right create_reaction pats (do_guard (i+1)) in
  
  Lsequence
    (patch_table name (do_guard 0),
     r)

(*********************)
(* Global exceptions *)
(*********************)

let lambda_exn_global =  mk_lambda env_join "exn_global"

(* "exn_global" takes a location as a first argument,
   so as give a source position in case of failure *)

let transl_exn_global loc path =
  mk_apply lambda_exn_global [transl_location loc ; transl_path path]
