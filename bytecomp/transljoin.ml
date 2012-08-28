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
(*>JOCAML *)
open Auto
(*<JOCAML *)
open Env
open Lambda

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
  Join_prim module.
*)

let get_signature name =
  lazy begin
    try
      Env.open_pers_signature name Env.empty
  with Not_found ->
    fatal_error ("transjoin: module "^name^" not found")
end

let env_join = get_signature "Join_prim"

let transl_name env name =
  try
    Env.lookup_value (Lident name) (Lazy.force env)
  with
  | Not_found ->
      fatal_error ("Join primitive: "^name^" not found")

let mk_lambda env name = lazy (transl_name env name)
let lambda_init_unit_queue = mk_lambda env_join "init_unit_queue"
let lambda_create_process = mk_lambda env_join "create_process"

(* Channel creation *)
let lambda_create_async = mk_lambda env_join "create_async"
and lambda_create_alone = mk_lambda env_join "create_alone"
and lambda_alloc_alone = mk_lambda env_join "alloc_alone"
and lambda_patch_alone = mk_lambda env_join "patch_alone"
and lambda_create_sync = mk_lambda env_join "create_sync"
and lambda_create_sync_alone = mk_lambda env_join "create_sync_alone"
and lambda_alloc_stub_guard = mk_lambda env_join "alloc_stub_guard"
and lambda_alloc_sync_alone = mk_lambda env_join "alloc_sync_alone"
and lambda_patch_sync_alone = mk_lambda env_join "patch_sync_alone"


(* Asynchronous sends *)
let lambda_send_async = mk_lambda env_join "send_async"
and lambda_tail_send_async = mk_lambda env_join "tail_send_async"

(* Optimized sends *)
let lambda_local_send_async = mk_lambda env_join "local_send_async"
and lambda_local_tail_send_async = mk_lambda env_join "local_tail_send_async"
and lambda_local_send_sync = mk_lambda env_join "local_send_sync"

let lambda_create_automaton = mk_lambda env_join "create_automaton"
let lambda_create_automaton_debug = mk_lambda env_join "create_automaton_debug"
let lambda_wrap_automaton = mk_lambda env_join "wrap_automaton"
let lambda_patch_table = mk_lambda env_join "patch_table"
let lambda_get_queue = mk_lambda env_join "get_queue"
let lambda_reply_to = mk_lambda env_join "reply_to"
let lambda_reply_to_exn = mk_lambda env_join "reply_to_exn"
let lambda_raise_join_exit =  mk_lambda env_join "raise_join_exit"

let mk_apply f args loc = match Lazy.force f with
| _,{val_kind=Val_prim p}  -> Lprim (Pccall p,args)
| path,_                   -> Lapply (transl_path path, args, loc)
  

let lambda_int i = Lconst (Const_base (Const_int i))
and lambda_string s = Lconst (Const_base (Const_string s))

let init_unit_queue auto idx =
  mk_apply lambda_init_unit_queue [Lvar auto ; lambda_int idx]

let create_process p loc =  mk_apply lambda_create_process [p] loc

let do_send send auto num arg =
 mk_apply send [Lvar auto ; lambda_int  num ; arg]

let create_async auto num =
  mk_apply lambda_create_async [Lvar auto ; lambda_int num]

and create_alone id  name =
  mk_apply lambda_create_alone [Lvar id ; lambda_string name]

and alloc_alone name = mk_apply lambda_alloc_alone [lambda_string name]

and patch_alone id g =
  mk_apply lambda_patch_alone [Lvar id ; Lvar g]

and send_async chan arg = mk_apply lambda_send_async [chan ; arg]

and tail_send_async chan arg = mk_apply lambda_tail_send_async [chan ; arg]

and local_send_async auto idx arg =
  mk_apply lambda_local_send_async [Lvar auto ; lambda_int idx ; arg]

and local_tail_send_async auto idx arg =
  mk_apply lambda_local_tail_send_async [Lvar auto ; lambda_int idx ; arg]

and local_tail_send_async2 auto idx arg =
  mk_apply lambda_local_tail_send_async [Lvar auto ; idx ; arg]
    Location.none

and local_send_sync auto idx arg =
  mk_apply lambda_local_send_sync [Lvar auto ; lambda_int idx ; arg]    

and local_send_sync2 auto idx arg =
  mk_apply lambda_local_send_sync [Lvar auto ; idx ; arg]
    Location.none

(* Those two are inlined *)
let local_tail_send_alone guard arg loc  = 
  Lapply (Lvar guard, [arg],loc)

let local_send_alone guard arg loc =
  create_process
    (Lfunction
       (Curried, [Ident.create "_x"],
	local_tail_send_alone guard arg Location.none))
    loc

let lambda_string s = Lconst (Const_base (Const_string s))

let create_sync auto num =
  mk_apply lambda_create_sync [Lvar auto ; lambda_int num]

and create_sync_alone id name =
  mk_apply lambda_create_sync_alone [Lvar id; lambda_string name]

let alloc_stub_guard =
  mk_apply lambda_alloc_stub_guard [lambda_unit]

and alloc_sync_alone id name =
  mk_apply lambda_alloc_sync_alone [Lvar id ; lambda_string name]

and patch_sync_alone id stub =
  mk_apply lambda_patch_sync_alone [Lvar id ; Lvar stub]

let create_automaton nchans names =
   mk_apply lambda_create_automaton_debug [lambda_int nchans ; names]

let wrap_automaton id = mk_apply lambda_wrap_automaton [Lvar id]

let reply_to lam1 lam2 = mk_apply lambda_reply_to [lam1; lam2]
and reply_to_exn exn kont =
  mk_apply lambda_reply_to_exn [Lvar exn ; Lvar kont]

let raise_join_exit = mk_apply lambda_raise_join_exit [lambda_unit]

let get_replies sync p =
  let reps = Typejoin.get_replies p in
  match reps, sync with
  | (id,_)::rem, Some oid when Ident.same id oid ->
      true, List.map fst rem
  | _, _ -> false, List.map fst reps

  
let do_spawn p loc =
 if p = lambda_unit then
   p
 else
   let param = Ident.create "_x" in
   create_process (Lfunction (Curried, [param], p)) loc

let do_get_queue auto num = mk_apply lambda_get_queue [auto ; lambda_int num]

(*

 All about synchronous threads.

 Synchronous threads are guarded processes, when one of matched names
 at least is synchronous.

 In such case the guarded process is compiled into a function,
 whose result is the answer to a distinguished synchronous name
 (principal name)

 The list of replied-to ports is computed by the typer.
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
| Texp_let (_,_,p) | Texp_def (_,p)
| Texp_sequence (_,p) | Texp_when (_,p) -> 
   is_principal id p
| Texp_match (_,(_,p)::cls,_) ->
   is_principal id p &&
   List.for_all (fun (_,p) -> is_principal id p) cls
| Texp_ifthenelse (_,pifso, Some pifno) ->
   is_principal id pifso && is_principal id pifno
| Texp_ifthenelse (_,_,None) -> false
| Texp_for (_,_,_,_,_,_) -> false
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

(* forward declaration, filled by Translcore *)
let simple_prim = ref ((fun p -> assert false) : Primitive.description -> bool)

let rec simple_pat p = match p.pat_desc with
| Tpat_any | Tpat_var _ -> true
| Tpat_alias (p,_,_)|Tpat_lazy p -> simple_pat p
| Tpat_tuple ps -> List.for_all simple_pat ps
| Tpat_record (lps,_) ->  List.for_all (fun (_,_,_,p) -> simple_pat p) lps
| Tpat_or (p1,p2,_) -> simple_pat p1 && simple_pat p2
| Tpat_constant _|Tpat_construct _|Tpat_variant _
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
| Texp_def (_,e) -> simple_exp e
(* Simple simple expressions *)
| Texp_ident _ | Texp_constant _ | Texp_function _
| Texp_variant (_,None) 
| Texp_instvar _ | Texp_setinstvar _ | Texp_spawn (_)
 -> true
(* Recursion *)
| Texp_construct (_,_,_,es,_) | Texp_tuple (es) | Texp_array (es)
 -> List.for_all simple_exp es
| Texp_variant (_, Some e) | Texp_field (e,_,_,_)
   -> simple_exp e
| Texp_setfield (e1,_,_,_,e2) -> simple_exp e1 && simple_exp e2
| Texp_apply ({exp_desc=Texp_ident (_,_,{val_kind=Val_prim p})}, args) ->
   List.length args < p.prim_arity || (* will be compiled as function *)
   (!simple_prim p &&
   List.for_all (fun (_,eo,_) -> simple_exp_option eo) args)
| Texp_apply (_,_) -> false
| Texp_for (_,_,e1,e2,_,e3) ->
   simple_exp e1 && simple_exp e2 && simple_exp e3
| Texp_record (les,eo) ->
   List.for_all (fun (_,_,_,e) -> simple_exp e) les &&
   simple_exp_option eo
(* Asserts are special *)
| Texp_assert e -> !Clflags.noassert || simple_exp e
| Texp_assertfalse -> !Clflags.noassert
(* Who knows ? *)
| Texp_letmodule _ | Texp_override (_,_) | Texp_lazy (_)
| Texp_send _ | Texp_while (_,_) | Texp_new _ | Texp_try (_,_)
| Texp_object _| Texp_pack _
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
   List.for_all (fun (_,e) -> simple_proc e) pps
| Texp_match (_,_,Partial) -> false
| Texp_ifthenelse (e, pifso, Some pifno) ->
   simple_exp e && simple_proc pifso && simple_proc pifno
| Texp_ifthenelse (e, pifso, None) ->
   simple_exp e && simple_proc pifso    
| Texp_def (_,p) -> simple_proc p
| Texp_for (_,_,e1,e2,_,_body) -> (* _body is compiled so a not to fail *)
   simple_exp e1 && simple_exp e2
(* Process constructs *)
| Texp_reply (e, _) -> simple_exp e
| Texp_par (p1, p2) -> simple_proc p1 || simple_proc p2
| Texp_asyncsend (e1, e2) -> simple_exp e1 && simple_exp e2
| Texp_null -> true
(* Plain expressions no longer are errors *)
| Texp_spawn _|Texp_object _|Texp_lazy _|Texp_assert _|
  Texp_letmodule _|Texp_override _|Texp_setinstvar _|
  Texp_instvar _|Texp_new _|Texp_send _|
  Texp_while _|Texp_array _|
  Texp_setfield _|Texp_field _|Texp_record _|
  Texp_variant _|Texp_construct _|Texp_tuple _|Texp_try _|
  Texp_apply _|Texp_function _|Texp_constant _|Texp_ident _|
  Texp_assertfalse|Texp_pack _
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
           (fun kid k -> Lsequence (reply_to_exn param kid Location.none, k))
           kids
           (if pri then Lprim (Praise, [Lvar param]) else
           raise_join_exit Location.none))

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

let rec get_chan_rec id = function
 | [] -> raise Not_found
 | (oid,x)::rem ->
     if Ident.same id oid then x else get_chan_rec id rem

open Printf

let dump_chan fp chan =
  fprintf fp "{id=%s, sync=%b}"
    (Ident.unique_name chan.jchannel_ident)
    chan.jchannel_sync

let compile_auto a =
(*
  eprintf "AUTO:" ;
  List.iter (fun (_,chan) -> eprintf " %a" dump_chan chan) a.jauto_names ;
  eprintf "\n%!" ;
*)
  let get_chan tag id =
    try get_chan_rec id a.jauto_names
    with Not_found ->
      fatal_error
        (Printf.sprintf "Transljoin.compile_auto [%s]: %s" tag
           (Ident.unique_name id)) in
          
  let (disps, reacs), new_names = 
    Joinmatching.compile a.jauto_loc
      (List.map
         (fun cl ->
           let jpats,g = cl.jclause_desc in
           cl.jclause_loc,jpats,g)
         a.jauto_desc) in
(* Allocate new names for dispatchers and guarded processes *)
  let disps =  List.map (fun disp -> Ident.create "#d#", disp) disps
  and reacs,fwds = match reacs with
  | [((_,[_],_),_) as reac] -> (* Case of (single) forwarder *)
      [],[Ident.create "#f", reac]
  | _ ->
      List.map (fun reac -> Ident.create "#g#", reac) reacs, [] in
(* Automaton names *)
   let name = Ident.create "#auto#"
   and name_wrapped = Ident.create "#wrapped#" in
(* Collect forwarders *)
  let fwd_names =
    List.map
      (fun (d_id,(id,_,_)) ->
        let chan = get_chan "dispatchers" id in
        id,(chan,(Alone d_id)))
      disps in
  let fwd_names =
    List.fold_right
      (fun fwd r ->
        let g_id,(_old,(patss,_gd)) = fwd in
         List.fold_right
          (fun pats r ->
            List.fold_right
              (fun pat r ->
                let jid,_ = pat.jpat_desc in
                let id = jid.jident_desc in
                let chan = get_chan "forwarders" id in
                (id,(chan,Alone g_id))::r)
              pats r)
          patss r)
      fwds fwd_names in
(* Collect channels *)
  let is_fwd id =
    try ignore(get_chan_rec id fwd_names) ; true
    with Not_found -> false in

(* First orignal names unsplit by compilation, and not a forwarder *)
  let all_names,nchans =
    List.fold_right
      (fun (id,chan) (chans,nchans as k) ->
        if is_fwd id then k
        else
          (id,(chan,Chan (name,nchans)))::chans,nchans+1)
      a.jauto_names (fwd_names,0) in
(* Then new names, introduced by pattern matching compilation *)
  let all_names,nchans =
     List.fold_right
      (fun (id,ids) k ->
        let chan = get_chan "old_name" id in
        List.fold_right
          (fun id (chans,nchans) ->
            let chan = { chan with jchannel_ident = id} in
            (id,(chan,Chan (name,nchans)))::chans,nchans+1)
          ids k)
      new_names (all_names,nchans) in
(* Original names, with information *)
  let is_orig id =
     try ignore(get_chan_rec id a.jauto_names) ; true
    with Not_found -> false in
  let original = List.filter (fun (id,_) -> is_orig id) all_names in
(* Precompile dispatchers *)
  let disps =
    let find_channel id =
      try get_chan_rec id all_names
      with Not_found ->
        fatal_error
        (Printf.sprintf "find_channel: %s"
           (Ident.unique_name id)) in
    List.map
      (fun disp ->
        let d_id,(chan_id, cls, par) = disp in
        let chan = find_channel chan_id
        and cls = List.map (fun (p,id) -> p,find_channel id) cls in
        d_id,chan,cls,par)
      disps in
(* Precompile reactions, little to do: flatten *)
  let precomp_reac (id,((_loc,pat,e),(pats,bv))) = id,pat,pats,bv,e in
  let fwds = List.map precomp_reac fwds
  and reacs = List.map precomp_reac reacs in
(* Gather everything... *)
  {
    cauto_name = name,name_wrapped;
    cauto_channels = all_names;
    cauto_nchans = nchans;
    cauto_original = original;
    cauto_loc = a.jauto_loc;
    cauto_dispatchers = disps;
    cauto_forwarders = fwds;
    cauto_reactions = reacs;
  }
let get_num msg names id =
 try
   let _,opt = get_chan_rec id names in
   match opt with
   | Chan (_,num) -> num
   | _ ->
     fatal_error
       (Printf.sprintf "Transljoin.get_num: %s is a forwarder"
          (Ident.unique_name id))       
 with
 | Not_found ->
     fatal_error
       (Printf.sprintf "Transljoin.get_num: %s" (Ident.unique_name id))

let get_chan msg names id =
  try
   let _,opt = get_chan_rec id names in
   opt
 with
 | Not_found ->
     fatal_error
       (Printf.sprintf "Transljoin.get_chan: %s" (Ident.unique_name id))



let patch_table auto t =
 mk_apply
   lambda_patch_table
   [Lvar auto ; Lprim (Pmakeblock (0,Immutable), t)]

let rec principal_param ipri params nums = match params, nums with
| param::params, num::nums ->
   if num=ipri then param
   else principal_param ipri params nums
| _,_ -> assert false

let names_block nchans names =
  let t = Array.create nchans "" in
  List.iter
    (fun (id, (_,opt)) ->
      match opt with
      | Chan (_,i) -> t.(i) <- Ident.unique_name id
      | _ -> ())
    names ;
  Lconst
    (Const_block
       (0, Array.fold_right (fun s r -> Const_base (Const_string s)::r) t []))

let create_auto
   { cauto_name=(auto_name, wrapped_name);
     cauto_channels = names ; cauto_nchans=nchans ; } k =
  if nchans > 0 then
    Llet
      (Strict, auto_name,
       create_automaton
	 nchans (names_block nchans names)
	 Location.none,
       Llet
         (Strict, wrapped_name, wrap_automaton auto_name Location.none, k))
  else k

let create_channels {cauto_name=(raw_name, name) ; cauto_channels=names} k =
  List.fold_right
    (fun (id,(jc,opt)) k ->
      match opt with
      | Chan (_,num) ->
	  Llet
	    (StrictOpt, id,
	     (if jc.jchannel_sync then
               create_sync
	     else
               create_async) name num Location.none,
	     if
               Typeopt.is_unit_channel_type
                 jc.jchannel_type
                 jc.jchannel_env
	     then
               Lsequence (init_unit_queue raw_name num Location.none, k)
	     else k)
      | _ -> k)
    names k

let create_dispatchers disps k =
  List.fold_right
    (fun (id,chan,lam) k ->
      Llet
        (StrictOpt, id, lam,
	 let name = Ident.unique_name chan.jchannel_ident in
         Llet
           (StrictOpt, chan.jchannel_ident,
            (if chan.jchannel_sync then create_sync_alone
	    else create_alone) id name Location.none, k)))
    disps k

let make_g caller chan g =
(*  Printf.eprintf "make_g %s <%s>\n"
    caller (Ident.unique_name chan.jchannel_ident) ; *)
  if chan.jchannel_sync then
    chan.jchannel_ident, (Some (Ident.create "#stub"),g)
  else
    chan.jchannel_ident, (None,g)

let create_forwarders autos dispss fwdss r =
  (* collect all pairs channel ident X (stub_ident option X guard_ident) *)
  let id2g = [] in
(*
    List.fold_right
      (fun disps r ->
        List.fold_right
          (fun (g,chan,_) r ->
            make_g "disp" chan g::r)
          disps r)
      dispss [] in
*)
  let id2g =
    List.fold_right
      (fun auto r ->
        List.fold_right
          (fun (_,(chan,opt)) r -> match opt with
          | Alone g -> make_g "fwd" chan g::r
          | Chan (_,_) -> r)
          auto.cauto_channels r)
      autos id2g in
(* patch forwarder data structure *)
   let r =
     List.fold_right
      (fun (id,(sync,g)) r ->
        match sync with
        | Some stub ->
            Lsequence (patch_sync_alone stub g Location.none, r)
        | None ->
            Lsequence (patch_alone id g Location.none, r))
       id2g r in
(* Big let rec of guards *)
   let d =
     List.fold_right
       (fun disps d ->
         List.fold_right
           (fun disp d -> let (x,_,lam) = disp in (x,lam)::d)
           disps d)
       dispss [] in
   let d =
     List.fold_right
       (fun fwds d ->
         List.fold_right (fun fwd d -> fwd::d) fwds d)
       fwdss d in
   let r = Lletrec (d, r) in
(* Allocate forwarders *)
   let r =
     List.fold_right
       (fun (id,(sync,_)) r ->
         match sync with
         | Some stub ->
             Llet
               (Strict, stub, alloc_stub_guard Location.none,
                Llet
		  (StrictOpt,
		   id,
		   alloc_sync_alone stub (Ident.unique_name id) Location.none,
		   r))
         |  None ->
             Llet
	       (StrictOpt,
		id,alloc_alone (Ident.unique_name id) Location.none,
		r))
       id2g r in
   r
             


               
let get_queue names jpat =
 let jid,_ = jpat.jpat_desc in
 let id = jid.jident_desc in
 let x = get_chan "(get_queue)" names id in
 match x with
 | Alone _ -> assert false
 | Chan (name, i) ->
     let k = !(jpat.jpat_kont) in
     match k with
     | None ->
         None, do_get_queue (Lvar name) i Location.none
     | Some kid ->
         let y = Ident.create "_y" in
         Some y, do_get_queue (Lvar name) i Location.none



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

  Lconst
    (Const_block
       (0,
        List.map (fun i -> Const_base (Const_int i))
          (do_rec (empty nslots) jpats)))

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

(* 3.10 -> 3.11, a third argument 'Location.t' appeared here,
   just pretend it is not useful at the moment *)

let lapply (f,args) = Lapply (f,args,Location.none)

(* gs is a list of compiled guarded processes *)

let create_table auto gs r =
  let n_chans = auto.cauto_nchans in
  if n_chans =0 then r
  else
    let name,_ = auto.cauto_name       (* wrapped name of automaton *)
    and names = auto.cauto_channels in (* all channels *)

    let rec do_guard reac (_, sync, _) k =
      let (g, _, actual, _, _) = reac in

      let create_reaction jpats r =
        let ipri = match sync with
        | None -> -1
        | Some _ ->
            let rec find_rec = function
              | [] -> -1
              | jpat::rem ->
                  if !(jpat.jpat_kont) = sync then
                    let jid,_ = jpat.jpat_desc in
                    get_num "(real_ipri)" names jid.jident_desc
                  else
                    find_rec rem in
            find_rec jpats in
        
        let bds = List.map (get_queue names) jpats in
        let args =
          List.fold_right2
            (fun bd jpat r -> match bd with
            | None,lam -> lam::r
            | Some y,_ ->
                if !(jpat.jpat_kont) = sync then
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
                 (lapply
                    (Lvar goid, [Lvar name ; lapply (Lvar g, args)])))
          else
            let pri_kont =
              let rec find_rec bds jpats = match bds, jpats with
              | (Some y,_)::bds, jpat::jpats
                                          when !(jpat.jpat_kont) = sync ->
                                            Lprim (Pfield 0, [Lvar y])
              | _::bds, _::jpats ->
                  find_rec bds jpats
              | _, _ -> assert false in
              find_rec bds jpats in
            Lfunction
              (Curried, [goid],
               build_lets bds
                 (lapply
                    (Lvar goid, [pri_kont ; lapply (Lvar g, args)]))) in
        Lprim
          (Pmakeblock (0, Immutable),
           [build_mask n_chans names jpats ;
             lambda_int ipri ; real_g])::r in

      let pats = explode actual in
      List.fold_right create_reaction pats k in

    let reacs = auto.cauto_reactions in
    Lsequence
      (patch_table name
         (List.fold_right2 do_guard reacs gs []) Location.none,
       r)

(*********************)
(* Global exceptions *)
(*********************)

let lambda_exn_global =  mk_lambda env_join "exn_global"

(* "exn_global" takes a location as a first argument,
   so as give a source position in case of failure *)

let transl_exn_global loc path =
  mk_apply
    lambda_exn_global [transl_location loc ; transl_path path]
    Location.none
