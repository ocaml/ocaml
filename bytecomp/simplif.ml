(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Elimination of useless Llet(Alias) bindings.
   Also transform let-bound references into variables. *)

open Asttypes
open Lambda

(* To transform let-bound references into variables *)

exception Real_reference

let rec eliminate_ref id = function
    Lvar v as lam ->
      if Ident.same v id then raise Real_reference else lam
  | Lconst cst as lam -> lam
  | Lapply(e1, el) -> 
      Lapply(eliminate_ref id e1, List.map (eliminate_ref id) el)
  | Lfunction(kind, params, body) as lam ->
      if IdentSet.mem id (free_variables lam)
      then raise Real_reference
      else lam
  | Llet(str, v, e1, e2) ->
      Llet(str, v, eliminate_ref id e1, eliminate_ref id e2)
  | Lletrec(idel, e2) ->
      Lletrec(List.map (fun (v, e) -> (v, eliminate_ref id e)) idel,
              eliminate_ref id e2)
  | Lprim(Pfield 0, [Lvar v]) when Ident.same v id ->
      Lvar id
  | Lprim(Psetfield(0, _), [Lvar v; e]) when Ident.same v id ->
      Lassign(id, eliminate_ref id e)
  | Lprim(Poffsetref delta, [Lvar v]) when Ident.same v id ->
      Lassign(id, Lprim(Poffsetint delta, [Lvar id]))
  | Lprim(p, el) ->
      Lprim(p, List.map (eliminate_ref id) el)
  | Lswitch(e, sw) ->
      Lswitch(eliminate_ref id e,
        {sw_numconsts = sw.sw_numconsts;
         sw_consts =
            List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_consts;
         sw_numblocks = sw.sw_numblocks;
         sw_blocks =
            List.map (fun (n, e) -> (n, eliminate_ref id e)) sw.sw_blocks;
         sw_checked = sw.sw_checked; sw_nofail = sw.sw_nofail})
  | Lstaticfail as l -> l
  | Lstaticraise _ as l -> l
  | Lcatch(e1, e2) ->
      Lcatch(eliminate_ref id e1, eliminate_ref id e2)
  | Lstaticcatch(e1, i, e2) ->
      Lstaticcatch(eliminate_ref id e1, i, eliminate_ref id e2)
  | Ltrywith(e1, v, e2) ->
      Ltrywith(eliminate_ref id e1, v, eliminate_ref id e2)
  | Lifthenelse(e1, e2, e3) ->
      Lifthenelse(eliminate_ref id e1,
                  eliminate_ref id e2,
                  eliminate_ref id e3)
  | Lsequence(e1, e2) ->
      Lsequence(eliminate_ref id e1, eliminate_ref id e2)
  | Lwhile(e1, e2) ->
      Lwhile(eliminate_ref id e1, eliminate_ref id e2)
  | Lfor(v, e1, e2, dir, e3) ->
      Lfor(v, eliminate_ref id e1, eliminate_ref id e2,
           dir, eliminate_ref id e3)
  | Lassign(v, e) ->
      Lassign(v, eliminate_ref id e)
  | Lsend(m, o, el) ->
      Lsend(eliminate_ref id m, eliminate_ref id o,
            List.map (eliminate_ref id) el)
  | Levent(l, ev) ->
      Levent(eliminate_ref id l, ev)
  | Lifused(v, e) ->
      Lifused(v, eliminate_ref id e)


(* Simplification of lets *)

let simplify_lambda lam =

  (* First pass: count the occurrences of all identifiers *)
  let occ = Hashtbl.create 83 in
  let count_var v =
    try
      !(Hashtbl.find occ v)
    with Not_found ->
      0
  and incr_var v = 
    try
      incr(Hashtbl.find occ v)
    with Not_found ->
      Hashtbl.add occ v (ref 1) in

  (* Also count occurrences of (exit n) statements with no arguments *)
  let exits = Hashtbl.create 17 in
  let count_exit i =
    try
      !(Hashtbl.find exits i)
    with
    | Not_found -> 0
  and incr_exit i =
    try
      incr(Hashtbl.find exits i)
    with
    | Not_found -> Hashtbl.add exits i (ref 1) in
  (* And occurences of Lstaticfail, in every staticcatch scope *)
  let count_fail = ref (ref 0) in
  let at_catch = ref [] in
  
  
  let rec count = function
  | Lvar v -> incr_var v
  | Lconst cst -> ()
  | Lapply(l1, ll) -> count l1; List.iter count ll
  | Lfunction(kind, params, l) -> count l
  | Llet(str, v, Lvar w, l2) when not !Clflags.debug ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count l2;
      let vc = count_var v in
      begin try
        let r = Hashtbl.find occ w in r := !r + vc
      with Not_found ->
        Hashtbl.add occ w (ref vc)
      end
  | Llet(str, v, l1, l2) ->
      count l2;
      (* If v is unused, l1 will be removed, so don't count its variables *)
      if str = Strict || count_var v > 0 then count l1
  | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count l) bindings;
      count body
  | Lprim(p, ll) -> List.iter count ll
  | Lswitch(l, sw) ->
      (* switch may generate Lstaticfail *)
      if
        (not sw.sw_nofail) &&
        (sw.sw_numconsts > List.length sw.sw_consts ||
        sw.sw_numblocks > List.length sw.sw_blocks)
      then
        !count_fail := !(!count_fail) + 2 ;
      count l;
      List.iter (fun (n, l) -> count l) sw.sw_consts;
      List.iter (fun (n, l) -> count l) sw.sw_blocks ;
  | Lstaticfail -> incr !count_fail
  | Lstaticraise (i,ls) -> incr_exit i ; List.iter count ls
  | Lcatch(l1, l2) as l ->
      let save_count_fail = !count_fail in
      count_fail := ref 0 ;                                      
      count l1;
      let this_count = !(!count_fail) in
      at_catch := (l,!(!count_fail)) :: !at_catch ;
      count_fail := save_count_fail ;
      (* If l1 does not contain staticfail,
         l2 will be removed, so don't count its variables *)
      if this_count > 0 then
        count l2
  | Lstaticcatch(l1, (i,_), l2) ->
      count l1;
      (* If l1 does not contain (exit i),
         l2 will be removed, so don't count its variables *)
      if count_exit i > 0 then
        count l2
  | Ltrywith(l1, v, l2) -> count l1; count l2
  | Lifthenelse(l1, l2, l3) -> count l1; count l2; count l3
  | Lsequence(l1, l2) -> count l1; count l2
  | Lwhile(l1, l2) -> count l1; count l2
  | Lfor(_, l1, l2, dir, l3) -> count l1; count l2; count l3
  | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count l
  | Lsend(m, o, ll) -> List.iter count (m::o::ll)
  | Levent(l, _) -> count l
  | Lifused(v, l) ->
      if count_var v > 0 then count l
  in
  count lam;
  (* Second pass: remove Lalias bindings of unused variables,
     and substitute the bindings of variables used exactly once. *)
  (* Also treat  ``catch body with (i) handler''
      - if (exit i) does not occur in body, suppress catch
      - if (exit i) occurs exactly once in body,
        substitute it with handler *)  
  let subst = Hashtbl.create 83
  and subst_exit = Hashtbl.create 17
  and subst_fail = ref Lstaticfail in

  let rec simplif = function
    Lvar v as l ->
      begin try
        Hashtbl.find subst v
      with Not_found ->
        l
      end
  | Lconst cst as l -> l
  | Lapply(l1, ll) -> Lapply(simplif l1, List.map simplif ll)
  | Lfunction(kind, params, l) -> Lfunction(kind, params, simplif l)
  | Llet(str, v, Lvar w, l2) when not !Clflags.debug ->
      Hashtbl.add subst v (simplif (Lvar w));
      simplif l2
  | Llet(Strict, v, Lprim(Pmakeblock(0, Mutable), [linit]), lbody)
    when not !Clflags.debug ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin try
        Llet(Variable, v, slinit, eliminate_ref v slbody)
      with Real_reference ->
        Llet(Strict, v, Lprim(Pmakeblock(0, Mutable), [slinit]), slbody)
      end
  | Llet(Alias, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | 1 when not !Clflags.debug ->
             Hashtbl.add subst v (simplif l1); simplif l2
      | n -> Llet(Alias, v, simplif l1, simplif l2)
      end
  | Llet(StrictOpt, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | n -> Llet(Alias, v, simplif l1, simplif l2)
      end
  | Llet(kind, v, l1, l2) -> Llet(kind, v, simplif l1, simplif l2)
  | Lletrec(bindings, body) ->
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> Lprim(p, List.map simplif ll)
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks in
      Lswitch
        (new_l,{sw with sw_consts = new_consts ; sw_blocks = new_blocks})
  | Lstaticfail as l -> !subst_fail
  | Lstaticraise (i,[]) as l ->
      begin try
        Hashtbl.find subst_exit i
      with
      | Not_found -> l
      end
  | Lstaticraise (i,ls) ->
      Lstaticraise (i, List.map simplif ls)         
  | Lcatch(l1, l2) as l ->
      let nfail =
        try
          List.assq l !at_catch
        with
        | Not_found -> Misc.fatal_error "Simplif: catch" in
      begin match nfail with
      | 0 -> simplif l1
      | 1 ->
          let new_l2 = simplif l2 in
          let save_subst_fail = !subst_fail in
          subst_fail := new_l2 ;
          let r = simplif l1 in
          subst_fail := save_subst_fail ;
          r
      | _ ->
          let save_subst_fail = !subst_fail in
          subst_fail := Lstaticfail ;
          let r = simplif l1 in
          subst_fail := save_subst_fail ;
          Lcatch (r,simplif l2)
      end
  | Lstaticcatch (l1,(i,[]),l2) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | 1 ->
          Hashtbl.add subst_exit i (simplif l2) ;
          simplif l1
      | _ ->
          Lstaticcatch (simplif l1, (i,[]), simplif l2)
      end
  | Lstaticcatch(l1, (i,args), l2) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | _ ->
          Lstaticcatch (simplif l1, (i,args), simplif l2)
      end
  | Ltrywith(l1, v, l2) -> Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence(Lifused(v, l1), l2) ->
      if count_var v > 0
      then Lsequence(simplif l1, simplif l2)
      else simplif l2
  | Lsequence(l1, l2) -> Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lassign(v, l) -> Lassign(v, simplif l)
  | Lsend(m, o, ll) -> Lsend(simplif m, simplif o, List.map simplif ll)
  | Levent(l, ev) -> Levent(simplif l, ev)
  | Lifused(v, l) ->
      if count_var v > 0 then simplif l else lambda_unit
  in
  simplif lam
