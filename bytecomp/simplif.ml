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
  | Lapply(e1, el, loc) -> 
      Lapply(eliminate_ref id e1, List.map (eliminate_ref id) el, loc)
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
         sw_failaction = match sw.sw_failaction with
         | None -> None
         | Some l -> Some (eliminate_ref id l)})
  | Lstaticraise (i,args) ->
      Lstaticraise (i,List.map (eliminate_ref id) args)
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
  | Lsend(k, m, o, el) ->
      Lsend(k, eliminate_ref id m, eliminate_ref id o,
            List.map (eliminate_ref id) el)
  | Levent(l, ev) ->
      Levent(eliminate_ref id l, ev)
  | Lifused(v, e) ->
      Lifused(v, eliminate_ref id e)

(* Simplification of exits *)

let simplify_exits lam = 

  (* Count occurrences of (exit n ...) statements *)
  let exits = Hashtbl.create 17 in

  let count_exit i =
    try
      !(Hashtbl.find exits i)
    with
    | Not_found -> 0

  and incr_exit i =
    try
      incr (Hashtbl.find exits i)
    with
    | Not_found -> Hashtbl.add exits i (ref 1) in
  
  let rec count = function
  | (Lvar _| Lconst _) -> ()
  | Lapply(l1, ll, _) -> count l1; List.iter count ll
  | Lfunction(kind, params, l) -> count l
  | Llet(str, v, l1, l2) ->
      count l2; count l1
  | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count l) bindings;
      count body
  | Lprim(p, ll) -> List.iter count ll
  | Lswitch(l, sw) ->
      count_default sw ;
      count l;
      List.iter (fun (_, l) -> count l) sw.sw_consts;
      List.iter (fun (_, l) -> count l) sw.sw_blocks
  | Lstaticraise (i,ls) -> incr_exit i ; List.iter count ls
  | Lstaticcatch (l1,(i,[]),Lstaticraise (j,[])) ->
      (* i will be replaced by j in l1, so each occurence of i in l1
         increases j's ref count *)
      count l1 ;
      let ic = count_exit i in
      begin try
        let r = Hashtbl.find exits j in r := !r + ic
      with
      | Not_found ->
          Hashtbl.add exits j (ref ic)
      end
  | Lstaticcatch(l1, (i,_), l2) ->
      count l1;
      (* If l1 does not contain (exit i),
         l2 will be removed, so don't count its exits *)
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
  | Lsend(k, m, o, ll) -> List.iter count (m::o::ll)
  | Levent(l, _) -> count l
  | Lifused(v, l) -> count l

  and count_default sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count al ; count al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count al
      end
  in
  count lam;

  (*
     Second pass simplify  ``catch body with (i ...) handler''
      - if (exit i ...) does not occur in body, suppress catch
      - if (exit i ...) occurs exactly once in body,
        substitute it with handler
      - If handler is a single variable, replace (exit i ..) with it
   Note:
    In ``catch body with (i x1 .. xn) handler''
     Substituted expression is
      let y1 = x1 and ... yn = xn in
      handler[x1 <- y1 ; ... ; xn <- yn]
     For the sake of preserving the uniqueness  of bound variables.
     (No alpha conversion of ``handler'' is presently needed, since
     substitution of several ``(exit i ...)''
     occurs only when ``handler'' is a variable.)
  *)

  let subst = Hashtbl.create 17 in

  let rec simplif = function
  | (Lvar _|Lconst _) as l -> l
  | Lapply(l1, ll, loc) -> Lapply(simplif l1, List.map simplif ll, loc)
  | Lfunction(kind, params, l) -> Lfunction(kind, params, simplif l)
  | Llet(kind, v, l1, l2) -> Llet(kind, v, simplif l1, simplif l2)
  | Lletrec(bindings, body) ->
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> Lprim(p, List.map simplif ll)
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = match sw.sw_failaction with
      | None -> None
      | Some l -> Some (simplif l) in
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstaticraise (i,[]) as l ->
      begin try
        let _,handler =  Hashtbl.find subst i in
        handler
      with
      | Not_found -> l
      end
  | Lstaticraise (i,ls) ->
      let ls = List.map simplif ls in
      begin try
        let xs,handler =  Hashtbl.find subst i in
        let ys = List.map Ident.rename xs in
        let env =
          List.fold_right2
            (fun x y t -> Ident.add x (Lvar y) t)
            xs ys Ident.empty in
        List.fold_right2
          (fun y l r -> Llet (Alias, y, l, r))
          ys ls (Lambda.subst_lambda env handler)
      with
      | Not_found -> Lstaticraise (i,ls)
      end
  | Lstaticcatch (l1,(i,[]),(Lstaticraise (j,[]) as l2)) ->
      Hashtbl.add subst i ([],simplif l2) ;
      simplif l1
  | Lstaticcatch (l1,(i,xs), (Lvar _ as l2)) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | _ ->
          Hashtbl.add subst i (xs,l2) ;
          simplif l1
      end
  | Lstaticcatch (l1,(i,xs),l2) ->
      begin match count_exit i with
      | 0 -> simplif l1
      | 1 ->
          Hashtbl.add subst i (xs,simplif l2) ;
          simplif l1
      | _ ->
          Lstaticcatch (simplif l1, (i,xs), simplif l2)
      end
  | Ltrywith(l1, v, l2) -> Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence(l1, l2) -> Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lassign(v, l) -> Lassign(v, simplif l)
  | Lsend(k, m, o, ll) -> Lsend(k, simplif m, simplif o, List.map simplif ll)
  | Levent(l, ev) -> Levent(simplif l, ev)
  | Lifused(v, l) -> Lifused (v,simplif l)
  in
  simplif lam

(* Simplification of lets *)

let simplify_lets lam =

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

  let rec count = function
  | Lvar v -> incr_var v
  | Lconst cst -> ()
  | Lapply(l1, ll, _) -> count l1; List.iter count ll
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
      count_default sw ;
      count l;
      List.iter (fun (_, l) -> count l) sw.sw_consts;
      List.iter (fun (_, l) -> count l) sw.sw_blocks
  | Lstaticraise (i,ls) -> List.iter count ls
  | Lstaticcatch(l1, (i,_), l2) ->
      count l1; count l2
  | Ltrywith(l1, v, l2) -> count l1; count l2
  | Lifthenelse(l1, l2, l3) -> count l1; count l2; count l3
  | Lsequence(l1, l2) -> count l1; count l2
  | Lwhile(l1, l2) -> count l1; count l2
  | Lfor(_, l1, l2, dir, l3) -> count l1; count l2; count l3
  | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count l
  | Lsend(_, m, o, ll) -> List.iter count (m::o::ll)
  | Levent(l, _) -> count l
  | Lifused(v, l) ->
      if count_var v > 0 then count l

  and count_default sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count al ; count al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count al
      end
  in
  count lam;
  (* Second pass: remove Lalias bindings of unused variables,
     and substitute the bindings of variables used exactly once. *)

  let subst = Hashtbl.create 83 in

  let rec simplif = function
    Lvar v as l ->
      begin try
        Hashtbl.find subst v
      with Not_found ->
        l
      end
  | Lconst cst as l -> l
  | Lapply(l1, ll, loc) -> Lapply(simplif l1, List.map simplif ll, loc)
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
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = match sw.sw_failaction with
      | None -> None
      | Some l -> Some (simplif l) in
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstaticraise (i,ls) ->
      Lstaticraise (i, List.map simplif ls)   
  | Lstaticcatch(l1, (i,args), l2) ->
      Lstaticcatch (simplif l1, (i,args), simplif l2)
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
  | Lsend(k, m, o, ll) -> Lsend(k, simplif m, simplif o, List.map simplif ll)
  | Levent(l, ev) -> Levent(simplif l, ev)
  | Lifused(v, l) ->
      if count_var v > 0 then simplif l else lambda_unit
  in
  simplif lam

let simplify_lambda lam = simplify_lets (simplify_exits lam)
