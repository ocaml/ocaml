(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
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
  | Lfunction(param, body) as lam ->
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
  | Lswitch(e, n1, cases1, n2, cases2) ->
      Lswitch(eliminate_ref id e,
        n1, List.map (fun (n, e) -> (n, eliminate_ref id e)) cases1,
        n2, List.map (fun (n, e) -> (n, eliminate_ref id e)) cases2)
  | Lstaticfail ->
      Lstaticfail
  | Lcatch(e1, e2) ->
      Lcatch(eliminate_ref id e1, eliminate_ref id e2)
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
  | Lshared(e, lbl) ->
      Lshared(eliminate_ref id e, lbl)
  | Lassign(v, e) ->
      Lassign(v, eliminate_ref id e)

(* Simplification of lets *)

let simplify_lambda lam =
  (* First pass: count the occurrences of all identifiers *)
  let occ = Hashtbl.new 83 in
  let count_var v =
    try
      !(Hashtbl.find occ v)
    with Not_found ->
      0 in
  let rec count = function
    Lvar v ->
      begin try
        incr(Hashtbl.find occ v)
      with Not_found ->
        Hashtbl.add occ v (ref 1)
      end
  | Lconst cst -> ()
  | Lapply(l1, ll) -> count l1; List.iter count ll
  | Lfunction(v, l) -> count l
  | Llet(str, v, Lvar w, l2) ->
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
      if str = Strict or count_var v > 0 then count l1
  | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count l) bindings;
      count body
  | Lprim(p, ll) -> List.iter count ll
  | Lswitch(l, n1, cases1, n2, cases2) ->
      count l;
      List.iter (fun (n, l) -> count l) cases1;
      List.iter (fun (n, l) -> count l) cases2
  | Lstaticfail -> ()
  | Lcatch(l1, l2) -> count l1; count l2
  | Ltrywith(l1, v, l2) -> count l1; count l2
  | Lifthenelse(l1, l2, l3) -> count l1; count l2; count l3
  | Lsequence(l1, l2) -> count l1; count l2
  | Lwhile(l1, l2) -> count l1; count l2
  | Lfor(v, l1, l2, dir, l3) -> count l1; count l2; count l3
  | Lshared(l, lblref) -> count l
  | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count l
  in
  count lam;
  (* Second pass: remove Lalias bindings of unused variables,
     and substitute the bindings of variables used exactly once. *)
  let subst = Hashtbl.new 83 in
  let rec simplif = function
    Lvar v as l ->
      begin try
        Hashtbl.find subst v
      with Not_found ->
        l
      end
  | Lconst cst as l -> l
  | Lapply(l1, ll) -> Lapply(simplif l1, List.map simplif ll)
  | Lfunction(v, l) -> Lfunction(v, simplif l)
  | Llet(str, v, Lvar w, l2) ->
      Hashtbl.add subst v (simplif (Lvar w));
      simplif l2
  | Llet(Strict, v, Lprim(Pmakeblock(0, Mutable), [linit]), lbody) ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin try
        Llet(Strict, v, slinit, eliminate_ref v slbody)
      with Real_reference ->
        Llet(Strict, v, Lprim(Pmakeblock(0, Mutable), [slinit]), slbody)
      end
  | Llet(Strict, v, l1, l2) -> Llet(Strict, v, simplif l1, simplif l2)
  | Llet(Alias, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | 1 -> Hashtbl.add subst v (simplif l1); simplif l2
      | n -> Llet(Alias, v, simplif l1, simplif l2)
      end
  | Lletrec(bindings, body) ->
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> Lprim(p, List.map simplif ll)
  | Lswitch(l, n1, cases1, n2, cases2) ->
      Lswitch(simplif l, n1, List.map (fun (n, l) -> (n, simplif l)) cases1,
                         n2, List.map (fun (n, l) -> (n, simplif l)) cases2)
  | Lstaticfail -> Lstaticfail
  | Lcatch(l1, l2) -> Lcatch(simplif l1, simplif l2)
  | Ltrywith(l1, v, l2) -> Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence(l1, l2) -> Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lshared(l, lblref) -> Lshared(simplif l, lblref)
  | Lassign(v, l) -> Lassign(v, simplif l)
  in
  simplif lam

    
    
