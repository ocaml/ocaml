(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

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
         sw_failaction =
            Misc.may_map (eliminate_ref id) sw.sw_failaction; })
  | Lstringswitch(e, sw, default) ->
      Lstringswitch
        (eliminate_ref id e,
         List.map (fun (s, e) -> (s, eliminate_ref id e)) sw,
         Misc.may_map (eliminate_ref id) default)
  | Lstaticraise (i,args) ->
      Lstaticraise (i,List.map (eliminate_ref id) args)
  | Lstaticcatch(body, handlers) ->
      let handlers = List.map (fun (n, ids, handler) ->
          n, ids, eliminate_ref id handler)
          handlers in
      Lstaticcatch(eliminate_ref id body, handlers)
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
  | Lsend(k, m, o, el, loc) ->
      Lsend(k, eliminate_ref id m, eliminate_ref id o,
            List.map (eliminate_ref id) el, loc)
  | Levent(l, ev) ->
      Levent(eliminate_ref id l, ev)
  | Lifused(v, e) ->
      Lifused(v, eliminate_ref id e)

(* Simplification of exits *)

module StexnSet = Set.Make(
  struct
    type t = stexn
    let compare (x:stexn) (y:stexn) = (x:>int) - (y:>int)
  end)

let simplify_exits lam =

  (* Count occurrences of (exit n ...) statements *)
  let exits = Hashtbl.create 17 in
  let non_substituable = Hashtbl.create 17 in

  let count_exit i =
    try
      !(Hashtbl.find exits i)
    with
    | Not_found -> 0

  and is_non_substituable i =
    try
      Hashtbl.find non_substituable i
    with
    | Not_found -> false

  and incr_exit i =
    try
      incr (Hashtbl.find exits i)
    with
    | Not_found -> Hashtbl.add exits i (ref 1)

  and non_substituable i =
    Hashtbl.replace non_substituable i true
  in

  let no_tail = StexnSet.empty in

  let rec count tail = function
  | (Lvar _| Lconst _) -> ()
  | Lapply(l1, ll, _) -> count no_tail l1; List.iter (count no_tail) ll
  | Lfunction(kind, params, l) -> count no_tail l
  | Llet(str, v, l1, l2) ->
      count tail l2; count no_tail l1
  | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count no_tail l) bindings;
      count tail body
  | Lprim(p, ll) -> List.iter (count no_tail) ll
  | Lswitch(l, sw) ->
      count_default tail sw ;
      count no_tail l;
      List.iter (fun (_, l) -> count tail l) sw.sw_consts;
      List.iter (fun (_, l) -> count tail l) sw.sw_blocks
  | Lstringswitch(l, sw, d) ->
      count no_tail l;
      List.iter (fun (_, l) -> count tail l) sw;
      begin match  d with
      | None -> ()
      | Some d -> match sw with
        | []|[_] -> count tail d
        | _ -> count tail d; count tail d (* default will get replicated *)
      end
  | Lstaticraise (stexn,ls) ->
      begin match stexn with
      | i ->
          incr_exit i;
          if not (StexnSet.mem i tail)
          then non_substituable i
      end;
      List.iter (count no_tail) ls
  | Lstaticcatch (l1,[i,[],Lstaticraise (j,[])]) ->
      (* i will be replaced by j in l1, so each occurence of i in l1
         increases j's ref count *)
      let tail = StexnSet.add i tail in
      count tail l1;
      let ic = count_exit i in
      begin try
        let r = Hashtbl.find exits j in r := !r + ic
      with
      | Not_found ->
          Hashtbl.add exits j (ref ic)
      end;
      if is_non_substituable i
      then non_substituable j
  | Lstaticcatch(l1, handlers) ->
      let tail = List.fold_right (fun (i, _, _) -> StexnSet.add i)
          handlers tail in
      count tail l1;
      let rec fixpoint handlers =
        let handlers' =
          List.filter (fun (i, _, l2) ->
              (* If (exit i) is not reachable,
                 l2 will be removed, so don't count its exits *)
              if count_exit i > 0
              then
                (count tail l2; false)
              else true) handlers in
        if List.length handlers' <> List.length handlers
        then fixpoint handlers' in
      fixpoint handlers
  | Ltrywith(l1, v, l2) -> count no_tail l1; count tail l2
  | Lifthenelse(l1, l2, l3) -> count no_tail l1; count tail l2; count tail l3
  | Lsequence(l1, l2) -> count no_tail l1; count tail l2
  | Lwhile(l1, l2) -> count no_tail l1; count no_tail l2
  | Lfor(_, l1, l2, dir, l3) ->
      count no_tail l1; count no_tail l2; count no_tail l3
  | Lassign(v, l) -> count no_tail l
  | Lsend(k, m, o, ll, _) -> List.iter (count no_tail) (m::o::ll)
  | Levent(l, _) -> count tail l
  | Lifused(v, l) -> count tail l

  and count_default tail sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count tail al ; count tail al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count tail al
      end
  in
  count StexnSet.empty lam;

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
  | Lprim(p, ll) -> begin
    let ll = List.map simplif ll in
    match p, ll with
        (* Simplify %revapply, for n-ary functions with n > 1 *)
      | Prevapply loc, [x; Lapply(f, args, _)]
      | Prevapply loc, [x; Levent (Lapply(f, args, _),_)] ->
        Lapply(f, args@[x], loc)
      | Prevapply loc, [x; f] -> Lapply(f, [x], loc)

        (* Simplify %apply, for n-ary functions with n > 1 *)
      | Pdirapply loc, [Lapply(f, args, _); x]
      | Pdirapply loc, [Levent (Lapply(f, args, _),_); x] ->
        Lapply(f, args@[x], loc)
      | Pdirapply loc, [f; x] -> Lapply(f, [x], loc)

      | _ -> Lprim(p, ll)
     end
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstringswitch(l,sw,d) ->
      Lstringswitch
        (simplif l,List.map (fun (s,l) -> s,simplif l) sw,
         Misc.may_map simplif d)
  | Lstaticraise (i,[]) as l ->
      begin try
        let _,handler =  Hashtbl.find subst i in
        handler
      with
      | Not_found -> l
      end
  | Lstaticraise (i,ls) as expr ->
      let ls = List.map simplif ls in
      begin try
        let xs,handler = Hashtbl.find subst i in
        let ys = List.map Ident.rename xs in
        let env =
          List.fold_right2
            (fun x y t -> Ident.add x (Lvar y) t)
            xs ys Ident.empty in
        List.fold_right2
          (fun y l r -> Llet (Alias, y, l, r))
          ys ls (Lambda.subst_lambda env handler)
      with
      | Not_found -> expr
      end
  | Lstaticcatch (l1,[i,[],(Lstaticraise (j,[]) as l2)]) ->
      Hashtbl.add subst i ([],simplif l2) ;
      simplif l1
  | Lstaticcatch(l1, handlers) ->
      let hs = List.map (fun ((i, _, _) as h) -> count_exit i, h) handlers in
      let hs = List.filter (fun (count, _) -> count > 0) hs in
      let substituables, kept = List.partition (fun (count, (i,_,_)) ->
          count = 1 && not (is_non_substituable i)) hs in
      List.iter
        (fun (_, (i, args, l2)) -> Hashtbl.add subst i (args,simplif l2))
        substituables;
      let kept = List.map
          (fun (_, (i, ids, h)) -> (i, ids, simplif h)) kept in
      Lstaticcatch(simplif l1, kept)
  | Ltrywith(l1, v, l2) -> Ltrywith(simplif l1, v, simplif l2)
  | Lifthenelse(l1, l2, l3) -> Lifthenelse(simplif l1, simplif l2, simplif l3)
  | Lsequence(l1, l2) -> Lsequence(simplif l1, simplif l2)
  | Lwhile(l1, l2) -> Lwhile(simplif l1, simplif l2)
  | Lfor(v, l1, l2, dir, l3) ->
      Lfor(v, simplif l1, simplif l2, dir, simplif l3)
  | Lassign(v, l) -> Lassign(v, simplif l)
  | Lsend(k, m, o, ll, loc) ->
      Lsend(k, simplif m, simplif o, List.map simplif ll, loc)
  | Levent(l, ev) -> Levent(simplif l, ev)
  | Lifused(v, l) -> Lifused (v,simplif l)
  in
  simplif lam

(* Compile-time beta-reduction of functions immediately applied:
      Lapply(Lfunction(Curried, params, body), args, loc) ->
        let paramN = argN in ... let param1 = arg1 in body
      Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock(args))], loc) ->
        let paramN = argN in ... let param1 = arg1 in body
   Assumes |args| = |params|.
*)

let beta_reduce params body args =
  List.fold_left2 (fun l param arg -> Llet(Strict, param, arg, l))
                  body params args

(* Simplification of lets *)

let simplify_lets lam =

  (* Disable optimisations for bytecode compilation with -g flag *)
  let optimize = !Clflags.native_code || not !Clflags.debug in

  (* First pass: count the occurrences of all let-bound identifiers *)

  let occ = (Hashtbl.create 83: (Ident.t, int ref) Hashtbl.t) in
  (* The global table [occ] associates to each let-bound identifier
     the number of its uses (as a reference):
     - 0 if never used
     - 1 if used exactly once in and not under a lambda or within a loop
     - > 1 if used several times or under a lambda or within a loop.
     The local table [bv] associates to each locally-let-bound variable
     its reference count, as above.  [bv] is enriched at let bindings
     but emptied when crossing lambdas and loops. *)

  (* Current use count of a variable. *)
  let count_var v =
    try
      !(Hashtbl.find occ v)
    with Not_found ->
      0

  (* Entering a [let].  Returns updated [bv]. *)
  and bind_var bv v =
    let r = ref 0 in
    Hashtbl.add occ v r;
    Tbl.add v r bv

  (* Record a use of a variable *)
  and use_var bv v n =
    try
      let r = Tbl.find v bv in r := !r + n
    with Not_found ->
      (* v is not locally bound, therefore this is a use under a lambda
         or within a loop.  Increase use count by 2 -- enough so
         that single-use optimizations will not apply. *)
    try
      let r = Hashtbl.find occ v in r := !r + 2
    with Not_found ->
      (* Not a let-bound variable, ignore *)
      () in

  let rec count bv = function
  | Lconst cst -> ()
  | Lvar v ->
      use_var bv v 1
  | Lapply(Lfunction(Curried, params, body), args, _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
  | Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args)], _)
    when optimize && List.length params = List.length args ->
      count bv (beta_reduce params body args)
  | Lapply(l1, ll, _) ->
      count bv l1; List.iter (count bv) ll
  | Lfunction(kind, params, l) ->
      count Tbl.empty l
  | Llet(str, v, Lvar w, l2) when optimize ->
      (* v will be replaced by w in l2, so each occurrence of v in l2
         increases w's refcount *)
      count (bind_var bv v) l2;
      use_var bv w (count_var v)
  | Llet(str, v, l1, l2) ->
      count (bind_var bv v) l2;
      (* If v is unused, l1 will be removed, so don't count its variables *)
      if str = Strict || count_var v > 0 then count bv l1
  | Lletrec(bindings, body) ->
      List.iter (fun (v, l) -> count bv l) bindings;
      count bv body
  | Lprim(p, ll) -> List.iter (count bv) ll
  | Lswitch(l, sw) ->
      count_default bv sw ;
      count bv l;
      List.iter (fun (_, l) -> count bv l) sw.sw_consts;
      List.iter (fun (_, l) -> count bv l) sw.sw_blocks
  | Lstringswitch(l, sw, d) ->
      count bv l ;
      List.iter (fun (_, l) -> count bv l) sw ;
      begin match d with
      | Some d ->
          begin match sw with
          | []|[_] -> count bv d
          | _ -> count bv d ; count bv d
          end
      | None -> ()
      end
  | Lstaticraise (i,ls) -> List.iter (count bv) ls
  | Lstaticcatch(body, handlers) ->
      count bv body;
      List.iter (fun (_,_,handler) -> count bv handler) handlers
  | Ltrywith(l1, v, l2) -> count bv l1; count bv l2
  | Lifthenelse(l1, l2, l3) -> count bv l1; count bv l2; count bv l3
  | Lsequence(l1, l2) -> count bv l1; count bv l2
  | Lwhile(l1, l2) -> count Tbl.empty l1; count Tbl.empty l2
  | Lfor(_, l1, l2, dir, l3) -> count bv l1; count bv l2; count Tbl.empty l3
  | Lassign(v, l) ->
      (* Lalias-bound variables are never assigned, so don't increase
         v's refcount *)
      count bv l
  | Lsend(_, m, o, ll, _) -> List.iter (count bv) (m::o::ll)
  | Levent(l, _) -> count bv l
  | Lifused(v, l) ->
      if count_var v > 0 then count bv l

  and count_default bv sw = match sw.sw_failaction with
  | None -> ()
  | Some al ->
      let nconsts = List.length sw.sw_consts
      and nblocks = List.length sw.sw_blocks in
      if
        nconsts < sw.sw_numconsts && nblocks < sw.sw_numblocks
      then begin (* default action will occur twice in native code *)
        count bv al ; count bv al
      end else begin (* default action will occur once *)
        assert (nconsts < sw.sw_numconsts || nblocks < sw.sw_numblocks) ;
        count bv al
      end
  in
  count Tbl.empty lam;

  (* Second pass: remove Lalias bindings of unused variables,
     and substitute the bindings of variables used exactly once. *)

  let subst = Hashtbl.create 83 in

(* This (small)  optimisation is always legal, it may uncover some
   tail call later on. *)

  let mklet (kind,v,e1,e2) = match e2 with
  | Lvar w when optimize && Ident.same v w -> e1
  | _ -> Llet (kind,v,e1,e2) in


  let rec simplif = function
    Lvar v as l ->
      begin try
        Hashtbl.find subst v
      with Not_found ->
        l
      end
  | Lconst cst as l -> l
  | Lapply(Lfunction(Curried, params, body), args, _)
    when optimize && List.length params = List.length args ->
      simplif (beta_reduce params body args)
  | Lapply(Lfunction(Tupled, params, body), [Lprim(Pmakeblock _, args)], _)
    when optimize && List.length params = List.length args ->
      simplif (beta_reduce params body args)
  | Lapply(l1, ll, loc) -> Lapply(simplif l1, List.map simplif ll, loc)
  | Lfunction(kind, params, l) -> Lfunction(kind, params, simplif l)
  | Llet(str, v, Lvar w, l2) when optimize ->
      Hashtbl.add subst v (simplif (Lvar w));
      simplif l2
  | Llet(Strict, v, Lprim(Pmakeblock(0, Mutable), [linit]), lbody)
    when optimize ->
      let slinit = simplif linit in
      let slbody = simplif lbody in
      begin try
       mklet (Variable, v, slinit, eliminate_ref v slbody)
      with Real_reference ->
        mklet(Strict, v, Lprim(Pmakeblock(0, Mutable), [slinit]), slbody)
      end
  | Llet(Alias, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | 1 when optimize -> Hashtbl.add subst v (simplif l1); simplif l2
      | n -> Llet(Alias, v, simplif l1, simplif l2)
      end
  | Llet(StrictOpt, v, l1, l2) ->
      begin match count_var v with
        0 -> simplif l2
      | n -> mklet(Alias, v, simplif l1, simplif l2)
      end
  | Llet(kind, v, l1, l2) -> mklet(kind, v, simplif l1, simplif l2)
  | Lletrec(bindings, body) ->
      Lletrec(List.map (fun (v, l) -> (v, simplif l)) bindings, simplif body)
  | Lprim(p, ll) -> Lprim(p, List.map simplif ll)
  | Lswitch(l, sw) ->
      let new_l = simplif l
      and new_consts =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_consts
      and new_blocks =  List.map (fun (n, e) -> (n, simplif e)) sw.sw_blocks
      and new_fail = Misc.may_map simplif sw.sw_failaction in
      Lswitch
        (new_l,
         {sw with sw_consts = new_consts ; sw_blocks = new_blocks;
                  sw_failaction = new_fail})
  | Lstringswitch (l,sw,d) ->
      Lstringswitch
        (simplif l,List.map (fun (s,l) -> s,simplif l) sw,
         Misc.may_map simplif d)
  | Lstaticraise (i,ls) ->
      Lstaticraise (i, List.map simplif ls)
  | Lstaticcatch(body, handlers) ->
      let handlers = List.map (fun (i, args, handler) ->
          (i, args, simplif handler))
          handlers in
      Lstaticcatch (simplif body, handlers)
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
  | Lsend(k, m, o, ll, loc) ->
      Lsend(k, simplif m, simplif o, List.map simplif ll, loc)
  | Levent(l, ev) -> Levent(simplif l, ev)
  | Lifused(v, l) ->
      if count_var v > 0 then simplif l else lambda_unit
  in
  simplif lam

(* Tail call info in annotation files *)

let is_tail_native_heuristic : (int -> bool) ref =
  ref (fun n -> true)

let rec emit_tail_infos is_tail lambda =
  let call_kind args =
    if is_tail
    && ((not !Clflags.native_code)
        || (!is_tail_native_heuristic (List.length args)))
   then Annot.Tail
   else Annot.Stack in
  match lambda with
  | Lvar _ -> ()
  | Lconst _ -> ()
  | Lapply (func, l, loc) ->
      list_emit_tail_infos false l;
      Stypes.record (Stypes.An_call (loc, call_kind l))
  | Lfunction (_, _, lam) ->
      emit_tail_infos true lam
  | Llet (_, _, lam, body) ->
      emit_tail_infos false lam;
      emit_tail_infos is_tail body
  | Lletrec (bindings, body) ->
      List.iter (fun (_, lam) -> emit_tail_infos false lam) bindings;
      emit_tail_infos is_tail body
  | Lprim (Pidentity, [arg]) ->
      emit_tail_infos is_tail arg
  | Lprim (Psequand, [arg1; arg2])
  | Lprim (Psequor, [arg1; arg2]) ->
      emit_tail_infos false arg1;
      emit_tail_infos is_tail arg2
  | Lprim (_, l) ->
      list_emit_tail_infos false l
  | Lswitch (lam, sw) ->
      emit_tail_infos false lam;
      list_emit_tail_infos_fun snd is_tail sw.sw_consts;
      list_emit_tail_infos_fun snd is_tail sw.sw_blocks;
      Misc.may  (emit_tail_infos is_tail) sw.sw_failaction
  | Lstringswitch (lam, sw, d) ->
      emit_tail_infos false lam;
      List.iter
        (fun (_,lam) ->  emit_tail_infos is_tail lam)
        sw ;
      Misc.may (emit_tail_infos is_tail) d
  | Lstaticraise (_, l) ->
      list_emit_tail_infos false l
  | Lstaticcatch (body, handlers) ->
      emit_tail_infos is_tail body;
      List.iter (fun (_,_,handler) ->
          emit_tail_infos is_tail handler)
        handlers
  | Ltrywith (body, _, handler) ->
      emit_tail_infos false body;
      emit_tail_infos is_tail handler
  | Lifthenelse (cond, ifso, ifno) ->
      emit_tail_infos false cond;
      emit_tail_infos is_tail ifso;
      emit_tail_infos is_tail ifno
  | Lsequence (lam1, lam2) ->
      emit_tail_infos false lam1;
      emit_tail_infos is_tail lam2
  | Lwhile (cond, body) ->
      emit_tail_infos false cond;
      emit_tail_infos false body
  | Lfor (_, low, high, _, body) ->
      emit_tail_infos false low;
      emit_tail_infos false high;
      emit_tail_infos false body
  | Lassign (_, lam) ->
      emit_tail_infos false lam
  | Lsend (_, meth, obj, args, loc) ->
      emit_tail_infos false meth;
      emit_tail_infos false obj;
      list_emit_tail_infos false args;
      Stypes.record (Stypes.An_call (loc, call_kind (obj :: args)))
  | Levent (lam, _) ->
      emit_tail_infos is_tail lam
  | Lifused (_, lam) ->
      emit_tail_infos is_tail lam
and list_emit_tail_infos_fun f is_tail =
  List.iter (fun x -> emit_tail_infos is_tail (f x))
and list_emit_tail_infos is_tail =
  List.iter (emit_tail_infos is_tail)

module IdentMap =
  Map.Make(struct
    type t = Ident.t
    let compare = compare
  end)

let empty_env = IdentMap.empty

let add_function env id params =
  IdentMap.add id (List.length params) env

let mem_function env id =
  IdentMap.mem id env

type result =
  { potential_functions : IdentSet.t;
    called_functions : IdentSet.t;
    non_rec_called_functions : IdentSet.t;
    escaping : IdentSet.t;
    non_tail_call : IdentSet.t }

let escape id =
  { potential_functions = IdentSet.empty;
    called_functions = IdentSet.empty;
    non_rec_called_functions = IdentSet.empty;
    non_tail_call = IdentSet.empty;
    escaping = IdentSet.singleton id }

let non_tail_call id =
  { potential_functions = IdentSet.empty;
    called_functions = IdentSet.empty;
    non_rec_called_functions = IdentSet.empty;
    non_tail_call = IdentSet.singleton id;
    escaping = IdentSet.empty }

let called_function id =
  { potential_functions = IdentSet.empty;
    called_functions = IdentSet.singleton id;
    non_rec_called_functions = IdentSet.empty;
    non_tail_call = IdentSet.empty;
    escaping = IdentSet.empty }

let non_rec_scope id res =
  if IdentSet.mem id res.called_functions
  then { res with
         non_rec_called_functions =
           IdentSet.add id res.non_rec_called_functions }
  else res

let add_potential id res =
  { res with
    potential_functions = IdentSet.add id res.potential_functions }

let empty_result =
  { potential_functions = IdentSet.empty;
    called_functions = IdentSet.empty;
    non_rec_called_functions = IdentSet.empty;
    non_tail_call = IdentSet.empty;
    escaping = IdentSet.empty }

let called_functions_are_escaping res =
  { res with
    escaping =
      IdentSet.union
        res.escaping
        res.called_functions }

let leave_function_scope res set =
  { res with
    called_functions = IdentSet.diff res.called_functions set }

let is_escaping_function res id =
  IdentSet.mem id res.escaping

let has_non_tail_call res id =
  IdentSet.mem id res.non_tail_call

let has_non_rec_call res id =
  IdentSet.mem id res.non_rec_called_functions

let is_transformable_tail_function res id =
  IdentSet.mem id res.potential_functions
  && not (is_escaping_function res id)
  && not (has_non_tail_call res id)

let are_transformable_tail_functions res defs =
  List.for_all (fun (id, _) -> is_transformable_tail_function res id) defs

let are_transformable_inner_functions res defs =
  List.for_all (fun (id, _) -> IdentSet.mem id res.potential_functions) defs

let union r1 r2 =
  { potential_functions =
      IdentSet.union r1.potential_functions r2.potential_functions;
    called_functions =
      IdentSet.union r1.called_functions r2.called_functions;
    non_rec_called_functions =
      IdentSet.union r1.non_rec_called_functions r2.non_rec_called_functions;
    escaping =
      IdentSet.union r1.escaping r2.escaping;
    non_tail_call =
      IdentSet.union r1.non_tail_call r2.non_tail_call }

let unions l =
  match l with
  | [] -> empty_result
  | h :: t -> List.fold_left union h t

let no_tail = IdentSet.empty

let function_infos lam =

  let rec loop (tail:IdentSet.t) env = function
    | Lvar id ->
        if mem_function env id
        then escape id
        else empty_result

    | Llet ((Strict | Alias | StrictOpt), id, Lfunction (Curried, params, fbody), body) ->
        function_decl env ~tail [id, params, fbody] ~recursive:false body

    | Lapply (Lvar id, args, _) ->
        let arg_result = loops env args in
        let apply_result =
          try
            let nargs = IdentMap.find id env in
            if nargs = List.length args
            then
              if IdentSet.mem id tail
              then empty_result
              else non_tail_call id
            else escape id
          with Not_found -> empty_result
        in
        union
          (called_function id)
          (union arg_result apply_result)

    | Lapply (func, args, _) ->
        union
          (loop no_tail env func)
          (loops env args)

    | Lfunction (_kind, _args, def) ->
        loop no_tail env def

    | Lletrec (defs, body) ->
        let funcs =
          List.fold_right (function
              | (id, Lfunction (Curried, params, fbody)) ->
                  (function
                    | None -> None
                    | Some l -> Some ((id, params, fbody) :: l))
              | _ ->
                  fun _ -> None)
            defs (Some []) in
        begin
          match funcs with
          | None ->
              union
                (loop tail env body)
                (loops env (List.map snd defs))
          | Some l -> function_decl env ~tail l ~recursive:true body
        end

    | Llet (_, _, def, body) ->
        union
          (loop no_tail env def)
          (loop tail env body)

    | Lprim (_, args) ->
        loops env args

    | Lifthenelse(cond, ifso, ifnot) ->
        union
          (loop no_tail env cond)
          (union
            (loop tail env ifso)
            (loop tail env ifnot))

    | Lconst _ -> empty_result

    | Lsequence (e1, e2) ->
        union
          (loop no_tail env e1)
          (loop tail env e2)

    | Lswitch (cond, sw) ->
        let add_branches l res =
          List.fold_left (fun res (_, lam) ->
              union res (loop tail env lam)) res l in
        let res_branches =
          (match sw.sw_failaction with
           | None -> empty_result
           | Some lam -> loop tail env lam)
          |> add_branches sw.sw_consts
          |> add_branches sw.sw_blocks in

        union
          res_branches
          (loop no_tail env cond)

    | Lstringswitch (cond, cases, default) ->
        let res =
          union
            (loop no_tail env cond)
            (unions (List.map (fun (_, lam) -> loop tail env lam) cases))
        in
        begin
          match default with
          | None -> res
          | Some lam -> union res (loop tail env lam)
        end

    | Lstaticraise (_, args) ->
        loops env args

    | Lstaticcatch (body, handlers) ->
        let res_handlers =
          unions
            (List.map (fun (_,_,handler) ->
                 loop tail env handler)
                handlers) in
        union
          (loop tail env body)
          res_handlers

    | Levent (lam, _)
    | Lifused (_, lam) ->
        loop tail env lam

    | Lwhile (cond, body) ->
        union
          (loop no_tail env cond)
          (loop no_tail env body)

    | Lfor (_, lo, hi, _, body) ->
        union
          (union
             (loop no_tail env lo)
             (loop no_tail env hi))
          (loop no_tail env body)

    | Lassign (_, lam) ->
        loop no_tail env lam

    | Ltrywith (body, _, handler) ->
        union
          (loop no_tail env body)
          (loop tail env handler)

    | Lsend (_, e1, e2, el, _) ->
        union
          (union
             (loop no_tail env e1)
             (loop no_tail env e2))
          (loops env el)

  and loops env l =
    unions (List.map (loop no_tail env) l)

  and function_decl env ~tail defs ~recursive body =

    let function_set =
      List.fold_right (fun (id, _, _) -> IdentSet.add id) defs IdentSet.empty in

    let env_body =
      List.fold_left
        (fun env (id, params, _) -> add_function env id params) env defs in
    let tail_body = IdentSet.union function_set tail in

    let res_body =
      loop tail_body env_body body |>
      List.fold_right (fun (id, _, _) -> non_rec_scope id) defs
    in

    let res_def =
      let env_def =
        if recursive
        then env_body
        else env in
      let tail_def =
        if recursive
        then tail_body
        else tail
      in
      unions (List.map (fun (_,_,fbody) -> loop tail_def env_def fbody) defs) in

    let does_any f res = List.exists (fun (id, _, _) -> f res id) defs in

    let res_def = leave_function_scope res_def function_set in
    let res_body = leave_function_scope res_body function_set in

    if does_any is_escaping_function res_def
    || does_any is_escaping_function res_body
    || does_any has_non_tail_call res_body
    || does_any has_non_tail_call res_def
    then
      union
        (called_functions_are_escaping res_def)
        res_body
    else
      union
        res_def
        res_body |>
      List.fold_right (fun (id,_,_) -> add_potential id) defs
  in

  loop no_tail empty_env lam

let transformable_functions defs info =
  let functions =
    List.fold_right (function
        | (id, Lfunction (Curried, params, fbody)) ->
            begin function
            | None -> None
            | Some l -> Some (id :: l)
            end
        | _ -> fun _ -> None) defs (Some []) in
  match functions with
  | None -> false
  | Some l ->
      let non_rec_called, rec_called = List.partition (has_non_rec_call info) l in
      List.length non_rec_called = 1 &&
      List.for_all (fun id -> not (has_non_tail_call info id)) rec_called &&
      List.for_all (fun id -> not (is_escaping_function info id)) rec_called
      (* A function can only have a single entry point, so to transform
         a set of mutualy recursive functions we must ensure that only
         a single one of them is used *)

let simplify_tail_calls lam =

  let info = function_infos lam in

  let empty_env = IdentMap.empty in
  let add_function id nfail env =
    (* Format.printf "add function %a@." Ident.print id; *)
    IdentMap.add id nfail env in
  let find_function env id =
    (* Format.printf "find function %a@." Ident.print id; *)
    IdentMap.find id env in

  let rec loop (tail:IdentSet.t) env lam = match lam with
    | Lvar _ | Lconst _ -> lam

    | Llet (_, id, Lfunction (_, params, fbody), body)
      when is_transformable_tail_function info id ->
        let nfail = next_raise_count () in
        let tail = IdentSet.add id tail in
        Lstaticcatch (loop tail (add_function id nfail env) body,
                      [nfail, params, loop tail env fbody])

    | Lletrec (defs, body)
        when are_transformable_tail_functions info defs ->
        let tail = List.fold_right (fun (id, _) -> IdentSet.add id) defs tail in
        let env = List.fold_right (fun (id, _) ->
            let nfail = next_raise_count () in
            add_function id nfail) defs env in
        let handlers =
          List.map (function
              | (id, Lfunction (Curried, params, fbody)) ->
                  find_function env id, params, loop tail env fbody
              | _ -> assert false) defs in
        Lstaticcatch (loop tail env body, handlers)

    | Lletrec ([id, Lfunction (Curried, params, fbody)], body) ->
        (* Multiply recursive functions are not possible in that case
           because a function cannot have multiple entry points *)
        let fbody_tail = IdentSet.singleton id in
        let nfail = next_raise_count () in
        let fbody_env = add_function id nfail env in

        let params' = List.map Ident.rename params in
        let function_entry =
          Lstaticraise(nfail,
                       List.map (fun v -> Lvar v) params') in
        let handlers = [nfail, params, loop fbody_tail fbody_env fbody] in
        let fbody = Lstaticcatch (function_entry, handlers) in
        Lletrec ([id, Lfunction (Curried, params', fbody)], loop tail env body)

    | Lletrec (defs, body)
      when transformable_functions defs info ->
        (* same case as preceeding one, but more general *)
        let functions = List.map (function
            | (id, Lfunction (Curried, params, fbody)) ->
                let nfail = next_raise_count () in
                (id, nfail, params, fbody)
            | _ -> assert false) defs in
        let fbody_tail = List.fold_left (fun tail (id, _, _, _) ->
            IdentSet.add id tail) IdentSet.empty functions in
        let fbody_env = List.fold_left (fun env (id, nfail, _, _) ->
            add_function id nfail env) env functions in
        let exported_function =
          List.find (fun (id, _, _, _) -> has_non_rec_call info id) functions in
        let (ex_id, ex_nfail, ex_params, _) = exported_function in
        let params' = List.map Ident.rename ex_params in
        let function_entry =
          Lstaticraise(ex_nfail,
                       List.map (fun v -> Lvar v) params') in
        let handlers = List.map (fun (id, nfail, params, fbody) ->
            (nfail, params, loop fbody_tail fbody_env fbody)) functions in
        let fbody = Lstaticcatch (function_entry, handlers) in
        Lletrec ([ex_id, Lfunction (Curried, params', fbody)], loop tail env body)

    | Lapply (Lvar id, args, loc)
      when IdentSet.mem id tail ->
        let nfail = find_function env id in
        Lstaticraise(nfail, args)

    | Lapply (func, args, loc) ->
        Lapply (loop no_tail env func, loops env args, loc)

    | Lfunction (kind, args, def) ->
        Lfunction (kind, args, loop no_tail env def)

    | Lletrec (defs, body) ->
        Lletrec
          (List.map (fun (id, def) -> id, loop no_tail env def) defs,
           loop tail env body)

    | Llet (kind, id, def, body) ->
        Llet (kind, id, loop no_tail env def,
              loop tail env body)

    | Lprim (prim, args) ->
        Lprim (prim, loops env args)

    | Lifthenelse(cond, ifso, ifnot) ->
        Lifthenelse(loop no_tail env cond,
                    loop tail env ifso,
                    loop tail env ifnot)

    | Lsequence (e1, e2) ->
        Lsequence
          (loop no_tail env e1,
           loop tail env e2)

    | Lswitch (cond, sw) ->
        let branch (n, lam) = n, loop tail env lam in
        let sw =
          { sw_numconsts = sw.sw_numconsts;
            sw_numblocks = sw.sw_numblocks;
            sw_consts = List.map branch sw.sw_consts;
            sw_blocks = List.map branch sw.sw_blocks;
            sw_failaction =
              match sw.sw_failaction with
              | None -> None
              | Some lam -> Some (loop tail env lam) } in
        Lswitch (loop no_tail env cond,
                 sw)


    | Lstringswitch (cond, cases, default) ->
        let branch (n, lam) = n, loop tail env lam in
        Lstringswitch
          (loop no_tail env cond,
           List.map branch cases,
           match default with
           | None -> None
           | Some lam -> Some (loop tail env lam))

    | Lstaticraise (nfail, args) ->
        Lstaticraise (nfail, loops env args)

    | Lstaticcatch (body, handlers) ->
        Lstaticcatch
          (loop tail env body,
           List.map (fun (n, ids, handler) ->
               (n, ids, loop tail env handler))
             handlers)

    | Levent (lam, ev) ->
        Levent (loop tail env lam, ev)
    | Lifused (id, lam) ->
        Lifused (id, loop tail env lam)

    | Lwhile (cond, body) ->
        Lwhile (
          loop no_tail env cond,
          loop no_tail env body)

    | Lfor (id, lo, hi, dir, body) ->
        Lfor (id, loop no_tail env lo, loop no_tail env hi, dir,
              loop no_tail env body)

    | Lassign (id, lam) ->
        Lassign (id, loop no_tail env lam)

    | Ltrywith (body, id, handler) ->
        Ltrywith
          (loop no_tail env body,
           id,
           loop tail env handler)

    | Lsend (kind, e1, e2, el, loc) ->
        Lsend
          (kind,
           loop no_tail env e1,
           loop no_tail env e2,
           loops env el,
           loc)

  and loops env lams =
    List.map (loop no_tail env) lams
  in

  loop no_tail empty_env lam

let simplify_tail_calls lam =
  let optimize = !Clflags.native_code in
  if optimize
  then simplify_tail_calls lam
  else lam


(* simplify default wrapper *)

let rec map f lam =
  let lam = match lam with
      Lvar v -> lam
    | Lconst cst -> lam
    | Lapply(e1, el, loc) ->
        Lapply(map f e1, List.map (map f) el, loc)
    | Lfunction(kind, params, body) ->
        Lfunction(kind, params, map f body)
    | Llet(str, v, e1, e2) ->
        Llet(str, v, map f e1, map f e2)
    | Lletrec(idel, e2) ->
        Lletrec(List.map (fun (v, e) -> (v, map f e)) idel,
                map f e2)
    | Lprim(p, el) ->
        Lprim(p, List.map (map f) el)
    | Lswitch(e, sw) ->
        Lswitch(map f e,
                {sw_numconsts = sw.sw_numconsts;
                 sw_consts =
                   List.map (fun (n, e) -> (n, map f e)) sw.sw_consts;
                 sw_numblocks = sw.sw_numblocks;
                 sw_blocks =
                   List.map (fun (n, e) -> (n, map f e)) sw.sw_blocks;
                 sw_failaction =
                   Misc.may_map (map f) sw.sw_failaction; })
    | Lstringswitch(e, sw, default) ->
        Lstringswitch
          (map f e,
           List.map (fun (s, e) -> (s, map f e)) sw,
           Misc.may_map (map f) default)
    | Lstaticraise (i,args) ->
        Lstaticraise (i,List.map (map f) args)
    | Lstaticcatch(body, handlers) ->
        let handlers = List.map (fun (n, ids, handler) ->
            n, ids, map f handler)
            handlers in
        Lstaticcatch(map f body, handlers)
    | Ltrywith(e1, v, e2) ->
        Ltrywith(map f e1, v, map f e2)
    | Lifthenelse(e1, e2, e3) ->
        Lifthenelse(map f e1,
                    map f e2,
                    map f e3)
    | Lsequence(e1, e2) ->
        Lsequence(map f e1, map f e2)
    | Lwhile(e1, e2) ->
        Lwhile(map f e1, map f e2)
    | Lfor(v, e1, e2, dir, e3) ->
        Lfor(v, map f e1, map f e2,
             dir, map f e3)
    | Lassign(v, e) ->
        Lassign(v, map f e)
    | Lsend(k, m, o, el, loc) ->
        Lsend(k, map f m, map f o,
              List.map (map f) el, loc)
    | Levent(l, ev) ->
        Levent(map f l, ev)
    | Lifused(v, e) ->
        Lifused(v, map f e)
  in
  f lam

(* Split a function with default parameters into a wrapper and an
   inner function.  The wrapper fills in missing optional parameters
   with their default value and tail-calls the inner function.  The
   wrapper can then hopefully be inlined on most call sites to avoid
   the overhead associated with boxing an optional argument with a
   'Some' constructor, only to deconstruct it immediately in the
   function's body. *)

let split_default_wrapper fun_id kind params body =
  let rec aux map = function
    | Llet(Strict, id, (Lifthenelse(Lvar optparam, _, _) as def), rest) when
        Ident.name optparam = "*opt*" && List.mem optparam params
          && not (List.mem_assoc optparam map)
      ->
        let wrapper_body, inner = aux ((optparam, id) :: map) rest in
        Llet(Strict, id, def, wrapper_body), inner
    | _ when map = [] -> raise Exit
    | body ->
        (* Check that those *opt* identifiers don't appear in the remaining
           body. This should not appear, but let's be on the safe side. *)
        let fv = Lambda.free_variables body in
        List.iter (fun (id, _) -> if IdentSet.mem id fv then raise Exit) map;

        let inner_id = Ident.create (Ident.name fun_id ^ "_inner") in
        let map_param p = try List.assoc p map with Not_found -> p in
        let args = List.map (fun p -> Lvar (map_param p)) params in
        let wrapper_body = Lapply (Lvar inner_id, args, Location.none) in

        let inner_params = List.map map_param params in
        let new_ids = List.map Ident.rename inner_params in
        let subst = List.fold_left2
            (fun s id new_id ->
               Ident.add id (Lvar new_id) s)
            Ident.empty inner_params new_ids
        in
        let body = Lambda.subst_lambda subst body in
        let inner_fun = Lfunction(Curried, new_ids, body) in
        (wrapper_body, (inner_id, inner_fun))
  in
  try
    let wrapper_body, inner = aux [] body in
    [(fun_id, Lfunction(kind, params, wrapper_body)); inner]
  with Exit ->
    [(fun_id, Lfunction(kind, params, body))]

let simplify_default_wrapper lam =
  let f = function
    | Llet(( Strict | Alias | StrictOpt), id,
           Lfunction(kind, params, fbody), body) ->
        begin match split_default_wrapper id kind params fbody with
        | [fun_id, def] ->
            Llet(Alias, fun_id, def, body)
        | [fun_id, def; inner_fun_id, def_inner] ->
            Llet(Alias, inner_fun_id, def_inner,
                 Llet(Alias, fun_id, def, body))
        | _ -> assert false
        end
    | Lletrec(defs, body) as lam ->
        if List.for_all (function (_, Lfunction _) -> true | _ -> false) defs
        then
          let defs =
            List.flatten
              (List.map
                 (function
                   | (id, Lfunction(kind, params, body)) ->
                       split_default_wrapper id kind params body
                   | _ -> assert false)
                 defs)
          in
          Lletrec(defs, body)
        else lam
    | lam -> lam
  in
  map f lam

let simplify_default_wrapper lam =
  let optimize = !Clflags.native_code in
  if optimize
  then simplify_default_wrapper lam
  else lam

(* The entry point:
   simplification + emission of tailcall annotations, if needed. *)

let simplify_lambda lam =
  let res =
    lam
    |> simplify_default_wrapper
    |> simplify_tail_calls
    |> simplify_exits
    |> simplify_lets
  in
  if !Clflags.annotations then emit_tail_infos true res;
  res
