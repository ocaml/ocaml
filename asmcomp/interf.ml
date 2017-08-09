(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Construction of the interference graph.
   Annotate pseudoregs with interference lists and preference lists. *)

module IntPairSet =
  Hashtbl.Make(struct
    type t = int * int

    let equal ((a1, b1) : t) (a2, b2) =
      a1 = a2 && b1 = b2

    let hash = Hashtbl.hash
  end)

open Reg
open Mach

let mat = IntPairSet.create 42

let max_for_fast_mode = 4000  (* 8Mb memory used *)
let previous_interference = ref 0
let fast_mat = Bytes.make (max_for_fast_mode * (max_for_fast_mode - 1) / 2) '\000'
let max_index = ref (-1)

let build_graph fundecl =

  (* The interference graph is represented in two ways:
     - by adjacency lists for each register
     - by a sparse bit matrix (a set of pairs of register stamps).

     The sparse bit matrix is represented by a string unless the
     number of registers involved in the function is very large.  The
     index in the string for a pair of register (i, j) assuming j < i
     is given by:

         index(i, j) = i * (i - 1) / 2 + j

     (This takes into account the fact that only half of the square
     matrix is needed).

     In the fast "string" case, when we call [add_interf] below the
     situation will be as follows: an interference (i, j) is
     registered in the fast matrix iff the character index(i,j) of
     [fast_mat] has a code equal to [interference].  All strictly
     lower codes indicate a non-interference.  When we compile the
     next function, [interference] will have been incremented, which
     means we automatically treat interferences from the previous
     function(s) as non-interferences for the current function.  (This
     trick saves us from clearing the matrix between each function.)
     If sufficiently many functions are compiled such that the counter
     might wrap around, the array is re-allocated. *)

  if !previous_interference = 0xff then begin
    Bytes.fill fast_mat 0 (!max_index + 1) '\000';
    previous_interference := 1;
    max_index := (-1);
  end else
    incr previous_interference;

  let interference = Char.chr !previous_interference in

  if Reg.num_registers () > max_for_fast_mode then
    IntPairSet.clear mat;

  (* Record an interference between two registers *)
  let add_interf ri rj =
    if Proc.register_class ri = Proc.register_class rj then begin
      let i = ri.stamp and j = rj.stamp in
      let is_new =
        i <> j
        && begin
          let i, j = if i < j then i, j else j, i in
          if j < max_for_fast_mode then
            let index = (j * (j - 1)) lsr 1 + i in
            if !max_index < index then max_index := index;
            let b = Bytes.unsafe_get fast_mat index < interference in
            if b then Bytes.unsafe_set fast_mat index interference;
            b
          else
            let p = (i, j) in
            let b = not (IntPairSet.mem mat p) in
            if b then IntPairSet.add mat p ();
            b
        end
      in
      if is_new then begin
        if ri.loc = Unknown then begin
          ri.interf <- rj :: ri.interf;
          if not rj.spill then ri.degree <- ri.degree + 1
        end;
        if rj.loc = Unknown then begin
          rj.interf <- ri :: rj.interf;
          if not ri.spill then rj.degree <- rj.degree + 1
        end
      end
    end in

  (* Record interferences between a register array and a set of registers *)
  let add_interf_set v s =
    for i = 0 to Array.length v - 1 do
      let r1 = v.(i) in
      Reg.Set.iter (add_interf r1) s
    done in

  (* Record interferences between elements of an array *)
  let add_interf_self v =
    for i = 0 to Array.length v - 2 do
      let ri = v.(i) in
      for j = i+1 to Array.length v - 1 do
        add_interf ri v.(j)
      done
    done in

  (* Record interferences between the destination of a move and a set
     of live registers. Since the destination is equal to the source,
     do not add an interference between them if the source is still live
     afterwards. *)
  let add_interf_move src dst s =
    Reg.Set.iter (fun r -> if r.stamp <> src.stamp then add_interf dst r) s in

  (* Compute interferences *)

  let rec interf i =
    let destroyed = Proc.destroyed_at_oper i.desc in
    if Array.length destroyed > 0 then add_interf_set destroyed i.live;
    match i.desc with
      Iend -> ()
    | Ireturn -> ()
    | Iop(Imove | Ispill | Ireload) ->
        add_interf_move i.arg.(0) i.res.(0) i.live;
        interf i.next
    | Iop(Itailcall_ind _) -> ()
    | Iop(Itailcall_imm _) -> ()
    | Iop _ ->
        add_interf_set i.res i.live;
        add_interf_self i.res;
        interf i.next
    | Iifthenelse(_tst, ifso, ifnot) ->
        interf ifso;
        interf ifnot;
        interf i.next
    | Iswitch(_index, cases) ->
        for i = 0 to Array.length cases - 1 do
          interf cases.(i)
        done;
        interf i.next
    | Iloop body ->
        interf body; interf i.next
    | Icatch(_rec_flag, handlers, body) ->
        interf body;
        List.iter (fun (_, handler) -> interf handler) handlers;
        interf i.next
    | Iexit _ ->
        ()
    | Itrywith(body, handler) ->
        add_interf_set Proc.destroyed_at_raise handler.live;
        interf body; interf handler; interf i.next
    | Iraise _ -> () in

  (* Add a preference from one reg to another.
     Do not add anything if the two registers conflict,
     or if the source register already has a location,
     or if the two registers belong to different classes.
     (The last case can occur e.g. on Sparc when passing
      float arguments in integer registers, PR#6227.) *)

  let add_pref weight r1 r2 =
    if weight > 0 then begin
      let i = r1.stamp and j = r2.stamp in
      if i <> j
      && r1.loc = Unknown
      && Proc.register_class r1 = Proc.register_class r2
      && begin
        let i, j = if i < j then i, j else j, i in
        if j < max_for_fast_mode then
          let index = (j * (j - 1)) lsr 1 + i in
          Bytes.unsafe_get fast_mat index < interference
        else not (IntPairSet.mem mat (i, j))
      end

      then r1.prefer <- (r2, weight) :: r1.prefer
    end in

  (* Add a mutual preference between two regs *)
  let add_mutual_pref weight r1 r2 =
    add_pref weight r1 r2; add_pref weight r2 r1 in

  (* Update the spill cost of the registers involved in an operation *)

  let add_spill_cost cost arg =
    for i = 0 to Array.length arg - 1 do
      let r = arg.(i) in r.spill_cost <- r.spill_cost + cost
    done in

  (* Compute preferences and spill costs *)

  let rec prefer weight i =
    add_spill_cost weight i.arg;
    add_spill_cost weight i.res;
    match i.desc with
      Iend -> ()
    | Ireturn -> ()
    | Iop(Imove) ->
        add_mutual_pref weight i.arg.(0) i.res.(0);
        prefer weight i.next
    | Iop(Ispill) ->
        add_pref (weight / 4) i.arg.(0) i.res.(0);
        prefer weight i.next
    | Iop(Ireload) ->
        add_pref (weight / 4) i.res.(0) i.arg.(0);
        prefer weight i.next
    | Iop(Itailcall_ind _) -> ()
    | Iop(Itailcall_imm _) -> ()
    | Iop _ ->
        prefer weight i.next
    | Iifthenelse(_tst, ifso, ifnot) ->
        prefer (weight / 2) ifso;
        prefer (weight / 2) ifnot;
        prefer weight i.next
    | Iswitch(_index, cases) ->
        for i = 0 to Array.length cases - 1 do
          prefer (weight / 2) cases.(i)
        done;
        prefer weight i.next
    | Iloop body ->
        (* Avoid overflow of weight and spill_cost *)
        prefer (if weight < 1000 then 8 * weight else weight) body;
        prefer weight i.next
    | Icatch(rec_flag, handlers, body) ->
        prefer weight body;
        List.iter (fun (_nfail, handler) ->
            let weight =
              match rec_flag with
              | Cmm.Recursive ->
                  (* Avoid overflow of weight and spill_cost *)
                  if weight < 1000 then 8 * weight else weight
              | Cmm.Nonrecursive ->
                  weight in
            prefer weight handler) handlers;
        prefer weight i.next
    | Iexit _ ->
        ()
    | Itrywith(body, handler) ->
        prefer weight body; prefer weight handler; prefer weight i.next
    | Iraise _ -> ()
  in
  interf fundecl.fun_body; prefer 8 fundecl.fun_body
