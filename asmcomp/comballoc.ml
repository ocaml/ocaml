(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Combine heap allocations occurring in the same basic block *)

open Mach

type allocation_state =
    No_alloc
  | Pending_alloc of
    { reg: Reg.t;    (* register holding the result of the last allocation *)
      totalsz: int } (* amount to be allocated in this block *)

let allocated_size = function
    No_alloc -> 0
  | Pending_alloc {totalsz; _} -> totalsz

let rec combine i allocstate =
  match i.desc with
    Iend | Ireturn | Iexit _ | Iraise _ ->
      (i, allocated_size allocstate)
  | Iop(Ialloc { bytes = sz; _ }) ->
      begin match allocstate with
      | Pending_alloc {reg; totalsz}
          when totalsz + sz < Config.max_young_wosize * Arch.size_addr ->
         let (next, totalsz) =
           combine i.next
             (Pending_alloc { reg = i.res.(0); totalsz = totalsz + sz }) in
         (instr_cons_from i (Iop(Iintop_imm(Iadd, -sz))) ~arg:[| reg |] ~next,
          totalsz)
      | No_alloc | Pending_alloc _ ->
         let (next, totalsz) =
           combine i.next
             (Pending_alloc { reg = i.res.(0); totalsz = sz }) in
         let next =
           let offset = totalsz - sz in
           if offset = 0 then next
           else
             instr_cons_from i (Iop(Iintop_imm(Iadd, offset)))
               ~arg:i.res ~res:i.res ~next
         in
         (instr_cons_from i (Iop(Ialloc {bytes = totalsz; spacetime_index = 0;
                                        label_after_call_gc = None; }))
            ~next,
          allocated_size allocstate)
      end
  | Iop(Icall_ind _ | Icall_imm _ | Iextcall _ |
        Itailcall_ind _ | Itailcall_imm _) ->
      let newnext = combine_restart i.next in
      (instr_cons_from i i.desc ~next:newnext,
       allocated_size allocstate)
  | Iop _ ->
      let (newnext, sz) = combine i.next allocstate in
      (instr_cons_from i i.desc ~next:newnext, sz)
  | Iifthenelse(test, ifso, ifnot) ->
      let newifso = combine_restart ifso in
      let newifnot = combine_restart ifnot in
      let newnext = combine_restart i.next in
      (instr_cons_from i (Iifthenelse(test, newifso, newifnot)) ~next:newnext,
       allocated_size allocstate)
  | Iswitch(table, cases) ->
      let newcases = Array.map combine_restart cases in
      let newnext = combine_restart i.next in
      (instr_cons_from i (Iswitch(table, newcases)) ~next:newnext,
       allocated_size allocstate)
  | Icatch(rec_flag, handlers, body) ->
      let (newbody, sz) = combine body allocstate in
      let newhandlers =
        List.map (fun (io, handler) -> io, combine_restart handler) handlers in
      let newnext = combine_restart i.next in
      (instr_cons_from i (Icatch(rec_flag, newhandlers, newbody)) ~next:newnext,
       sz)
  | Itrywith(body, handler) ->
      let (newbody, sz) = combine body allocstate in
      let newhandler = combine_restart handler in
      let newnext = combine_restart i.next in
      (instr_cons_from i (Itrywith(newbody, newhandler)) ~next:newnext, sz)

and combine_restart i =
  let (newi, _) = combine i No_alloc in newi

let fundecl f =
  if Config.spacetime then f
  else {f with fun_body = combine_restart f.fun_body}
