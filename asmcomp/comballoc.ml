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
    No_alloc                            (* no allocation is pending *)
  | Pending_alloc of Reg.t * alloc_info list * int
                                        (* an allocation is pending *)
(* The arguments of Pending_alloc(reg, lst, sz) are:
     reg  the register holding the result of the last allocation
     lst  the list of pending allocations
     sz the sum of all the sizes of lst *)

let allocated_list_sz = function
    No_alloc -> ([], 0)
  | Pending_alloc(_, lst, _) -> (lst, sz)

let rec combine i allocstate =
  match i.desc with
    Iend | Ireturn | Iexit _ | Iraise _ ->
      (i, allocated_list_sz allocstate)
  | Iop(Ialloc { words = sz; blocks = l; _ }) ->
      begin match allocstate with
        No_alloc ->
          let (newnext, (newl, newsz)) =
            combine i.next (Pending_alloc(i.res.(0), l, sz)) in
          (instr_cons_debug (Iop(Ialloc {
               words = newsz; blocks = newl; spacetime_index = 0;
               label_after_call_gc = None; }))
            i.arg i.res i.dbg newnext, ([], 0))
      | Pending_alloc(reg, lcur, ofs) ->
          if ofs + sz < Config.max_young_wosize * Arch.size_addr then begin
            let (newnext, newlsz)) =
              combine i.next (Pending_alloc(reg, l @ lcur, ofs + sz)) in
            (instr_cons (Iop(Iintop_imm(Iadd, ofs))) [| reg |] i.res newnext,
             newlsz)
          end else begin
            let (newnext, (newl, newsz)) =
              combine i.next (Pending_alloc(i.res.(0), l, sz)) in
            (instr_cons_debug (Iop(Ialloc {
                 words = newsz; blocks = newl; spacetime_index = 0;
                 label_after_call_gc = None; }))
              i.arg i.res i.dbg newnext, (lcur, ofs))
          end
      end
  | Iop(Icall_ind _ | Icall_imm _ | Iextcall _ |
        Itailcall_ind _ | Itailcall_imm _) ->
      let newnext = combine_restart i.next in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext,
       allocated_list_sz allocstate)
  | Iop _ ->
      let (newnext, lsz) = combine i.next allocstate in
      (instr_cons_debug i.desc i.arg i.res i.dbg newnext, lsz)
  | Iifthenelse(test, ifso, ifnot) ->
      let newifso = combine_restart ifso in
      let newifnot = combine_restart ifnot in
      let newnext = combine_restart i.next in
      (instr_cons (Iifthenelse(test, newifso, newifnot)) i.arg i.res newnext,
       allocated_list_sz allocstate)
  | Iswitch(table, cases) ->
      let newcases = Array.map combine_restart cases in
      let newnext = combine_restart i.next in
      (instr_cons (Iswitch(table, newcases)) i.arg i.res newnext,
       allocated_list_sz allocstate)
  | Iloop(body) ->
      let newbody = combine_restart body in
      (instr_cons (Iloop(newbody)) i.arg i.res i.next,
       allocated_list_sz allocstate)
  | Icatch(io, body, handler) ->
      let (newbody, lsz) = combine body allocstate in
      let newhandler = combine_restart handler in
      let newnext = combine_restart i.next in
      (instr_cons (Icatch(io, newbody, newhandler)) i.arg i.res newnext, lsz)
  | Itrywith(body, handler) ->
      let (newbody, lsz) = combine body allocstate in
      let newhandler = combine_restart handler in
      let newnext = combine_restart i.next in
      (instr_cons (Itrywith(newbody, newhandler)) i.arg i.res newnext, lsz)

and combine_restart i =
  let (newi, _) = combine i No_alloc in newi

let fundecl f =
  if Config.spacetime then f
  else {f with fun_body = combine_restart f.fun_body}
