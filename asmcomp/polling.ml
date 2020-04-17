(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
open Mach
open Reg

(* constants *)
let lmax = 50
let k = 10
let e = lmax/k

let is_addr_live i =
  not (Reg.Set.is_empty (Reg.Set.filter (fun f -> f.typ = Cmm.Addr) i))

let insert_poll_instr instr =
    { desc = Iop (Ipoll);
      next = instr;
      arg = instr.arg;
      res = [||];
      dbg = Debuginfo.none;
      live = instr.live;
      available_before = instr.available_before;
      available_across = instr.available_across;
    }

let rec insert_poll_aux delta instr =
    if (is_addr_live instr.live) then begin
        { instr with next = (insert_poll_aux delta instr.next) }
    end else begin
        match instr.desc with
        (* terminating condition *)
        | Iend -> instr

        (* reset counter *)
        | Iop (Ipoll)
        | Iop (Ialloc _) ->
            { instr with next = (insert_poll_aux 0 instr.next) }

        (* | Iop (Imove) *)
        (* | Iop (Ispill) *)
        (* | Iop (Ireload) *)
        (*
        ->
            if (instr.Mach.arg.(0).loc = instr.Mach.res.(0).loc) then begin
                { instr with next = (insert_poll_aux delta instr.next) }
            end else begin
                let updated_instr = { instr with next = insert_poll_aux delta instr.next} in
                insert_poll_instr updated_instr
            end
        *)

        (* call type *)
        (*
        | Iop (Icall_imm _) ->
            let updated_instr = { instr with next = insert_poll_aux delta instr.next} in
            let poll_instr = insert_poll_instr updated_instr in
            (poll_instr.live <- Reg.Set.add ({Reg.dummy with loc = Reg.Reg 0 ; typ = Cmm.Val}) poll_instr.live);
            poll_instr
        *)

        | Iop (Iconst_int _)
        | Iop (Iconst_float _)
        | Iop (Iconst_symbol _)
        (* | Iop (Icall_ind _) *)
        (* | Iop (Itailcall_ind _) *)
        (* | Iop (Itailcall_imm _) *)
        (* | Iop (Iextcall _) *)
        | Iop (Istackoffset _)
        (* | Iop (Iload _) *)
        (* | Iop (Iintop _) (* signal_alloc.ml *) *)
        | Iop (Inegf)
        | Iop (Iabsf)
        | Iop (Iaddf)
        | Iop (Isubf)
        | Iop (Imulf)
        | Iop (Idivf)
        | Iop (Ifloatofint)
        | Iop (Iintoffloat)
        (* | Iop (Iname_for_debugger _) *)
        ->
            if (delta > lmax-e) then begin
                let updated_instr = { instr with next = insert_poll_aux 0 instr.next} in
                insert_poll_instr updated_instr
            end else begin
                { instr with next = insert_poll_aux (delta+1) instr.next}
            end

        (* other *)
        (* | Iop (Iintop_imm (_, _, is_addr_upd)) -> (* signals_alloc failing *)
            if (is_addr_upd) then begin
                { instr with next = (insert_poll_aux delta instr.next) }
            end else begin
                let updated_instr = { instr with next = insert_poll_aux delta instr.next} in
                insert_poll_instr updated_instr
            end *)
        (*
        | Iop (Istore (_, _, is_assign)) ->
            if (is_assign) then begin
                let updated_instr = { instr with next = insert_poll_aux delta instr.next} in
                insert_poll_instr updated_instr
            end else begin
                { instr with next = (insert_poll_aux delta instr.next) }
            end
        | Iop (Ispecific (Istore_int _)) ->
            { instr with next = (insert_poll_aux delta instr.next) }
        *)
        (* signal_alloc failing
        | Iop (Ispecific _) ->
            let updated_instr = { instr with next = insert_poll_aux delta instr.next} in
            insert_poll_instr updated_instr
        *)
        (* pass through - temp until all instructions handled *)
        | Iifthenelse (t, ifso, ifnot) ->
           { instr with
             desc = Iifthenelse(t,
                                insert_poll_aux delta ifso,
                                insert_poll_aux delta ifnot);
             next = insert_poll_aux delta instr.next }
        | Iswitch (vals, cases) ->
           { instr with
             desc = Iswitch(vals, Array.map (insert_poll_aux delta) cases);
             next = insert_poll_aux delta instr.next }
        | Icatch (isrec, handlers, body) ->
           { instr with
             desc = Icatch(isrec,
                           List.map (fun (i,c) -> i, insert_poll_aux delta c) handlers,
                           insert_poll_aux delta body);
             next = insert_poll_aux delta instr.next }
        | Itrywith (e, h) ->
           { instr with
             desc = Itrywith(insert_poll_aux delta e,
                             insert_poll_aux delta h);
             next = insert_poll_aux delta instr.next }
        | _ -> { instr with next = (insert_poll_aux delta instr.next) }
    end

let insert_poll fun_body =
    insert_poll_aux (lmax-e) fun_body

let fundecl f =
    let new_body =
        insert_poll f.fun_body
    in
      { f with fun_body = new_body }
