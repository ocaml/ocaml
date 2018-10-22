(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let module M = Mach

let rec propagate (instr : M.instruction) dbg =
  if Debuginfo.is_none instr.dbg then begin
    instr.dbg <- dbg
  end;
  let dbg = instr.dbg in
  begin match instr.desc with
  | Iend | Ireturn | Iop _ | Iexit _ | Iraise _ -> ()
  | Iifthenelse (_, ifso, ifnot) ->
    propagate ifso dbg;
    propagate ifnot dbg
  | Iswitch (_, cases) ->
    Array.iter (fun case -> propagate case dbg) cases
  | Iloop body ->
    propagate body dbg
  | Icatch (_recursive, handlers, body) ->
    List.iter (fun (_nfail, handler) -> propagate handler dbg) handlers;
    propagate body dbg
  | Itrywith (body, handler) ->
    propagate body dbg;
    propagate handler dbg
  end;
  match instr.desc with
  | Iend -> ()
  | _ -> propagate instr.next dbg

let fundecl (f : M.fundecl) =
  match !Clflags.debug_full with
  | None -> f
  | Some _ ->
    propagate f.fun_body Debuginfo.none;
    f
