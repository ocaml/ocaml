(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Linearize

let canonicalise (insn : L.instruction) avail_set =
  let avail_set = Reg_availability_set.canonicalise avail_set in
  (* Don't show variables and parameters of inlined functions except when in
     the inlined body. *)
  Reg_availability_set.map avail_set ~f:(fun avail_set ->
    Reg_with_debug_info.Set.filter (fun reg ->
        let debug_info = Reg_with_debug_info.debug_info reg in
        match debug_info with
        | None -> true
        | Some debug_info ->
          match Reg_with_debug_info.Debug_info.provenance debug_info with
          | None -> true
          | Some provenance ->
            let dbg = Backend_var.Provenance.location provenance in
            match List.rev dbg, List.rev insn.dbg with
            | _::((_::_) as dbg_rev1), _::((_::_) as dbg_rev2) ->
              let in_inlined_body =
                Misc.Stdlib.List.equal (fun dbg1 dbg2 -> dbg1 = dbg2)
                  dbg_rev1
                  dbg_rev2
              in
              not in_inlined_body
            | _, _ -> true)
      avail_set)

let filter_inplace (decl : L.fundecl) =
  let rec filter_inplace (insn : L.instruction) =
    match insn.desc with
    | Lend -> ()
    | _ ->
      insn.available_before <- canonicalise insn insn.available_before;
      begin match insn.available_across with
      | None -> ()
      | Some available_across ->
        insn.available_across <- Some (canonicalise insn available_across)
      end;
      filter_inplace insn.next
  in
  filter_inplace decl.fun_body;
  decl

let fundecl (decl : L.fundecl) =
  match !Clflags.debug_full with
  | None -> decl
  | Some _ -> filter_inplace decl
