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

let filter_inplace (decl : L.fundecl) =
  let rec filter_inplace (insn : L.instruction) =
    match insn.desc with
    | Lend -> ()
    | _ ->
      insn.available_before
        <- Reg_availability_set.canonicalise insn.available_before;
      begin match insn.available_across with
      | None -> ()
      | Some available_across ->
        insn.available_across
          <- Some (Reg_availability_set.canonicalise available_across)
      end;
      filter_inplace insn.next
  in
  filter_inplace decl.fun_body;
  decl

let fundecl (decl : L.fundecl) =
  if not !Clflags.debug_full then decl
  else filter_inplace decl
