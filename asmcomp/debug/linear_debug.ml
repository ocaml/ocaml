(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functionality for augmenting Linear code with debugging information. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Ir_debug.Make (struct
  type t = Linearize.fundecl

  let print_without_debuginfo ppf t =
    Printlinear.fundecl ~no_debuginfo:() ppf t

  let rewrite_debuginfo t ~rewrite_code_range =
    Linearize.map_debuginfo_fundecl t
      ~f:(fun dbg ->
        match Insn_debuginfo.linear_position dbg with
        | None -> dbg
        | Some pos ->
          let pos = rewrite_code_range pos in
          Insn_debuginfo.with_linear_position dbg pos)
      ~f_function:(fun fun_dbg ->
        let pos =
          rewrite_code_range (Debuginfo.Function.position fun_dbg)
        in
        Debuginfo.Function.with_position fun_dbg pos)
end)
