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

type t =
  | Pieces of (Simple_location_description.t * Targetint.t) list

let pieces_of_simple_location_descriptions slds = Pieces slds

let size t =
  match t with
  | Pieces slds ->
    List.fold_left (fun size (sld, size_in_bytes) ->
        let pieces = Dwarf_operator.DW_op_piece { size_in_bytes; } in
        Dwarf_int.add size
          (Dwarf_int.add (Simple_location_description.size sld)
            (Dwarf_operator.size pieces)))
      (Dwarf_int.zero ())
      slds

let emit t =
  match t with
  | Pieces slds ->
    List.iter (fun (sld, size_in_bytes) ->
        Simple_location_description.emit sld;
        let pieces = Dwarf_operator.DW_op_piece { size_in_bytes; } in
        Dwarf_operator.emit pieces)
      slds
