(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t =
  | Pieces of (Simple_location_description.t * int) list

let pieces_of_simple_location_descriptions slds = Pieces slds

let size t =
  match t with
  | Pieces slds ->
    List.fold_left (fun size (sld, size_in_bytes) ->
        let pieces = Dwarf_operator.piece ~size_in_bytes in
        Int64.add size
          (Int64.add (Simple_location_description.size sld)
            (Dwarf_operator.size pieces)))
      Int64.zero
      slds

let emit t asm =
  match t with
  | Pieces slds ->
    List.iter (fun (sld, size_in_bytes) ->
        Simple_location_description.emit sld asm;
        let pieces = Dwarf_operator.piece ~size_in_bytes in
        Dwarf_operator.emit pieces asm)
      slds
