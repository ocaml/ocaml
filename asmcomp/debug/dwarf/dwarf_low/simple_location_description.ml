(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Operator = Dwarf_operator

type t = Dwarf_operator.t list

let size t =
  List.fold_left (fun size op -> Dwarf_int.add size (Operator.size op))
    (Dwarf_int.zero ())
    t

let emit t =
  List.iter (fun op -> Operator.emit op) t
