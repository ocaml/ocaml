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

include Numbers.Uint64

let size t = Dwarf_value.size (Dwarf_value.uleb128 t)
let emit t = Dwarf_value.emit (Dwarf_value.uleb128 t)

module Pair = struct
  type nonrec t = t * t

  include Identifiable.Make (struct
    type nonrec t = t

    (* XXX uint comparisons *)
    let compare = Stdlib.compare
    let hash = Hashtbl.hash
    let equal t1 t2 = (compare t1 t2 = 0)

    let print ppf (x, y) = Format.fprintf ppf "(%i, %i)" x y
  end)
end
