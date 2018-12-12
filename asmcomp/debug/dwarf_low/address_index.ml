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

module Uint64 = Numbers.Uint64

(* CR-someday mshinwell: Change this to [Uint64] once there is a comparison
   function on values of that type. *)
include Int64

let size t = Dwarf_value.size (Dwarf_value.uleb128 (Uint64.of_int64_exn t))
let emit ?comment t =
  Dwarf_value.emit (Dwarf_value.uleb128 ?comment (Uint64.of_int64_exn t))

include Identifiable.Make (struct
  type nonrec t = t

  let compare = Stdlib.compare
  let hash = Hashtbl.hash
  let equal t1 t2 = (compare t1 t2 = 0)

  let print ppf t = Format.fprintf ppf "%Ld" t
  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

module Pair = struct
  type nonrec t = t * t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare = Stdlib.compare
    let hash = Hashtbl.hash
    let equal t1 t2 = (compare t1 t2 = 0)

    let print ppf (x, y) = Format.fprintf ppf "(%Ld, %Ld)" x y
    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end
