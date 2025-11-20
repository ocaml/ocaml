(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  range : Address_range.t;
}

let create range =
  { range }

let create_from_addresses ~start ~end_ =
  { range = Address_range.create ~start ~end_ }

let range t = t.range

let start_address t = Address_range.start t.range

let end_address t = Address_range.end_ t.range

let to_string t =
  Address_range.to_string t.range

let print ppf t =
  Format.fprintf ppf "%s" (to_string t)
