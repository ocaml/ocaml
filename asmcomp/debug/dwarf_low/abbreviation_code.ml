(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Dwarf_value.t

exception Bad_abbreviation_code of int

let of_int i tag =
  if i < 1 then raise (Bad_abbreviation_code i);
  Dwarf_value.uleb128
    ~comment:(Printf.sprintf "abbrev. code (abbrev. has tag %s)"
      (Dwarf_tag.tag_name tag))
    (Int64.of_int i)

let null () =
  Dwarf_value.uleb128 ~comment:"null abbreviation code" 0L

let emit t =
  Dwarf_value.emit t

let size t =
  Dwarf_value.size t
