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

module Uint64 = Numbers.Uint64

type t = {
  code : int;
  comment : string;
}

exception Bad_abbreviation_code of int

let of_int code tag =
  if code < 1 then raise (Bad_abbreviation_code code);
  let comment =
    Printf.sprintf "abbrev. code (abbrev. has tag %s)" (Dwarf_tag.tag_name tag)
  in
  { code;
    comment;
  }

let null =
  { code = 0;
    comment = "null abbreviation code";
  }

let is_null t =
  t == null

let encode t =
  Dwarf_value.uleb128 ~comment:t.comment (Uint64.of_int_exn t.code)

let emit t = Dwarf_value.emit (encode t)

let size t = Dwarf_value.size (encode t)

let compare t1 t2 = Stdlib.compare t1.code t2.code

let hash t = Hashtbl.hash t.code
