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

module AS = Dwarf_attributes.Attribute_specification.Sealed
module Uint64 = Numbers.Uint64

type t = {
  abbreviation_code : Abbreviation_code.t;
  tag : Dwarf_tag.t;
  has_children : Child_determination.t;
  attribute_specs : Dwarf_attributes.Attribute_specification.Sealed.Set.t;
}

let create ~abbreviation_code ~tag ~has_children ~attribute_specs =
  { abbreviation_code;
    tag;
    has_children;
    attribute_specs;
  }

let size t =
  let (+) = Dwarf_int.add in
  Abbreviation_code.size t.abbreviation_code
    + Dwarf_tag.size t.tag
    + Child_determination.size t.has_children
    + AS.Set.fold (fun attr_spec size ->
          Dwarf_int.add size (AS.size attr_spec))
        t.attribute_specs
        (Dwarf_int.zero ())
    (* See below regarding the two zero words. *)
    + Dwarf_value.size (Dwarf_value.uleb128 Uint64.zero)
    + Dwarf_value.size (Dwarf_value.uleb128 Uint64.zero)

let emit t =
  Abbreviation_code.emit t.abbreviation_code;
  Dwarf_tag.emit t.tag;
  Child_determination.emit t.has_children;
  AS.Set.iter (fun spec -> AS.emit spec) t.attribute_specs;
  (* DWARF-4 spec section 7.5.3: "The series of attribute specifications ends
     with an entry containing 0 for the name and 0 for the form." *)
  Dwarf_value.emit (
    Dwarf_value.uleb128 ~comment:"terminator word 1" Uint64.zero);
  Dwarf_value.emit (
    Dwarf_value.uleb128 ~comment:"terminator word 2" Uint64.zero)

let tag t = t.tag
let has_children t = t.has_children
let attribute_specs t = t.attribute_specs
let abbreviation_code t = t.abbreviation_code
