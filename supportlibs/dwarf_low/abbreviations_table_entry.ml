(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = {
  abbreviation_code : Abbreviation_code.t;
  tag : Dwarf_tag.t;
  has_children : Child_determination.t;
  attribute_specs : Dwarf_attributes.Attribute_specification.Sealed.Set.t;
}

module AS = Dwarf_attributes.Attribute_specification.Sealed

let create ~abbreviation_code ~tag ~has_children ~attribute_specs =
  { abbreviation_code;
    tag;
    has_children;
    attribute_specs;
  }

let size t =
  let (+) = Int64.add in
  Abbreviation_code.size t.abbreviation_code
    + Dwarf_tag.size t.tag
    + Child_determination.size t.has_children
    + AS.Set.fold (fun attr_spec size -> Int64.add size (AS.size attr_spec))
        t.attribute_specs
        0L
    (* See below regarding the two zero words. *)
    + Dwarf_value.size (Dwarf_value.Uleb128 0L)
    + Dwarf_value.size (Dwarf_value.Uleb128 0L)

let emit t asm =
  Abbreviation_code.emit t.abbreviation_code asm;
  Dwarf_tag.emit t.tag asm;
  Child_determination.emit t.has_children asm;
  AS.Set.iter (fun spec -> AS.emit spec asm) t.attribute_specs;
  (* DWARF-4 spec section 7.5.3: "The series of attribute specifications ends
     with an entry containing 0 for the name and 0 for the form." *)
  Dwarf_value.emit (Dwarf_value.Uleb128 0L) asm;
  Dwarf_value.emit (Dwarf_value.Uleb128 0L) asm

let tag t = t.tag
let has_children t = t.has_children
let attribute_specs t = t.attribute_specs
let abbreviation_code t = t.abbreviation_code
