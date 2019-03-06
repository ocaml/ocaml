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

module Key = struct
  type t = {
    tag : Dwarf_tag.t;
    has_children : Child_determination.t;
    attribute_specs : Dwarf_attributes.Attribute_specification.Sealed.Set.t;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare
          { tag = tag1;
            has_children = has_children1;
            attribute_specs = attribute_specs1;
          }
          { tag = tag2;
            has_children = has_children2;
            attribute_specs = attribute_specs2;
          } =
      let c = Dwarf_tag.compare tag1 tag2 in
      if c <> 0 then c
      else
        let c = Child_determination.compare has_children1 has_children2 in
        if c <> 0 then c
        else
          Dwarf_attributes.Attribute_specification.Sealed.Set.compare
            attribute_specs1 attribute_specs2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash _ = Misc.fatal_error "Not yet implemented"
    let output _ _ = Misc.fatal_error "Not yet implemented"
    let print _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

type t = Abbreviations_table_entry.t Key.Map.t

let create () = Key.Map.empty

let add t entry =
  let tag = Abbreviations_table_entry.tag entry in
  let has_children = Abbreviations_table_entry.has_children entry in
  let attribute_specs = Abbreviations_table_entry.attribute_specs entry in
  let key : Key.t =
    { tag;
      has_children;
      attribute_specs;
    }
  in
  Key.Map.add key entry t

let find t ~tag ~has_children ~attribute_specs =
  let key : Key.t =
    { tag;
      has_children;
      attribute_specs;
    }
  in
  match Key.Map.find key t with
  | exception Not_found -> None
  | entry -> Some (Abbreviations_table_entry.abbreviation_code entry)

let size t =
  let (+) = Dwarf_int.add in
  (* See below re. the zero word. *)
  Dwarf_value.size (Dwarf_value.uleb128 Uint64.zero)
    + Key.Map.fold
        (fun _key entry size -> size + Abbreviations_table_entry.size entry)
        t
        (Dwarf_int.zero ())

let emit t =
  (* There appears to be no statement in the DWARF-4 spec (section 7.5.3)
     saying that the abbrevation table entries have to be in abbrevation
     code order.  (Ours might not be.) *)
  Key.Map.iter (fun _key entry ->
      Asm_directives.new_line ();
      Abbreviations_table_entry.emit entry)
    t;
  (* DWARF-4 spec section 7.5.3: "The abbreviations for a given compilation
     unit end with an entry consisting of a 0 byte for the abbreviation
     code." *)
  Dwarf_value.emit (
    Dwarf_value.uleb128 ~comment:"End of abbrevs for compilation unit"
      Uint64.zero)
