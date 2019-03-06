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

module A = Asm_directives
module ASS = Dwarf_attributes.Attribute_specification.Sealed
module AV = Dwarf_attribute_values.Attribute_value

type t = {
  label : Asm_label.t;
  name : Asm_symbol.t option;
  abbreviation_code : Abbreviation_code.t;
  attribute_values : AV.t ASS.Map.t;
}

let create ~label ~name ~abbreviation_code ~attribute_values =
  { label;
    name;
    abbreviation_code;
    attribute_values;
  }

let null =
  lazy (
    { label = Asm_label.create (DWARF Debug_info);
      name = None;
      abbreviation_code = Abbreviation_code.null;
      attribute_values = ASS.Map.empty;
    })

let create_null () = Lazy.force null

let emit t =
  (* The null DIE is likely to be emitted multiple times; we must not
     emit its label multiple times, or the assembler would complain.
     We don't actually need to point at the null DIE from anywhere else, so
     we elide emission of the label altogether. *)
  if not (Abbreviation_code.is_null t.abbreviation_code) then begin
    begin match t.name with
    | None -> ()
    | Some symbol ->
      A.define_data_symbol symbol;
      A.global symbol
    end;
    A.define_label t.label
  end;
  Abbreviation_code.emit t.abbreviation_code;
  ASS.Map.iter (fun _spec av -> AV.emit av) t.attribute_values

let size t =
  ASS.Map.fold (fun _attribute_spec attribute_value size ->
      Dwarf_int.add size (AV.size attribute_value))
    t.attribute_values
    (Abbreviation_code.size t.abbreviation_code)

let label t = t.label
let abbreviation_code t = t.abbreviation_code
let attribute_values t = t.attribute_values
let is_null t = (t == (Lazy.force null))
let symbol t = t.name
