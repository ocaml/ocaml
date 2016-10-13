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

module Attribute_value = Dwarf_attribute_values.Attribute_value

type t = {
  label : Linearize.label;
  name : Symbol.t option;
  abbreviation_code : Abbreviation_code.t;
  attribute_values : Attribute_value.t list;
}

let create ~label ~name ~abbreviation_code ~attribute_values =
  { label;
    name;
    abbreviation_code;
    attribute_values;
  }

let null =
  lazy (
    { label = Cmm.new_label ();
      name = None;
      abbreviation_code = Abbreviation_code.null ();
      attribute_values = [];
    })

let create_null () = Lazy.force null

let emit t asm =
  let module A = (val asm : Asm_directives.S) in
  (* The null DIE is likely to be emitted multiple times; we must not
     emit its label multiple times, or the assembler would complain.
     We don't actually need to point at the null DIE from anywhere else, so
     we elide emission of the label altogether. *)
  if t.abbreviation_code <> Abbreviation_code.null () then begin
    begin match t.name with
    | None -> ()
    | Some symbol -> A.define_symbol symbol
    end;
    A.label_declaration ~label_name:t.label
  end;
  Abbreviation_code.emit t.abbreviation_code asm;
  List.iter (fun av -> Attribute_value.emit av asm) t.attribute_values

let size t =
  List.fold_left (fun size attribute_value ->
      Int64.add size (Attribute_value.size attribute_value))
    (Abbreviation_code.size t.abbreviation_code)
    t.attribute_values

let label t = t.label
let abbreviation_code t = t.abbreviation_code
let attribute_values t = t.attribute_values
let is_null t = (t == (Lazy.force null))
let symbol t = t.name
