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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module AS = Dwarf_attributes.Attribute_specification
module AV = Dwarf_attribute_values.Attribute_value
module DIE = Debugging_information_entry

type result = {
  abbrev_table : Abbreviations_table.t;
  dies : Debugging_information_entry.t list;
  compilation_unit_die : Debugging_information_entry.t option;
}

(* For each pattern of attributes found in the tree of proto-DIEs (of which
   there should be few compared to the number of DIEs), assign an abbreviation
   code, generating an abbreviations table in the process.  At the same time,
   generate a list of DIEs in flattened format, ready for emission.  (These
   DIEs reference the particular patterns of attributes they use via the
   abbreviation codes.) *)
let run ~proto_die_root =
  let abbrev_table, dies, compilation_unit_die =
    let next_abbreviation_code = ref 1 in
    Proto_die.depth_first_fold proto_die_root
      ~init:(Abbreviations_table.create (), [], None)
      ~f:(fun (abbrev_table, dies, compilation_unit_die) action ->
        let abbrev_table, die, compilation_unit_die =
          match action with
          | `End_of_siblings ->
            abbrev_table, DIE.create_null (), compilation_unit_die
          | `DIE (tag, has_children, attribute_values, label, name) ->
            let attribute_specs =
              List.fold_left (fun attribute_specs attribute_value ->
                  AS.Sealed.Set.add (AV.attribute_spec attribute_value)
                    attribute_specs)
                AS.Sealed.Set.empty
                attribute_values
            in
            (* Sort the list of attribute values so they match up with the
               abbreviations table entry (which uses [AS.Set]). *)
            let attribute_values =
              List.sort (fun av1 av2 ->
                  AS.Sealed.compare (AV.attribute_spec av1)
                    (AV.attribute_spec av2))
                attribute_values
            in
            let abbrev_table, abbreviation_code =
              match
                Abbreviations_table.find abbrev_table ~tag ~has_children
                  ~attribute_specs
              with
              | Some abbrev_code -> abbrev_table, abbrev_code
              | None -> 
                let abbreviation_code =
                  Abbreviation_code.of_int !next_abbreviation_code
                in
                incr next_abbreviation_code;
                let abbrev_table_entry =
                  Abbreviations_table_entry.create ~abbreviation_code ~tag
                    ~has_children ~attribute_specs
                in
                Abbreviations_table.add abbrev_table abbrev_table_entry,
                  abbreviation_code
            in
            let die =
              DIE.create ~label ~abbreviation_code ~attribute_values ~name
            in
            let compilation_unit_die =
              let is_compilation_unit =
                match tag with
                | Compile_unit -> true
                | _ -> false
              in
              if not is_compilation_unit then
                compilation_unit_die
              else
                match compilation_unit_die with
                | None -> Some die
                | Some _ ->
                  failwith "More than one `Compile_unit' DIE is present"
            in
            abbrev_table, die, compilation_unit_die
        in
        abbrev_table, die::dies, compilation_unit_die)
  in
  { abbrev_table;
    dies = List.rev dies;
    compilation_unit_die;
  }
