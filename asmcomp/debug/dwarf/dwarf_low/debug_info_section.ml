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
module DIE = Debugging_information_entry

type t = {
  dies : DIE.t list;
  debug_abbrev_label : Asm_label.t;
  compilation_unit_header_label : Asm_label.t;
}

let create ~dies ~debug_abbrev_label ~compilation_unit_header_label =
  { dies;
    debug_abbrev_label;
    compilation_unit_header_label;
  }

let dwarf_version () =
  match !Clflags.gdwarf_version with
  | Four -> Dwarf_version.four
  | Five -> Dwarf_version.five

(* CR-someday mshinwell: this used to have "label - section", but maybe zero
   will do. *)
let debug_abbrev_offset t =
  Dwarf_value.offset_into_debug_abbrev
    ~comment:"abbrevs. for this comp. unit"
    t.debug_abbrev_label

(* CR-someday mshinwell: fix for cross compilation *)
let address_width_in_bytes_on_target =
  Dwarf_value.int8 ~comment:"Arch.size_addr"
    (Numbers.Int8.of_int_exn Arch.size_addr)

let unit_type = Unit_type.Compile

let size_without_first_word t =
  let (+) = Dwarf_int.add in
  let total_die_size =
    List.fold_left (fun size die -> size + DIE.size die)
      (Dwarf_int.zero ())
      t.dies
  in
  match !Clflags.gdwarf_version with
  | Four ->
    Dwarf_version.size (dwarf_version ())
      + Dwarf_value.size (debug_abbrev_offset t)
      + Dwarf_value.size address_width_in_bytes_on_target
      + total_die_size
  | Five ->
    Dwarf_version.size (dwarf_version ())
      + Unit_type.size unit_type
      + Dwarf_value.size address_width_in_bytes_on_target
      + Dwarf_value.size (debug_abbrev_offset t)
      + total_die_size

let size t =
  let size_without_first_word = size_without_first_word t in
  let initial_length = Initial_length.create size_without_first_word in
  Dwarf_int.add (Initial_length.size initial_length) size_without_first_word

let emit t =
  let size_without_first_word = size_without_first_word t in
  let initial_length = Initial_length.create size_without_first_word in
  A.define_label t.compilation_unit_header_label;
  Initial_length.emit initial_length;
  Dwarf_version.emit (dwarf_version ());
  begin match dwarf_version () with
  | Four ->
    Dwarf_value.emit (debug_abbrev_offset t);
    Dwarf_value.emit address_width_in_bytes_on_target
  | Five ->
    Unit_type.emit unit_type;
    Dwarf_value.emit address_width_in_bytes_on_target;
    Dwarf_value.emit (debug_abbrev_offset t)
  end;
  A.new_line ();
  A.comment "Debugging information entries:";
  A.new_line ();
  Profile.record "die_emission" (fun dies ->
      List.iter (fun die -> DIE.emit die) dies)
    t.dies
