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

let dwarf_version = Dwarf_version.four

(* CR-someday mshinwell: this used to have "label - section", but maybe zero
   will do. *)
let debug_abbrev_offset t =
  Dwarf_value.offset_into_debug_abbrev t.debug_abbrev_label

(* CR-someday mshinwell: fix for cross compilation *)
let address_width_in_bytes_on_target =
  Dwarf_value.int8 ~comment:"Arch.size_addr"
    (Numbers.Int8.of_int_exn Arch.size_addr)

let size_without_first_word t =
  let (+) = Dwarf_int.add in
  let total_die_size =
    List.fold_left (fun size die -> size + DIE.size die)
      (Dwarf_int.zero ())
      t.dies
  in
  Dwarf_version.size dwarf_version
    + Dwarf_value.size (debug_abbrev_offset t)
    + Dwarf_value.size address_width_in_bytes_on_target
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
  Dwarf_version.emit dwarf_version;
  Dwarf_value.emit (debug_abbrev_offset t);
  Dwarf_value.emit address_width_in_bytes_on_target;
  List.iter (fun die -> DIE.emit die) t.dies
