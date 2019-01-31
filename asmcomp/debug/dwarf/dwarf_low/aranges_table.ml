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

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16

type t = {
  size : Dwarf_int.t;
  values : Dwarf_value.t list;
}

let create ~start_of_code_symbol ~end_of_code_symbol
      ~debug_info_label =
  let module V = Dwarf_value in
  let values = [
    (* The initial length is inserted here by the code below. *)
    V.int16 ~comment:"section version number" (Int16.of_int_exn 2);
    (* N.B. The following offset is to the compilation unit *header*, not
       the compilation unit DIE. *)
    V.offset_into_debug_info ~comment:"offset to compilation unit header"
      debug_info_label;
    V.int8 ~comment:"Arch.size_addr" (Int8.of_int_exn Arch.size_addr);
    V.int8 ~comment:"flat address space" Int8.zero;
    (* end of header *)
    (* The mystery values match up with what gcc emits and stop bfd from
       mangling the aranges tables.  They do not appear to be referenced in
       the DWARF specification. *)
    V.int16 ~comment:"mystery value 1" Int16.zero;
    V.int16 ~comment:"mystery value 2" Int16.zero;
    (* segment selector omitted (since we selected "flat address space") *)
    V.code_address_from_symbol start_of_code_symbol;
    V.code_address_from_symbol_diff ~comment:"length of code"
      ~upper:end_of_code_symbol ~lower:start_of_code_symbol ();
    (* The terminating entry is only two words since the segment selector
       word is again absent. *)
    V.absolute_address ~comment:"terminator word 1" Targetint.zero;
    V.absolute_address ~comment:"terminator word 2" Targetint.zero;
  ]
  in
  let size =
    List.fold_left (fun size value -> Dwarf_int.add size (V.size value))
      (Dwarf_int.zero ())
      values
  in
  { size; values; }

let size t = t.size

let emit t =
  Initial_length.emit (Initial_length.create t.size);
  List.iter (fun v -> Dwarf_value.emit v) t.values
