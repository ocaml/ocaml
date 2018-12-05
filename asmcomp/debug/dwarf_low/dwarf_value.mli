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

(** Values written into DWARF sections.
    (For attribute values, see [Dwarf_attribute_values].)
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val flag_true : ?comment:string -> unit -> t

val bool : ?comment:string -> bool -> t

val int8 : ?comment:string -> Numbers.Int8.t -> t

val int16 : ?comment:string -> Numbers.Int16.t -> t

val int32 : ?comment:string -> Int32.t -> t

val int64 : ?comment:string -> Int64.t -> t

val uint8 : ?comment:string -> Numbers.Uint8.t -> t

val uint16 : ?comment:string -> Numbers.Uint16.t -> t

val uint32 : ?comment:string -> Numbers.Uint32.t -> t

val uint64 : ?comment:string -> Numbers.Uint64.t -> t

val uleb128 : ?comment:string -> Numbers.Uint64.t -> t

val sleb128 : ?comment:string -> Int64.t -> t

val string : ?comment:string -> string -> t

val indirect_string : ?comment:string -> string -> t

val absolute_address : ?comment:string -> Targetint.t -> t

val code_address_from_label : ?comment:string -> Asm_label.t -> t

val code_address_from_symbol : ?comment:string -> Asm_symbol.t -> t

(** The calculation is: (upper + offset_upper) - lower. *)
val code_address_from_label_symbol_diff
   : ?comment:string
  -> upper:Asm_label.t
  -> lower:Asm_symbol.t
  -> offset_upper:Targetint.t
  -> t

val code_address_from_symbol_diff
   : upper:Asm_symbol.t
  -> lower:Asm_symbol.t
  -> t

val code_address_from_symbol_plus_bytes : Asm_symbol.t -> Targetint.t -> t

(** N.B. The basic "offset" constructions here take labels rather than
    absolute addresses---this is important so that the references are
    relocated when multiple object files are linked together (and DWARF
    information from them concatenated inside each of the various
    sections).

    - Offsets into .debug_info are of DW_FORM_ref_addr.

    - Offsets into any other section are of DW_FORM_sec_offset.
      (DWARF-4 spec p.142.)  However the widths are the same in both cases,
      corresponding to the DWARF format.
*)

val offset_into_debug_info : ?comment:string -> Asm_label.t -> t

val offset_into_debug_info_from_symbol : ?comment:string -> Asm_symbol.t -> t

val offset_into_debug_line : ?comment:string -> Asm_label.t -> t

val offset_into_debug_line_from_symbol : ?comment:string -> Asm_symbol.t -> t

val offset_into_debug_loclists : ?comment:string -> Asm_label.t -> t

val offset_into_debug_rnglists : ?comment:string -> Asm_label.t -> t

val offset_into_debug_ranges : ?comment:string -> Asm_label.t -> t

val offset_into_debug_abbrev : ?comment:string -> Asm_label.t -> t

val distance_between_labels_16bit : upper:Asm_label.t -> lower:Asm_label.t -> t

val distance_between_labels_32bit : upper:Asm_label.t -> lower:Asm_label.t -> t

val distance_between_labels_64bit : upper:Asm_label.t -> lower:Asm_label.t -> t

val append_to_comment : t -> string -> t

val print : Format.formatter -> t -> unit

include Dwarf_emittable.S with type t := t
