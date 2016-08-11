(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16

type width =
  | Thirty_two
  | Sixty_four

type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_loc
  | Debug_str
  | Debug_line

type section =
  | Dwarf of dwarf_section

let debug_info_label = Linearize.new_label ()
let debug_abbrev_label = Linearize.new_label ()
let debug_aranges_label = Linearize.new_label ()
let debug_loc_label = Linearize.new_label ()
let debug_str_label = Linearize.new_label ()
let debug_line_label = Linearize.new_label ()

let label_for_section = function
  | Dwarf Debug_info -> debug_info_label
  | Dwarf Debug_abbrev -> debug_abbrev_label
  | Dwarf Debug_aranges -> debug_aranges_label
  | Dwarf Debug_loc -> debug_loc_label
  | Dwarf Debug_str -> debug_str_label
  | Dwarf Debug_line -> debug_line_label

module type S = sig
  val init : unit -> unit
  val switch_to_section : section -> unit
  val symbol : Symbol.t -> unit
  val define_symbol : Symbol.t -> unit
  val between_symbols : upper:Symbol.t -> lower:Symbol.t -> unit
  val between_symbol_and_label_offset
     : upper:Linearize.label
    -> lower:Symbol.t
    -> offset_upper:Target_system.Address.t
    -> unit
  val symbol_plus_offset
     : Symbol.t
    -> offset_in_bytes:Target_system.Address.t
    -> unit
  val label : Linearize.label -> unit
  val label_declaration : label_name:Linearize.label -> unit
  val int8 : Numbers.Int8.t -> unit
  val int16 : Numbers.Int16.t -> unit
  val int32 : Int32.t -> unit
  val int64 : Int64.t -> unit
  val target_address : Target_system.Address.t -> unit
  val uleb128 : Int64.t -> unit
  val sleb128 : Int64.t -> unit
  val string : string -> unit
  val cache_string : string -> Linearize.label
  val emit_cached_strings : unit -> unit
  val offset_into_section_label
     : section:section
    -> label:Linearize.label
    -> width:width
    -> unit
  val offset_into_section_symbol
     : section:section
    -> symbol:Symbol.t
    -> width:width
    -> unit
  val reset : unit -> unit
end
