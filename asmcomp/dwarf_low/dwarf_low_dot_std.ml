(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright and licence information to be added.                     *)
(*                                                                     *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

module Dwarf_low = struct
  module Abbreviations_table = Abbreviations_table
  module Aranges_table = Aranges_table
  module Attribute_value = Attribute_value
  module Debug_info_section = Debug_info_section
  module Debug_loc_table = Debug_loc_table
  module Emitter = Emitter
  module Encoding_attribute = Encoding_attribute
  module Location_expression = Location_expression
  module Location_list = Location_list
  module Location_list_entry = Location_list_entry
  module Pubnames_table = Pubnames_table
  module Section_names = Section_names
  module Tag = Tag
end
