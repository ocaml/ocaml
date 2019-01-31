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

module Value = struct
  type internal_t =
    | Dwarf_value of Dwarf_value.t
    (* Hack to break circular dependency. *)
    | Single_location_description of Single_location_description.t
    | Composite_location_description of Composite_location_description.t

  type _ t = internal_t

  module V = Dwarf_value

  let flag_true ?comment () = Dwarf_value (V.flag_true ?comment ())
  let bool ?comment b = Dwarf_value (V.bool ?comment b)
  let int8 ?comment i = Dwarf_value (V.int8 ?comment i)
  let int16 ?comment i = Dwarf_value (V.int16 ?comment i)
  let int32 ?comment i = Dwarf_value (V.int32 ?comment i)
  let int64 ?comment i = Dwarf_value (V.int64 ?comment i)
  let uleb128 ?comment i = Dwarf_value (V.uleb128 ?comment i)
  let string ?comment s = Dwarf_value (V.string ?comment s)
  let indirect_string ?comment ptr =
    Dwarf_value (V.indirect_string ?comment ptr)

  let distance_between_symbols_32_bit ?comment ~upper ~lower () =
    assert (Arch.size_addr = 4);
    Dwarf_value (V.code_address_from_symbol_diff ?comment ~upper ~lower ())

  let distance_between_symbols_64_bit ?comment ~upper ~lower () =
    assert (Arch.size_addr = 8);
    Dwarf_value (V.code_address_from_symbol_diff ?comment ~upper ~lower ())

  let distance_between_labels_32_bit ?comment ~upper ~lower () =
    Dwarf_value (V.distance_between_labels_32_bit ?comment ~upper ~lower ())

  let distance_between_labels_64_bit ?comment ~upper ~lower () =
    Dwarf_value (V.distance_between_labels_64_bit ?comment ~upper ~lower ())

  let distance_between_label_and_symbol_32_bit ?comment ~upper ~lower () =
    assert (Arch.size_addr = 4);
    Dwarf_value (V.code_address_from_label_symbol_diff ?comment
      ~upper ~lower ~offset_upper:Targetint.zero ())

  let distance_between_label_and_symbol_64_bit ?comment ~upper ~lower () =
    assert (Arch.size_addr = 8);
    Dwarf_value (V.code_address_from_label_symbol_diff ?comment
      ~upper ~lower ~offset_upper:Targetint.zero ())

  let code_address_from_label ?comment lbl =
    Dwarf_value (V.code_address_from_label ?comment lbl)
  let code_address_from_symbol ?comment sym =
    Dwarf_value (V.code_address_from_symbol ?comment sym)
  let offset_into_debug_line lbl = Dwarf_value (V.offset_into_debug_line lbl)
  let offset_into_debug_line_from_symbol sym =
    Dwarf_value (V.offset_into_debug_line_from_symbol sym)
  let offset_into_debug_info ?comment lbl =
    Dwarf_value (V.offset_into_debug_info ?comment lbl)
  let offset_into_debug_info_from_symbol ?comment sym =
    Dwarf_value (V.offset_into_debug_info_from_symbol ?comment sym)
  let offset_into_debug_loc lbl =
    Dwarf_value (V.offset_into_debug_loc lbl)
  let offset_into_debug_ranges lbl =
    Dwarf_value (V.offset_into_debug_ranges lbl)
  let offset_into_debug_addr lbl =
    Dwarf_value (V.offset_into_debug_addr lbl)
  let offset_into_debug_loclists lbl =
    Dwarf_value (V.offset_into_debug_loclists lbl)
  let offset_into_debug_rnglists lbl =
    Dwarf_value (V.offset_into_debug_rnglists lbl)
  let single_location_description sld = Single_location_description sld
  let composite_location_description sld = Composite_location_description sld
  let encoding_attribute attr =
    Dwarf_value (Encoding_attribute.as_dwarf_value attr)

  let symbol_32 sym =
    Dwarf_value (V.code_address_from_symbol sym)

  let symbol_64 sym =
    Dwarf_value (V.code_address_from_symbol sym)

  let loclistx ~index =
    Dwarf_value (V.uleb128 index)

  let rnglistx ~index =
    Dwarf_value (V.uleb128 index)

  let inline_code inline_code =
    Dwarf_value (Inline_code.as_dwarf_value inline_code)

  let language lang =
    Dwarf_value (Dwarf_language.as_dwarf_value lang)
end

module Attribute_value = struct
  (* CR mshinwell: Try to remove the attribute spec *)
  type t =
    Dwarf_attributes.Attribute_specification.Sealed.t * Value.internal_t

  let create attr_spec value =
    let attr_spec = Dwarf_attributes.Attribute_specification.seal attr_spec in
    attr_spec, value

  (* CR-someday mshinwell: as per CR above.  This shouldn't be here *)
  let rec uleb128_size i =
    assert (Int64.compare i 0L >= 0);
    if Int64.compare i 128L < 0 then Dwarf_int.one ()
    else Dwarf_int.succ (uleb128_size (Int64.shift_right_logical i 7))

  let size ((_spec, value) : t) =
    match value with
    | Dwarf_value value -> Dwarf_value.size value
    | Single_location_description loc_desc ->
      let loc_desc_size = Single_location_description.size loc_desc in
      Dwarf_int.add (uleb128_size (Dwarf_int.to_int64 loc_desc_size))
        loc_desc_size
    | Composite_location_description loc_desc ->
      let loc_desc_size = Composite_location_description.size loc_desc in
      Dwarf_int.add (uleb128_size (Dwarf_int.to_int64 loc_desc_size))
        loc_desc_size

  let emit ((spec, value) : t) =
    match value with
    | Dwarf_value value ->
      let value =
        if not !Clflags.keep_asm_file then value
        else
          let comment =
            Format.asprintf "(%a)"
              Dwarf_attributes.Attribute_specification.Sealed.print spec
          in
          Dwarf_value.append_to_comment value comment
      in
      Dwarf_value.emit value
    | Single_location_description loc_desc ->
      A.uleb128 ~comment:"size of single location desc."
        (Dwarf_int.to_uint64_exn (Single_location_description.size loc_desc));
      Single_location_description.emit loc_desc
    | Composite_location_description loc_desc ->
      A.uleb128 ~comment:"size of composite location desc."
        (Dwarf_int.to_uint64_exn
          (Composite_location_description.size loc_desc));
      Composite_location_description.emit loc_desc

  let attribute_spec (spec, _value) = spec
end
