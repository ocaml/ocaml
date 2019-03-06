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

module Uint64 = Numbers.Uint64

module Class = struct
  type address = [ `address ]
  type addrptr = [ `addrptr ]
  type block = [ `block ]
  type constant = [ `constant ]
  type exprloc = [ `exprloc ]
  type flag = [ `flag ]
  type lineptr = [ `lineptr ]
  type loclist = [ `loclist ]
  type loclistsptr = [ `loclistsptr ]
  type macptr = [ `macptr ]
  type rnglist = [ `rnglist ]
  type rnglistsptr = [ `rnglistsptr ]
  type reference = [ `reference ]
  type string = [ `string ]
  type stroffsetsptr = [ `stroffsetsptr ]

  module Dwarf_4 = struct
    type loclistptr = [ `loclistptr ]
    type rangelistptr = [ `rangelistptr ]
  end
end

module Form = struct
  type addr = [ `addr ]
  type block = [ `block ]
  type block1 = [ `block1 ]
  type block2 = [ `block2 ]
  type block4 = [ `block4 ]
  type data1 = [ `data1 ]
  type data2 = [ `data2 ]
  type data4 = [ `data4 ]
  type data8 = [ `data8 ]
  type string = [ `string ]
  type flag = [ `flag ]
  type sdata = [ `sdata ]
  type strp = [ `strp ]
  type udata = [ `udata ]
  type ref_addr = [ `ref_addr ]
  type ref1 = [ `ref1 ]
  type ref2 = [ `ref2 ]
  type ref4 = [ `ref4 ]
  type ref8 = [ `ref8 ]
  type ref_udata = [ `ref_udata ]
  type indirect = [ `indirect ]
  type sec_offset = [ `sec_offset ]
  type exprloc = [ `exprloc ]
  type flag_present = [ `flag_present ]
  type strx = [ `strx ]
  type addrx = [ `addrx ]
  type ref_sup4 = [ `ref_sup4 ]
  type strp_sup = [ `strp_sup ]
  type data16 = [ `data16 ]
  type line_strp = [ `line_strp ]
  type ref_sig8 = [ `ref_sig8 ]
  type implicit_const = [ `implicit_const ]
  type loclistx = [ `loclistx ]
  type rnglistx = [ `rnglistx ]
  type ref_sup8 = [ `ref_sup8 ]
  type strx1 = [ `strx1 ]
  type strx2 = [ `strx2 ]
  type strx3 = [ `strx3 ]
  type strx4 = [ `strx4 ]
  type addrx1 = [ `addrx1 ]
  type addrx2 = [ `addrx2 ]
  type addrx3 = [ `addrx3 ]
  type addrx4 = [ `addrx4 ]

  module Dwarf_4 = struct
    type ('dwarf_classes, 'form) t =
      | Sec_offset_loclistptr : (Class.Dwarf_4.loclistptr, sec_offset) t
      | Sec_offset_rangelistptr : (Class.Dwarf_4.rangelistptr, sec_offset) t
  end

  type ('dwarf_classes, 'form) t =
    | Addr : (Class.address, addr) t
    | Block : (Class.block, block) t
    | Block1 : (Class.block, block1) t
    | Block2 : (Class.block, [< block1 | block2 ]) t
    | Block4 : (Class.block, [< block1 | block2 | block4 ]) t
    | Data1 : (Class.constant, data1) t
    | Data2 : (Class.constant, [< data1 | data2 ]) t
    | Data4 : (Class.constant, [< data1 | data2 | data4 ]) t
    | Data8 : (Class.constant, [< data1 | data2 | data4 | data8 ]) t
    | String : (Class.string, string) t
    | Flag : (Class.flag, data1) t
    | Sdata : (Class.constant, sdata) t
    | Strp : (Class.string, strp) t
    | Udata : (Class.constant, udata) t
    | Ref_addr : (Class.reference, ref_addr) t
    | Ref1 : (Class.reference, ref1) t
    | Ref2 : (Class.reference, [< ref1 | ref2 ]) t
    | Ref4 : (Class.reference, [< ref1 | ref2 | ref4 ]) t
    | Ref8 : (Class.reference, [< ref1 | ref2 | ref4 | ref8 ]) t
    | Ref_udata : (Class.reference, ref_udata) t
    (* [DW_FORM_indirect] is not currently supported because it cannot be
       statically-typed in the manner we use here. *)
    | Sec_offset_addrptr : (Class.addrptr, sec_offset) t
    | Sec_offset_lineptr : (Class.lineptr, sec_offset) t
    | Sec_offset_loclist : (Class.loclist, sec_offset) t
    | Sec_offset_loclistsptr : (Class.loclistsptr, sec_offset) t
    | Sec_offset_macptr : (Class.macptr, sec_offset) t
    | Sec_offset_rnglist : (Class.rnglist, sec_offset) t
    | Sec_offset_rnglistsptr : (Class.rnglistsptr, sec_offset) t
    | Sec_offset_stroffsetsptr : (Class.stroffsetsptr, sec_offset) t
    | Exprloc : (Class.exprloc, exprloc) t
    | Flag_present : (Class.flag, flag_present) t
    | Strx : (Class.string, strx) t
    | Addrx : (Class.address, addrx) t
    | Ref_sup4 : (Class.reference, ref_sup4) t
    | Strp_sup : (Class.string, strp_sup) t
    | Data16 : (Class.constant, data16) t
    | Line_strp : (Class.string, line_strp) t
    | Ref_sig8 : (Class.reference, ref_sig8) t
    | Implicit_const : (Class.constant, implicit_const) t
    | Loclistx : (Class.loclist, loclistx) t
    | Rnglistx : (Class.rnglist, rnglistx) t
    | Ref_sup8 : (Class.reference, ref_sup8) t
    | Strx1 : (Class.string, strx1) t
    | Strx2 : (Class.string, strx2) t
    | Strx3 : (Class.string, strx3) t
    | Strx4 : (Class.string, strx4) t
    | Addrx1 : (Class.string, addrx1) t
    | Addrx2 : (Class.string, addrx2) t
    | Addrx3 : (Class.string, addrx3) t
    | Addrx4 : (Class.string, addrx4) t
    | Dwarf_4 : ('dwarf_classes, 'form) Dwarf_4.t -> ('dwarf_classes, 'form) t

  let name (type dwarf_class) (type form) (t : (dwarf_class, form) t) =
    let name =
      match t with
      | Addr -> "addr"
      | Block -> "block"
      | Block1 -> "block1"
      | Block2 -> "block2"
      | Block4 -> "block4"
      | Data1 -> "data1"
      | Data2 -> "data2"
      | Data4 -> "data4"
      | Data8 -> "data8"
      | String -> "string"
      | Flag -> "flag"
      | Sdata -> "sdata"
      | Strp -> "strp"
      | Udata -> "udata"
      | Ref_addr -> "ref_addr"
      | Ref1 -> "ref1"
      | Ref2 -> "ref2"
      | Ref4 -> "ref4"
      | Ref8 -> "ref8"
      | Ref_udata -> "ref_udata"
      | Sec_offset_addrptr -> "sec_offset_addrptr"
      | Sec_offset_lineptr -> "sec_offset_lineptr"
      | Sec_offset_loclist -> "sec_offset_loclist"
      | Sec_offset_loclistsptr -> "sec_offset_loclistsptr"
      | Sec_offset_macptr -> "sec_offset_macptr"
      | Sec_offset_rnglist -> "sec_offset_rnglist"
      | Sec_offset_rnglistsptr -> "sec_offset_rnglistsptr"
      | Sec_offset_stroffsetsptr -> "sec_offset_stroffsetsptr"
      | Exprloc -> "exprloc"
      | Flag_present -> "flag_present"
      | Strx -> "strx"
      | Addrx -> "addrx"
      | Ref_sup4 -> "ref_sup4"
      | Strp_sup -> "strp_sup"
      | Data16 -> "data16"
      | Line_strp -> "line_strp"
      | Ref_sig8 -> "ref_sig8"
      | Implicit_const -> "implicit_const"
      | Loclistx -> "loclistx"
      | Rnglistx -> "rnglistx"
      | Ref_sup8 -> "ref_sup8"
      | Strx1 -> "strx1"
      | Strx2 -> "strx2"
      | Strx3 -> "strx3"
      | Strx4 -> "strx4"
      | Addrx1 -> "addrx1"
      | Addrx2 -> "addrx2"
      | Addrx3 -> "addrx3"
      | Addrx4 -> "addrx4"
      | Dwarf_4 Sec_offset_loclistptr -> "sec_offset_loclistptr"
      | Dwarf_4 Sec_offset_rangelistptr -> "sec_offset_rangelistptr"
    in
    "DW_FORM_" ^ name

  let encode (type dwarf_class) (type form) (t : (dwarf_class, form) t) =
    let code =
      (* DWARF-4 standard page 160 onwards. *)
      match t with
      | Addr -> 0x01
      | Block -> 0x09
      | Block1 -> 0x0a
      | Block2 -> 0x03
      | Block4 -> 0x04
      | Data1 -> 0x0b
      | Data2 -> 0x05
      | Data4 -> 0x06
      | Data8 -> 0x07
      | String -> 0x08
      | Flag -> 0x0c
      | Sdata -> 0x0d
      | Strp -> 0x0e
      | Udata -> 0x0f
      | Ref_addr -> 0x10
      | Ref1 -> 0x11
      | Ref2 -> 0x12
      | Ref4 -> 0x13
      | Ref8 -> 0x14
      | Ref_udata -> 0x15
      | Sec_offset_addrptr -> 0x17
      | Sec_offset_lineptr -> 0x17
      | Sec_offset_loclist -> 0x17
      | Sec_offset_loclistsptr -> 0x17
      | Sec_offset_macptr -> 0x17
      | Sec_offset_rnglist -> 0x17
      | Sec_offset_rnglistsptr -> 0x17
      | Sec_offset_stroffsetsptr -> 0x17
      | Exprloc -> 0x18
      | Flag_present -> 0x19
      | Strx -> 0x1a
      | Addrx -> 0x1b
      | Ref_sup4 -> 0x1c
      | Strp_sup -> 0x1d
      | Data16 -> 0x1e
      | Line_strp -> 0x1f
      | Ref_sig8 -> 0x20
      | Implicit_const -> 0x21
      | Loclistx -> 0x22
      | Rnglistx -> 0x23
      | Ref_sup8 -> 0x24
      | Strx1 -> 0x25
      | Strx2 -> 0x26
      | Strx3 -> 0x27
      | Strx4 -> 0x28
      | Addrx1 -> 0x29
      | Addrx2 -> 0x2a
      | Addrx3 -> 0x2b
      | Addrx4 -> 0x2c
      | Dwarf_4 Sec_offset_loclistptr -> 0x17
      | Dwarf_4 Sec_offset_rangelistptr -> 0x17
    in
    Dwarf_value.uleb128 ~comment:(name t) (Uint64.of_int_exn code)

  let size t =
    Dwarf_value.size (encode t)

  let emit t =
    Dwarf_value.emit (encode t)
end

module Attribute = struct
  module Dwarf_4 = struct
    type 'dwarf_classes t =
      | Location : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | String_length : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Return_addr : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Start_scope : [< Class.constant | Class.Dwarf_4.rangelistptr ] t
      | Data_member_location :
          [< Class.constant | Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Frame_base : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Segment : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Static_link : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Use_location : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Vtable_elem_location : [< Class.exprloc | Class.Dwarf_4.loclistptr ] t
      | Ranges : Class.Dwarf_4.rangelistptr t
      | GNU_call_site_value : Class.exprloc t
      | GNU_call_site_data_value : Class.exprloc t
      | GNU_call_site_target : Class.exprloc t
      | GNU_call_site_target_clobbered : Class.exprloc t
      | GNU_tail_call : Class.flag t
      | GNU_all_tail_call_sites : Class.flag t
      | GNU_all_call_sites : Class.flag t
      | GNU_all_source_call_sites : Class.flag t
  end

  module Ocaml_specific = struct
    type 'dwarf_classes t =
      | Compiler_version : Class.string t
      | Unit_name : Class.string t
      | Config_digest : Class.string t
      | Prefix_name : Class.string t
      | Linker_dirs : Class.string t
      | Cmt_file_digest : Class.string t
  end

  type 'dwarf_classes t =
    | Sibling : Class.reference t
    | Location : [< Class.exprloc | Class.loclist ] t
    | Name : Class.string t
    | Ordering : Class.constant t
    | Byte_size : [< Class.constant | Class.exprloc | Class.reference ] t
    | Bit_offset : [< Class.constant | Class.exprloc | Class.reference ] t
    | Bit_size : [< Class.constant | Class.exprloc | Class.reference ] t
    | Stmt_list : Class.lineptr t
    | Low_pc : Class.address t
    | High_pc : [< Class.address | Class.constant ] t
    | Language : Class.constant t
    | Discr : Class.reference t
    | Discr_value : Class.constant t
    | Visibility : Class.constant t
    | Import : Class.reference t
    | String_length : [< Class.exprloc | Class.loclistsptr ] t
    | Common_reference : Class.reference t
    | Comp_dir : Class.string t
    | Const_value : [< Class.block | Class.constant | Class.string ] t
    | Containing_type : Class.reference t
    | Default_value : Class.reference t
    | Inline : Class.constant t
    | Is_optional : Class.flag t
    | Lower_bound : [< Class.constant | Class.exprloc | Class.reference ] t
    | Producer : Class.string t
    | Prototyped : Class.flag t
    | Return_addr : [< Class.exprloc | Class.loclistsptr ] t
    | Start_scope : [< Class.constant | Class.rnglistsptr ] t
    | Bit_stride : [< Class.constant | Class.exprloc | Class.reference ] t
    | Upper_bound : [< Class.constant | Class.exprloc | Class.reference ] t
    | Abstract_origin : Class.reference t
    | Accessibility : Class.constant t
    | Address_class : Class.constant t
    | Artificial : Class.flag t
    | Base_types : Class.reference t
    | Calling_convention : Class.constant t
    | Count : [< Class.constant | Class.exprloc | Class.reference ] t
    | Data_member_location :
        [< Class.constant | Class.exprloc | Class.loclistsptr ] t
    | Decl_column : Class.constant t
    | Decl_file : Class.constant t
    | Decl_line : Class.constant t
    | Declaration : Class.flag t
    | Discr_list : Class.block t
    | Encoding : Class.constant t
    | External : Class.flag t
    | Frame_base : [< Class.exprloc | Class.loclistsptr ] t
    | Friend : Class.reference t
    | Identifier_case : Class.constant t
    | Macro_info : Class.macptr t
    | Namelist_item : Class.reference t
    | Priority : Class.reference t
    | Segment : [< Class.exprloc | Class.loclistsptr ] t
    | Specification : Class.reference t
    | Static_link : [< Class.exprloc | Class.loclistsptr ] t
    | Type : Class.reference t
    | Use_location : [< Class.exprloc | Class.loclistsptr ] t
    | Variable_parameter : Class.flag t
    | Virtuality : Class.constant t
    | Vtable_elem_location : [< Class.exprloc | Class.loclistsptr ] t
    | Allocated : [< Class.constant | Class.exprloc | Class.reference ] t
    | Associated : [< Class.constant | Class.exprloc | Class.reference ] t
    | Data_location : Class.exprloc t
    | Byte_stride : [< Class.constant | Class.exprloc | Class.reference ] t
    | Entry_pc : Class.address t
    | Use_UTF8 : Class.flag t
    | Extension : Class.reference t
    | Ranges : Class.rnglist t
    | Trampoline :
        [< Class.address | Class.flag | Class.reference | Class.string ] t
    | Call_column : Class.constant t
    | Call_file : Class.constant t
    | Call_line : Class.constant t
    | Description : Class.string t
    | Binary_scale : Class.constant t
    | Decimal_scale : Class.constant t
    | Small : Class.reference t
    | Decimal_sign : Class.constant t
    | Digit_count : Class.constant t
    | Picture_string : Class.string t
    | Mutable : Class.flag t
    | Threads_scaled : Class.flag t
    | Explicit : Class.flag t
    | Object_pointer : Class.reference t
    | Endianity : Class.constant t
    | Elemental : Class.flag t
    | Pure : Class.flag t
    | Recursive : Class.flag t
    | Signature : Class.reference t
    | Main_subprogram : Class.flag t
    | Data_bit_offset : Class.constant t
    | Const_expr : Class.flag t
    | Enum_class : Class.flag t
    | Linkage_name : Class.string t
    | String_length_bit_size : Class.constant t
    | String_length_byte_size : Class.constant t
    | Rank : Class.exprloc t
    | Str_offsets_base : Class.stroffsetsptr t
    | Addr_base : Class.addrptr t
    | Rnglists_base : Class.rnglistsptr t
    | Dwo_name : Class.string t
    | Reference : Class.flag t
    | Rvalue_reference : Class.flag t
    | Macros : Class.macptr t
    | Call_all_calls : Class.flag t
    | Call_all_source_calls : Class.flag t
    | Call_all_tail_calls : Class.flag t
    | Call_return_pc : Class.address t
    | Call_value : Class.exprloc t
    | Call_origin : Class.reference t
    | Call_parameter : Class.reference t
    | Call_pc : Class.address t
    | Call_tail_call : Class.flag t
    | Call_target : Class.exprloc t
    | Call_target_clobbered : Class.exprloc t
    | Call_data_location : Class.exprloc t
    | Call_data_value : Class.exprloc t
    | Noreturn : Class.flag t
    | Alignment : Class.constant t
    | Export_symbols : Class.flag t
    | Deleted : Class.flag t
    | Defaulted : Class.constant t
    | Loclists_base : Class.loclistsptr t
    | Dwarf_4 : 'dwarf_classes Dwarf_4.t -> 'dwarf_classes t
    | Ocaml_specific : 'dwarf_classes Ocaml_specific.t -> 'dwarf_classes t

  let name (type dwarf_class) (t : dwarf_class t) =
    let name =
      match t with
      | Sibling -> "sibling"
      | Location -> "location"
      | Name -> "name"
      | Ordering -> "ordering"
      | Byte_size -> "byte_size"
      | Bit_offset -> "bit_offset"
      | Bit_size -> "bit_size"
      | Stmt_list -> "stmt_list"
      | Low_pc -> "low_pc"
      | High_pc -> "high_pc"
      | Language -> "language"
      | Discr -> "discr"
      | Discr_value -> "discr_value"
      | Visibility -> "visibility"
      | Import -> "import"
      | String_length -> "string_length"
      | Common_reference -> "common_reference"
      | Comp_dir -> "comp_dir"
      | Const_value -> "const_value"
      | Containing_type -> "containing_type"
      | Default_value -> "default_value"
      | Inline -> "inline"
      | Is_optional -> "is_optional"
      | Lower_bound -> "lower_bound"
      | Producer -> "producer"
      | Prototyped -> "prototyped"
      | Return_addr -> "return_addr"
      | Start_scope -> "start_scope"
      | Bit_stride -> "bit_stride"
      | Upper_bound -> "upper_bound"
      | Abstract_origin -> "abstract_origin"
      | Accessibility -> "accessibility"
      | Address_class -> "address_class"
      | Artificial -> "artificial"
      | Base_types -> "base_types"
      | Calling_convention -> "calling_convention"
      | Count -> "count"
      | Data_member_location -> "data_member_location"
      | Decl_column -> "decl_column"
      | Decl_file -> "decl_file"
      | Decl_line -> "decl_line"
      | Declaration -> "declaration"
      | Discr_list -> "discr_list"
      | Encoding -> "encoding"
      | External -> "external"
      | Frame_base -> "frame_base"
      | Friend -> "friend"
      | Identifier_case -> "identifier_case"
      | Macro_info -> "macro_info"
      | Namelist_item -> "namelist_item"
      | Priority -> "priority"
      | Segment -> "segment"
      | Specification -> "specification"
      | Static_link -> "static_link"
      | Type -> "type"
      | Use_location -> "use_location"
      | Variable_parameter -> "variable_parameter"
      | Virtuality -> "virtuality"
      | Vtable_elem_location -> "vtable_elem_location"
      | Allocated -> "allocated"
      | Associated -> "associated"
      | Data_location -> "data_location"
      | Byte_stride -> "byte_stride"
      | Entry_pc -> "entry_pc"
      | Use_UTF8 -> "use_utf8"
      | Extension -> "extension"
      | Ranges -> "ranges"
      | Trampoline -> "trampoline"
      | Call_column -> "call_column"
      | Call_file -> "call_file"
      | Call_line -> "call_line"
      | Description -> "description"
      | Binary_scale -> "binary_scale"
      | Decimal_scale -> "decimal_scale"
      | Small -> "small"
      | Decimal_sign -> "decimal_sign"
      | Digit_count -> "digit_count"
      | Picture_string -> "picture_string"
      | Mutable -> "mutable"
      | Threads_scaled -> "threads_scaled"
      | Explicit -> "explicit"
      | Object_pointer -> "object_pointer"
      | Endianity -> "endianity"
      | Elemental -> "elemental"
      | Pure -> "pure"
      | Recursive -> "recursive"
      | Signature -> "signature"
      | Main_subprogram -> "main_subprogram"
      | Data_bit_offset -> "data_bit_offset"
      | Const_expr -> "const_expr"
      | Enum_class -> "enum_class"
      | Linkage_name -> "linkage_name"
      | String_length_bit_size -> "string_length_bit_size"
      | String_length_byte_size -> "string_length_byte_size"
      | Rank -> "rank"
      | Str_offsets_base -> "str_offsets_base"
      | Addr_base -> "addr_base"
      | Rnglists_base -> "rnglists_base"
      | Dwo_name -> "dwo_name"
      | Reference -> "reference"
      | Rvalue_reference -> "rvalue_reference"
      | Macros -> "macros"
      | Call_all_calls -> "call_all_calls"
      | Call_all_source_calls -> "call_all_source_calls"
      | Call_all_tail_calls -> "call_all_tail_calls"
      | Call_return_pc -> "call_return_pc"
      | Call_value -> "call_value"
      | Call_origin -> "call_origin"
      | Call_parameter -> "call_parameter"
      | Call_pc -> "call_pc"
      | Call_tail_call -> "call_tail_call"
      | Call_target -> "call_target"
      | Call_target_clobbered -> "call_target_clobbered"
      | Call_data_location -> "call_data_location"
      | Call_data_value -> "call_data_value"
      | Noreturn -> "noreturn"
      | Alignment -> "alignment"
      | Export_symbols -> "export_symbols"
      | Deleted -> "deleted"
      | Defaulted -> "defaulted"
      | Loclists_base -> "loclists_base"
      | Dwarf_4 Location -> "location"
      | Dwarf_4 String_length -> "string_length"
      | Dwarf_4 Return_addr -> "return_addr"
      | Dwarf_4 Start_scope -> "start_scope"
      | Dwarf_4 Data_member_location -> "data_member_location"
      | Dwarf_4 Frame_base -> "frame_base"
      | Dwarf_4 Segment -> "segment"
      | Dwarf_4 Static_link -> "static_link"
      | Dwarf_4 Use_location -> "use_location"
      | Dwarf_4 Vtable_elem_location -> "vtable_elem_location"
      | Dwarf_4 Ranges -> "ranges"
      | Dwarf_4 GNU_call_site_value -> "GNU_call_site_value"
      | Dwarf_4 GNU_call_site_data_value -> "GNU_call_site_data_value"
      | Dwarf_4 GNU_call_site_target -> "GNU_call_site_target"
      | Dwarf_4 GNU_call_site_target_clobbered ->
        "GNU_call_site_target_clobbered"
      | Dwarf_4 GNU_tail_call -> "GNU_tail_call"
      | Dwarf_4 GNU_all_tail_call_sites -> "GNU_all_tail_call_sites"
      | Dwarf_4 GNU_all_call_sites -> "GNU_all_call_sites"
      | Dwarf_4 GNU_all_source_call_sites -> "GNU_all_source_call_sites"
      | Ocaml_specific Compiler_version -> "Ocaml_compiler_version"
      | Ocaml_specific Unit_name -> "Ocaml_unit_name"
      | Ocaml_specific Config_digest -> "Ocaml_config_digest"
      | Ocaml_specific Prefix_name -> "Ocaml_prefix_name"
      | Ocaml_specific Linker_dirs -> "Ocaml_linker_dirs"
      | Ocaml_specific Cmt_file_digest -> "Ocaml_cmt_file_digest"
    in
    "DW_AT_" ^ name

  let code (type dwarf_class) (t : dwarf_class t) =
    match t with
    | Sibling -> 0x01
    | Location -> 0x02
    | Name -> 0x03
    | Ordering -> 0x09
    | Byte_size -> 0x0b
    | Bit_offset -> 0x0c
    | Bit_size -> 0x0d
    | Stmt_list -> 0x10
    | Low_pc -> 0x11
    | High_pc -> 0x12
    | Language -> 0x13
    | Discr -> 0x15
    | Discr_value -> 0x16
    | Visibility -> 0x17
    | Import -> 0x18
    | String_length -> 0x19
    | Common_reference -> 0x1a
    | Comp_dir -> 0x1b
    | Const_value -> 0x1c
    | Containing_type -> 0x1d
    | Default_value -> 0x1e
    | Inline -> 0x20
    | Is_optional -> 0x21
    | Lower_bound -> 0x22
    | Producer -> 0x25
    | Prototyped -> 0x27
    | Return_addr -> 0x2a
    | Start_scope -> 0x2c
    | Bit_stride -> 0x2e
    | Upper_bound -> 0x2f
    | Abstract_origin -> 0x31
    | Accessibility -> 0x32
    | Address_class -> 0x33
    | Artificial -> 0x34
    | Base_types -> 0x35
    | Calling_convention -> 0x36
    | Count -> 0x37
    | Data_member_location -> 0x38
    | Decl_column -> 0x39
    | Decl_file -> 0x3a
    | Decl_line -> 0x3b
    | Declaration -> 0x3c
    | Discr_list -> 0x3d
    | Encoding -> 0x3e
    | External -> 0x3f
    | Frame_base -> 0x40
    | Friend -> 0x41
    | Identifier_case -> 0x42
    | Macro_info -> 0x43
    | Namelist_item -> 0x44
    | Priority -> 0x45
    | Segment -> 0x46
    | Specification -> 0x47
    | Static_link -> 0x48
    | Type -> 0x49
    | Use_location -> 0x4a
    | Variable_parameter -> 0x4b
    | Virtuality -> 0x4c
    | Vtable_elem_location -> 0x4d
    | Allocated -> 0x4e
    | Associated -> 0x4f
    | Data_location -> 0x50
    | Byte_stride -> 0x51
    | Entry_pc -> 0x52
    | Use_UTF8 -> 0x53
    | Extension -> 0x54
    | Ranges -> 0x55
    | Trampoline -> 0x56
    | Call_column -> 0x57
    | Call_file -> 0x58
    | Call_line -> 0x59
    | Description -> 0x5a
    | Binary_scale -> 0x5b
    | Decimal_scale -> 0x5c
    | Small -> 0x5d
    | Decimal_sign -> 0x5e
    | Digit_count -> 0x5f
    | Picture_string -> 0x60
    | Mutable -> 0x61
    | Threads_scaled -> 0x62
    | Explicit -> 0x63
    | Object_pointer -> 0x64
    | Endianity -> 0x65
    | Elemental -> 0x66
    | Pure -> 0x67
    | Recursive -> 0x68
    | Signature -> 0x69
    | Main_subprogram -> 0x6a
    | Data_bit_offset -> 0x6b
    | Const_expr -> 0x6c
    | Enum_class -> 0x6d
    | Linkage_name -> 0x6e
    | String_length_bit_size -> 0x6f
    | String_length_byte_size -> 0x70
    | Rank -> 0x71
    | Str_offsets_base -> 0x72
    | Addr_base -> 0x73
    | Rnglists_base -> 0x74
    | Dwo_name -> 0x76
    | Reference -> 0x77
    | Rvalue_reference -> 0x78
    | Macros -> 0x79
    | Call_all_calls -> 0x7a
    | Call_all_source_calls -> 0x7b
    | Call_all_tail_calls -> 0x7c
    | Call_return_pc -> 0x7d
    | Call_value -> 0x7e
    | Call_origin -> 0x7f
    | Call_parameter -> 0x80
    | Call_pc -> 0x81
    | Call_tail_call -> 0x82
    | Call_target -> 0x83
    | Call_target_clobbered -> 0x84
    | Call_data_location -> 0x85
    | Call_data_value -> 0x86
    | Noreturn -> 0x87
    | Alignment -> 0x88
    | Export_symbols -> 0x89
    | Deleted -> 0x8a
    | Defaulted -> 0x8b
    | Loclists_base -> 0x8c
    | Dwarf_4 Location -> 0x02
    | Dwarf_4 String_length -> 0x19
    | Dwarf_4 Return_addr -> 0x2a
    | Dwarf_4 Start_scope -> 0x2c
    | Dwarf_4 Data_member_location -> 0x38
    | Dwarf_4 Frame_base -> 0x40
    | Dwarf_4 Segment -> 0x46
    | Dwarf_4 Static_link -> 0x48
    | Dwarf_4 Use_location -> 0x4a
    | Dwarf_4 Vtable_elem_location -> 0x4d
    | Dwarf_4 Ranges -> 0x55
    | Dwarf_4 GNU_call_site_value -> 0x2111
    | Dwarf_4 GNU_call_site_data_value -> 0x2112
    | Dwarf_4 GNU_call_site_target -> 0x2113
    | Dwarf_4 GNU_call_site_target_clobbered -> 0x2114
    | Dwarf_4 GNU_tail_call -> 0x2115
    | Dwarf_4 GNU_all_tail_call_sites -> 0x2116
    | Dwarf_4 GNU_all_call_sites -> 0x2117
    | Dwarf_4 GNU_all_source_call_sites -> 0x2118
    | Ocaml_specific Compiler_version -> 0x3100
    | Ocaml_specific Unit_name -> 0x3101
    | Ocaml_specific Config_digest -> 0x3102
    | Ocaml_specific Prefix_name -> 0x3103
    | Ocaml_specific Linker_dirs -> 0x3104
    | Ocaml_specific Cmt_file_digest -> 0x3105

  let encode t =
    Dwarf_value.uleb128 ~comment:(name t) (Uint64.of_int_exn (code t))

  let size t =
    Dwarf_value.size (encode t)

  let emit t =
    Dwarf_value.emit (encode t)

  module Sealed = struct
    type t = {
      code : int;
      name : string;
    }

    include Identifiable.Make (struct
      type nonrec t = t
      let compare = Stdlib.compare
      let equal (t1 : t) t2 = (t1 = t2)
      let hash _ = failwith "Sealed.hash unsupported"
      let print ppf { code = _; name; } = Format.pp_print_string ppf name
      let output _ = failwith "Sealed.output unsupported"
    end)
  end

  let seal (t : _ t) : Sealed.t =
    { code = code t;
      name = name t;
    }
end

module Attribute_specification = struct
  type 'form t =
    | T : 'dwarf_classes Attribute.t * ('dwarf_classes, 'form) Form.t
      -> 'form t

  type 'form spec = 'form t

  let create attribute form = T (attribute, form)

  module Sealed = struct
    type t = T1 : 'form spec -> t

    include Identifiable.Make (struct
      type nonrec t = t

      let compare t1 t2 =
        match t1, t2 with
        | T1 (T (attr1, form1)), T1 (T (attr2, form2)) ->
          let c =
            Stdlib.compare (Attribute.encode attr1)
              (Attribute.encode attr2)
          in
          if c <> 0 then c
          else
            Stdlib.compare (Form.encode form1) (Form.encode form2)

      let equal t1 t2 =
        compare t1 t2 = 0

      let hash _ = failwith "Sealed.hash unsupported"

      let print ppf (T1 (T (attr, form))) =
        Format.fprintf ppf "%s : %s"
          (Attribute.name attr)
          (Form.name form)

      let output _ = failwith "Sealed.output unsupported"
    end)

    let emit t =
      match t with
      | T1 (T (attribute, form)) ->
        Attribute.emit attribute;
        Form.emit form

    let size t =
      match t with
      | T1 (T (attribute, form)) ->
        Dwarf_int.add (Attribute.size attribute) (Form.size form)
  end

  let seal (t : _ t) : Sealed.t = Sealed.T1 t
end
