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

module Class = struct
  type address = [ `address ]
  type block = [ `block ]
  type constant = [ `constant ]
  type exprloc = [ `exprloc ]
  type flag = [ `flag ]
  type lineptr = [ `lineptr ]
  type loclistptr = [ `loclistptr ]
  type macptr = [ `macptr ]
  type rangelistptr = [ `rangelistptr ]
  type reference = [ `reference ]
  type string = [ `string ]
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
  type ref_sig8 = [ `ref_sig8 ]

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
    | Sec_offset_lineptr : (Class.lineptr, sec_offset) t
    | Sec_offset_loclistptr : (Class.loclistptr, sec_offset) t
    | Sec_offset_macptr : (Class.macptr, sec_offset) t
    | Sec_offset_rangelistptr : (Class.rangelistptr, sec_offset) t
    | Exprloc : (Class.exprloc, exprloc) t
    | Flag_present : (Class.flag, flag_present) t
    | Ref_sig8 : (Class.reference, ref_sig8) t

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
      | Sec_offset_lineptr -> 0x17
      | Sec_offset_loclistptr -> 0x17
      | Sec_offset_macptr -> 0x17
      | Sec_offset_rangelistptr -> 0x17
      | Exprloc -> 0x18
      | Flag_present -> 0x19
      | Ref_sig8 -> 0x20
    in
    Dwarf_value.Uleb128 (Int64.of_int code)

  let size t =
    Dwarf_value.size (encode t)

  let emit t asm =
    Dwarf_value.emit (encode t) asm
end

module Attribute = struct
  type 'dwarf_classes t =
    | Sibling : Class.reference t
    | Location : [< Class.exprloc | Class.loclistptr ] t
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
    | String_length : [< Class.exprloc | Class.loclistptr ] t
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
    | Return_addr : [< Class.exprloc | Class.loclistptr ] t
    | Start_scope : [< Class.constant | Class.rangelistptr ] t
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
        [< Class.constant | Class.exprloc | Class.loclistptr ] t
    | Decl_column : Class.constant t
    | Decl_file : Class.constant t
    | Decl_line : Class.constant t
    | Declaration : Class.flag t
    | Discr_list : Class.block t
    | Encoding : Class.constant t
    | External : Class.flag t
    | Frame_base : [< Class.exprloc | Class.loclistptr ] t
    | Friend : Class.reference t
    | Identifier_case : Class.constant t
    | Macro_info : Class.macptr t
    | Namelist_item : Class.reference t
    | Priority : Class.reference t
    | Segment : [< Class.exprloc | Class.loclistptr ] t
    | Specification : Class.reference t
    | Static_link : [< Class.exprloc | Class.loclistptr ] t
    | Type : Class.reference t
    | Use_location : [< Class.exprloc | Class.loclistptr ] t
    | Variable_parameter : Class.flag t
    | Virtuality : Class.constant t
    | Vtable_elem_location : [< Class.exprloc | Class.loclistptr ] t
    | Allocated : [< Class.constant | Class.exprloc | Class.reference ] t
    | Associated : [< Class.constant | Class.exprloc | Class.reference ] t
    | Data_location : Class.exprloc t
    | Byte_stride : [< Class.constant | Class.exprloc | Class.reference ] t
    | Entry_pc : Class.address t
    | Use_UTF8 : Class.flag t
    | Extension : Class.reference t
    | Ranges : Class.rangelistptr t
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
(* CR mshinwell: decide what to do about these *)
(*
  let low_user = 0x2000
  let hi_user = 0x3fff
*)

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
(*    | User code -> code *)

  let encode t =
    Dwarf_value.Uleb128 (Int64.of_int (code t))

  let size t =
    Dwarf_value.size (encode t)

  let emit t asm =
    Dwarf_value.emit (encode t) asm

  module Sealed = struct
    type t = int

    include Identifiable.Make (struct
      type nonrec t = t
      let compare = Pervasives.compare
      let equal (t1 : t) t2 = (t1 = t2)
      let hash _ = failwith "Sealed.hash unsupported"
      let print _ _ = failwith "Sealed.print unsupported"
      let output _ = failwith "Sealed.output unsupported"
    end)
  end

  let seal (t : _ t) : Sealed.t = code t
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
            Pervasives.compare (Attribute.encode attr1)
              (Attribute.encode attr2)
          in
          if c <> 0 then c
          else
            Pervasives.compare (Form.encode form1) (Form.encode form2)

      let equal t1 t2 =
        compare t1 t2 = 0

      let hash _ = failwith "Sealed.hash unsupported"
      let print _ _ = failwith "Sealed.print unsupported"
      let output _ = failwith "Sealed.output unsupported"
    end)

    let emit t asm =
      match t with
      | T1 (T (attribute, form)) ->
        Attribute.emit attribute asm;
        Form.emit form asm

    let size t =
      match t with
      | T1 (T (attribute, form)) ->
        Int64.add (Attribute.size attribute) (Form.size form)
  end

  let seal (t : _ t) : Sealed.t = Sealed.T1 t
end
