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

(** Everything relating to DWARF forms, classes, attributes and attribute
    specifications.  That is to say, everything about attributes with the
    exception of the actual values assigned to them. *)

module Class : sig
  (* DWARF-4 specification section 7.5.4, page 147.
     "Each class is a set of forms which have related representations and
      which are given a common interpretation according to the attribute
      in which the form is used."
  *)

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

module Form : sig
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

  (** We omit the "DW_FORM_" prefix.
      "Indirect" is not currently supported.
  *)
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
end

module Attribute : sig
  (** We omit the "DW_AT_" prefix. *)
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

  module Sealed : sig
    type t

    include Identifiable.S with type t := t
  end

  val seal : _ t -> Sealed.t
end

module Attribute_specification : sig
  (* Attribute specifications: pairs of attributes and forms
     (DWARF-4 specification section 7.5.3, page 146). *)

  type 'form t

  val create : 'dwarf_classes Attribute.t
    -> ('dwarf_classes, 'form) Form.t
    -> 'form t

  module Sealed : sig
    type t

    include Identifiable.S with type t := t
    include Dwarf_emittable.S with type t := t
  end

  val seal : _ t -> Sealed.t
end
