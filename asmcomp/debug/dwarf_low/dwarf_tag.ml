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

module Int16 = Numbers.Int16
module Uint64 = Numbers.Uint64

type user = Int16.t

type dwarf_4 =
  | GNU_call_site
  | GNU_call_site_parameter

type t =
  | Array_type
  | Class_type
  | Entry_point
  | Enumeration_type
  | Formal_parameter
  | Imported_declaration
  | Label
  | Lexical_block
  | Member
  | Pointer_type
  | Reference_type
  | Compile_unit
  | String_type
  | Structure_type
  | Subroutine_type
  | Typedef
  | Union_type
  | Unspecified_parameters
  | Variant
  | Common_block
  | Common_inclusion
  | Inheritance
  | Inlined_subroutine
  | Module
  | Ptr_to_member_type
  | Set_type
  | Subrange_type
  | With_stmt
  | Access_declaration
  | Base_type
  | Catch_block
  | Const_type
  | Constant
  | Enumerator
  | File_type
  | Friend
  | Namelist
  | Namelist_item
  | Packed_type
  | Subprogram
  | Template_type_parameter
  | Template_value_parameter
  | Thrown_type
  | Try_block
  | Variant_part
  | Variable
  | Volatile_type
  | Dwarf_procedure
  | Restrict_type
  | Interface_type
  | Namespace
  | Imported_module
  | Unspecified_type
  | Partial_unit
  | Imported_unit
  | Condition
  | Shared_type
  | Type_unit
  | Rvalue_reference_type
  | Template_alias
  | Coarray_type
  | Generic_subrange
  | Dynamic_type
  | Atomic_type
  | Call_site
  | Call_site_parameter
  | Skeleton_unit
  | Immutable_type
  | Dwarf_4 of dwarf_4
  | User of user

let tag_name t =
  let name =
    match t with
    | Array_type -> "array_type"
    | Class_type -> "class_type"
    | Entry_point -> "entry_point"
    | Enumeration_type -> "enumeration_type"
    | Formal_parameter -> "formal_parameter"
    | Imported_declaration -> "imported_declaration"
    | Label -> "label"
    | Lexical_block -> "lexical_block"
    | Member -> "member"
    | Pointer_type -> "pointer_type"
    | Reference_type -> "reference_type"
    | Compile_unit -> "compile_unit"
    | String_type -> "string_type"
    | Structure_type -> "structure_type"
    | Subroutine_type -> "subroutine_type"
    | Typedef -> "typedef"
    | Union_type -> "union_type"
    | Unspecified_parameters -> "unspecified_parameters"
    | Variant -> "variant"
    | Common_block -> "common_block"
    | Common_inclusion -> "common_inclusion"
    | Inheritance -> "inheritance"
    | Inlined_subroutine -> "inlined_subroutine"
    | Module -> "module"
    | Ptr_to_member_type -> "ptr_to_member_type"
    | Set_type -> "set_type"
    | Subrange_type -> "subrange_type"
    | With_stmt -> "with_stmt"
    | Access_declaration -> "access_declaration"
    | Base_type -> "base_type"
    | Catch_block -> "catch_block"
    | Const_type -> "const_type"
    | Constant -> "constant"
    | Enumerator -> "enumerator"
    | File_type -> "file_type"
    | Friend -> "friend"
    | Namelist -> "namelist"
    | Namelist_item -> "namelist_item"
    | Packed_type -> "packed_type"
    | Subprogram -> "subprogram"
    | Template_type_parameter -> "template_type_parameter"
    | Template_value_parameter -> "template_value_parameter"
    | Thrown_type -> "thrown_type"
    | Try_block -> "try_block"
    | Variant_part -> "variant_part"
    | Variable -> "variable"
    | Volatile_type -> "volatile_type"
    | Dwarf_procedure -> "dwarf_procedure"
    | Restrict_type -> "restrict_type"
    | Interface_type -> "interface_type"
    | Namespace -> "namespace"
    | Imported_module -> "imported_module"
    | Unspecified_type -> "unspecified_type"
    | Partial_unit -> "partial_unit"
    | Imported_unit -> "imported_unit"
    | Condition -> "condition"
    | Shared_type -> "shared_type"
    | Type_unit -> "type_unit"
    | Rvalue_reference_type -> "rvalue_reference_type"
    | Template_alias -> "template_alias"
    | Coarray_type -> "coarray_type"
    | Generic_subrange -> "generic_subrange"
    | Dynamic_type -> "dynamic_type"
    | Atomic_type -> "atomic_type"
    | Call_site -> "call_site"
    | Call_site_parameter -> "call_site_parameter"
    | Skeleton_unit -> "skeleton_unit"
    | Immutable_type -> "immutable_type"
    | Dwarf_4 GNU_call_site -> "GNU_call_site"
    | Dwarf_4 GNU_call_site_parameter -> "GNU_call_site_parameter"
    | User i -> Format.asprintf "user_%a" Int16.print i
  in
  "DW_TAG_" ^ name

let dw_tag_lo_user = Int16.of_int_exn 0x4080
(* The high limit should be [0xffff], but we can't currently encode this since
   [Int16.t] is signed. *)
let dw_tag_hi_user = Int16.of_int_exn 0x7fff

let encode t =
  let code =
    match t with
    | Array_type -> 0x01
    | Class_type -> 0x02
    | Entry_point -> 0x03
    | Enumeration_type -> 0x04
    | Formal_parameter -> 0x05
    | Imported_declaration -> 0x08
    | Label -> 0x0a
    | Lexical_block -> 0x0b
    | Member -> 0x0d
    | Pointer_type -> 0x0f
    | Reference_type -> 0x10
    | Compile_unit -> 0x11
    | String_type -> 0x12
    | Structure_type -> 0x13
    | Subroutine_type -> 0x15
    | Typedef -> 0x16
    | Union_type -> 0x17
    | Unspecified_parameters -> 0x18
    | Variant -> 0x19
    | Common_block -> 0x1a
    | Common_inclusion -> 0x1b
    | Inheritance -> 0x1c
    | Inlined_subroutine -> 0x1d
    | Module -> 0x1e
    | Ptr_to_member_type -> 0x1f
    | Set_type -> 0x20
    | Subrange_type -> 0x21
    | With_stmt -> 0x22
    | Access_declaration -> 0x23
    | Base_type -> 0x24
    | Catch_block -> 0x25
    | Const_type -> 0x26
    | Constant -> 0x27
    | Enumerator -> 0x28
    | File_type -> 0x29
    | Friend -> 0x2a
    | Namelist -> 0x2b
    | Namelist_item -> 0x2c
    | Packed_type -> 0x2d
    | Subprogram -> 0x2e
    | Template_type_parameter -> 0x2f
    | Template_value_parameter -> 0x30
    | Thrown_type -> 0x31
    | Try_block -> 0x32
    | Variant_part -> 0x33
    | Variable -> 0x34
    | Volatile_type -> 0x35
    | Dwarf_procedure -> 0x36
    | Restrict_type -> 0x37
    | Interface_type -> 0x38
    | Namespace -> 0x39
    | Imported_module -> 0x3a
    | Unspecified_type -> 0x3b
    | Partial_unit -> 0x3c
    | Imported_unit -> 0x3d
    | Condition -> 0x3f
    | Shared_type -> 0x40
    | Type_unit -> 0x41
    | Rvalue_reference_type -> 0x42
    | Template_alias -> 0x43
    | Coarray_type -> 0x44
    | Generic_subrange -> 0x45
    | Dynamic_type -> 0x46
    | Atomic_type -> 0x47
    | Call_site -> 0x48
    | Call_site_parameter -> 0x49
    | Skeleton_unit -> 0x4a
    | Immutable_type -> 0x4b
    | Dwarf_4 GNU_call_site -> 0x4109
    | Dwarf_4 GNU_call_site_parameter -> 0x410a
    | User code ->
      assert (code >= dw_tag_lo_user && code <= dw_tag_hi_user);
      Int16.to_int code
  in
  Dwarf_value.uleb128 ~comment:(tag_name t) (Uint64.of_int_exn code)

let size t =
  Dwarf_value.size (encode t)

let emit t =
  Dwarf_value.emit (encode t)

let compare t1 t2 = Stdlib.compare t1 t2
