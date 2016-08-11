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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int16 = Numbers.Int16

type user = Int16.t

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
  | User of user

let dw_tag_lo_user = Int16.of_int_exn 0x4080
let dw_tag_hi_user = Int16.of_int_exn 0xffff

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
    | User code ->
      assert (code >= dw_tag_lo_user && code <= dw_tag_hi_user);
      Int16.to_int code
  in
  Dwarf_value.Uleb128 (Int64.of_int code)

let size t =
  Dwarf_value.size (encode t)

let emit t asm =
  Dwarf_value.emit (encode t) asm

(* This function is permitted to say "Yes" when there might not be any
   children, but not the opposite. *)
let child_determination t : Child_determination.t =
  match t with
  | Compile_unit
  | Lexical_block
  | Subprogram -> Yes
  | Array_type
  | Class_type
  | Entry_point
  | Enumeration_type
  | Formal_parameter
  | Imported_declaration
  | Label
  | Member
  | Pointer_type
  | Reference_type
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
  | User _ -> No
