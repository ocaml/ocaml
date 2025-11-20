(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | DW_FORM_addr | DW_FORM_block2 | DW_FORM_block4 | DW_FORM_data2
  | DW_FORM_data4 | DW_FORM_data8 | DW_FORM_string | DW_FORM_block
  | DW_FORM_block1 | DW_FORM_data1 | DW_FORM_flag | DW_FORM_sdata
  | DW_FORM_strp | DW_FORM_udata | DW_FORM_ref_addr | DW_FORM_ref1
  | DW_FORM_ref2 | DW_FORM_ref4 | DW_FORM_ref8 | DW_FORM_ref_udata
  | DW_FORM_indirect | DW_FORM_sec_offset | DW_FORM_exprloc
  | DW_FORM_flag_present | DW_FORM_ref_sig8
  | DW_FORM_strx | DW_FORM_addrx | DW_FORM_ref_sup4 | DW_FORM_strp_sup
  | DW_FORM_data16 | DW_FORM_line_strp | DW_FORM_implicit_const
  | DW_FORM_loclistx | DW_FORM_rnglistx | DW_FORM_ref_sup8
  | DW_FORM_strx1 | DW_FORM_strx2 | DW_FORM_strx3 | DW_FORM_strx4
  | DW_FORM_addrx1 | DW_FORM_addrx2 | DW_FORM_addrx3 | DW_FORM_addrx4

(* DWARF 4 specification section 7.5.4 *)
let to_code = function
  | DW_FORM_addr -> 0x01
  | DW_FORM_block2 -> 0x03
  | DW_FORM_block4 -> 0x04
  | DW_FORM_data2 -> 0x05
  | DW_FORM_data4 -> 0x06
  | DW_FORM_data8 -> 0x07
  | DW_FORM_string -> 0x08
  | DW_FORM_block -> 0x09
  | DW_FORM_block1 -> 0x0a
  | DW_FORM_data1 -> 0x0b
  | DW_FORM_flag -> 0x0c
  | DW_FORM_sdata -> 0x0d
  | DW_FORM_strp -> 0x0e
  | DW_FORM_udata -> 0x0f
  | DW_FORM_ref_addr -> 0x10
  | DW_FORM_ref1 -> 0x11
  | DW_FORM_ref2 -> 0x12
  | DW_FORM_ref4 -> 0x13
  | DW_FORM_ref8 -> 0x14
  | DW_FORM_ref_udata -> 0x15
  | DW_FORM_indirect -> 0x16
  | DW_FORM_sec_offset -> 0x17
  | DW_FORM_exprloc -> 0x18
  | DW_FORM_flag_present -> 0x19
  | DW_FORM_ref_sig8 -> 0x20
  (* DWARF 5 *)
  | DW_FORM_strx -> 0x1a
  | DW_FORM_addrx -> 0x1b
  | DW_FORM_ref_sup4 -> 0x1c
  | DW_FORM_strp_sup -> 0x1d
  | DW_FORM_data16 -> 0x1e
  | DW_FORM_line_strp -> 0x1f
  | DW_FORM_implicit_const -> 0x21
  | DW_FORM_loclistx -> 0x22
  | DW_FORM_rnglistx -> 0x23
  | DW_FORM_ref_sup8 -> 0x24
  | DW_FORM_strx1 -> 0x25
  | DW_FORM_strx2 -> 0x26
  | DW_FORM_strx3 -> 0x27
  | DW_FORM_strx4 -> 0x28
  | DW_FORM_addrx1 -> 0x29
  | DW_FORM_addrx2 -> 0x2a
  | DW_FORM_addrx3 -> 0x2b
  | DW_FORM_addrx4 -> 0x2c

let to_string = function
  | DW_FORM_addr -> "DW_FORM_addr"
  | DW_FORM_block2 -> "DW_FORM_block2"
  | DW_FORM_block4 -> "DW_FORM_block4"
  | DW_FORM_data2 -> "DW_FORM_data2"
  | DW_FORM_data4 -> "DW_FORM_data4"
  | DW_FORM_data8 -> "DW_FORM_data8"
  | DW_FORM_string -> "DW_FORM_string"
  | DW_FORM_block -> "DW_FORM_block"
  | DW_FORM_block1 -> "DW_FORM_block1"
  | DW_FORM_data1 -> "DW_FORM_data1"
  | DW_FORM_flag -> "DW_FORM_flag"
  | DW_FORM_sdata -> "DW_FORM_sdata"
  | DW_FORM_strp -> "DW_FORM_strp"
  | DW_FORM_udata -> "DW_FORM_udata"
  | DW_FORM_ref_addr -> "DW_FORM_ref_addr"
  | DW_FORM_ref1 -> "DW_FORM_ref1"
  | DW_FORM_ref2 -> "DW_FORM_ref2"
  | DW_FORM_ref4 -> "DW_FORM_ref4"
  | DW_FORM_ref8 -> "DW_FORM_ref8"
  | DW_FORM_ref_udata -> "DW_FORM_ref_udata"
  | DW_FORM_indirect -> "DW_FORM_indirect"
  | DW_FORM_sec_offset -> "DW_FORM_sec_offset"
  | DW_FORM_exprloc -> "DW_FORM_exprloc"
  | DW_FORM_flag_present -> "DW_FORM_flag_present"
  | DW_FORM_ref_sig8 -> "DW_FORM_ref_sig8"
  | DW_FORM_strx -> "DW_FORM_strx"
  | DW_FORM_addrx -> "DW_FORM_addrx"
  | DW_FORM_ref_sup4 -> "DW_FORM_ref_sup4"
  | DW_FORM_strp_sup -> "DW_FORM_strp_sup"
  | DW_FORM_data16 -> "DW_FORM_data16"
  | DW_FORM_line_strp -> "DW_FORM_line_strp"
  | DW_FORM_implicit_const -> "DW_FORM_implicit_const"
  | DW_FORM_loclistx -> "DW_FORM_loclistx"
  | DW_FORM_rnglistx -> "DW_FORM_rnglistx"
  | DW_FORM_ref_sup8 -> "DW_FORM_ref_sup8"
  | DW_FORM_strx1 -> "DW_FORM_strx1"
  | DW_FORM_strx2 -> "DW_FORM_strx2"
  | DW_FORM_strx3 -> "DW_FORM_strx3"
  | DW_FORM_strx4 -> "DW_FORM_strx4"
  | DW_FORM_addrx1 -> "DW_FORM_addrx1"
  | DW_FORM_addrx2 -> "DW_FORM_addrx2"
  | DW_FORM_addrx3 -> "DW_FORM_addrx3"
  | DW_FORM_addrx4 -> "DW_FORM_addrx4"

let print ppf form =
  Format.fprintf ppf "%s" (to_string form)
