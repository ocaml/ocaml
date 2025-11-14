(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type offset = int

type address = int64

type die_reference =
  | Offset of offset
  | Unique_id of int

type constant =
  | Int of int
  | Int64 of int64
  | String of string

type block = bytes

type t =
  | Address of address
  | Label_address of string  (* For addresses that need relocations *)
  | Block of block
  | Constant of constant
  | String of string
  | Flag of bool
  | Reference of die_reference
  | Expr_loc of block
  | Sec_offset of offset
  | Label_sec_offset of string  (* For section offsets that need relocations *)

let print ppf = function
  | Address addr ->
      Format.fprintf ppf "Address(0x%Lx)" addr
  | Label_address label ->
      Format.fprintf ppf "Label_address(\"%s\")" label
  | Block bytes ->
      Format.fprintf ppf "Block(%d bytes)" (Bytes.length bytes)
  | Constant const ->
      begin match const with
      | Int i -> Format.fprintf ppf "Constant(%d)" i
      | Int64 i -> Format.fprintf ppf "Constant(%Ld)" i
      | String s -> Format.fprintf ppf "Constant(\"%s\")" s
      end
  | String s ->
      Format.fprintf ppf "String(\"%s\")" s
  | Flag b ->
      Format.fprintf ppf "Flag(%b)" b
  | Reference ref ->
      begin match ref with
      | Offset off -> Format.fprintf ppf "Reference(offset=%d)" off
      | Unique_id id -> Format.fprintf ppf "Reference(id=%d)" id
      end
  | Expr_loc bytes ->
      Format.fprintf ppf "Expr_loc(%d bytes)" (Bytes.length bytes)
  | Sec_offset off ->
      Format.fprintf ppf "Sec_offset(%d)" off
  | Label_sec_offset label ->
      Format.fprintf ppf "Label_sec_offset(\"%s\")" label
