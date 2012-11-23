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

open Std_internal

type t =
  | DW_ATE_signed

let signed = DW_ATE_signed

let encode = function
  | DW_ATE_signed -> 0x05

let size _t = 1

let as_dwarf_value t =
  Value.as_byte (encode t)
