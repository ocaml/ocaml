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

(** ARM64 DWARF register number mappings.

    Maps OCaml backend register numbers to DWARF register numbers
    as defined by the AARCH64 DWARF specification.

    OCaml backend numbering (from proc.ml):
      0-15: x0-x15, 16-22: x19-x25, 23-25: x26-x28, 26-27: x16-x17
      100-131: d0-d31

    DWARF AARCH64 numbering:
      x0-x30: 0-30, SP: 31
      v0-v31 (d0-d31): 64-95 *)

let backend_to_dwarf_int_register = [|
  (* OCaml backend index → DWARF register number *)
  0; 1; 2; 3; 4; 5; 6; 7;        (* 0-7:   x0-x7   → x0-x7   (0-7) *)
  8; 9; 10; 11; 12; 13; 14; 15;  (* 8-15:  x8-x15  → x8-x15  (8-15) *)
  19; 20; 21; 22; 23; 24; 25;    (* 16-22: x19-x25 → x19-x25 (19-25) *)
  26; 27; 28;                    (* 23-25: x26-x28 → x26-x28 (26-28) *)
  16; 17;                        (* 26-27: x16-x17 → x16-x17 (16-17) *)
|]

let backend_to_dwarf_float_register_offset = 100  (* Backend uses 100-131 for d0-d31 *)
let dwarf_vreg_base = 64  (* DWARF uses 64-95 for v0-v31/d0-d31 *)

(** Convert OCaml backend register number to DWARF register number.

    Raises Invalid_argument if the register number is out of range. *)
let to_dwarf_register backend_reg =
  if backend_reg < 100 then begin
    (* Integer/general-purpose register *)
    if backend_reg >= 0 && backend_reg < Array.length backend_to_dwarf_int_register then
      backend_to_dwarf_int_register.(backend_reg)
    else
      invalid_arg (Printf.sprintf
        "ARM64 backend register %d out of range (0-%d expected)"
        backend_reg (Array.length backend_to_dwarf_int_register - 1))
  end else begin
    (* Float/SIMD register: backend 100-131 → DWARF 64-95 *)
    let float_index = backend_reg - backend_to_dwarf_float_register_offset in
    if float_index >= 0 && float_index < 32 then
      dwarf_vreg_base + float_index
    else
      invalid_arg (Printf.sprintf
        "ARM64 backend float register %d out of range (100-131 expected)"
        backend_reg)
  end

(** Get the register name for debugging purposes *)
let register_name backend_reg =
  if backend_reg < 100 then
    let names = [| "x0"; "x1"; "x2"; "x3"; "x4"; "x5"; "x6"; "x7";
                   "x8"; "x9"; "x10"; "x11"; "x12"; "x13"; "x14"; "x15";
                   "x19"; "x20"; "x21"; "x22"; "x23"; "x24"; "x25";
                   "x26"; "x27"; "x28"; "x16"; "x17" |] in
    if backend_reg >= 0 && backend_reg < Array.length names then
      names.(backend_reg)
    else
      Printf.sprintf "x_reg_%d" backend_reg
  else
    let float_index = backend_reg - 100 in
    if float_index >= 0 && float_index < 32 then
      Printf.sprintf "d%d" float_index
    else
      Printf.sprintf "float_reg_%d" backend_reg
