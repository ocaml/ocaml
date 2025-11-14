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

(** AMD64 DWARF register number mappings.

    Maps OCaml backend register numbers to DWARF register numbers
    as defined by the System V AMD64 ABI specification.

    OCaml backend numbering (from proc.ml):
      rax=0, rbx=1, rdi=2, rsi=3, rdx=4, rcx=5, r8=6, r9=7,
      r12=8, r13=9, r10=10, r11=11, rbp=12
      xmm0-15 = 100-115

    DWARF AMD64 ABI numbering:
      rax=0, rdx=1, rcx=2, rbx=3, rsi=4, rdi=5, rbp=6, rsp=7,
      r8=8, r9=9, r10=10, r11=11, r12=12, r13=13, r14=14, r15=15,
      return_address=16, xmm0-15=17-32 *)

let backend_to_dwarf_int_register = [|
  (* OCaml backend index → DWARF register number *)
  0;   (* 0:  rax → rax (0) *)
  3;   (* 1:  rbx → rbx (3) *)
  5;   (* 2:  rdi → rdi (5) *)
  4;   (* 3:  rsi → rsi (4) *)
  1;   (* 4:  rdx → rdx (1) *)
  2;   (* 5:  rcx → rcx (2) *)
  8;   (* 6:  r8  → r8  (8) *)
  9;   (* 7:  r9  → r9  (9) *)
  12;  (* 8:  r12 → r12 (12) *)
  13;  (* 9:  r13 → r13 (13) *)
  10;  (* 10: r10 → r10 (10) *)
  11;  (* 11: r11 → r11 (11) *)
  6;   (* 12: rbp → rbp (6) *)
|]

let backend_to_dwarf_float_register_offset = 100  (* Backend uses 100-115 for xmm0-15 *)
let dwarf_xmm_base = 17  (* DWARF uses 17-32 for xmm0-15 *)

(** Convert OCaml backend register number to DWARF register number.

    Raises Invalid_argument if the register number is out of range. *)
let to_dwarf_register backend_reg =
  if backend_reg < 100 then begin
    (* Integer/general-purpose register *)
    if backend_reg >= 0 && backend_reg < Array.length backend_to_dwarf_int_register then
      backend_to_dwarf_int_register.(backend_reg)
    else
      invalid_arg (Printf.sprintf
        "AMD64 backend register %d out of range (0-%d expected)"
        backend_reg (Array.length backend_to_dwarf_int_register - 1))
  end else begin
    (* Float/SSE register: backend 100-115 → DWARF 17-32 *)
    let float_index = backend_reg - backend_to_dwarf_float_register_offset in
    if float_index >= 0 && float_index < 16 then
      dwarf_xmm_base + float_index
    else
      invalid_arg (Printf.sprintf
        "AMD64 backend float register %d out of range (100-115 expected)"
        backend_reg)
  end

(** Get the register name for debugging purposes *)
let register_name backend_reg =
  (* Use proc.ml's register_name function if available, otherwise provide fallback *)
  if backend_reg < 100 then
    let names = [| "rax"; "rbx"; "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9";
                   "r12"; "r13"; "r10"; "r11"; "rbp" |] in
    if backend_reg >= 0 && backend_reg < Array.length names then
      names.(backend_reg)
    else
      Printf.sprintf "int_reg_%d" backend_reg
  else
    let float_index = backend_reg - 100 in
    if float_index >= 0 && float_index < 16 then
      Printf.sprintf "xmm%d" float_index
    else
      Printf.sprintf "float_reg_%d" backend_reg
