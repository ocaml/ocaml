(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the IA64 processor *)

open Misc
open Format

(* Machine-specific command-line options *)

let command_line_options = []

(* Addressing modes -- only one! (register with no displacement) *)

type addressing_mode = Iindexed

(* Specific operations *)

type specific_operation =
    Iadd1                               (* x + y + 1 or x + x + 1 *)
  | Isub1                               (* x - y - 1 *)
  | Ishladd of int                      (* x << N + y *)
  | Isignextend of int                  (* truncate 64-bit int to 8N-bit int *)
  | Imultaddf                           (* x *. y +. z *)
  | Imultsubf                           (* x *. y -. z *)
  | Isubmultf                           (* z -. x *. y *)
  | Istoreincr of int                   (* store y at x; x <- x + N *)
  | Iinitbarrier                        (* end of object initialization *)

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed

let offset_addressing addr delta = assert false

let num_args_addressing = function Iindexed -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  printreg ppf arg.(0)

let print_specific_operation printreg op ppf arg =
  match op with
  | Iadd1 ->
      if Array.length arg >= 2 then
        fprintf ppf "%a + %a + 1 " printreg arg.(0) printreg arg.(1)
      else
        fprintf ppf "%a << 1 + 1 " printreg arg.(0)
  | Isub1 ->
      fprintf ppf "%a - %a - 1 " printreg arg.(0) printreg arg.(1)
  | Ishladd n ->
      fprintf ppf "%a << %d + %a" printreg arg.(0) n printreg arg.(1)
  | Isignextend n ->
      fprintf ppf "truncate%d %a" (n * 8) printreg arg.(0)
  | Imultaddf ->
      fprintf ppf "%a * %a + %a"
              printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf ->
      fprintf ppf "%a * %a - %a"
              printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Isubmultf ->
      fprintf ppf "%a - %a * %a"
              printreg arg.(2) printreg arg.(0) printreg arg.(1)
  | Istoreincr n ->
      fprintf ppf "[%a] := %a; %a += %d"
              printreg arg.(0) printreg arg.(1) printreg arg.(0) n
  | Iinitbarrier ->
      fprintf ppf "initbarrier"
