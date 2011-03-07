(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Specific operations for the Alpha processor *)

open Misc
open Format

(* Machine-specific command-line options *)

let command_line_options = []

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)

(* Specific operations *)

type specific_operation =
    Iadd4 | Iadd8 | Isub4 | Isub8       (* Scaled adds and subs *)
  | Ireloadgp of bool                   (* The ldgp instruction *)
  | Itrunc32                            (* Truncate 64-bit int to 32 bit *)

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, n) ->
      fprintf ppf "\"%s\"%s" s
      (if n <> 0 then Printf.sprintf " + %i" n else "")
  | Iindexed n ->
      fprintf ppf "%a%s" printreg arg.(0)
      (if n <> 0 then Printf.sprintf " + %i" n else "")

let print_specific_operation printreg op ppf arg =
  match op with
  | Iadd4 -> fprintf ppf "%a  * 4 + %a" printreg arg.(0) printreg arg.(1)
  | Iadd8 -> fprintf ppf "%a  * 8 + %a" printreg arg.(0) printreg arg.(1)
  | Isub4 -> fprintf ppf "%a  * 4 - %a" printreg arg.(0) printreg arg.(1)
  | Isub8 -> fprintf ppf "%a  * 8 - %a" printreg arg.(0) printreg arg.(1)
  | Ireloadgp _ -> fprintf ppf "ldgp"
  | Itrunc32 -> fprintf ppf "truncate32 %a" printreg arg.(0)

(* Distinguish between the Digital assembler and other assemblers (e.g. gas) *)

let digital_asm =
  match Config.system with
    "digital" -> true
  | _ -> false
