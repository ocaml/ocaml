(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt         *)
(*                          Bill O'Farrell, IBM                        *)
(*                                                                     *)
(*    Copyright 2015 Institut National de Recherche en Informatique    *)
(*    et en Automatique. Copyright 2015 IBM (Bill O'Farrell with       *)
(*    help from Tristan Amini). All rights reserved.  This file is     *)
(*    distributed under the terms of the Q Public License version 1.0. *)
(*                                                                     *)
(***********************************************************************)

(* Specific operations for the Z processor *)

open Format

(* Machine-specific command-line options *)

let pic_code = ref true

let command_line_options =
  [ "-fPIC", Arg.Set pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear pic_code,
      " Generate position-dependent machine code" ]

(* Specific operations *)

type specific_operation =
    Imultaddf                           (* multiply and add *)
  | Imultsubf                           (* multiply and subtract *)

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2                           (* reg + reg *)

(* Sizes, endianness *)

let big_endian = true

let s390x =
  match Config.model with "s390x" -> true | _ -> false

let size_addr = 8
let size_int = size_addr
let size_float = 8

let allow_unaligned_access = false

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 -> assert false

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1
  | Iindexed2 -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "\"%s\"%s" s idx
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 ->
      fprintf ppf "%a + %a" printreg arg.(0) printreg arg.(1)

let print_specific_operation printreg op ppf arg =
  match op with
  | Imultaddf ->
      fprintf ppf "%a *f %a +f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf ->
      fprintf ppf "%a *f %a -f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
