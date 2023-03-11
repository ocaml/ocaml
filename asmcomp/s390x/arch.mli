# 2 "asmcomp/s390x/arch.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Xavier Leroy, projet Gallium, INRIA Rocquencourt            *)
(*                          Bill O'Farrell, IBM                           *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2015 IBM (Bill O'Farrell with help from Tristan Amini).    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the Z processor *)

(* Machine-specific command-line options *)

val pic_code : bool ref

val command_line_options : (string * Arg.spec * string) list

(* Specific operations *)

type specific_operation =
    Imultaddf                           (* multiply and add *)
  | Imultsubf                           (* multiply and subtract *)

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2 of int                    (* reg + reg + displ *)

(* Sizes, endianness *)

val big_endian : bool

val size_addr : int

val size_int : int

val size_float : int

val allow_unaligned_access : bool

(* Behavior of division *)

val division_crashes_on_overflow : bool

(* Operations on addressing modes *)

val identity_addressing : addressing_mode

val offset_addressing : addressing_mode -> int -> addressing_mode

val num_args_addressing : addressing_mode -> int

(* Printing operations and addressing modes *)

val print_addressing :
  (Format.formatter -> 'a -> unit) -> addressing_mode ->
  Format.formatter -> 'a array -> unit

val print_specific_operation :
  (Format.formatter -> 'a -> unit) -> specific_operation ->
  Format.formatter -> 'a array -> unit

(* Specific operations that are pure *)

val operation_is_pure : specific_operation -> bool

(* Specific operations that can raise *)

val operation_can_raise : specific_operation -> bool
