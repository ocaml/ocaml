# 2 "asmcomp/arm/arch.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the ARM processor *)

type abi = EABI | EABI_HF

type arch = ARMv4 | ARMv5 | ARMv5TE | ARMv6 | ARMv6T2 | ARMv7 | ARMv8

type fpu = Soft | VFPv2 | VFPv3_D16 | VFPv3

val abi : abi

val string_of_arch : arch -> string

val string_of_fpu : fpu -> string

(* Machine-specific command-line options *)

val arch : arch ref

val fpu : fpu ref

val thumb : bool ref

val farch : string -> unit

val ffpu : string -> unit

val command_line_options : (string * Arg.spec * string) list

(* Addressing modes *)

type addressing_mode =
    Iindexed of int                     (* reg + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type specific_operation =
    Ishiftarith of arith_operation * shift_operation * int
  | Ishiftcheckbound of shift_operation * int
  | Irevsubimm of int
  | Imulhadd      (* multiply high and add *)
  | Imuladd       (* multiply and add *)
  | Imulsub       (* multiply and subtract *)
  | Inegmulf      (* floating-point negate and multiply *)
  | Imuladdf      (* floating-point multiply and add *)
  | Inegmuladdf   (* floating-point negate, multiply and add *)
  | Imulsubf      (* floating-point multiply and subtract *)
  | Inegmulsubf   (* floating-point negate, multiply and subtract *)
  | Isqrtf        (* floating-point square root *)
  | Ibswap of int (* endianness conversion *)

and arith_operation =
    Ishiftadd
  | Ishiftsub
  | Ishiftsubrev
  | Ishiftand
  | Ishiftor
  | Ishiftxor

and shift_operation =
    Ishiftlogicalleft
  | Ishiftlogicalright
  | Ishiftarithmeticright

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

(* Recognize immediate operands *)

(* Immediate operands are 8-bit immediate values, zero-extended,
   and rotated right by 0 ... 30 bits.
   In Thumb/Thumb-2 mode we utilize 26 ... 30. *)

val is_immediate : int32 -> bool

(* Specific operations that are pure *)

val operation_is_pure : specific_operation -> bool

(* Specific operations that can raise *)

val operation_can_raise : specific_operation -> bool
