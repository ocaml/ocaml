(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types corresponding to the compiler's target machine. *)

type linux_abi =
  | SVR4
  | ARM_EABI
  | ARM_EABI_hard_float

type windows_system =
  | Cygwin
  | MinGW
  | Native

type system =
  | Linux of linux_abi
  | Windows of windows_system
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Other_BSD
  | Solaris
  | GNU
  | BeOS
  (* CR mshinwell: I think we should delete "Unknown".  Likewise probably
     GNU and BeOS. *)
  | Unknown

type architecture = 
  | IA32
  | IA64
  | ARM
  | AArch64
  | POWER
  | SPARC
  | S390x

type assembler =
  | GAS_like
  | MacOS
  | MASM

type machine_width =
  | Thirty_two
  | Sixty_four

(** The target system of the OCaml compiler. *)
val system : unit -> system

(** Whether the target system is a Windows platform. *)
val windows : unit -> bool

(** Whether the target system is a Windows 64-bit native platform (not
    MinGW or Cygwin). *)
val win64 : unit -> bool

(** The architecture of the target system. *)
val architecture : unit -> architecture

(** The assembler being used. *)
val assembler : unit -> assembler

(** The natural machine width of the target system. *)
val machine_width : unit -> machine_width
