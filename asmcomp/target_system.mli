(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types corresponding to the compiler's target machine. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type elf_abi = private
  | ARM_GNU_EABI
  | ARM_GNU_EABI_hard_float
  | AArch64
  | IA32
  | POWER_ELF32
  | POWER_ELF64v1
  | POWER_ELF64v2
  | X86_64
  | Z

type object_file_format_and_abi = private
  | A_out
  | ELF of elf_abi
  | Mach_O
  | PE
  | Unknown

type windows_system = private
  | Cygwin
  | MinGW
  | Native

type system = private
  | Linux
  | Windows of windows_system
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | Dragonfly
  | GNU
  | BeOS
  | Unknown

type architecture = private
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z

type assembler = private
  | GAS_like
  | MacOS
  | MASM

type machine_width =
  | Thirty_two
  | Sixty_four

(** The target system of the OCaml compiler. *)
val system : unit -> system

(** The target object file format and ABI of the OCaml compiler. *)
val object_file_format_and_abi : unit -> object_file_format_and_abi

(** Whether the target system is a Linux platform. *)
val linux : unit -> bool

(** Whether the target system is a Windows platform. *)
val windows : unit -> bool

(** Whether the target system is a Windows 32-bit native platform (not
    MinGW or Cygwin). *)
val win32 : unit -> bool

(** Whether the target system is a Windows 64-bit native platform (not
    MinGW or Cygwin). *)
val win64 : unit -> bool

(** Whether the target system is Mac OS X, macOS, or some other system
    running a Darwin kernel and associated userland tools. *)
val macos_like : unit -> bool

(** The architecture of the target system. *)
val architecture : unit -> architecture

(** Convert an architecture to a string. *)
val string_of_architecture : architecture -> string

(** The assembler being used. *)
val assembler : unit -> assembler

(** The natural machine width of the target system. *)
val machine_width : unit -> machine_width
