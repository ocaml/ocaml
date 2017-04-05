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
  | Unknown

type hardware = 
  | X86_32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | SPARC
  | S390x

type assembler = 
  | GNU_compatible
  | MASM

type machine_width = 
  | Thirty_two
  | Sixty_four

let system () =
  match Config.system with
  | "solaris" -> Solaris
  | "bsd" -> Other_bsd
  | "linux" -> Linux SVR4
  | "gnu" -> GNU
  | "beos" -> BeOS
  | "cygwin" -> Windows Cygwin
  | "macosx" -> MacOS_like
  | "mingw" -> Windows MinGW
  | "freebsd" -> FreeBSD
  | "netbsd" -> NetBSD
  | "openbsd" -> OpenBSD
  | "linux_eabihf" -> Linux ARM_EABI_hard_float
  | "linux_eabi" -> Linux ARM_EABI
  | "win64" -> Windows Native
  | _ -> Unknown

let windows () =
  match system () with
  | Windows _ -> true
  | Linux _
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Other_BSD
  | Solaris
  | GNU
  | BeOS
  | Unknown -> false

let assembler () =
  if windows () then MASM
  else GAS_compatible

let machine_width () =
  match Targetint.size with
  | 32 -> Thirty_two
  | 64 -> Sixty_four
  | bits -> Misc.fatal_errorf "Unknown machine width: %d" bits
