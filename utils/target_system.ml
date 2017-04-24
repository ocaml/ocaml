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

type architecture =
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | SPARC
  | Z

type assembler =
  | GAS_like
  | MacOS
  | MASM

type machine_width =
  | Thirty_two
  | Sixty_four

let system () =
  match Config.system with
  | "solaris" -> Solaris
  | "bsd" -> Other_BSD
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
  | "windows" -> Windows Native
  | _ -> Unknown

let architecture () =
  match Config.architecture with
  | "i386" -> IA32
  | "amd64" -> X86_64
  | "arm" -> ARM
  | "arm64" -> AArch64
  | "power" -> POWER
  | "sparc" -> SPARC
  | "s390x" -> Z
  | arch -> Misc.fatal_errorf "Unknown architecture `%s'" arch

let string_of_architecture arch =
  match arch with
  | IA32 -> "IA32"
  | X86_64 -> "X86_64"
  | ARM -> "ARM"
  | AArch64 -> "AArch64"
  | POWER -> "POWER"
  | SPARC -> "SPARC"
  | Z -> "Z"

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
  match system () with
  | Windows Native -> MASM
  | MacOS_like -> MacOS
  | Linux _
  | Windows _
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Other_BSD
  | Solaris
  | GNU
  | BeOS
  | Unknown -> GAS_like

let machine_width () =
  match Targetint.size with
  | 32 -> Thirty_two
  | 64 -> Sixty_four
  | bits -> Misc.fatal_errorf "Unknown machine width: %d" bits

let win32 () =
  match system (), machine_width () with
  | Windows Native, Thirty_two -> true
  | _, _ -> false

let win64 () =
  match system (), machine_width () with
  | Windows Native, Sixty_four -> true
  | _, _ -> false

let macos_like () =
  match system () with
  | MacOS_like -> true
  | _ -> false
