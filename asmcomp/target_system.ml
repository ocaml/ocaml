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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type elf_abi =
  | ARM_GNU_EABI
  | ARM_GNU_EABI_hard_float
  | AArch64
  | IA32
  | POWER_ELF32
  | POWER_ELF64v1
  | POWER_ELF64v2
  | X86_64
  | Z

type object_file_format_and_abi =
  | A_out
  | ELF of elf_abi
  | Mach_O
  | PE
  | Unknown

type windows_system =
  | Cygwin
  | MinGW
  | Native

type system =
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

(* Some references remain to Solaris in the configure script, although it
   appears that [Config.system] never gets set to "solaris" any more. *)
let (_ : system) = Solaris

type architecture =
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z

type assembler =
  | GAS_like
  | MacOS
  | MASM

type machine_width =
  | Thirty_two
  | Sixty_four

let architecture () =
  match Config.architecture with
  | "i386" -> IA32
  | "amd64" -> X86_64
  | "arm" -> ARM
  | "arm64" -> AArch64
  | "power" -> POWER
  | "s390x" -> Z
  | arch -> Misc.fatal_errorf "Unknown architecture `%s'" arch

let system_and_object_file_format_and_abi ()
      : system * object_file_format_and_abi =
  match architecture (), Config.model, Config.system with
  | IA32, _, "linux_aout" -> Linux, A_out
  | IA32, _, "linux_elf" -> Linux, ELF IA32
  | IA32, _, "bsd_aout" -> Generic_BSD, A_out
  | IA32, _, "bsd_elf" -> Generic_BSD, ELF IA32
  | IA32, _, "beos" -> BeOS, ELF IA32
  | IA32, _, "cygwin" -> Windows Cygwin, PE
  | (X86_64 | IA32), _, "macosx" -> MacOS_like, Mach_O
  | IA32, _, "gnu" -> GNU, ELF IA32
  | IA32, _, "mingw" -> Windows MinGW, PE
  | IA32, _, "win32" -> Windows Native, PE
  | POWER, "ppc64le", "elf" -> Linux, ELF POWER_ELF64v2
  | POWER, "ppc64", "elf" -> Linux, ELF POWER_ELF64v1
  | POWER, "ppc", "elf" -> Linux, ELF POWER_ELF32
  | POWER, "ppc", "netbsd" -> NetBSD, ELF IA32
  | POWER, "ppc", "bsd_elf" -> OpenBSD, ELF IA32
  | Z, _, "elf" -> Linux, ELF Z
  | ARM, _, "linux_eabihf" -> Linux, ELF ARM_GNU_EABI_hard_float
  | ARM, _, "linux_eabi" -> Linux, ELF ARM_GNU_EABI
  | ARM, _, "bsd" -> OpenBSD, ELF IA32
  | X86_64, _, "linux" -> Linux, ELF X86_64
  | X86_64, _, "gnu" -> GNU, ELF X86_64
  | X86_64, _, "dragonfly" -> Dragonfly, ELF X86_64
  | X86_64, _, "freebsd" -> FreeBSD, ELF X86_64
  | X86_64, _, "netbsd" -> NetBSD, ELF X86_64
  | X86_64, _, "openbsd" -> OpenBSD, ELF X86_64
  | X86_64, _, "darwin" -> MacOS_like, Mach_O
  | X86_64, _, "mingw" -> Windows MinGW, PE
  | AArch64, _, "linux" -> Linux, ELF AArch64
  | X86_64, _, "cygwin" -> Windows Cygwin, PE
  | X86_64, _, "win64" -> Windows Native, PE
  | _, _, "unknown" -> Unknown, Unknown
  | _, _, _ ->
    Misc.fatal_errorf "Cannot determine system type (model %s, system %s): \
        ensure `target_system.ml' matches `configure'"
      Config.model Config.system

let system () =
  fst (system_and_object_file_format_and_abi ())

let object_file_format_and_abi () =
  snd (system_and_object_file_format_and_abi ())

let string_of_architecture arch =
  match arch with
  | IA32 -> "IA32"
  | X86_64 -> "X86_64"
  | ARM -> "ARM"
  | AArch64 -> "AArch64"
  | POWER -> "POWER"
  | Z -> "Z"

let linux () =
  match system () with
  | Linux -> true
  | Windows _
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | GNU
  | Dragonfly
  | BeOS
  | Unknown -> false

let windows () =
  match system () with
  | Windows _ -> true
  | Linux
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | GNU
  | Dragonfly
  | BeOS
  | Unknown -> false

let assembler () =
  match system () with
  | Windows Native -> MASM
  | MacOS_like -> MacOS
  | Linux
  | Windows _
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | GNU
  | Dragonfly
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
