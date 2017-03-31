(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types corresponding to the compiler's target machine. *)

(** Target systems of the OCaml native code compiler. *)
type system = private
  (* 32 and 64 bit *)
  | MacOS
  | Gnu
  | Cygwin
  (* 32 bit only *)
  | Solaris
  | Win32
  | Linux_elf
  | Bsd_elf
  | Beos
  | Mingw
  (* 64 bit only *)
  | Win64
  | Linux
  | Mingw64
  | Unknown

(** The selected target system of the OCaml compiler. *)
val system : system

(** Whether the target system is a Windows platform. *)
val windows : bool

(** Whether the target system uses the MASM assembler. *)
val masm : bool

type platform = private
  | X86_32
  | X86_64
  | ARM_32
  | ARM_64
  | POWER
  | SPARC
  | S390

val platform : platform
