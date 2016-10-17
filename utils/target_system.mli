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
  | S_macosx
  | S_gnu
  | S_cygwin
  (* 32 bit only *)
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  (* 64 bit only *)
  | S_win64
  | S_linux
  | S_mingw64
  | S_unknown

(** The selected target system of the OCaml compiler. *)
val system : system

(** Whether the target system is a Windows platform. *)
val windows : bool

(** Whether the target system uses the MASM assembler. *)
val masm : bool
