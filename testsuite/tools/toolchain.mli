(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Toolchain properties. These are various properties, principally derived from
    the compiler's [Config] module, which describe the behaviour of the
    C compiler and assembler and how OCaml invokes them. *)

val c_compiler_debug_paths_can_be_absolute : bool
(** [true] if the C compiler is {e capable} of embedding absolute source file
    locations in debug information. *)

val linker_propagates_debug_information : bool
(** If objects have been compiled/assembled with debug information (i.e. with
    {v -g v}), [linker_propagates_debug_information] is [true] if executables
    produced by the linker contain that debug information even if {v -g v} (or
    equivalent) was not passed to the process which links that executable. *)

val c_compiler_always_embeds_build_path : bool
(** [true] if the C compiler unconditionally embeds the path of the source file
    in compiled objects. *)

val asmrun_assembled_with_cc : bool
(** [true] if the runtime assembly files are assembled using the C compiler,
    rather than by calling the assembler directly. *)

val assembler_embeds_build_path : bool
(** [true] if the assembler (as called by ocamlopt) will end up embedding the
    build path in assembled objects. *)

val linker_embeds_build_path : bool
(** [true] if the linker always embeds the build path in both executables and
    shared libraries. *)

val linker_is_flexlink : bool
(** [true] if {v flexlink v} is responsible for linking executables and shared
    libraries. *)
