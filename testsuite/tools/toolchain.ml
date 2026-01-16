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

(* cf. OCAML_CC_VENDOR in aclocal.m4 and utils.config.mli *)
let is_clang =
  List.mem "clang" (String.split_on_char '-' Config.c_compiler_vendor)

let is_msvc = Config.ccomp_type = "msvc"

let is_clang_assembler =
  (* The clang-cl build of the MSVC port still has to use MASM at present; other
     systems which use clang use its internal assembler. *)
  is_clang && not is_msvc

let c_compiler_debug_paths_can_be_absolute =
  if is_msvc then
    (* clang-cl always embeds relative paths in objects (for reasons which are
       not entirely clear) *)
    not is_clang
  else
    true

let linker_propagates_debug_information =
  (* For MSVC, executables will only contain debug information if it's
     explicitly requested at link-time. At present, even when compiling with
     clang-cl, the Microsoft linker is still used. *)
  not is_msvc

let c_compiler_always_embeds_build_path =
  (* .obj files always contain the build path, regardless of flags *)
  is_msvc && not is_clang

let asmrun_assembled_with_cc =
  (* The MSVC port directly assembles amd64nt.asm; all other systems use the C
     compiler in order to use the preprocessor. *)
  not is_msvc

let assembler_embeds_build_path =
  (* The clang internal assembler only embeds build paths when called by
     ocamlopt if clang is emitting DWARF v5 by default. *)
  if is_clang_assembler then
    Option.exists (fun version -> version > 4) Config.asm_dwarf_version
  else
    (* The clang internal assembler does not embed build paths when called by
       ocamlopt and neither does the GNU assembler on Windows. *)
    not (String.starts_with ~prefix:"mingw" Config.system)
    && not is_clang_assembler

let linker_embeds_build_path =
  (* At present, we don't do anything to prevent the macOS linker embedding the
     build path as part of RPATH (Runtime Search Path), so macOS executables and
     shared libraries {e always} contain the build path. *)
  Config.system = "macosx"

let linker_is_flexlink =
  (* Windows always uses flexlink; Cygwin only uses it when shared library
     support is enabled. *)
  Sys.win32 || Sys.cygwin && Config.supports_shared_libraries
