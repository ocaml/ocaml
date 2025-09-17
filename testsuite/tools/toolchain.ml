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
  is_msvc

let asmrun_assembled_with_cc =
  (* The MSVC port directly assembles amd64nt.asm; all other systems use the C
     compiler in order to use the preprocessor. *)
  not is_msvc

let assembler_embeds_build_path =
  if is_clang_assembler && Config.system = "macosx" then
    (* Xcode 16 targetting macOS 15 or later uses DWARF v5 and embeds build
       paths by default, cf. https://developer.apple.com/documentation/xcode-release-notes/xcode-16-release-notes *)
    match String.split_on_char '-' Config.c_compiler_vendor,
          String.split_on_char '-' Config.target with
    | ["clang"; major; _], [_; "apple"; darwin]
      when String.starts_with ~prefix:"darwin" darwin ->
        (* Xcode 16.0 shipped with clang-16.00.0.26.3
           macOS 15 uses Darwin 24.x *)
        let clang_major =
          Scanf.sscanf_opt major "%u%!" (fun x -> x >= 16)
          |> Option.value ~default:true (* Assume up-to-date *)
        and darwin_major =
          Scanf.sscanf_opt darwin "darwin%u." (fun x -> x >= 24)
          |> Option.value ~default:true (* Assume up-to-date *)
        in
        clang_major && darwin_major
    | _ ->
        false
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
