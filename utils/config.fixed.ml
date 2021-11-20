#2 "utils/config.fixed.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       David Allsopp, Tarides UK.                       *)
(*                                                                        *)
(*   Copyright 2022 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Configuration for the boot compiler. The compiler should refuse to bootstrap
   if configured with values which would contradict the configuration below.
   Config.in_boot_compiler is used throughout the compiler to prevent accidental
   use of "<boot compiler>" values, but should not be used to control
   compilation decisions (i.e. if Config.in_boot_compiler is found to be true,
   the result of the code path should be a fatal error) *)

let bindir = "<boot compiler>"
let standard_library_default = "<boot compiler>"
let in_boot_compiler = true
let ccomp_type = "<boot compiler>"
let c_compiler = "<boot compiler>"
let c_output_obj = "<boot compiler>"
let c_has_debug_prefix_map = false
let as_has_debug_prefix_map = false
let ocamlc_cflags = "<boot compiler>"
let ocamlc_cppflags = "<boot compiler>"
let ocamlopt_cflags = "<boot compiler>"
let ocamlopt_cppflags = "<boot compiler>"
let bytecomp_c_libraries = "<boot compiler>"
let bytecomp_c_compiler = "<boot compiler>"
let native_c_compiler = "<boot compiler>"
let native_c_libraries = "<boot compiler>"
let native_pack_linker = "<boot compiler>"
let default_rpath = "<boot compiler>"
let mksharedlibrpath = "<boot compiler>"
let ar = "<boot compiler>"
let supports_shared_libraries = false
let mkdll = "<boot compiler>"
let mkexe = "<boot compiler>"
let mkmaindll = "<boot compiler>"
let flambda = false
let with_flambda_invariants = false
let with_cmm_invariants = false
let windows_unicode = false
let force_instrumented_runtime = false
let flat_float_array = true
let function_sections = false
let afl_instrument = false
let architecture = "none"
let model = "default"
let system = "unknown"
let asm = "<boot compiler>"
let asm_cfi_supported = false
let with_frame_pointers = false
let profinfo = false
let profinfo_width = 0
let ext_exe = "<boot compiler>"
let ext_obj = "<boot compiler>"
let ext_asm = "<boot compiler>"
let ext_lib = "<boot compiler>"
let ext_dll = "<boot compiler>"
let host = "<boot compiler>"
let target = "<boot compiler>"
let systhread_supported = false
let flexdll_dirs = []
