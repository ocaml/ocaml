(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Id *)

let version = "3.02+5 (2001-09-07)"

let standard_library =
  try
    Sys.getenv "CAMLLIB"
  with Not_found ->
    ""

let standard_runtime = ""
let bytecomp_c_compiler = "gcc -fno-defer-pop -Wall -Wno-unused -fPIC"
let bytecomp_c_linker = "gcc -Wl,-E"
let bytecomp_c_libraries = "-lm -ldl -lcurses"
let bytecomp_c_rpath = "-Wl,-rpath,"
let native_c_compiler = "gcc -Wall -Wno-unused"
let native_c_linker = "gcc "
let native_c_libraries = "-lm"
let native_c_rpath = "-Wl,-rpath,"
let native_partial_linker = "ld -r "
let ranlib = "ranlib"
let systhreads_link = ""

let exec_magic_number = "Caml1999X006"
and cmi_magic_number = "Caml1999I008"
and cmo_magic_number = "Caml1999O004"
and cma_magic_number = "Caml1999A005"
and cmx_magic_number = "Caml1999Y006"
and cmxa_magic_number = "Caml1999Z007"
and ast_impl_magic_number = "Caml1999M008"
and ast_intf_magic_number = "Caml1999N007"

let load_path = ref ([] : string list)

let interface_suffix = ref ".mli"

let max_tag = 248
let max_young_wosize = 256
let stack_threshold = 256 (* see byterun/config.h *)

let architecture = "i386"
let model = "default"
let system = "linux_elf"

let ext_obj = ".o"
let ext_asm = ".s"
let ext_lib = ".a"
let ext_dll = ".so"
