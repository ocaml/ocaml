(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Versions = Diagnostic_history.Make()

let v1 = Versions.new_version { major = 1; minor = 0 }
include Diagnostic.New_record(Versions)(struct
    let name = "config"
    let description =
      "The set of compiler options selected at configuration time"
    let update = v1
  end)()
open Diagnostic
let set_config = Dynarray.create ()
let cvar name typ config =
  let f = new_field v1 name typ in
  Dynarray.add_last set_config (fun log -> log.Log.%[f] <- config)
let cvar_opt name typ config =
  let f = new_field v1 name (List typ) in
  Dynarray.add_last set_config (fun log -> log.Log.%[f] <- Option.to_list config)
open Config
let bytecomp_c_compiler =
  c_compiler ^ " " ^ bytecode_cflags ^ " " ^ bytecode_cppflags
let native_c_compiler =
  c_compiler ^ " " ^ native_cflags ^ " " ^ native_cppflags
let () =
  cvar "version" String version;
  cvar "standard_library_default" String standard_library_default;
  cvar_opt "standard_library_relative" String standard_library_relative;
  cvar "standard_library" String standard_library;
  cvar "ccomp_type" String ccomp_type;
  cvar "c_compiler" String c_compiler;
  cvar "bytecode_cflags" String bytecode_cflags;
  cvar "ocamlc_cflags" String bytecode_cflags;
  cvar "bytecode_cppflags" String bytecode_cppflags;
  cvar "ocamlc_cppflags" String bytecode_cppflags;
  cvar "native_cflags" String native_cflags;
  cvar "ocamlopt_cflags" String Config.native_cflags;
  cvar "native_cppflags" String native_cppflags;
  cvar "ocamlopt_cppflags" String Config.native_cppflags;

  (* bytecomp_c_compiler and native_c_compiler have been supported for a
     long time and are retained for backwards compatibility.
     For programs that don't need compatibility with older OCaml releases
     the recommended approach is to use the constituent variables
     c_compiler, ocamlc_cflags, ocamlc_cppflags etc., directly.
  *)
  cvar "bytecomp_c_compiler" String bytecomp_c_compiler;
  cvar "native_c_compiler" String native_c_compiler;

  cvar "bytecomp_c_libraries" String bytecomp_c_libraries;
  cvar "native_c_libraries" String native_c_libraries;
  cvar "compression_c_libraries" String compression_c_libraries;
  cvar "native_ldflags" String native_ldflags;
  cvar "native_pack_linker" String native_pack_linker;
  cvar "native_compiler" Bool native_compiler;
  cvar "architecture" String architecture;
  cvar "model" String model;
  cvar "int_size" Int Sys.int_size;
  cvar "word_size" Int Sys.word_size;
  cvar "system" String system;
  cvar "asm" String asm;
  cvar "asm_cfi_supported" Bool asm_cfi_supported;
  cvar "with_frame_pointers" Bool with_frame_pointers;
  cvar "ext_exe" String ext_exe;
  cvar "ext_obj" String ext_obj;
  cvar "ext_asm" String ext_asm;
  cvar "ext_lib" String ext_lib;
  cvar "ext_dll" String ext_dll;
  cvar "os_type" String Sys.os_type;
  cvar "default_executable_name" String default_executable_name;
  cvar "systhread_supported" Bool systhread_supported;
  cvar "host" String host;
  cvar "target" String target;
  cvar "bytecode_runtime_id" String bytecode_runtime_id;
  cvar "native_runtime_id" String native_runtime_id;
  cvar "flambda" Bool flambda;
  cvar "safe_string" Bool true;
  cvar "default_safe_string" Bool true;
  cvar "flat_float_array" Bool flat_float_array;
  cvar "align_double" Bool align_double;
  cvar "align_int64" Bool align_int64;
  cvar "function_sections" Bool function_sections;
  cvar "afl_instrument" Bool afl_instrument;
  cvar "tsan" Bool tsan;
  cvar "windows_unicode" Bool windows_unicode;
  cvar "supports_shared_libraries" Bool supports_shared_libraries;
  cvar "native_dynlink" Bool native_dynlink;
  cvar "naked_pointers" Bool naked_pointers;
  cvar "with_codegen_invariants" Bool with_codegen_invariants;
  cvar "reserved_header_bits" Int reserved_header_bits;

  cvar "exec_magic_number" String exec_magic_number;
  cvar "cmi_magic_number" String cmi_magic_number;
  cvar "cmo_magic_number" String cmo_magic_number;
  cvar "cma_magic_number" String cma_magic_number;
  cvar "cmx_magic_number" String cmx_magic_number;
  cvar "cmxa_magic_number" String cmxa_magic_number;
  cvar "ast_impl_magic_number" String ast_impl_magic_number;
  cvar "ast_intf_magic_number" String ast_intf_magic_number;
  cvar "cmxs_magic_number" String cmxs_magic_number;
  cvar "cmt_magic_number" String cmt_magic_number;
  cvar "linear_magic_number" String linear_magic_number;
  seal v1

let log_variables log =
  Dynarray.iter (fun f -> f log) set_config

let print log =
  log_variables log;
  Log.flush log

let var x =
  let log = Log.tmp scheme in
  let () = log_variables log in
  match Log.dynamic_get log x with
  | None -> None
  | Some (Diagnostic.V (ty,v)) ->
      let s = match ty with
        | Diagnostic.String -> (v:string)
        | Diagnostic.Int -> Int.to_string v
        | Diagnostic.Bool -> string_of_bool v
        | Diagnostic.Unit -> "()"
        | Diagnostic.(List String) ->
            let pp_sep ppf () = Format.pp_print_space ppf () in
            Format.asprintf "@[%a@]"
              (Format.pp_print_list ~pp_sep Format.pp_print_string)
              v
        | _ -> assert false
      in
      Some s

let show_variable_and_exit x =
  match var x with
  | Some v ->
      (* we intentionally don't print a newline to avoid Windows \r
         issues: bash only strips the trailing \n when using a command
         substitution $(ocamlc -config-var foo), so a trailing \r would
         remain if printing a newline under Windows and scripts would
         have to use $(ocamlc -config-var foo | tr -d '\r')
         for portability. Ugh. *)
      print_string v;
      exit 0
  | None ->
      exit 2
