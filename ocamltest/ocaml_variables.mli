(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of OCaml-specific variables *)

(* The variables are listed in alphabetical order *)

val all_modules : Variables.t

val arch : Variables.t

val binary_modules : Variables.t

val bytecc_libs : Variables.t
(** Libraries to link with for bytecode *)

val cpp : Variables.t

val cppflags : Variables.t

val cc : Variables.t

val cflags : Variables.t

val caml_ld_library_path : Variables.t

val codegen_exit_status : Variables.t

val compare_programs : Variables.t

val compiler_directory_suffix : Variables.t

val compiler_reference : Variables.t

val compiler_reference2 : Variables.t

val compiler_reference_suffix : Variables.t

val compiler_output : Variables.t

val compiler_output2 : Variables.t

val compiler_stdin : Variables.t

val compile_only : Variables.t

val csc : Variables.t

val csc_flags : Variables.t

val directories : Variables.t

val flags : Variables.t

val last_flags : Variables.t

val libraries : Variables.t

val mkdll : Variables.t
(** Command used to make a DLL *)

val mkexe : Variables.t
(** Command used to build an executable program *)

val module_ : Variables.t

val modules : Variables.t

val nativecc_libs : Variables.t
(** Libraries to link with for native code *)

val objext : Variables.t
val libext : Variables.t
val asmext : Variables.t

val ocamlc_byte : Variables.t
val ocamlopt_byte : Variables.t
val ocamlrun : Variables.t

val ocamlc_flags : Variables.t
val ocamlc_default_flags : Variables.t

val ocamllex_flags : Variables.t

val ocamlopt_flags : Variables.t
val ocamlopt_default_flags : Variables.t

val ocamlyacc_flags : Variables.t

val ocaml_exit_status : Variables.t

val ocaml_filetype_flag : Variables.t

val ocamlc_byte_exit_status : Variables.t

val ocamlopt_byte_exit_status : Variables.t

val ocamlnat_exit_status : Variables.t

val ocamlc_opt_exit_status : Variables.t

val ocamlopt_opt_exit_status : Variables.t

val ocamlrunparam : Variables.t

val ocamlsrcdir : Variables.t

val ocamldebug_flags : Variables.t


val os_type : Variables.t

val ocamldoc_flags : Variables.t
val ocamldoc_backend : Variables.t
val ocamldoc_exit_status : Variables.t
val ocamldoc_output : Variables.t
val ocamldoc_reference : Variables.t

val ocaml_script_as_argument : Variables.t

val plugins : Variables.t

val shared_library_cflags : Variables.t

val sharedobjext : Variables.t

val use_runtime : Variables.t
