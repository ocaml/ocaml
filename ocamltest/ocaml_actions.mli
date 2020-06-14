(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Actions specific to the OCaml compilers *)

val setup_ocamlc_byte_build_env : Actions.t
val ocamlc_byte : Actions.t
val check_ocamlc_byte_output : Actions.t
val setup_ocamlc_opt_build_env : Actions.t
val ocamlc_opt : Actions.t
val check_ocamlc_opt_output : Actions.t
val setup_ocamlopt_byte_build_env : Actions.t
val ocamlopt_byte : Actions.t
val check_ocamlopt_byte_output : Actions.t
val setup_ocamlopt_opt_build_env : Actions.t
val ocamlopt_opt : Actions.t
val check_ocamlopt_opt_output : Actions.t
val run_expect : Actions.t
val compare_bytecode_programs : Actions.t
val compare_binary_files : Actions.t
val setup_ocaml_build_env : Actions.t
val ocaml : Actions.t
val check_ocaml_output : Actions.t
val setup_ocamlnat_build_env : Actions.t
val ocamlnat : Actions.t
val check_ocamlnat_output : Actions.t

val setup_ocamldoc_build_env : Actions.t
val run_ocamldoc: Actions.t
val check_ocamldoc_output: Actions.t

val flat_float_array : Actions.t
val no_flat_float_array : Actions.t

val shared_libraries : Actions.t
val no_shared_libraries : Actions.t

val native_compiler : Actions.t

val afl_instrument : Actions.t
val no_afl_instrument : Actions.t

val codegen : Actions.t

val cc : Actions.t
