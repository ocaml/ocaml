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

val c_preprocessor : Variables.t

val compare_programs : Variables.t

val compiler_directory_suffix : Variables.t

val compiler_reference : Variables.t

val compiler_reference2 : Variables.t

val compiler_reference_suffix : Variables.t

val compiler_output : Variables.t

val compiler_output2 : Variables.t

val directories : Variables.t

val flags : Variables.t

val libraries : Variables.t

val modules : Variables.t

val ocamlc_flags : Variables.t
val ocamlc_default_flags : Variables.t

val ocamlopt_flags : Variables.t
val ocamlopt_default_flags : Variables.t

val ocaml_exit_status : Variables.t

val ocamlc_byte_exit_status : Variables.t

val ocamlopt_byte_exit_status : Variables.t

val ocamlnat_exit_status : Variables.t

val ocamlc_opt_exit_status : Variables.t

val ocamlopt_opt_exit_status : Variables.t

val ocamlsrcdir : Variables.t

val os_type : Variables.t

val source_modules : Variables.t
