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

(* Definition of variables used by built-in actions *)

(* The variables are listed in alphabetical order *)

val arguments : Variables.t

val files : Variables.t

val ld_library_path : Variables.t

val ocamltest_env : Variables.t

val ocamltest_log : Variables.t

val output : Variables.t

val program : Variables.t
val program2 : Variables.t

val reference : Variables.t

val script : Variables.t

val stdin : Variables.t
val stdout : Variables.t
val stderr : Variables.t

val test_build_directory : Variables.t
val test_build_directory_prefix : Variables.t

val test_file : Variables.t

val test_source_directory : Variables.t

val test_pass : Variables.t

val test_skip : Variables.t

val test_fail : Variables.t
