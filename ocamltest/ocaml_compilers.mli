(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Descriptions of the OCaml compilers *)

type t

val host: t -> Ocaml_backends.t
val target: t -> Ocaml_backends.t

val program_variable: t -> Variables.t
val program_output_variable: t -> Variables.t option

val ocamlc_byte : t

val ocamlc_opt : t

val ocamlopt_byte : t

val ocamlopt_opt : t

val name : t -> string

val exit_status_variable: t -> Variables.t
val reference_variable: t -> Variables.t
val output_variable: t -> Variables.t
val directory: t -> string

val reference_file_suffix: Environments.t -> string

val reference_file: t -> Environments.t -> string -> string
