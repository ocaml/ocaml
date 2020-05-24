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

(* Descriptions of the OCaml tools *)

module Compiler : sig
  type t

  val name: t -> string
  val reference_variable: t -> Variables.t
  val output_variable: t -> Variables.t
  val directory: t -> string
  val reference_file: t -> Environments.t -> string -> string
  val exit_status_variable: t -> Variables.t

  val host: t -> Ocaml_backends.t
  val target: t -> Ocaml_backends.t

  val program_variable: t -> Variables.t
  val program_output_variable: t -> Variables.t option

  val ocamlc_byte: t
  val ocamlc_opt: t
  val ocamlopt_byte: t
  val ocamlopt_opt: t
end

module Toplevel : sig
  type t

  val name: t -> string
  val reference_variable: t -> Variables.t
  val output_variable: t -> Variables.t
  val directory: t -> string
  val reference_file: t -> Environments.t -> string -> string
  val exit_status_variable: t -> Variables.t

  val compiler: t -> Compiler.t
  val backend: t -> Ocaml_backends.t

  val flags: t -> string

  val ocaml: t
  val ocamlnat: t
end

module Ocamldoc : sig
  type t

  val name: t -> string
  val reference_variable: t -> Variables.t
  val output_variable: t -> Variables.t
  val directory: t -> string
  val reference_file: t -> Environments.t -> string -> string
  val reference_file_suffix: Environments.t -> string
  val exit_status_variable: t -> Variables.t
  val output_file: t -> Environments.t -> string -> string

  val ocamldoc: t
end

type t =
  | Compiler of Compiler.t
  | Toplevel of Toplevel.t
  | Ocamldoc of Ocamldoc.t

val family: t -> string

val expected_exit_status: Environments.t -> t -> int
val reference_variable: t -> Variables.t
val reference_file: t -> Environments.t -> string -> string
val output_variable: t -> Variables.t
val directory: t -> string
