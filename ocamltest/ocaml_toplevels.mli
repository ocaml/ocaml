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

(* Descriptions of the OCaml toplevels *)

type t

val ocaml : t
val ocamlnat : t

val name: t -> string
val exit_status_variable: t -> Variables.t
val reference_variable: t -> Variables.t
val output_variable: t -> Variables.t
val directory: t -> string
val compiler: t -> Ocaml_compilers.t
val backend: t -> Ocaml_backends.t

val reference_file: t -> string Actions.A.t -> string Actions.A.t
