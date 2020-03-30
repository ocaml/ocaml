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

(* Locations of files in the OCaml source tree *)

type runtime_variant =
  | Normal
  | Debug
  | Instrumented

val runtime_variant : unit -> runtime_variant

val ocamlrun : string -> string

val ocamlc : string -> string

val ocaml : string -> string

val ocamlc_dot_opt : string -> string

val ocamlopt : string -> string

val ocamlopt_dot_opt : string -> string

val ocamlnat : string -> string

val cmpbyt : string -> string

val expect_test : string -> string

val ocamllex : string -> string

val ocamlyacc : string -> string

val ocamldoc : string -> string
val ocamldebug : string -> string
val ocamlobjinfo : string -> string
