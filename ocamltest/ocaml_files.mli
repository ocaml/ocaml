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

val ocamlrun : string

val ocamlc : string

val ocaml : string

val ocamlc_dot_opt : string

val ocamlopt : string

val ocamlopt_dot_opt : string

val ocamlnat : string

val cmpbyt : string

val expect_test : string

val ocamllex : string

val ocamlyacc : string

val ocamldoc : string
val ocamldebug : string
val ocamlobjinfo : string
val ocamlmklib : string
val codegen : string

val asmgen_archmod : string
