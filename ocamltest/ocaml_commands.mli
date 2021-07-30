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

(* Helper functions to build OCaml-related commands *)

val ocamlrun_ocamlc : string

val ocamlrun_ocamlopt : string

val ocamlrun_ocaml : string

val ocamlrun_expect_test : string

val ocamlrun_ocamllex : string

val ocamlrun_ocamldoc : string

val ocamlrun_ocamldebug : string

val ocamlrun_ocamlobjinfo : string

val ocamlrun_ocamlmklib : string
val ocamlrun_codegen : string
