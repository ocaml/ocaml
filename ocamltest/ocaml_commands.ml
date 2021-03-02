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

let ocamlrun program =
  Ocaml_files.ocamlrun ^ " " ^ program

let ocamlrun_ocamlc = ocamlrun Ocaml_files.ocamlc

let ocamlrun_ocamlopt = ocamlrun Ocaml_files.ocamlopt

let ocamlrun_ocaml = ocamlrun Ocaml_files.ocaml

let ocamlrun_expect_test =
  ocamlrun Ocaml_files.expect_test

let ocamlrun_ocamllex = ocamlrun Ocaml_files.ocamllex

let ocamlrun_ocamldoc =
  ocamlrun Ocaml_files.ocamldoc

let ocamlrun_ocamldebug =
  ocamlrun Ocaml_files.ocamldebug

let ocamlrun_ocamlobjinfo =
  ocamlrun Ocaml_files.ocamlobjinfo

let ocamlrun_ocamlmklib =
  ocamlrun Ocaml_files.ocamlmklib

let ocamlrun_codegen =
  ocamlrun Ocaml_files.codegen
