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

let ocamlrun ocamlsrcdir program =
  (Ocaml_files.ocamlrun ocamlsrcdir) ^ " " ^ (program ocamlsrcdir)

let ocamlrun_ocamlc ocamlsrcdir = ocamlrun ocamlsrcdir Ocaml_files.ocamlc

let ocamlrun_ocamlopt ocamlsrcdir = ocamlrun ocamlsrcdir Ocaml_files.ocamlopt

let ocamlrun_ocaml ocamlsrcdir = ocamlrun ocamlsrcdir Ocaml_files.ocaml

let ocamlrun_expect_test ocamlsrcdir =
  ocamlrun ocamlsrcdir Ocaml_files.expect_test

let ocamlrun_ocamllex ocamlsrcdir = ocamlrun ocamlsrcdir Ocaml_files.ocamllex

let ocamlrun_ocamldoc ocamlsrcdir =
  ocamlrun ocamlsrcdir Ocaml_files.ocamldoc

let ocamlrun_ocamldebug ocamlsrcdir =
  ocamlrun ocamlsrcdir Ocaml_files.ocamldebug

let ocamlrun_ocamlobjinfo ocamlsrcdir =
  ocamlrun ocamlsrcdir Ocaml_files.ocamlobjinfo
