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

open Ocamltest_stdlib

type runtime_variant =
  | Normal
  | Debug
  | Instrumented

let runtime_variant() =
  let use_runtime = Sys.safe_getenv "USE_RUNTIME" in
  if use_runtime="d" then Debug
  else if use_runtime="i" then Instrumented
  else Normal

let ocamlrun ocamlsrcdir =
  let runtime = match runtime_variant () with
    | Normal -> "ocamlrun"
    | Debug -> "ocamlrund"
    | Instrumented -> "ocamlruni" in
  let ocamlrunfile = Filename.mkexe runtime in
  Filename.make_path [ocamlsrcdir; "byterun"; ocamlrunfile]

let ocamlc ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlc"]

let ocaml ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocaml"]

let ocamlc_dot_opt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlc.opt"]

let ocamlopt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlopt"]

let ocamlopt_dot_opt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamlopt.opt"]

let ocamlnat ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; Filename.mkexe "ocamlnat"]

let cmpbyt ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "tools"; "cmpbyt"]

let expect_test ocamlsrcdir =
  Filename.make_path
    [ocamlsrcdir; "testsuite"; "tools"; Filename.mkexe "expect_test"]

let ocamllex ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "lex"; "ocamllex"]

let ocamlyacc ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "yacc"; Filename.mkexe "ocamlyacc"]

let ocamldoc ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "ocamldoc"; "ocamldoc"]

let ocamldebug ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "debugger"; Filename.mkexe "ocamldebug"]

let ocamlobjinfo ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "tools"; "ocamlobjinfo"]
