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

let ocamlrun =
  let runtime = match runtime_variant () with
    | Normal -> "ocamlrun"
    | Debug -> "ocamlrund"
    | Instrumented -> "ocamlruni" in
  let ocamlrunfile = Filename.mkexe runtime in
  Filename.make_path [Ocaml_directories.srcdir; "runtime"; ocamlrunfile]

let ocamlc =
  Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlc"]

let ocaml =
  Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocaml"]

let ocamlc_dot_opt =
  Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlc.opt"]

let ocamlopt =
  Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlopt"]

let ocamlopt_dot_opt =
  Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlopt.opt"]

let ocamlnat =
  Filename.make_path [Ocaml_directories.srcdir; Filename.mkexe "ocamlnat"]

let cmpbyt =
  Filename.make_path
    [Ocaml_directories.srcdir; "tools"; Filename.mkexe "cmpbyt"]

let expect_test =
  Filename.make_path
    [Ocaml_directories.srcdir; "testsuite"; "tools";
     Filename.mkexe "expect_test"]

let ocamllex =
  Filename.make_path
    [Ocaml_directories.srcdir; "lex"; Filename.mkexe "ocamllex"]

let ocamlyacc =
  Filename.make_path
    [Ocaml_directories.srcdir; "yacc"; Filename.mkexe "ocamlyacc"]

let ocamldoc =
  Filename.make_path
    [Ocaml_directories.srcdir; "ocamldoc"; Filename.mkexe "ocamldoc"]

let ocamldebug =
  Filename.make_path
    [Ocaml_directories.srcdir; "debugger"; Filename.mkexe "ocamldebug"]

let ocamlobjinfo =
  Filename.make_path
    [Ocaml_directories.srcdir; "tools"; Filename.mkexe "ocamlobjinfo"]

let ocamlmklib =
  Filename.make_path
    [Ocaml_directories.srcdir; "tools"; Filename.mkexe "ocamlmklib"]

let codegen =
  Filename.make_path
    [Ocaml_directories.srcdir; "testsuite"; "tools"; Filename.mkexe "codegen"]

let asmgen_archmod =
  let objname =
    "asmgen_" ^ Ocamltest_config.arch ^ "." ^ Ocamltest_config.objext
  in
  Filename.make_path [Ocaml_directories.srcdir; "testsuite"; "tools"; objname]
