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

(* Locations of files and directories inside the OCaml source tree *)

open Ocamltest_stdlib

let ocamlsrcdir () =
  try Sys.getenv "OCAMLSRCDIR"
  with Not_found -> Ocamltest_config.ocamlsrcdir

type runtime_variant =
  | Normal
  | Debug
  | Instrumented

let runtime_variant() =
  let use_runtime = try Sys.getenv "USE_RUNTIME" with Not_found -> "" in
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
