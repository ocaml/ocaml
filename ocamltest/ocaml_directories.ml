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

(* Locations of directories in the OCaml source tree *)

open Ocamltest_stdlib

let srcdir () =
  Sys.getenv_with_default_value "OCAMLSRCDIR" Ocamltest_config.ocamlsrcdir

let stdlib ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "stdlib"]

let libunix ocamlsrcdir =
  let subdir = if Sys.os_type="Win32" then "win32unix" else "unix" in
  Filename.make_path [ocamlsrcdir; "otherlibs"; subdir]

let toplevel ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "toplevel"]

let runtime ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "byterun"]

let runtime_library backend ocamlsrcdir =
  let backend_lib_dir = match backend with
    | Ocaml_backends.Native -> "asmrun"
    | Ocaml_backends.Bytecode -> "byterun" in
  Filename.make_path [ocamlsrcdir; backend_lib_dir]

let tools ocamlsrcdir =
  Filename.make_path [ocamlsrcdir; "tools"]
