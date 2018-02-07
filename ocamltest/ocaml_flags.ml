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

(* Flags used in OCaml commands *)

open Ocamltest_stdlib

let stdlib ocamlsrcdir =
  let stdlib_path = Ocaml_directories.stdlib ocamlsrcdir in
  "-nostdlib -I " ^ stdlib_path

let include_toplevel_directory ocamlsrcdir =
  "-I " ^ (Ocaml_directories.toplevel ocamlsrcdir)

let c_includes ocamlsrcdir =
  let dir = Ocaml_directories.runtime ocamlsrcdir in
  "-ccopt -I" ^ dir

let use_runtime backend ocamlsrcdir = match backend with
  | Ocaml_backends.Bytecode ->
    let ocamlrun = Ocaml_files.ocamlrun ocamlsrcdir in
    "-use-runtime " ^ ocamlrun
  | Ocaml_backends.Native -> ""

let runtime_variant backend ocamlsrcdir =
  let variant = Ocaml_files.runtime_variant() in
  if variant=Ocaml_files.Normal then ""
  else begin
    let variant_str = if variant=Ocaml_files.Debug then "d" else "i" in
    let backend_lib = match backend with
      | Ocaml_backends.Bytecode -> "byterun"
      | Ocaml_backends.Native -> "asmrun" in
    let backend_lib_dir = Filename.make_path [ocamlsrcdir; backend_lib] in
    ("-runtime-variant " ^ variant_str ^" -I " ^ backend_lib_dir)
  end
