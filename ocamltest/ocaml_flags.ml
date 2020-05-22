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

open Actions
open A.Infix

let stdlib =
  let stdlib_path = Ocaml_directories.stdlib in
  "-nostdlib -I " ^ stdlib_path

let include_toplevel_directory =
  "-I " ^ Ocaml_directories.toplevel

let c_includes =
  let dir = Ocaml_directories.runtime in
  "-ccopt -I" ^ dir

let runtime_variant_flags =
  match Ocaml_files.runtime_variant with
  | Ocaml_files.Normal -> ""
  | Ocaml_files.Debug -> " -runtime-variant d"
  | Ocaml_files.Instrumented -> " -runtime-variant i"

let runtime_flags backend c_files =
  let runtime_library_flags = "-I " ^ Ocaml_directories.runtime in
  let+ rt_flags =
    match backend with
    | Ocaml_backends.Native ->
        A.return runtime_variant_flags
    | Ocaml_backends.Bytecode ->
        A.if_ c_files
          (A.return ("-custom " ^ runtime_variant_flags))
          (let+ use_runtime = (* non-custom mode *)
             A.lookup_as_bool Ocaml_variables.use_runtime
           in
           match use_runtime with
           | Some false -> ""
           | _ -> "-use-runtime " ^ Ocaml_files.ocamlrun
          )
  in
  rt_flags ^ " " ^ runtime_library_flags

let toplevel_default_flags = "-noinit -no-version -noprompt"

let ocamldebug_default_flags =
  "-no-version -no-prompt -no-time -no-breakpoint-message " ^
  ("-I " ^ Ocaml_directories.stdlib ^ " ") ^
  ("-topdirs-path " ^ Ocaml_directories.toplevel)

let ocamlobjinfo_default_flags = "-null-crc"
