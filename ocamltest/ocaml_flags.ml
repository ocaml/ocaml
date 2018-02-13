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

let stdlib ocamlsrcdir =
  let stdlib_path = Ocaml_directories.stdlib ocamlsrcdir in
  "-nostdlib -I " ^ stdlib_path

let include_toplevel_directory ocamlsrcdir =
  "-I " ^ (Ocaml_directories.toplevel ocamlsrcdir)

let c_includes ocamlsrcdir =
  let dir = Ocaml_directories.runtime ocamlsrcdir in
  "-ccopt -I" ^ dir

let runtime_flags ocamlsrcdir backend c_files = match backend with
  | Ocaml_backends.Native -> ""
  | Ocaml_backends.Bytecode ->
    if c_files then begin (* custm mode *)
      let runtime_variant = Ocaml_files.runtime_variant() in
      let variant_flag = match runtime_variant with
        | Ocaml_files.Normal -> ""
        | Ocaml_files.Debug -> " -runtime-variant d"
        | Ocaml_files.Instrumented -> " -runtime-variant i" in
      "-custom" ^ variant_flag ^ " -I " ^
        (Ocaml_directories.runtime ocamlsrcdir)
      (* when using the debug or instrumented runtimes, we need to include
         the byterun directory so that libcamlrund and libcamlruni
         can be found. This is not necessary with the normal runtime
         because libcamlrun is copied to the stdlib directory which
         will be included anyway
      *)
    end else begin (* non-custom mode *)
      let ocamlrun = Ocaml_files.ocamlrun ocamlsrcdir in
      "-use-runtime " ^ ocamlrun
    end
      