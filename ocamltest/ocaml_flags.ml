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

let runtime_variant_flags () = match Ocaml_files.runtime_variant() with
  | Ocaml_files.Normal -> ""
  | Ocaml_files.Debug -> " -runtime-variant d"
  | Ocaml_files.Instrumented -> " -runtime-variant i"

let runtime_flags ocamlsrcdir backend c_files =
  let runtime_library_flags = "-I " ^
    (Ocaml_directories.runtime_library backend ocamlsrcdir) in
  let rt_flags = match backend with
    | Ocaml_backends.Native -> runtime_variant_flags ()
    | Ocaml_backends.Bytecode ->
      begin
        if c_files then begin (* custom mode *)
          "-custom " ^ (runtime_variant_flags ())
        end else begin (* non-custom mode *)
          "-use-runtime " ^ (Ocaml_files.ocamlrun ocamlsrcdir)
        end
      end in
  rt_flags ^ " " ^ runtime_library_flags

let toplevel_default_flags = "-noinit -no-version -noprompt"

let ocamldebug_default_flags ocamlsrcdir =
  "-no-version -no-prompt -no-time -no-breakpoint-message " ^
  ("-topdirs-path " ^ (Ocaml_directories.toplevel ocamlsrcdir))

let ocamlobjinfo_default_flags = "-null-crc"
