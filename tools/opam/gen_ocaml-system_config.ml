(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Louis Gesbert, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let () =
  let exe = ".exe" in
  let ocamlc =
    let (base, suffix) =
      let s = Sys.executable_name in
      if Filename.check_suffix s exe then
        (Filename.chop_suffix s exe, exe)
      else
        (s, "") in
    base ^ "c" ^ suffix in
  let required_version =
    if Sys.argv.(1) = "" then
      Sys.ocaml_version
    else
      Sys.argv.(1)
  in
  let package_name = Sys.argv.(2) in
  if Sys.ocaml_version <> required_version then begin
    Printf.eprintf
      "ERROR: The compiler found at %s has version %s,\n\
       and this package requires %s.\n\
       You should use e.g. 'opam switch create %s.%s' \
       instead."
      ocamlc Sys.ocaml_version required_version package_name Sys.ocaml_version;
    exit 1
  end else
    let ocamlc_digest = Digest.to_hex (Digest.file ocamlc) in
    let config = package_name ^ ".config" in
    let libdir =
      if Sys.command (ocamlc ^ " -where > " ^ config) = 0 then
        let ic = open_in config in
        let r = input_line ic in
        close_in ic;
        Sys.remove config;
        r
      else
        failwith "Bad return from 'ocamlc -where'"
    in
    let graphics = Filename.concat libdir "graphics.cmi" in
    let graphics_digest =
      if Sys.file_exists graphics then
        Digest.to_hex (Digest.file graphics)
      else
        String.make 32 '0'
    in
    let oc = open_out config in
    Printf.fprintf oc "opam-version: \"2.0\"\n\
                       file-depends: [ [ %S %S ] [ %S %S ] ]\n\
                       variables { path: %S }\n"
      ocamlc ocamlc_digest graphics graphics_digest (Filename.dirname ocamlc);
    close_out oc
