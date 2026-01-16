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

(* For as long it remains not totally impractical to do so, this script is
   written in OCaml 3.07. Its purpose is to generate an opam .config file
   for the ocaml-system package. It defines one package variable path, which is
   the directory containing the compiler binaries (e.g. /usr/bin). It also uses
   opam's file-depends mechanism adding the checksum for the ocamlc binary,
   which causes the ocaml-system to be reconsidered if the user changes the
   system compiler (e.g. by running brew upgrade). For legacy support, it also
   captures the checksum of graphics.cmi, which is used by the graphics package
   for old compilers to manually add the graphics library if it isn't compiled
   by default. The purpose of this is to ensure that an opam switch recompiles
   for example if the user switches from the Debian ocaml-nox to the full ocaml
   package. *)

(* The script must be invoked using the interpreter, for example:
      ocaml gen_ocaml-system_config.ml 5.4.0 ocaml-system
   where "5.4.0" is the expected value of Sys.ocaml_version, the resulting
   configuration should be written to "ocaml-system.config". *)
let expected_ocaml_version, package_name, package_config_file =
  match Sys.argv with
  | [| _; expected_ocaml_version; package_name |] ->
      expected_ocaml_version, package_name, package_name ^ ".config"
  | _ ->
      prerr_endline "Invalid arguments";
      exit 1

let ocamlc =
  if Filename.check_suffix Sys.executable_name "exe" then
    Filename.chop_suffix Sys.executable_name "exe" ^ "c.exe"
  else
    Sys.executable_name ^ "c"

(* Check that Sys.ocaml_version is as expected *)
let () =
  if Sys.ocaml_version <> expected_ocaml_version then begin
    Printf.eprintf
      "ERROR: The compiler found at %s has version %s,\n\
       and this package requires %s.\n\
       You should use e.g. 'opam switch create %s.%s' instead.\n"
       ocamlc Sys.ocaml_version expected_ocaml_version package_name
       Sys.ocaml_version;
    exit 1
  end

(* Write the .config file *)
let () =
  let ocamlc_digest = Digest.to_hex (Digest.file ocamlc) in
  let libdir =
    let ocamlc =
      if Sys.os_type = "Win32" then
        if String.contains ocamlc ' ' then
          "\"" ^ ocamlc ^ "\""
        else
          ocamlc
      else
        Filename.quote ocamlc
    in
    let exit_code = Sys.command (ocamlc ^ " -where > where") in
    if exit_code = 0 then
      (* Must be opened in text mode for Windows *)
      let ic = open_in "where" in
      let r = input_line ic in
      close_in ic; Sys.remove "where"; r
    else begin
      Printf.eprintf "Unexpected exit code %d from `ocamlc -where'\n" exit_code;
      exit 1
    end
  in
  let graphics = Filename.concat libdir "graphics.cmi" in
  let graphics_digest =
    if Sys.file_exists graphics then
      Digest.to_hex (Digest.file graphics)
    else
      String.make 32 '0'
  in
  let oc = open_out package_config_file in
  Printf.fprintf oc "opam-version: \"2.0\"\n\
                     file-depends: [ [ %S %S ] [ %S %S ] ]\n\
                     variables { path: %S }\n"
    ocamlc ocamlc_digest graphics graphics_digest (Filename.dirname ocamlc);
  close_out oc
