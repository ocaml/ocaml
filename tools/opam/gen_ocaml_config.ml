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
   containing the following variables:
   - native: True if ocamlopt is located with ocaml
   - native-tools: True if ocamlc.opt is located with ocaml
   - native-dynlink: True if dynlink.cmxa exists in -I + or -I +dynlink
   - stubsdir: Content of +ld.conf in CAML_LD_LIBRARY_PATH format
   - preinstalled: True if this installation is provided by the system, rather
                   than compiled from sources by opam
   - compiler: ["system"], if [preinstalled], otherwise the version of the opam
               compiler package which provided the compiler (e.g. "5.4.0"). For
               largely historical reasons, custom compiler append additional
               configuration information (e.g. "5.4.0+options+flambda"). This
               variable should be considered deprecated and the content
               unstable. *)

(* The script must be invoked using the interpreter, for example:
      ocaml gen_ocaml_config.ml 5.4.0 ocaml 5.4.0+options false +flambda
   where "5.4.0" is the expected value of Sys.ocaml_version (with any additional
   information removed), the resulting configuration should be written to
   "ocaml.config" and the "compiler" variable should be set to
   "5.4.0+options+flambda". *)
let expected_ocaml_version,
    package_config_file,
    compiler_package_version,
    preinstalled,
    option_names =
  match Array.to_list Sys.argv with
  | _ ::
    expected_ocaml_version ::
    package_config_file ::
    compiler_package_version ::
    preinstalled ::
    options ->
      expected_ocaml_version,
      package_config_file ^ ".config",
      compiler_package_version,
      preinstalled,
      String.concat "" (List.filter ((<>) "") options)
  | _ ->
      prerr_endline "Invalid arguments";
      exit 1

(* Check that Sys.ocaml_version is as expected *)
let () =
  let ocaml_version =
    Scanf.sscanf Sys.ocaml_version "%u.%u" (fun major minor ->
      if (major, minor) > (3, 7) then
        (* Strip off any additional information *)
        Scanf.sscanf Sys.ocaml_version "%[^~+]" (fun x -> x)
      else
        Sys.ocaml_version)
  in
  if ocaml_version <> expected_ocaml_version then begin
    Printf.eprintf "OCaml version mismatch: %s, expected %s\n"
                   ocaml_version expected_ocaml_version;
    exit 1
  end

(* Write the .config file *)
let () =
  let binary =
    let dir = Filename.dirname Sys.executable_name in
    if Filename.check_suffix Sys.executable_name ".exe" then
      fun name -> Filename.concat dir (name ^ ".exe")
    else
      fun name -> Filename.concat dir name
  in
  let libdir =
    let exit_code =
      let ocamlc = binary "ocamlc" in
      let ocamlc =
        if Sys.os_type = "Win32" then
          if String.contains ocamlc ' ' then
            "\"" ^ ocamlc ^ "\""
          else
            ocamlc
        else
          Filename.quote ocamlc
      in
      Sys.command (ocamlc ^ " -where > where") in
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
  let stubsdir =
    let ld_conf = Filename.concat libdir "ld.conf" in
    if Sys.file_exists ld_conf then
      let input_line ic =
        let line = input_line ic in
        if line = Filename.current_dir_name then
          libdir
        else if line = Filename.parent_dir_name then
          Filename.concat libdir line
        else
          Scanf.sscanf line "%[.]%1[/\\]" (fun prefix separator ->
            if separator = "/" || Sys.os_type <> "Unix" && separator = "\\" then
              if prefix = Filename.current_dir_name then
                let line = String.sub line 2 (String.length line - 2) in
                Filename.concat libdir line
              else if prefix = Filename.parent_dir_name then
                Filename.concat libdir line
              else
                line
            else
              line)
      in
      let ic = open_in ld_conf in
      let rec input_lines acc =
        try input_lines (input_line ic :: acc)
        with End_of_file -> close_in ic; List.rev acc
      in
      let lines = input_lines [] in
      let sep = if Sys.os_type = "Win32" then ";" else ":" in
      String.concat sep lines
    else
      ""
  in
  let native = Sys.file_exists (binary "ocamlopt") in
  let native_tools = Sys.file_exists (binary "ocamlc.opt") in
  let native_dynlink =
    let check_dir libdir =
      Sys.file_exists (Filename.concat libdir "dynlink.cmxa")
    in
    List.exists check_dir [Filename.concat libdir "dynlink"; libdir]
  in
  let oc = open_out package_config_file in
  (* Quoted strings need OCaml 4.02; "\ " needs OCaml 3.09! *)
  Printf.fprintf oc "\
    opam-version: \"2.0\"\n\
    variables {\n  \
      native: %b\n  \
      native-tools: %b\n  \
      native-dynlink: %b\n  \
      stubsdir: %S\n  \
      preinstalled: %s\n  \
      compiler: \"%s%s\"\n\
    }\n" native native_tools native_dynlink stubsdir preinstalled
         compiler_package_version option_names;
  close_out oc
