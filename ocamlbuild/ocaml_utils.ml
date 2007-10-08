(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log
open Pathname.Operators
open Tags.Operators
open Tools
open Flags
open Command;;


module S = Set.Make(String)

let stdlib_dir = lazy begin
  (* FIXME *)
  let ocamlc_where = sprintf "%s/ocamlc.where" (Pathname.pwd / !Options.build_dir) in
  let () = Command.execute ~quiet:true (Cmd(S[!Options.ocamlc; A"-where"; Sh">"; P ocamlc_where])) in
  String.chomp (read_file ocamlc_where)
end

let module_name_of_filename f = String.capitalize (Pathname.remove_extensions f)
let module_name_of_pathname x =
  module_name_of_filename (Pathname.to_string (Pathname.basename x))

let ignore_stdlib x =
  if !Options.nostdlib then false
  else
    let x' = !*stdlib_dir/((String.uncapitalize x)-.-"cmi") in
    Pathname.exists x'

let non_dependencies = ref []
let non_dependency m1 m2 = non_dependencies := (m1, m2) :: !non_dependencies

let module_importance modpath x =
  if List.mem (modpath, x) !non_dependencies
  || (List.mem x !Options.ignore_list) then begin
    let () = dprintf 3 "This module (%s) is ignored by %s" x modpath in
    `ignored
  end
  else if ignore_stdlib x then `just_try else `mandatory

let expand_module include_dirs module_name exts =
  let dirname = Pathname.dirname module_name in
  let basename = Pathname.basename module_name in
  let module_name_cap = dirname/(String.capitalize basename) in
  let module_name_uncap = dirname/(String.uncapitalize basename) in
  List.fold_right begin fun include_dir ->
    List.fold_right begin fun ext acc ->
      include_dir/(module_name_uncap-.-ext) ::
      include_dir/(module_name_cap-.-ext) :: acc
    end exts
  end include_dirs []

let string_list_of_file file =
  with_input_file file begin fun ic ->
    Lexers.blank_sep_strings (Lexing.from_channel ic)
  end
let print_path_list = Pathname.print_path_list

let ocaml_ppflags tags =
  let flags = Flags.of_tags (tags++"ocaml"++"pp") in
  let reduced = Command.reduce flags in
  if reduced = N then N else S[A"-pp"; Quote reduced]

let ocaml_add_include_flag x acc =
  if x = Pathname.current_dir_name then acc else A"-I" :: A x :: acc

let ocaml_include_flags path =
  S (List.fold_right ocaml_add_include_flag (Pathname.include_dirs_of (Pathname.dirname path)) [])

let info_libraries = Hashtbl.create 103

let libraries = Hashtbl.create 103
let libraries_of m =
  try Hashtbl.find libraries m with Not_found -> []
let use_lib m lib = Hashtbl.replace libraries m (lib :: libraries_of m)

let ocaml_lib ?(extern=false) ?(byte=true) ?(native=true) ?dir ?tag_name libpath =
  let add_dir x =
    match dir with
    | Some dir -> S[A"-I"; P dir; x]
    | None -> x
  in
  let tag_name =
    match tag_name with
    | Some x -> x
    | None -> "use_" ^ Pathname.basename libpath
  in
  Hashtbl.replace info_libraries tag_name (libpath, extern);
  if extern then begin
    if byte then
      flag ["ocaml"; tag_name; "link"; "byte"] (add_dir (A (libpath^".cma")));
    if native then
      flag ["ocaml"; tag_name; "link"; "native"] (add_dir (A (libpath^".cmxa")));
  end else begin
    if not byte && not native then
      invalid_arg "ocaml_lib: ~byte:false or ~native:false only works with ~extern:true";
  end;
  match dir with
  | None -> ()
  | Some dir -> flag ["ocaml"; tag_name; "compile"] (S[A"-I"; P dir])

let cmi_of = Pathname.update_extensions "cmi"
