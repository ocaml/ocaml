(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2023 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type modname = string
type filename = string
type file_prefix = string

type t = {
  source_file: filename;
  prefix: file_prefix;
  modname: modname;
}

let source_file (x: t) = x.source_file
let modname (x: t) = x.modname
let prefix (x: t) = x.prefix

let basename_chop_extensions basename  =
  match String.index basename '.' with
  | dot_pos -> String.sub basename 0 dot_pos
  | exception Not_found -> basename

let modulize s = String.capitalize_ascii s

(* We re-export the [Misc] definition *)
let normalize = Misc.normalized_unit_filename

let modname_from_source source_file =
  source_file |> Filename.basename |> basename_chop_extensions |> modulize

let start_char = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_identchar_latin1 = function
  | 'A'..'Z' | 'a'..'z' | '_' | '\192'..'\214' | '\216'..'\246'
  | '\248'..'\255' | '\'' | '0'..'9' -> true
  | _ -> false

(* Check validity of module name *)
let is_unit_name name =
  String.length name > 0
  && start_char name.[0]
  && String.for_all is_identchar_latin1 name

let check_unit_name file =
  if not (is_unit_name (modname file)) then
    Location.prerr_warning (Location.in_file (source_file file))
      (Warnings.Bad_module_name (modname file))

let make ?(check_modname=true) ~source_file prefix =
  let modname = modname_from_source prefix in
  let p = { modname; prefix; source_file } in
  if check_modname then check_unit_name p;
  p

module Artifact = struct
  type t =
   {
     source_file: filename option;
     filename: filename;
     modname: modname;
   }
  let source_file x = x.source_file
  let filename x = x.filename
  let modname x = x.modname
  let prefix x = Filename.remove_extension (filename x)

  let from_filename filename =
    let modname = modname_from_source filename in
    { modname; filename; source_file = None }

end

let mk_artifact ext u =
  {
    Artifact.filename = u.prefix ^ ext;
    modname = u.modname;
    source_file = Some u.source_file;
  }

let companion_artifact ext x =
  { x with Artifact.filename = Artifact.prefix x ^ ext }

let cmi f = mk_artifact ".cmi" f
let cmo f = mk_artifact ".cmo" f
let cmx f = mk_artifact ".cmx" f
let obj f = mk_artifact Config.ext_obj f
let cmt f = mk_artifact ".cmt" f
let cmti f = mk_artifact ".cmti" f
let annot f = mk_artifact ".annot" f

let companion_obj f = companion_artifact Config.ext_obj f
let companion_cmt f = companion_artifact ".cmt" f

let companion_cmi f =
  let prefix = Misc.chop_extensions f.Artifact.filename in
  { f with Artifact.filename = prefix ^ ".cmi"}

let mli_from_artifact f = Artifact.prefix f ^ !Config.interface_suffix
let mli_from_source u =
   let prefix = Filename.remove_extension (source_file u) in
   prefix  ^ !Config.interface_suffix

let is_cmi f = Filename.check_suffix (Artifact.filename f) ".cmi"

let find_normalized_cmi f =
  let filename = modname f ^ ".cmi" in
  let filename = Load_path.find_normalized filename in
  { Artifact.filename; modname = modname f; source_file = Some f.source_file  }
