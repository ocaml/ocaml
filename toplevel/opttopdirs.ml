(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Toplevel directives *)

open Format
open Misc
open Longident
open Types
open Opttoploop

(* The standard output formatter *)
let std_out = std_formatter

(* To quit *)

let dir_quit () = exit 0

let _ = Hashtbl.add directive_table "quit" (Directive_none dir_quit)

(* To add a directory to the load path *)

let dir_directory s =
  let d = expand_directory Config.standard_library s in
  let dir = Load_path.Dir.create d in
  Load_path.add dir;
  toplevel_env :=
    Stdlib.String.Set.fold
      (fun name env ->
         Env.add_persistent_structure (Ident.create_persistent name) env)
      (Env.persistent_structures_of_dir dir)
      !toplevel_env

let _ = Hashtbl.add directive_table "directory" (Directive_string dir_directory)
(* To remove a directory from the load path *)
let dir_remove_directory s =
  let d = expand_directory Config.standard_library s in
  let keep id =
    match Load_path.find_uncap (Ident.name id ^ ".cmi") with
    | exception Not_found -> true
    | fn -> Filename.dirname fn <> d
  in
  toplevel_env := Env.filter_non_loaded_persistent keep !toplevel_env;
  Load_path.remove_dir s

let _ =
  Hashtbl.add directive_table "remove_directory"
    (Directive_string dir_remove_directory)

let _ = Hashtbl.add directive_table "show_dirs"
  (Directive_none
     (fun () ->
        List.iter print_endline (Load_path.get_paths ())
     ))

(* To change the current directory *)

let dir_cd s = Sys.chdir s

let _ = Hashtbl.add directive_table "cd" (Directive_string dir_cd)

(* Load in-core a .cmxs file *)

let load_file ppf name0 =
  let name =
    try Some (Load_path.find name0)
    with Not_found -> None
  in
  match name with
  | None -> fprintf ppf "File not found: %s@." name0; false
  | Some name ->
    let fn,tmp =
      if Filename.check_suffix name ".cmx" || Filename.check_suffix name ".cmxa"
      then
        let cmxs = Filename.temp_file "caml" ".cmxs" in
        Asmlink.link_shared ~ppf_dump:ppf [name] cmxs;
        cmxs,true
      else
        name,false
    in
    let success =
      (* The Dynlink interface does not allow us to distinguish between
          a Dynlink.Error exceptions raised in the loaded modules
          or a genuine error during dynlink... *)
      try Compdynlink.loadfile fn; true
      with
      | Compdynlink.Error err ->
        fprintf ppf "Error while loading %s: %s.@."
          name (Compdynlink.error_message err);
        false
      | exn ->
        print_exception_outcome ppf exn;
        false
    in
    if tmp then (try Sys.remove fn with Sys_error _ -> ());
    success


let dir_load ppf name = ignore (load_file ppf name)

let _ = Hashtbl.add directive_table "load" (Directive_string (dir_load std_out))

(* Load commands from a file *)

let dir_use ppf name = ignore(Opttoploop.use_file ppf name)

let _ = Hashtbl.add directive_table "use" (Directive_string (dir_use std_out))

(* Install, remove a printer *)

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

let match_printer_type ppf desc typename =
  let printer_type =
    try
      Env.lookup_type (Ldot(Lident "Opttopdirs", typename)) !toplevel_env
    with Not_found ->
      fprintf ppf "Cannot find type Topdirs.%s.@." typename;
      raise Exit in
  Ctype.begin_def();
  let ty_arg = Ctype.newvar() in
  Ctype.unify !toplevel_env
    (Ctype.newconstr printer_type [ty_arg])
    (Ctype.instance desc.val_type);
  Ctype.end_def();
  Ctype.generalize ty_arg;
  ty_arg

let find_printer_type ppf lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    let (ty_arg, is_old_style) =
      try
        (match_printer_type ppf desc "printer_type_new", false)
      with Ctype.Unify _ ->
        (match_printer_type ppf desc "printer_type_old", true) in
    (ty_arg, path, is_old_style)
  with
  | Not_found ->
      fprintf ppf "Unbound value %a.@." Printtyp.longident lid;
      raise Exit
  | Ctype.Unify _ ->
      fprintf ppf "%a has a wrong type for a printing function.@."
      Printtyp.longident lid;
      raise Exit

let dir_install_printer ppf lid =
  try
    let (ty_arg, path, is_old_style) = find_printer_type ppf lid in
    let v = eval_value_path !toplevel_env path in
    let print_function =
      if is_old_style then
        (fun _formatter repr -> Obj.obj v (Obj.obj repr))
      else
        (fun formatter repr -> Obj.obj v formatter (Obj.obj repr)) in
    install_printer path ty_arg print_function
  with Exit -> ()

let dir_remove_printer ppf lid =
  try
    let (_ty_arg, path, _is_old_style) = find_printer_type ppf lid in
    begin try
      remove_printer path
    with Not_found ->
      fprintf ppf "No printer named %a.@." Printtyp.longident lid
    end
  with Exit -> ()

let _ = Hashtbl.add directive_table "install_printer"
             (Directive_ident (dir_install_printer std_out))
let _ = Hashtbl.add directive_table "remove_printer"
             (Directive_ident (dir_remove_printer std_out))

let parse_warnings ppf iserr s =
  try Warnings.parse_options iserr s
  with Arg.Bad err -> fprintf ppf "%s.@." err

let _ =
(* Control the printing of values *)

  Hashtbl.add directive_table "print_depth"
             (Directive_int(fun n -> max_printer_depth := n));
  Hashtbl.add directive_table "print_length"
             (Directive_int(fun n -> max_printer_steps := n));

(* Set various compiler flags *)

  Hashtbl.add directive_table "labels"
             (Directive_bool(fun b -> Clflags.classic := not b));

  Hashtbl.add directive_table "principal"
             (Directive_bool(fun b -> Clflags.principal := b));

  Hashtbl.add directive_table "warnings"
             (Directive_string (parse_warnings std_out false));

  Hashtbl.add directive_table "warn_error"
             (Directive_string (parse_warnings std_out true))
