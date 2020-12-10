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
open Toploop

(* The standard output formatter *)
let std_out = std_formatter

(* Directive sections (used in #help) *)
let section_general = "General"
let section_run = "Loading code"
let section_env = "Environment queries"

let section_print = "Pretty-printing"
let section_trace = "Tracing"
let section_options = "Compiler options"

let section_undocumented = "Undocumented"

(* we will print the sections in the first list,
   then all user-defined sections,
   then the sections in the second list,
   then all undocumented directives *)
let order_of_sections =
  ([
    section_general;
    section_run;
    section_env;
  ], [
    section_print;
    section_trace;
    section_options;

    section_undocumented;
  ])
(* Do not forget to keep the directives synchronized with the manual in
   manual/manual/cmds/top.etex *)

(* To quit *)

let dir_quit () = raise (Compenv.Exit_with_status 0)

let _ = add_directive "quit" (Directive_none dir_quit)
    {
      section = section_general;
      doc = "Exit the toplevel.";
    }

(* To add a directory to the load path *)

let dir_directory s =
  let d = expand_directory Config.standard_library s in
  Dll.add_path [d];
  let dir = Load_path.Dir.create d in
  Load_path.add dir;
  toplevel_env :=
    Stdlib.String.Set.fold
      (fun name env ->
         Env.add_persistent_structure (Ident.create_persistent name) env)
      (Env.persistent_structures_of_dir dir)
      !toplevel_env

let _ = add_directive "directory" (Directive_string dir_directory)
    {
      section = section_run;
      doc = "Add the given directory to search path for source and compiled \
             files.";
    }

(* To remove a directory from the load path *)
let dir_remove_directory s =
  let d = expand_directory Config.standard_library s in
  let keep id =
    match Load_path.find_uncap (Ident.name id ^ ".cmi") with
    | exception Not_found -> true
    | fn -> Filename.dirname fn <> d
  in
  toplevel_env := Env.filter_non_loaded_persistent keep !toplevel_env;
  Load_path.remove_dir s;
  Dll.remove_path [d]

let _ = add_directive "remove_directory" (Directive_string dir_remove_directory)
    {
      section = section_run;
      doc = "Remove the given directory from the search path.";
    }

let dir_show_dirs () =
  List.iter print_endline (Load_path.get_paths ())

let _ = add_directive "show_dirs" (Directive_none "show_dirs")
    {
      section = section_run;
      doc = "List directories currently in the search path.";
    }

(* To change the current directory *)

let dir_cd s = Sys.chdir s

let _ = add_directive "cd" (Directive_string dir_cd)
    {
      section = section_run;
      doc = "Change the current working directory.";
    }

let dir_load ppf name = ignore (load_file ppf name)

let _ = Hashtbl.add directive_table "load" (Directive_string (dir_load std_out))

(* Load commands from a file *)

let dir_use ppf name = ignore(Toploop.use_file ppf name)
let dir_use_output ppf name = ignore(Toploop.use_output ppf name)

let _ = Hashtbl.add directive_table "use" (Directive_string (dir_use std_out))
let _ = Hashtbl.add directive_table "use_output"
    (Directive_string (dir_use_output std_out))

(* Install, remove a printer *)

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

let match_printer_type ppf desc typename =
  let printer_type =
    match
      Env.find_type_by_name
        (Ldot(Lident "Topdirs", typename)) !toplevel_env
    with
    | (path, _) -> path
    | exception Not_found ->
        fprintf ppf "Cannot find type Topdirs.%s.@." typename;
        raise Exit
  in
  Ctype.begin_def();
  let ty_arg = Ctype.newvar() in
  Ctype.unify !toplevel_env
    (Ctype.newconstr printer_type [ty_arg])
    (Ctype.instance desc.val_type);
  Ctype.end_def();
  Ctype.generalize ty_arg;
  ty_arg

let find_printer_type ppf lid =
  match Env.find_value_by_name lid !toplevel_env with
  | (path, desc) -> begin
    match match_printer_type ppf desc "printer_type_new" with
    | ty_arg -> (ty_arg, path, false)
    | exception Ctype.Unify _ -> begin
        match match_printer_type ppf desc "printer_type_old" with
        | ty_arg -> (ty_arg, path, true)
        | exception Ctype.Unify _ ->
            fprintf ppf "%a has a wrong type for a printing function.@."
              Printtyp.longident lid;
            raise Exit
      end
  end
  | exception Not_found ->
      fprintf ppf "Unbound value %a.@." Printtyp.longident lid;
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

let unavailable () = invalid_arg "Directive unavailable in the native toplevel."

let dir_trace _ _ = unavailable ()
let dir_untrace _ _ = unavailable ()
let dir_untrace_all _ _ = unavailable ()

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
