(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Loading and installation of user-defined printer functions *)

open Misc
open Longident
open Types

(* Error report *)

type error =
  | Load_failure of Compdynlink.error
  | Unbound_identifier of Longident.t
  | Unavailable_module of string * Longident.t
  | Wrong_type of Longident.t
  | No_active_printer of Longident.t

exception Error of error

(* Symtable has global state, and normally holds the symbol table
   for the debuggee. We need to switch it temporarily to the
   symbol table for the debugger. *)

let debugger_symtable = ref (None: Symtable.global_map option)

let use_debugger_symtable fn arg =
  let old_symtable = Symtable.current_state() in
  begin match !debugger_symtable with
  | None ->
      Compdynlink.allow_unsafe_modules true;
      debugger_symtable := Some(Symtable.current_state())
  | Some st ->
      Symtable.restore_state st
  end;
  Misc.try_finally (fun () ->
      let result = fn arg in
      debugger_symtable := Some(Symtable.current_state());
      result
    )
    ~always:(fun () -> Symtable.restore_state old_symtable)

(* Load a .cmo or .cma file *)

open Format

let rec loadfiles ppf name =
  try
    let filename = Load_path.find name in
    use_debugger_symtable Compdynlink.loadfile filename;
    let d = Filename.dirname name in
    if d <> Filename.current_dir_name then begin
      if not (List.mem d (Load_path.get_paths ())) then
        Load_path.add_dir d;
    end;
    fprintf ppf "File %s loaded@." filename;
    true
  with
  | Compdynlink.Error (Compdynlink.Unavailable_unit unit) ->
      loadfiles ppf (String.uncapitalize_ascii unit ^ ".cmo")
        &&
      loadfiles ppf name
  | Not_found ->
      fprintf ppf "Cannot find file %s@." name;
      false
  | Sys_error msg ->
      fprintf ppf "%s: %s@." name msg;
      false
  | Compdynlink.Error e ->
      raise(Error(Load_failure e))

let loadfile ppf name =
  ignore(loadfiles ppf name)

(* Return the value referred to by a path (as in toplevel/topdirs) *)
(* Note: evaluation proceeds in the debugger memory space, not in
   the debuggee. *)

let rec eval_address = function
  | Env.Aident id -> Symtable.get_global_value id
  | Env.Adot(addr, pos) -> Obj.field (eval_address addr) pos

(* PR#7258: get rid of module aliases before evaluating paths *)

let eval_value_path env path =
  match Env.find_value_address path env with
  | addr -> eval_address addr
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))

(* Install, remove a printer (as in toplevel/topdirs) *)

(* since 4.00, "topdirs.cmi" is not in the same directory as the standard
  library, so we load it beforehand as it cannot be found in the search path. *)
let init () =
  let topdirs =
    Filename.concat !Parameters.topdirs_path "topdirs.cmi" in
  ignore (Env.read_signature "Topdirs" topdirs)

let match_printer_type desc typename =
  let printer_type =
    try
      Env.lookup_type (Ldot(Lident "Topdirs", typename)) Env.empty
    with Not_found ->
      raise (Error(Unbound_identifier(Ldot(Lident "Topdirs", typename)))) in
  Ctype.begin_def();
  let ty_arg = Ctype.newvar() in
  Ctype.unify Env.empty
    (Ctype.newconstr printer_type [ty_arg])
    (Ctype.instance desc.val_type);
  Ctype.end_def();
  Ctype.generalize ty_arg;
  ty_arg

let find_printer_type lid =
  try
    let (path, desc) = Env.lookup_value lid Env.empty in
    let (ty_arg, is_old_style) =
      try
        (match_printer_type desc "printer_type_new", false)
      with Ctype.Unify _ ->
        (match_printer_type desc "printer_type_old", true) in
    (ty_arg, path, is_old_style)
  with
  | Not_found -> raise(Error(Unbound_identifier lid))
  | Ctype.Unify _ -> raise(Error(Wrong_type lid))

let install_printer ppf lid =
  let (ty_arg, path, is_old_style) = find_printer_type lid in
  let v =
    try
      use_debugger_symtable (eval_value_path Env.empty) path
    with Symtable.Error(Symtable.Undefined_global s) ->
      raise(Error(Unavailable_module(s, lid))) in
  let print_function =
    if is_old_style then
      (fun _formatter repr -> Obj.obj v (Obj.obj repr))
    else
      (fun formatter repr -> Obj.obj v formatter (Obj.obj repr)) in
  Printval.install_printer path ty_arg ppf print_function

let remove_printer lid =
  let (_ty_arg, path, _is_old_style) = find_printer_type lid in
  try
    Printval.remove_printer path
  with Not_found ->
    raise(Error(No_active_printer lid))

(* Error report *)

open Format

let report_error ppf = function
  | Load_failure e ->
      fprintf ppf "@[Error during code loading: %s@]@."
        (Compdynlink.error_message e)
  | Unbound_identifier lid ->
      fprintf ppf "@[Unbound identifier %a@]@."
      Printtyp.longident lid
  | Unavailable_module(md, lid) ->
      fprintf ppf
        "@[The debugger does not contain the code for@ %a.@ \
           Please load an implementation of %s first.@]@."
        Printtyp.longident lid md
  | Wrong_type lid ->
      fprintf ppf "@[%a has the wrong type for a printing function.@]@."
      Printtyp.longident lid
  | No_active_printer lid ->
      fprintf ppf "@[%a is not currently active as a printing function.@]@."
      Printtyp.longident lid
