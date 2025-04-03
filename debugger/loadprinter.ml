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

(* Error report *)

type error = [
    Topprinters.error
  | `Load_failure of Dynlink.error
  | `Unavailable_module of string * Longident.t
]

exception Error of error

(* Load a .cmo or .cma file *)

open Format

let rec loadfiles ppf name =
  try
    let filename = Load_path.find name in
    Dynlink.allow_unsafe_modules true;
    Dynlink.loadfile filename;
    let d = Filename.dirname name in
    if d <> Filename.current_dir_name then begin
      if not (List.mem d (Load_path.get_path_list ())) then
        Load_path.add_dir ~hidden:false d;
    end;
    fprintf ppf "File %s loaded@."
      (if d <> Filename.current_dir_name then
         filename
       else
         Filename.basename filename);
    true
  with
  | Dynlink.Error (Dynlink.Unavailable_unit unit) ->
      loadfiles ppf (Unit_info.normalize unit ^ ".cmo")
        &&
      loadfiles ppf name
  | Not_found ->
      fprintf ppf "Cannot find file %s@." name;
      false
  | Sys_error msg ->
      fprintf ppf "%s: %s@." name msg;
      false
  | Dynlink.Error e ->
      raise(Error(`Load_failure e))

let loadfile ppf name =
  ignore(loadfiles ppf name)

(* Return the value referred to by a path (as in toplevel/topdirs) *)
(* Note: evaluation proceeds in the debugger memory space, not in
   the debuggee. *)

let rec eval_address = function
  | Env.Aident id ->
    assert (Ident.persistent id);
    let bytecode_or_asm_symbol = Ident.name id in
    begin match Dynlink.unsafe_get_global_value ~bytecode_or_asm_symbol with
    | None ->
      raise (Symtable.Error (Symtable.Undefined_global
        (Symtable.Global.Glob_compunit (Cmo_format.Compunit
          bytecode_or_asm_symbol))))
    | Some obj -> obj
    end
  | Env.Adot(addr, pos) -> Obj.field (eval_address addr) pos

let eval_value_path env path =
  match Env.find_value_address path env with
  | addr -> eval_address addr
  | exception Not_found ->
      fatal_error ("Cannot find address for: " ^ (Path.name path))

(* Install, remove a printer (as in toplevel/topdirs) *)

(* Very close to Topdirs.install_printer_by_kind
   except that we do fetch the (remote) values
   {b and fallback if it fails} *)
 let install_printer lid =
  match Topprinters.find_printer Env.empty lid with
  | Error error -> raise (Error (error :> error))
  | Ok (path, kind) ->
      let v =
        try
          eval_value_path Env.empty path
        with Symtable.Error(Symtable.Undefined_global global) ->
          let s = Symtable.Global.name global in
          raise (Error (`Unavailable_module(s, lid))) in
      let print_with_fallback ppf f remote_val =
        try
          f (Debugcom.Remote_value.obj remote_val)
        with
          Debugcom.Marshalling_error ->
            fprintf ppf "<cannot fetch remote object>" in
      match kind with
      | Topprinters.Old ty_arg ->
          let print_function ppf remote_val =
            print_with_fallback ppf (Obj.obj v) remote_val in
          Printval.install_printer path ty_arg print_function
      | Topprinters.Simple ty_arg ->
          let print_function ppf remote_val =
            print_with_fallback ppf (Obj.obj v ppf) remote_val in
          Printval.install_printer path ty_arg print_function
      | Topprinters.Generic { ty_path ; arity } ->
          let rec build v = function
            | 0 -> Genprintval.Zero (fun ppf remote_val ->
                print_with_fallback ppf (Obj.obj v ppf) remote_val)
            | n ->
                Genprintval.Succ
                  (fun fn -> build ((Obj.obj v : _ -> Obj.t) fn) (n - 1)) in
          Printval.install_generic_printer' path ty_path (build v arity)

let remove_printer lid =
  match Topprinters.find_printer Env.empty lid with
  | Error error -> raise (Error (error :> error))
  | Ok (path, _kind) ->
      try
        Printval.remove_printer path
      with Not_found ->
        raise(Error(`No_active_printer path))

(* Error report *)

open Format
module Style = Misc.Style
let quoted_longident =
  Format_doc.compat @@ Style.as_inline_code Printtyp.Doc.longident

let report_error ppf = function
  | `Load_failure e ->
      fprintf ppf "@[Error during code loading: %s@]@."
        (Dynlink.error_message e)
  | `Unbound_identifier lid ->
      fprintf ppf "@[Unbound identifier %a@]@."
        quoted_longident lid
  | `Unavailable_module(md, lid) ->
      fprintf ppf
        "@[The debugger does not contain the code for@ %a.@ \
         Please load an implementation of %s first.@]@."
        quoted_longident lid md
  | `Wrong_type lid ->
      fprintf ppf "@[%a has the wrong type for a printing function.@]@."
        quoted_longident lid
  | `No_active_printer path ->
      fprintf ppf "@[%a is not currently active as a printing function.@]@."
        Printtyp.path path
