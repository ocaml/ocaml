(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Toplevel directives *)

open Format
open Misc
open Longident
open Types
open Cmo_format
open Trace
open Toploop

(* The standard output formatter *)
let std_out = std_formatter

(* To quit *)

let dir_quit () = exit 0

let _ = Hashtbl.add directive_table "quit" (Directive_none dir_quit)

(* To add a directory to the load path *)

let dir_directory s =
  let d = expand_directory Config.standard_library s in
  Config.load_path := d :: !Config.load_path;
  Dll.add_path [d]

let _ = Hashtbl.add directive_table "directory" (Directive_string dir_directory)

(* To remove a directory from the load path *)
let dir_remove_directory s =
  let d = expand_directory Config.standard_library s in
  Config.load_path := List.filter (fun d' -> d' <> d) !Config.load_path;
  Dll.remove_path [d]

let _ =
  Hashtbl.add directive_table "remove_directory"
    (Directive_string dir_remove_directory)

(* To change the current directory *)

let dir_cd s = Sys.chdir s

let _ = Hashtbl.add directive_table "cd" (Directive_string dir_cd)

(* Load in-core a .cmo file *)

exception Load_failed

let check_consistency ppf filename cu =
  try
    List.iter
      (fun (name, crc) -> Consistbl.check Env.crc_units name crc filename)
      cu.cu_imports
  with Consistbl.Inconsistency(name, user, auth) ->
    fprintf ppf "@[<hv 0>The files %s@ and %s@ \
                 disagree over interface %s@]@."
            user auth name;
    raise Load_failed

let load_compunit ic filename ppf compunit =
  check_consistency ppf filename compunit;
  seek_in ic compunit.cu_pos;
  let code_size = compunit.cu_codesize + 8 in
  let code = Meta.static_alloc code_size in
  unsafe_really_input ic code 0 compunit.cu_codesize;
  String.unsafe_set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
  String.unsafe_blit "\000\000\000\001\000\000\000" 0
                     code (compunit.cu_codesize + 1) 7;
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code compunit.cu_reloc;
  Symtable.update_global_table();
  begin try
    may_trace := true;
    ignore((Meta.reify_bytecode code code_size) ());
    may_trace := false;
  with exn ->
    may_trace := false;
    Symtable.restore_state initial_symtable;
    print_exception_outcome ppf exn;
    raise Load_failed
  end

let rec load_file recursive ppf name =
  let filename =
    try Some (find_in_path !Config.load_path name) with Not_found -> None
  in
  match filename with
  | None -> fprintf ppf "Cannot find file %s.@." name; false
  | Some filename ->
      let ic = open_in_bin filename in
      try
        let success = really_load_file recursive ppf name filename ic in
        close_in ic;
        success
      with exn ->
        close_in ic;
        raise exn

and really_load_file recursive ppf name filename ic =
  let ic = open_in_bin filename in
  let buffer = Misc.input_bytes ic (String.length Config.cmo_magic_number) in
  try
    if buffer = Config.cmo_magic_number then begin
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let cu : compilation_unit = input_value ic in
      if recursive then
        List.iter
          (function
            | (Reloc_getglobal id, _)
              when not (Symtable.is_global_defined id) ->
                let file = Ident.name id ^ ".cmo" in
                begin match try Some (Misc.find_in_path_uncap !Config.load_path
                                        file)
                      with Not_found -> None
                with
                | None -> ()
                | Some file ->
                    if not (load_file recursive ppf file) then raise Load_failed
                end
            | _ -> ()
          )
          cu.cu_reloc;
      load_compunit ic filename ppf cu;
      true
    end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        let lib = (input_value ic : library) in
        List.iter
          (fun dllib ->
            let name = Dll.extract_dll_name dllib in
            try Dll.open_dlls Dll.For_execution [name]
            with Failure reason ->
              fprintf ppf
                "Cannot load required shared library %s.@.Reason: %s.@."
                name reason;
              raise Load_failed)
          lib.lib_dllibs;
        List.iter (load_compunit ic filename ppf) lib.lib_units;
        true
      end else begin
        fprintf ppf "File %s is not a bytecode object file.@." name;
        false
      end
  with Load_failed -> false

let dir_load ppf name = ignore (load_file false ppf name)

let _ = Hashtbl.add directive_table "load" (Directive_string (dir_load std_out))

let dir_load_rec ppf name = ignore (load_file true ppf name)

let _ = Hashtbl.add directive_table "load_rec"
                    (Directive_string (dir_load_rec std_out))

let load_file = load_file false

(* Load commands from a file *)

let dir_use ppf name = ignore(Toploop.use_file ppf name)
let dir_mod_use ppf name = ignore(Toploop.mod_use_file ppf name)

let _ = Hashtbl.add directive_table "use" (Directive_string (dir_use std_out))
let _ = Hashtbl.add directive_table "mod_use"
                    (Directive_string (dir_mod_use std_out))

(* Install, remove a printer *)

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

let match_printer_type ppf desc typename =
  let (printer_type, _) =
    try
      Env.lookup_type (Ldot(Lident "Topdirs", typename)) !toplevel_env
    with Not_found ->
      fprintf ppf "Cannot find type Topdirs.%s.@." typename;
      raise Exit in
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  let ty_arg = Ctype.newvar() in
  Ctype.unify !toplevel_env
    (Ctype.newconstr printer_type [ty_arg])
    (Ctype.instance_def desc.val_type);
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
    let v = eval_path path in
    let print_function =
      if is_old_style then
        (fun formatter repr -> Obj.obj v (Obj.obj repr))
      else
        (fun formatter repr -> Obj.obj v formatter (Obj.obj repr)) in
    install_printer path ty_arg print_function
  with Exit -> ()

let dir_remove_printer ppf lid =
  try
    let (ty_arg, path, is_old_style) = find_printer_type ppf lid in
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

(* The trace *)

external current_environment: unit -> Obj.t = "caml_get_current_environment"

let tracing_function_ptr =
  get_code_pointer
    (Obj.repr (fun arg -> Trace.print_trace (current_environment()) arg))

let dir_trace ppf lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    (* Check if this is a primitive *)
    match desc.val_kind with
    | Val_prim p ->
        fprintf ppf "%a is an external function and cannot be traced.@."
        Printtyp.longident lid
    | _ ->
        let clos = eval_path path in
        (* Nothing to do if it's not a closure *)
        if Obj.is_block clos
        && (Obj.tag clos = Obj.closure_tag || Obj.tag clos = Obj.infix_tag)
        then begin
        match is_traced clos with
        | Some opath ->
            fprintf ppf "%a is already traced (under the name %a).@."
            Printtyp.path path
            Printtyp.path opath
        | None ->
            (* Instrument the old closure *)
            traced_functions :=
              { path = path;
                closure = clos;
                actual_code = get_code_pointer clos;
                instrumented_fun =
                  instrument_closure !toplevel_env lid ppf desc.val_type }
              :: !traced_functions;
            (* Redirect the code field of the closure to point
               to the instrumentation function *)
            set_code_pointer clos tracing_function_ptr;
            fprintf ppf "%a is now traced.@." Printtyp.longident lid
        end else fprintf ppf "%a is not a function.@." Printtyp.longident lid
  with
  | Not_found -> fprintf ppf "Unbound value %a.@." Printtyp.longident lid

let dir_untrace ppf lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    let rec remove = function
    | [] ->
        fprintf ppf "%a was not traced.@." Printtyp.longident lid;
        []
    | f :: rem ->
        if Path.same f.path path then begin
          set_code_pointer f.closure f.actual_code;
          fprintf ppf "%a is no longer traced.@." Printtyp.longident lid;
          rem
        end else f :: remove rem in
    traced_functions := remove !traced_functions
  with
  | Not_found -> fprintf ppf "Unbound value %a.@." Printtyp.longident lid

let dir_untrace_all ppf () =
  List.iter
    (fun f ->
      set_code_pointer f.closure f.actual_code;
      fprintf ppf "%a is no longer traced.@." Printtyp.path f.path)
    !traced_functions;
  traced_functions := []

let parse_warnings ppf iserr s =
  try Warnings.parse_options iserr s
  with Arg.Bad err -> fprintf ppf "%s.@." err

let _ =
  Hashtbl.add directive_table "trace" (Directive_ident (dir_trace std_out));
  Hashtbl.add directive_table "untrace" (Directive_ident (dir_untrace std_out));
  Hashtbl.add directive_table
    "untrace_all" (Directive_none (dir_untrace_all std_out));

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

  Hashtbl.add directive_table "rectypes"
             (Directive_none(fun () -> Clflags.recursive_types := true));

  Hashtbl.add directive_table "warnings"
             (Directive_string (parse_warnings std_out false));

  Hashtbl.add directive_table "warn_error"
             (Directive_string (parse_warnings std_out true))
