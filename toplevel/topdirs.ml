(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Toplevel directives *)

open Format
open Misc
open Longident
open Path
open Types
open Emitcode
open Printval
open Trace
open Toploop

(* The standard error formatter *)
let std_err = err_formatter

(* To quit *)

let dir_quit () = exit 0

let _ = Hashtbl.add directive_table "quit" (Directive_none dir_quit)

(* To add a directory to the load path *)

let dir_directory s =
  Config.load_path := s :: !Config.load_path;
  Env.reset_cache()

let _ = Hashtbl.add directive_table "directory" (Directive_string dir_directory)

(* To change the current directory *)

let dir_cd s = Sys.chdir s

let _ = Hashtbl.add directive_table "cd" (Directive_string dir_cd)

(* Load in-core a .cmo file *)

exception Load_failed

let check_consistency ppf filename compunit =
  List.iter
    (fun (name, crc) ->
      let crc_intf =
        try
          Env.crc_of_unit name
        with Not_found ->
          fprintf ppf "Cannot find compiled interface for %s.@." name;
          raise Load_failed in
      if crc <> crc_intf then begin
        fprintf ppf "@[<hv 0>File %s is not up-to-date with respect to@ \
                             interface %s@]@."
                filename name;
        raise Load_failed
      end)
    compunit.cu_imports

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
    ignore((Meta.reify_bytecode code code_size) ())
  with exn ->
    Symtable.restore_state initial_symtable;
    print_exception_outcome ppf exn;
    raise Load_failed
  end

let dir_load ppf name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let buffer = String.create (String.length Config.cmo_magic_number) in
    really_input ic buffer 0 (String.length Config.cmo_magic_number);
    begin try
      if buffer = Config.cmo_magic_number then begin
        let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
        seek_in ic compunit_pos;
        load_compunit ic filename ppf (input_value ic : compilation_unit)
      end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        List.iter (load_compunit ic filename ppf)
                  (input_value ic : compilation_unit list)
      end else fprintf ppf "File %s is not a bytecode object file.@." name
    with Load_failed -> ()
    end;
    close_in ic
  with Not_found -> fprintf ppf "Cannot find file %s.@." name

let _ = Hashtbl.add directive_table "load" (Directive_string (dir_load std_err))

(* Load commands from a file *)

let dir_use ppf name = ignore(Toploop.use_file ppf name)

let _ = Hashtbl.add directive_table "use" (Directive_string (dir_use std_err))

(* Install, remove a printer *)

let find_printer_type ppf lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    Ctype.init_def(Ident.current_time());
    Ctype.begin_def();
    let ty_arg = Ctype.newvar() in
    Ctype.unify !toplevel_env
      (Ctype.newty (Tarrow("", ty_arg, Ctype.instance Predef.type_unit)))
      (Ctype.instance desc.val_type);
    Ctype.end_def();
    Ctype.generalize ty_arg;
    (ty_arg, path)
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
    let (ty_arg, path) = find_printer_type ppf lid in
    let v = (Obj.obj (eval_path path) : 'a -> unit) in
    Printval.install_printer path ty_arg (fun repr -> v (Obj.obj repr))
  with Exit -> ()

let dir_remove_printer ppf lid =
  try
    let (ty_arg, path) = find_printer_type ppf lid in
    begin try
      Printval.remove_printer path
    with Not_found ->
      fprintf ppf "No printer named %a.@." Printtyp.longident lid
    end
  with Exit -> ()

let _ = Hashtbl.add directive_table "install_printer"
             (Directive_ident (dir_install_printer std_err))
let _ = Hashtbl.add directive_table "remove_printer"
             (Directive_ident (dir_remove_printer std_err))

(* The trace *)

external current_environment: unit -> Obj.t = "get_current_environment"

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
        if Obj.is_block clos &&
           (Obj.tag clos = 250 || Obj.tag clos = 249) then begin
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
          set_code_pointer (eval_path path) f.actual_code;
          fprintf ppf "%a is no longer traced.@." Printtyp.longident lid;
          rem
        end else f :: remove rem in
    traced_functions := remove !traced_functions
  with
  | Not_found -> fprintf ppf "Unbound value %a.@." Printtyp.longident lid

let dir_untrace_all ppf () =
  List.iter
    (fun f ->
      set_code_pointer (eval_path f.path) f.actual_code;
      fprintf ppf "%a is no longer traced.@." Printtyp.path f.path)
    !traced_functions;
  traced_functions := []

let parse_warnings ppf s =
  try Warnings.parse_options s
  with Arg.Bad err -> fprintf ppf "%s.@." err

let _ =
  Hashtbl.add directive_table "trace" (Directive_ident (dir_trace std_err));
  Hashtbl.add directive_table "untrace" (Directive_ident (dir_untrace std_err));
  Hashtbl.add directive_table
    "untrace_all" (Directive_none (dir_untrace_all std_err));

(* Control the printing of values *)

  Hashtbl.add directive_table "print_depth"
             (Directive_int(fun n -> max_printer_depth := n));
  Hashtbl.add directive_table "print_length"
             (Directive_int(fun n -> max_printer_steps := n));

(* Set various compiler flags *)

  Hashtbl.add directive_table "labels"
             (Directive_bool(fun b -> Clflags.classic := not b));

  Hashtbl.add directive_table "warnings"
             (Directive_string (parse_warnings std_err))
