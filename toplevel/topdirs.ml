(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
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

(* Hooks for parsing functions *)

let parse_toplevel_phrase = Toploop.parse_toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print
let print_warning = Location.print_warning

(* Temporary assignment to a reference *)

let protect r newval body =
  let oldval = !r in
  try
    r := newval; 
    let res = body() in
    r := oldval;
    res
  with x ->
    r := oldval;
    raise x

(* Return the value referred to by a path *)

let rec eval_path = function
    Pident id -> Symtable.get_global_value id
  | Pdot(p, s, pos) -> Obj.field (eval_path p) pos
  | Papply(p1, p2) -> fatal_error "Topdirs.eval_path"

(* To quit *)

let dir_quit () = exit 0; ()

let _ = Hashtbl.add directive_table "quit" (Directive_none dir_quit)

(* To add a directory to the load path *)

let dir_directory s =
  Config.load_path := s :: !Config.load_path;
  Env.reset_cache()

let _ = Hashtbl.add directive_table "directory" (Directive_string dir_directory)

(* To change the current directory *)

let dir_cd s =
  Sys.chdir s

let _ = Hashtbl.add directive_table "cd" (Directive_string dir_cd)

(* Load in-core a .cmo file *)

exception Load_failed

let load_compunit ic filename compunit =
  Bytelink.check_consistency filename compunit;
  seek_in ic compunit.cu_pos;
  let code_size = compunit.cu_codesize + 8 in
  let code = Meta.static_alloc code_size in
  unsafe_really_input ic code 0 compunit.cu_codesize;
  String.unsafe_set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
  String.unsafe_set code (compunit.cu_codesize + 1) '\000';
  String.unsafe_set code (compunit.cu_codesize + 2) '\000';
  String.unsafe_set code (compunit.cu_codesize + 3) '\000';
  String.unsafe_set code (compunit.cu_codesize + 4) '\001';
  String.unsafe_set code (compunit.cu_codesize + 5) '\000';
  String.unsafe_set code (compunit.cu_codesize + 6) '\000';
  String.unsafe_set code (compunit.cu_codesize + 7) '\000';
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code compunit.cu_reloc;
  Symtable.update_global_table();
  begin try
    (Meta.reify_bytecode code code_size) (); ()
  with exn ->
    Symtable.restore_state initial_symtable;
    print_exception_outcome exn;
    raise Load_failed
  end

let dir_load name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let buffer = String.create (String.length Config.cmo_magic_number) in
    really_input ic buffer 0 (String.length Config.cmo_magic_number);
    begin try
      if buffer = Config.cmo_magic_number then begin
        let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
        seek_in ic compunit_pos;
        load_compunit ic filename (input_value ic : compilation_unit)
      end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        List.iter (load_compunit ic filename)
                  (input_value ic : compilation_unit list)
      end else begin
        print_string "File "; print_string name;
        print_string " is not a bytecode object file."; print_newline()
      end
    with Load_failed -> ()
    end;
    close_in ic
  with Not_found ->
         print_string "Cannot find file "; print_string name; print_newline()

let _ = Hashtbl.add directive_table "load" (Directive_string dir_load)

(* Load commands from a file *)

let dir_use name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let lb = Lexing.from_channel ic in
    protect Location.input_name filename (fun () ->
      try
        List.iter
          (fun ph -> if execute_phrase ph then () else raise Exit)
          (!parse_use_file lb)
      with
        Exit -> ()
      | Sys.Break ->
          print_string "Interrupted."; print_newline()
      | x ->
          Errors.report_error x);
    close_in ic
  with Not_found ->
    print_string "Cannot find file "; print_string name; print_newline()

let _ = Hashtbl.add directive_table "use" (Directive_string dir_use)

(* Install, remove a printer *)

let find_printer_type lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    Ctype.init_def(Ident.current_time());
    Ctype.begin_def();
    let ty_arg = Ctype.newvar() in
    Ctype.unify !toplevel_env
      (Ctype.newty (Tarrow(ty_arg, Ctype.instance Predef.type_unit)))
      (Ctype.instance desc.val_type);
    Ctype.end_def();
    Ctype.generalize ty_arg;
    (ty_arg, path)
  with 
    Not_found ->
      print_string "Unbound value "; Printtyp.longident lid;
      print_newline(); raise Exit
  | Ctype.Unify _ ->
      Printtyp.longident lid;
      print_string " has the wrong type for a printing function";
      print_newline(); raise Exit
    
let dir_install_printer lid =
  try
    let (ty_arg, path) = find_printer_type lid in
    let v = eval_path path in
    Printval.printers :=
      (path, ty_arg, (Obj.magic v : Obj.t -> unit)) :: !Printval.printers
  with Exit ->
    ()

let dir_remove_printer lid =
  try
    let (ty_arg, path) = find_printer_type lid in
    let rec remove = function
      [] ->
        print_string "No printer named "; Printtyp.longident lid;
        print_newline();
        []
    | (p, ty, fn as printer) :: rem ->
        if Path.same p path then rem else printer :: remove rem in
    Printval.printers := remove !Printval.printers
  with Exit ->
    ()

let _ = Hashtbl.add directive_table "install_printer"
             (Directive_ident dir_install_printer)
let _ = Hashtbl.add directive_table "remove_printer"
             (Directive_ident dir_remove_printer)

(* The trace *)

external current_environment: unit -> Obj.t = "get_current_environment"

let dir_trace lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    (* Check if this is a primitive *)
    match desc.val_kind with
      Val_prim p ->
        Printtyp.longident lid;
        print_string " is an external function and cannot be traced.";
        print_newline()
    | _ ->
        let clos = eval_path path in
        (* Nothing to do if it's not a closure *)
        if Obj.is_block clos & Obj.tag clos = 250 then begin
        match is_traced clos with
          Some opath ->
            Printtyp.path path;
            print_string " is already traced (under the name ";
            Printtyp.path opath; print_string ")";
            print_newline()
        | None ->
            (* Instrument the old closure *)
            let old_clos = copy_closure clos in
            traced_functions :=
              { path = path; 
                closure = clos;
                initial_closure = old_clos;
                instrumented_fun =
                  instrument_closure !toplevel_env lid
                                     (Ctype.instance desc.val_type)
                                     old_clos}
              :: !traced_functions;
            (* Redirect the code field of the old closure *)
            overwrite_closure clos
             (Obj.repr (fun arg ->
                          Trace.print_trace (current_environment()) arg));
            Printtyp.longident lid; print_string " is now traced.";
            print_newline()
        end else begin
          Printtyp.longident lid; print_string " is not a function.";
          print_newline()
        end      
  with Not_found ->
    print_string "Unbound value "; Printtyp.longident lid;
    print_newline()

let dir_untrace lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    let rec remove = function
      [] ->
        Printtyp.longident lid; print_string " was not traced.";
        print_newline();
        []
    | f :: rem ->
        if Path.same f.path path then begin
          overwrite_closure (eval_path path) f.initial_closure;
          Printtyp.longident lid; print_string " is no longer traced.";
          print_newline();
          rem
        end else f :: remove rem in
    traced_functions := remove !traced_functions
  with Not_found ->
    print_string "Unbound value "; Printtyp.longident lid;
    print_newline()

let dir_untrace_all () =
  List.iter
    (fun f ->
        overwrite_closure (eval_path f.path) f.initial_closure;
        Printtyp.path f.path; print_string " is no longer traced.";
        print_newline())
    !traced_functions;
  traced_functions := []

let _ = Hashtbl.add directive_table "trace" (Directive_ident dir_trace)
let _ = Hashtbl.add directive_table "untrace" (Directive_ident dir_untrace)
let _ = Hashtbl.add directive_table "untrace_all" (Directive_none dir_untrace_all)

(* Control the printing of values *)

let _ = Hashtbl.add directive_table "print_depth"
             (Directive_int(fun n -> max_printer_depth := n))
let _ = Hashtbl.add directive_table "print_length"
             (Directive_int(fun n -> max_printer_steps := n))
