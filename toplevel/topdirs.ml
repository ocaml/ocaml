(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Toplevel directives *)

open Format
open Misc
open Longident
open Path
open Typedtree
open Emitcode
open Printval
open Toploop

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

let dir_load name =
  try
    let filename = find_in_path !Config.load_path name in
    let ic = open_in_bin filename in
    let buffer = String.create (String.length Config.cmo_magic_number) in
    really_input ic buffer 0 (String.length Config.cmo_magic_number);
    if buffer <> Config.cmo_magic_number then begin
      print_string "File "; print_string name;
      print_string " is not a bytecode object file."; print_newline()
    end else begin
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      Bytelink.check_consistency filename compunit;
      seek_in ic compunit.cu_pos;
      let code_size = compunit.cu_codesize + 4 in
      let code = Meta.static_alloc code_size in
      unsafe_really_input ic code 0 compunit.cu_codesize;
      String.unsafe_set code compunit.cu_codesize
                                 (Char.chr Opcodes.opSTOP);
      String.unsafe_set code (compunit.cu_codesize + 1) '\000';
      String.unsafe_set code (compunit.cu_codesize + 2) '\000';
      String.unsafe_set code (compunit.cu_codesize + 3) '\000';
      Symtable.patch_object code compunit.cu_reloc;
      Symtable.update_global_table();
      begin try
        Meta.execute_bytecode code code_size; ()
      with exn ->
        print_exception_outcome exn
      end
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
        while true do
          execute_phrase (Parse.toplevel_phrase lb)
        done
      with End_of_file -> ());
    close_in ic
  with Not_found ->
    print_string "Cannot find file "; print_string name; print_newline()

let _ = Hashtbl.add directive_table "use" (Directive_string dir_use)

(* Install, remove a printer *)

let find_printer_type lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    Ctype.begin_def();
    let ty_arg = Ctype.newvar() in
    Ctype.unify !toplevel_env (Tarrow(ty_arg, Predef.type_unit))
                               (Ctype.instance desc.val_type);
    Ctype.end_def();
    Ctype.generalize ty_arg;
    (ty_arg, path)
  with 
    Not_found ->
      print_string "Unbound value "; Printtyp.longident lid;
      print_newline(); raise Exit
  | Ctype.Unify ->
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

(* Make a copy of a closure *)

let copy_closure cls =
  let sz = Obj.size cls in
  let new = Obj.new_block 251 sz in
  for i = 0 to sz - 1 do Obj.set_field new i (Obj.field cls i) done;
  new

(* Overwrite a closure by another *)

let overwrite_closure dst src =
  for i = 0 to Obj.size src - 1 do
    Obj.set_field dst i (Obj.field src i)
  done

(* The trace *)

let rec trace_closure name clos_typ =
  match Ctype.repr clos_typ with
    Tarrow(t1, t2) ->
      let starred_name =
        match name with
          Lident s -> Lident(s ^ "*")
        | Ldot(lid, s) -> Ldot(lid, s ^ "*") in
      let trace_res = trace_closure starred_name t2 in
      (fun clos_val ->
        Obj.repr(fun arg ->
          open_hovbox 2;
          Printtyp.longident name; print_string " <--"; print_space();
          print_value !toplevel_env arg t1; close_box(); print_newline();
          try
            let res = (Obj.magic clos_val : Obj.t -> Obj.t)(arg) in
            open_hovbox 2;
            Printtyp.longident name; print_string " -->"; print_space();
            print_value !toplevel_env res t2; close_box(); print_newline();
            trace_res res
          with exn ->
            open_hovbox 2;
            Printtyp.longident name; print_string " raises"; print_space();
            print_exception (Obj.repr exn); close_box(); print_newline();
            raise exn))
  | _ ->
      (fun v -> v)

let trace_env = ref ([] : (Path.t * Obj.t) list)

let dir_trace lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    let clos = eval_path path in
    (* Nothing to do if it's not a closure *)
    if Obj.is_block clos & Obj.tag clos = 251 then begin
      let old_clos = copy_closure clos in
      (* Instrument the old closure *)
      let new_clos =
        trace_closure lid (Ctype.instance desc.val_type) old_clos in
      trace_env := (path, old_clos) :: !trace_env;
      (* Overwrite the old closure *)
      overwrite_closure clos new_clos;
      match desc.val_prim with
        None ->
          Printtyp.longident lid; print_string " is now traced.";
          print_newline()
      | Some p ->
          open_hovbox 0;
          print_string "Warning: "; Printtyp.longident lid;
          print_string " is an external function."; print_space();
          print_string "Direct calls will not be traced.";
          close_box(); print_newline()
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
    | (p, oldval) :: rem ->
        if Path.same p path then begin
          overwrite_closure (eval_path path) oldval;
          Printtyp.longident lid; print_string " is no longer traced.";
          print_newline();
          rem
        end else remove rem in
    trace_env := remove !trace_env
  with Not_found ->
    print_string "Unbound value "; Printtyp.longident lid;
    print_newline()

let dir_untrace_all () =
  List.iter
    (fun (path, oldval) ->
        overwrite_closure (eval_path path) oldval;
        Printtyp.path path; print_string " is no longer traced.";
        print_newline())
    !trace_env;
  trace_env := []

let _ = Hashtbl.add directive_table "trace" (Directive_ident dir_trace)
let _ = Hashtbl.add directive_table "untrace" (Directive_ident dir_untrace)
let _ = Hashtbl.add directive_table "untrace_all" (Directive_none dir_untrace_all)

(* Control the printing of values *)

let _ = Hashtbl.add directive_table "print_depth"
             (Directive_int(fun n -> max_printer_depth := n))
let _ = Hashtbl.add directive_table "print_length"
             (Directive_int(fun n -> max_printer_steps := n))
