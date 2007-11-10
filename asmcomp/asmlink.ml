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

(* Link a set of .cmx/.o files and produce an executable *)

open Sys
open Misc
open Config
open Compilenv

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error
  | Multiple_definition of string * string * string
  | Missing_cmx of string * string

exception Error of error

(* Consistency check between interfaces and implementations *)

let crc_interfaces = Consistbl.create ()
let crc_implementations = Consistbl.create ()
let extra_implementations = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)
let cmx_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    List.iter
      (fun (name, crc) ->
        if name = unit.ui_name
        then Consistbl.set crc_interfaces name crc file_name
        else Consistbl.check crc_interfaces name crc file_name)
      unit.ui_imports_cmi
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crc) ->
        if crc <> cmx_not_found_crc then
          Consistbl.check crc_implementations name crc file_name
        else if List.mem name !cmx_required then
          raise(Error(Missing_cmx(file_name, name)))
        else
          extra_implementations := name :: !extra_implementations)
      unit.ui_imports_cmx
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  begin try
    let source = List.assoc unit.ui_name !implementations_defined in
    raise (Error(Multiple_definition(unit.ui_name, file_name, source)))
  with Not_found -> ()
  end;
  Consistbl.set crc_implementations unit.ui_name crc file_name;
  implementations_defined := 
    (unit.ui_name, file_name) :: !implementations_defined;
  if unit.ui_symbol <> unit.ui_name then
    cmx_required := unit.ui_name :: !cmx_required

let extract_crc_interfaces () =
  Consistbl.extract crc_interfaces
let extract_crc_implementations () = 
  List.fold_left
    (fun ncl n ->
      if List.mem_assoc n ncl then ncl else (n, cmx_not_found_crc) :: ncl)
    (Consistbl.extract crc_implementations)
    !extra_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    lib_ccopts := l.lib_ccopts @ !lib_ccopts
  end

(* First pass: determine which units are needed *)

let missing_globals = (Hashtbl.create 17 : (string, string list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  Hashtbl.iter (fun md rq -> mg := (md, !rq) :: !mg) missing_globals;
  !mg

let scan_file obj_name tolink =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = Compilenv.read_unit_info file_name in
    remove_required info.ui_name;
    List.iter (add_required file_name) info.ui_imports_cmx;
    (info, file_name, crc) :: tolink
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    (* This is an archive file. Each unit contained in it will be linked
       in only if needed. *)
    let ic = open_in_bin file_name in
    let buffer = String.create (String.length cmxa_magic_number) in
    really_input ic buffer 0 (String.length cmxa_magic_number);
    if buffer <> cmxa_magic_number then
      raise(Error(Not_an_object_file file_name));
    let infos = (input_value ic : library_infos) in
    close_in ic;
    add_ccobjs infos;
    List.fold_right
      (fun (info, crc) reqd ->
        if info.ui_force_link
        || !Clflags.link_everything
        || is_required info.ui_name
        then begin
          remove_required info.ui_name;
          List.iter (add_required (Printf.sprintf "%s(%s)"
                                                  file_name info.ui_name))
                    info.ui_imports_cmx;
          (info, file_name, crc) :: reqd
        end else
          reqd)
    infos.lib_units tolink
  end
  else raise(Error(Not_an_object_file file_name))

(* Second pass: generate the startup file and link it with everything else *)

module IntSet = Set.Make(
  struct
    type t = int
    let compare = compare
  end)

let make_startup_file ppf filename units_list =
  let compile_phrase p = Asmgen.compile_phrase ppf p in
  let oc = open_out filename in
  Emitaux.output_channel := oc;
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  Compilenv.reset "_startup"; (* set the name of the "current" compunit *)
  Emit.begin_assembly();
  let name_list =
    List.flatten (List.map (fun (info,_,_) -> info.ui_defines) units_list) in
  compile_phrase (Cmmgen.entry_point name_list);
  let apply_functions = ref (IntSet.add 2 (IntSet.add 3 IntSet.empty)) in
  (* The callback functions always reference caml_apply[23] *)
  let send_functions = ref IntSet.empty in
  let curry_functions = ref IntSet.empty in
  List.iter
    (fun (info,_,_) ->
      List.iter
        (fun n -> apply_functions := IntSet.add n !apply_functions)
        info.ui_apply_fun;
      List.iter
        (fun n -> send_functions := IntSet.add n !send_functions)
        info.ui_send_fun;
      List.iter
        (fun n -> curry_functions := IntSet.add n !curry_functions)
        info.ui_curry_fun)
    units_list;
  IntSet.iter
    (fun n -> compile_phrase (Cmmgen.apply_function n))
    !apply_functions;
  IntSet.iter
    (fun n -> compile_phrase (Cmmgen.send_function n))
    !send_functions;
  IntSet.iter
    (fun n -> List.iter (compile_phrase) (Cmmgen.curry_function n))
    !curry_functions;
  Array.iter
    (fun name -> compile_phrase (Cmmgen.predef_exception name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmmgen.global_table name_list);
  compile_phrase
    (Cmmgen.globals_map
      (List.map
        (fun (unit,_,_) ->
          try (unit.ui_name, List.assoc unit.ui_name unit.ui_imports_cmi)
          with Not_found -> assert false)
        units_list));
  compile_phrase(Cmmgen.data_segment_table ("_startup" :: name_list));
  compile_phrase(Cmmgen.code_segment_table ("_startup" :: name_list));
  compile_phrase
    (Cmmgen.frame_table("_startup" :: "_system" :: name_list));
  Emit.end_assembly();
  close_out oc

let call_linker file_list startup_file output_name =
  let libname =
    if !Clflags.gprofile
    then "libasmrunp" ^ ext_lib
    else "libasmrun" ^ ext_lib in
  let runtime_lib =
    try
      if !Clflags.nopervasives then None
      else Some(find_in_path !load_path libname)
    with Not_found ->
      raise(Error(File_not_found libname)) in
  let c_lib =
    if !Clflags.nopervasives then "" else Config.native_c_libraries in
  match Config.ccomp_type with
  | "cc" ->
      let cmd =
        if not !Clflags.output_c_object then
          Printf.sprintf "%s %s -o %s %s %s %s %s %s %s %s %s"
            !Clflags.c_linker
            (if !Clflags.gprofile then Config.cc_profile else "")
            (Filename.quote output_name)
            (Clflags.std_include_flag "-I")
            (String.concat " " (List.rev !Clflags.ccopts))
            (Filename.quote startup_file)
            (Ccomp.quote_files (List.rev file_list))
            (Ccomp.quote_files
              (List.map (fun dir -> if dir = "" then "" else "-L" ^ dir)
                        !load_path))
            (Ccomp.quote_files (List.rev !Clflags.ccobjs))
            (Ccomp.quote_optfile runtime_lib)
            c_lib
        else
          Printf.sprintf "%s -o %s %s %s"
            Config.native_partial_linker
            (Filename.quote output_name)
            (Filename.quote startup_file)
            (Ccomp.quote_files (List.rev file_list))
      in if Ccomp.command cmd <> 0 then raise(Error Linking_error)
  | "msvc" ->
      if not !Clflags.output_c_object then begin
        let cmd =
          Printf.sprintf "%s /Fe%s %s %s %s %s %s %s %s"
            !Clflags.c_linker
            (Filename.quote output_name)
            (Clflags.std_include_flag "-I")
            (Filename.quote startup_file)
            (Ccomp.quote_files (List.rev file_list))
            (Ccomp.quote_files 
              (List.rev_map Ccomp.expand_libname !Clflags.ccobjs))
            (Ccomp.quote_optfile runtime_lib)
            c_lib
            (Ccomp.make_link_options !Clflags.ccopts) in
        if Ccomp.command cmd <> 0 then raise(Error Linking_error);
        if Ccomp.merge_manifest output_name <> 0 then raise(Error Linking_error)
      end else begin
        let cmd =
          Printf.sprintf "%s /out:%s %s %s"
            Config.native_partial_linker
            (Filename.quote output_name)
            (Filename.quote startup_file)
            (Ccomp.quote_files (List.rev file_list))
        in if Ccomp.command cmd <> 0 then raise(Error Linking_error)
      end
  | _ -> assert false

let object_file_name name =
  let file_name =
    try
      find_in_path !load_path name
    with Not_found ->
      fatal_error "Asmlink.object_file_name: not found" in
  if Filename.check_suffix file_name ".cmx" then
    Filename.chop_suffix file_name ".cmx" ^ ext_obj
  else if Filename.check_suffix file_name ".cmxa" then
    Filename.chop_suffix file_name ".cmxa" ^ ext_lib
  else
    fatal_error "Asmlink.object_file_name: bad ext"

(* Main entry point *)

let link ppf objfiles output_name =
  let stdlib =
    if !Clflags.gprofile then "stdlib.p.cmxa" else "stdlib.cmxa" in
  let stdexit =
    if !Clflags.gprofile then "std_exit.p.cmx" else "std_exit.cmx" in
  let objfiles =
    if !Clflags.nopervasives then objfiles
    else if !Clflags.output_c_object then stdlib :: objfiles
    else stdlib :: (objfiles @ [stdexit]) in
  let units_tolink = List.fold_right scan_file objfiles [] in
  Array.iter remove_required Runtimedef.builtin_exceptions;
  begin match extract_missing_globals() with
    [] -> ()
  | mg -> raise(Error(Missing_implementations mg))
  end;
  List.iter
    (fun (info, file_name, crc) -> check_consistency file_name info crc)
    units_tolink;
  Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
  Clflags.ccopts := !lib_ccopts @ !Clflags.ccopts; (* put user's opts first *)
  let startup = Filename.temp_file "camlstartup" ext_asm in
  make_startup_file ppf startup units_tolink;
  let startup_obj = Filename.temp_file "camlstartup" ext_obj in
  if Proc.assemble_file startup startup_obj <> 0 then
    raise(Error(Assembler_error startup));
  try
    call_linker (List.map object_file_name objfiles) startup_obj output_name;
    if not !Clflags.keep_startup_file then remove_file startup;
    remove_file startup_obj
  with x ->
    remove_file startup_obj;
    raise x

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %s is not a compilation unit description" name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           fprintf ppf "%s" r1;
           List.iter (fun r -> fprintf ppf ",@ %s" r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
            fprintf ppf "@ @[<hov 2>%s referenced from %a@]" md
            print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %s@ and %s@ make inconsistent assumptions \
              over interface %s@]"
       file1 file2 intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %s@ and %s@ make inconsistent assumptions \
              over implementation %s@]"
       file1 file2 intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %s" file
  | Linking_error ->
      fprintf ppf "Error during linking"
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %s@ and %s@ both define a module named %s@]"
        file1 file2 modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %s@ was compiled without access@ \
         to the .cmx file@ for module %s,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %s@ with the correct `-I' option@ \
         so that %s.cmx@ is found.@]"
        filename name filename name
