(* Link a set of .cmx/.o files and produce an executable *)

open Sys
open Misc
open Config
open Compilenv

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error

exception Error of error

(* Consistency check between interfaces and implementations *)

let crc_interfaces = (Hashtbl.new 17 : (string, string * int) Hashtbl.t)
let crc_implementations = (Hashtbl.new 17 : (string, string * int) Hashtbl.t)

let check_consistency file_name unit crc =
  List.iter
    (fun (name, crc) ->
      try
        let (auth_name, auth_crc) = Hashtbl.find crc_interfaces name in
        if crc <> auth_crc then
          raise(Error(Inconsistent_interface(name, file_name, auth_name)))
      with Not_found ->
        Hashtbl.add crc_interfaces name (file_name, crc))
    unit.ui_interfaces;
  List.iter
    (fun (name, crc) ->
      try
        let (auth_name, auth_crc) = Hashtbl.find crc_implementations name in
        if crc <> auth_crc then
          raise(Error(Inconsistent_implementation(name, file_name, auth_name)))
      with Not_found ->
        Hashtbl.add crc_implementations name (file_name, crc))
    ((unit.ui_name, crc) :: unit.ui_imports)

(* First pass: determine which units are needed *)

module StringSet =
  Set.Make(struct
    type t = string
    let compare = compare
  end)

let missing_globals = ref StringSet.empty

let is_required name =
  StringSet.mem name !missing_globals

let add_required (name, crc) =
  missing_globals := StringSet.add name !missing_globals

let remove_required name =
  missing_globals := StringSet.remove name !missing_globals

let scan_file tolink obj_name =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = String.create (String.length cmx_magic_number) in
    really_input ic buffer 0 (String.length cmx_magic_number);
    if buffer = cmx_magic_number then begin
      (* This is a .cmx file. It must be linked in any case.
         Read the infos to see which modules it
         requires. *)
      let info = (input_value ic : unit_infos) in
      let crc = input_binary_int ic in
      close_in ic;
      check_consistency file_name info crc;
      List.iter add_required info.ui_imports;
      info :: tolink
    end
    else if buffer = cmxa_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let info_crc_list = (input_value ic : (unit_infos * int) list) in
      close_in ic;
      List.fold_left
        (fun reqd (info, crc) ->
          if is_required info.ui_name then begin
            check_consistency file_name info crc;
            remove_required info.ui_name;
            List.iter add_required info.ui_imports;
            info :: reqd
          end else
            reqd)
      tolink info_crc_list
    end
    else raise(Error(Not_an_object_file file_name))
  with x ->
    close_in ic; raise x

(* Second pass: generate the startup file and link it with everything else *)

module IntSet = Set.Make(
  struct
    type t = int
    let compare = compare
  end)

let make_startup_file filename info_list =
  let oc = open_out filename in
  Emitaux.output_channel := oc;
  Compilenv.reset "startup" 0; (* set the name of the "current" compunit *)
  Emit.begin_assembly();
  let name_list = List.map (fun ui -> ui.ui_name) info_list in
  Asmgen.compile_phrase(Cmmgen.entry_point name_list);
  let apply_functions = ref IntSet.empty in
  let curry_functions = ref IntSet.empty in
  List.iter
    (fun info ->
      List.iter
        (fun n -> apply_functions := IntSet.add n !apply_functions)
        info.ui_apply_fun;
      List.iter
        (fun n -> curry_functions := IntSet.add n !curry_functions)
        info.ui_curry_fun)
    info_list;
  IntSet.iter
    (fun n -> Asmgen.compile_phrase(Cmmgen.apply_function n))
    !apply_functions;
  IntSet.iter
    (fun n -> List.iter Asmgen.compile_phrase (Cmmgen.curry_function n))
    !curry_functions;
  Asmgen.compile_phrase(Cmmgen.global_table name_list);
  Asmgen.compile_phrase(Cmmgen.frame_table ("startup" :: name_list));
  Emit.end_assembly();
  close_out oc

let call_linker file_list startup_file =
  if Sys.command
   (Printf.sprintf
      "%s -I%s -o %s %s %s %s -L%s %s %s"
      Config.c_compiler
      Config.standard_library
      !Clflags.exec_name
      (String.concat " " (List.rev !Clflags.ccopts))
      (String.concat " " (List.rev file_list))
      startup_file
      Config.standard_library
      (String.concat " " (List.rev !Clflags.ccobjs))
      Config.c_libraries) <> 0
  then raise(Error Linking_error)

let object_file_name name =
  if Filename.check_suffix name ".cmx" then
    Filename.chop_suffix name ".cmx" ^ ".o"
  else if Filename.check_suffix name ".cmxa" then
    Filename.chop_suffix name ".cmxa" ^ ".a"
  else
    fatal_error "Asmlink.object_file_name"

(* Main entry point *)

let link objfiles =
(**  let objfiles = "stdlib.cmxa" :: objfiles in **)
  let units_tolink = List.fold_left scan_file [] (List.rev objfiles) in
  let startup = temp_file "camlstartup" ".s" in
  make_startup_file startup units_tolink;
  let startup_obj = temp_file "camlstartup" ".o" in
  if Proc.assemble_file startup startup_obj <> 0 then
    raise(Error(Assembler_error startup));
  try
    call_linker (List.map object_file_name objfiles) startup_obj;
    if not !Clflags.keep_startup_file then remove_file startup;
    remove_file startup_obj
  with x ->
    remove_file startup_obj;
    raise x

(* Error report *)

open Format

let report_error = function
    File_not_found name ->
      print_string "Cannot find file "; print_string name
  | Not_an_object_file name ->
      print_string "The file "; print_string name;
      print_string " is not a compilation unit description"
  | Inconsistent_interface(intf, file1, file2) ->
      open_hvbox 0;
      print_string "Files "; print_string file1; print_string " and ";
      print_string file2; print_space();
      print_string "make inconsistent assumptions over interface ";
      print_string intf;
      close_box()
  | Inconsistent_implementation(intf, file1, file2) ->
      open_hvbox 0;
      print_string "Files "; print_string file1; print_string " and ";
      print_string file2; print_space();
      print_string "make inconsistent assumptions over implementation ";
      print_string intf;
      close_box()
  | Assembler_error file ->
      print_string "Error while assembling "; print_string file
  | Linking_error ->
      print_string "Error during linking"
