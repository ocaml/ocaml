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

(* Link a set of .cmo files and produce a bytecode executable. *)

open Misc
open Config
open Cmo_format

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Wrong_object_name of filepath
  | Symbol_error of filepath * Symtable.error
  | Inconsistent_import of modname * filepath * filepath
  | Custom_runtime
  | File_exists of filepath
  | Cannot_open_dll of filepath
  | Required_module_unavailable of modname * modname
  | Camlheader of string * filepath
  | Wrong_link_order of (modname * modname) list

exception Error of error

type link_action =
    Link_object of string * compilation_unit
      (* Name of .cmo file and descriptor of the unit *)
  | Link_archive of string * compilation_unit list
      (* Name of .cma file and descriptors of the units to be linked. *)

(* Add C objects and options from a library descriptor *)
(* Ignore them if -noautolink or -use-runtime or -use-prim was given *)

let lib_ccobjs = ref []
let lib_ccopts = ref []
let lib_dllibs = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    if
      String.length !Clflags.use_runtime = 0
      && String.length !Clflags.use_prims = 0
    then begin
      if l.lib_custom then Clflags.custom_runtime := true;
      lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
      let replace_origin =
        Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
      in
      lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts;
    end;
    lib_dllibs := l.lib_dllibs @ !lib_dllibs
  end

(* A note on ccobj ordering:
   - Clflags.ccobjs is in reverse order w.r.t. what was given on the
        ocamlc command line;
   - l.lib_ccobjs is also in reverse order w.r.t. what was given on the
        ocamlc -a command line when the library was created;
   - Clflags.ccobjs is reversed just before calling the C compiler for the
        custom link;
   - .cma files on the command line of ocamlc are scanned right to left;
   - Before linking, we add lib_ccobjs after Clflags.ccobjs.
   Thus, for ocamlc a.cma b.cma obj1 obj2
   where a.cma was built with ocamlc -i ... obja1 obja2
     and b.cma was built with ocamlc -i ... objb1 objb2
   lib_ccobjs starts as [],
   becomes objb2 objb1 when b.cma is scanned,
   then obja2 obja1 objb2 objb1 when a.cma is scanned.
   Clflags.ccobjs was initially obj2 obj1.
   and is set to obj2 obj1 obja2 obja1 objb2 objb1.
   Finally, the C compiler is given objb1 objb2 obja1 obja2 obj1 obj2,
   which is what we need.  (If b depends on a, a.cma must appear before
   b.cma, but b's C libraries must appear before a's C libraries.)
*)

(* First pass: determine which units are needed *)

let missing_globals = ref Ident.Map.empty
let provided_globals = ref Ident.Set.empty
let badly_ordered_dependencies : (string * string) list ref = ref []

let is_required (rel, _pos) =
  match rel with
    Reloc_setglobal id ->
      Ident.Map.mem id !missing_globals
  | _ -> false

let add_required compunit =
  let add id =
    if Ident.Set.mem id !provided_globals then
      badly_ordered_dependencies :=
        ((Ident.name id), compunit.cu_name) :: !badly_ordered_dependencies;
    missing_globals := Ident.Map.add id compunit.cu_name !missing_globals
  in
  List.iter add (Symtable.required_globals compunit.cu_reloc);
  List.iter add compunit.cu_required_globals

let remove_required (rel, _pos) =
  match rel with
    Reloc_setglobal id ->
      missing_globals := Ident.Map.remove id !missing_globals;
      provided_globals := Ident.Set.add id !provided_globals;
  | _ -> ()

let scan_file obj_name tolink =
  let file_name =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  let ic = open_in_bin file_name in
  try
    let buffer = really_input_string ic (String.length cmo_magic_number) in
    if buffer = cmo_magic_number then begin
      (* This is a .cmo file. It must be linked in any case.
         Read the relocation information to see which modules it
         requires. *)
      let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
      seek_in ic compunit_pos;
      let compunit = (input_value ic : compilation_unit) in
      close_in ic;
      add_required compunit;
      List.iter remove_required compunit.cu_reloc;
      Link_object(file_name, compunit) :: tolink
    end
    else if buffer = cma_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let toc = (input_value ic : library) in
      close_in ic;
      add_ccobjs (Filename.dirname file_name) toc;
      let required =
        List.fold_right
          (fun compunit reqd ->
            if compunit.cu_force_link
            || !Clflags.link_everything
            || List.exists is_required compunit.cu_reloc
            then begin
              add_required compunit;
              List.iter remove_required compunit.cu_reloc;
              compunit :: reqd
            end else
              reqd)
          toc.lib_units [] in
      Link_archive(file_name, required) :: tolink
    end
    else raise(Error(Not_an_object_file file_name))
  with
    End_of_file -> close_in ic; raise(Error(Not_an_object_file file_name))
  | x -> close_in ic; raise x

(* Second pass: link in the required units *)

(* Consistency check between interfaces *)

module Consistbl = Consistbl.Make (Misc.Stdlib.String)

let crc_interfaces = Consistbl.create ()
let interfaces = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)

let check_consistency file_name cu =
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc ->
            if name = cu.cu_name
            then Consistbl.set crc_interfaces name crc file_name
            else Consistbl.check crc_interfaces name crc file_name)
      cu.cu_imports
  with Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_import(name, user, auth)))
  end;
  begin try
    let source = List.assoc cu.cu_name !implementations_defined in
    Location.prerr_warning (Location.in_file file_name)
      (Warnings.Module_linked_twice(cu.cu_name,
                                    Location.show_filename file_name,
                                    Location.show_filename source))
  with Not_found -> ()
  end;
  implementations_defined :=
    (cu.cu_name, file_name) :: !implementations_defined

let extract_crc_interfaces () =
  Consistbl.extract !interfaces crc_interfaces

let clear_crc_interfaces () =
  Consistbl.clear crc_interfaces;
  interfaces := []

(* Record compilation events *)

let debug_info = ref ([] : (int * Instruct.debug_event list * string list) list)

(* Link in a compilation unit *)

let link_compunit output_fun currpos_fun inchan file_name compunit =
  check_consistency file_name compunit;
  seek_in inchan compunit.cu_pos;
  let code_block = LongString.input_bytes inchan compunit.cu_codesize in
  Symtable.patch_object code_block compunit.cu_reloc;
  if !Clflags.debug && compunit.cu_debug > 0 then begin
    seek_in inchan compunit.cu_debug;
    let debug_event_list : Instruct.debug_event list = input_value inchan in
    let debug_dirs : string list = input_value inchan in
    let file_path = Filename.dirname (Location.absolute_path file_name) in
    let debug_dirs =
      if List.mem file_path debug_dirs
      then debug_dirs
      else file_path :: debug_dirs in
    debug_info := (currpos_fun(), debug_event_list, debug_dirs) :: !debug_info
  end;
  Array.iter output_fun code_block;
  if !Clflags.link_everything then
    List.iter Symtable.require_primitive compunit.cu_primitives

(* Link in a .cmo file *)

let link_object output_fun currpos_fun file_name compunit =
  let inchan = open_in_bin file_name in
  try
    link_compunit output_fun currpos_fun inchan file_name compunit;
    close_in inchan
  with
    Symtable.Error msg ->
      close_in inchan; raise(Error(Symbol_error(file_name, msg)))
  | x ->
      close_in inchan; raise x

(* Link in a .cma file *)

let link_archive output_fun currpos_fun file_name units_required =
  let inchan = open_in_bin file_name in
  try
    List.iter
      (fun cu ->
         let name = file_name ^ "(" ^ cu.cu_name ^ ")" in
         try
           link_compunit output_fun currpos_fun inchan name cu
         with Symtable.Error msg ->
           raise(Error(Symbol_error(name, msg))))
      units_required;
    close_in inchan
  with x -> close_in inchan; raise x

(* Link in a .cmo or .cma file *)

let link_file output_fun currpos_fun = function
    Link_object(file_name, unit) ->
      link_object output_fun currpos_fun file_name unit
  | Link_archive(file_name, units) ->
      link_archive output_fun currpos_fun file_name units

(* Output the debugging information *)
(* Format is:
      <int32>          number of event lists
      <int32>          offset of first event list
      <output_value>   first event list
      ...
      <int32>          offset of last event list
      <output_value>   last event list *)

let output_debug_info oc =
  output_binary_int oc (List.length !debug_info);
  List.iter
    (fun (ofs, evl, debug_dirs) ->
      output_binary_int oc ofs;
      output_value oc evl;
      output_value oc debug_dirs)
    !debug_info;
  debug_info := []

(* Output a list of strings with 0-termination *)

let output_stringlist oc l =
  List.iter (fun s -> output_string oc s; output_byte oc 0) l

(* Transform a file name into an absolute file name *)

let make_absolute file =
  if not (Filename.is_relative file) then file
  else Location.rewrite_absolute_path
         (Filename.concat (Sys.getcwd()) file)

(* Create a bytecode executable file *)

let link_bytecode ?final_name tolink exec_name standalone =
  let final_name = Option.value final_name ~default:exec_name in
  (* Avoid the case where the specified exec output file is the same as
     one of the objects to be linked *)
  List.iter (function
    | Link_object(file_name, _) when file_name = exec_name ->
      raise (Error (Wrong_object_name exec_name));
    | _ -> ()) tolink;
  (* Remove the output file if it exists to avoid permission problems (PR#8354),
     but don't risk removing a special file (PR#11302). *)
  Misc.remove_file exec_name;
  let outperm = if !Clflags.with_runtime then 0o777 else 0o666 in
  let outchan =
    open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
                 outperm exec_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file exec_name)
    (fun () ->
       if standalone && !Clflags.with_runtime then begin
         (* Copy the header *)
         let header =
           if String.length !Clflags.use_runtime > 0
           then "camlheader_ur" else "camlheader" ^ !Clflags.runtime_variant
         in
         try
           let inchan = open_in_bin (Load_path.find header) in
           copy_file inchan outchan;
           close_in inchan
         with
         | Not_found -> raise (Error (File_not_found header))
         | Sys_error msg -> raise (Error (Camlheader (header, msg)))
       end;
       Bytesections.init_record outchan;
       (* The path to the bytecode interpreter (in use_runtime mode) *)
       if String.length !Clflags.use_runtime > 0 && !Clflags.with_runtime then
       begin
         let runtime = make_absolute !Clflags.use_runtime in
         let runtime =
           (* shebang mustn't exceed 128 including the #! and \0 *)
           if String.length runtime > 125 || String.contains runtime ' ' then
             "/bin/sh\n\
              exec " ^ Filename.quote runtime ^ " \"$0\" \"$@\""
           else
             runtime
         in
         output_string outchan runtime;
         output_char outchan '\n';
         Bytesections.record outchan "RNTM"
       end;
       (* The bytecode *)
       let start_code = pos_out outchan in
       Symtable.init();
       clear_crc_interfaces ();
       let sharedobjs = List.map Dll.extract_dll_name !Clflags.dllibs in
       let check_dlls = standalone && Config.target = Config.host in
       if check_dlls then begin
         (* Initialize the DLL machinery *)
         Dll.init_compile !Clflags.no_std_include;
         Dll.add_path (Load_path.get_paths ());
         try Dll.open_dlls Dll.For_checking sharedobjs
         with Failure reason -> raise(Error(Cannot_open_dll reason))
       end;
       let output_fun = output_bytes outchan
       and currpos_fun () = pos_out outchan - start_code in
       List.iter (link_file output_fun currpos_fun) tolink;
       if check_dlls then Dll.close_all_dlls();
       (* The final STOP instruction *)
       output_byte outchan Opcodes.opSTOP;
       output_byte outchan 0; output_byte outchan 0; output_byte outchan 0;
       Bytesections.record outchan "CODE";
       (* DLL stuff *)
       if standalone then begin
         (* The extra search path for DLLs *)
         output_stringlist outchan !Clflags.dllpaths;
         Bytesections.record outchan "DLPT";
         (* The names of the DLLs *)
         output_stringlist outchan sharedobjs;
         Bytesections.record outchan "DLLS"
       end;
       (* The names of all primitives *)
       Symtable.output_primitive_names outchan;
       Bytesections.record outchan "PRIM";
       (* The table of global data *)
       Emitcode.marshal_to_channel_with_possibly_32bit_compat
         ~filename:final_name ~kind:"bytecode executable"
         outchan (Symtable.initial_global_table());
       Bytesections.record outchan "DATA";
       (* The map of global identifiers *)
       Symtable.output_global_map outchan;
       Bytesections.record outchan "SYMB";
       (* CRCs for modules *)
       output_value outchan (extract_crc_interfaces());
       Bytesections.record outchan "CRCS";
       (* Debug info *)
       if !Clflags.debug then begin
         output_debug_info outchan;
         Bytesections.record outchan "DBUG"
       end;
       (* The table of contents and the trailer *)
       Bytesections.write_toc_and_trailer outchan;
    )

(* Output a string as a C array of unsigned ints *)

let output_code_string_counter = ref 0

let output_code_string outchan code =
  let pos = ref 0 in
  let len = Bytes.length code in
  while !pos < len do
    let c1 = Char.code(Bytes.get code !pos) in
    let c2 = Char.code(Bytes.get code (!pos + 1)) in
    let c3 = Char.code(Bytes.get code (!pos + 2)) in
    let c4 = Char.code(Bytes.get code (!pos + 3)) in
    pos := !pos + 4;
    Printf.fprintf outchan "0x%02x%02x%02x%02x, " c4 c3 c2 c1;
    incr output_code_string_counter;
    if !output_code_string_counter >= 6 then begin
      output_char outchan '\n';
      output_code_string_counter := 0
    end
  done

(* Output a string as a C string *)

let output_data_string outchan data =
  let counter = ref 0 in
  for i = 0 to String.length data - 1 do
    Printf.fprintf outchan "%d, " (Char.code(data.[i]));
    incr counter;
    if !counter >= 12 then begin
      output_string outchan "\n";
      counter := 0
    end
  done

(* Output a debug stub *)

let output_cds_file outfile =
  Misc.remove_file outfile;
  let outchan =
    open_out_gen [Open_wronly; Open_trunc; Open_creat; Open_binary]
      0o777 outfile in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file outfile)
    (fun () ->
       Bytesections.init_record outchan;
       (* The map of global identifiers *)
       Symtable.output_global_map outchan;
       Bytesections.record outchan "SYMB";
       (* Debug info *)
       output_debug_info outchan;
       Bytesections.record outchan "DBUG";
       (* The table of contents and the trailer *)
       Bytesections.write_toc_and_trailer outchan;
    )

(* Output a bytecode executable as a C file *)

let link_bytecode_as_c tolink outfile with_main =
  let outchan = open_out outfile in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file outfile)
    (fun () ->
       (* The bytecode *)
       output_string outchan "\
#define CAML_INTERNALS\n\
#define CAMLDLLIMPORT\
\n\
\n#ifdef __cplusplus\
\nextern \"C\" {\
\n#endif\
\n#include <caml/mlvalues.h>\
\n#include <caml/startup.h>\
\n#include <caml/sys.h>\
\n#include <caml/misc.h>\n";
       output_string outchan "static int caml_code[] = {\n";
       Symtable.init();
       clear_crc_interfaces ();
       let currpos = ref 0 in
       let output_fun code =
         output_code_string outchan code;
         currpos := !currpos + Bytes.length code
       and currpos_fun () = !currpos in
       List.iter (link_file output_fun currpos_fun) tolink;
       (* The final STOP instruction *)
       Printf.fprintf outchan "\n0x%x};\n\n" Opcodes.opSTOP;
       (* The table of global data *)
       output_string outchan "static char caml_data[] = {\n";
       output_data_string outchan
         (Marshal.to_string (Symtable.initial_global_table()) []);
       output_string outchan "\n};\n\n";
       (* The sections *)
       let sections =
         [ "SYMB", Symtable.data_global_map();
           "PRIM", Obj.repr(Symtable.data_primitive_names());
           "CRCS", Obj.repr(extract_crc_interfaces()) ] in
       output_string outchan "static char caml_sections[] = {\n";
       output_data_string outchan
         (Marshal.to_string sections []);
       output_string outchan "\n};\n\n";
       (* The table of primitives *)
       Symtable.output_primitive_table outchan;
       (* The entry point *)
       if with_main then begin
         output_string outchan "\
\nint main_os(int argc, char_os **argv)\
\n{\
\n  caml_byte_program_mode = COMPLETE_EXE;\
\n  caml_startup_code(caml_code, sizeof(caml_code),\
\n                    caml_data, sizeof(caml_data),\
\n                    caml_sections, sizeof(caml_sections),\
\n                    /* pooling */ 0,\
\n                    argv);\
\n  caml_do_exit(0);\
\n  return 0; /* not reached */\
\n}\n"
       end else begin
         output_string outchan "\
\nvoid caml_startup(char_os ** argv)\
\n{\
\n  caml_startup_code(caml_code, sizeof(caml_code),\
\n                    caml_data, sizeof(caml_data),\
\n                    caml_sections, sizeof(caml_sections),\
\n                    /* pooling */ 0,\
\n                    argv);\
\n}\
\n\
\nvalue caml_startup_exn(char_os ** argv)\
\n{\
\n  return caml_startup_code_exn(caml_code, sizeof(caml_code),\
\n                               caml_data, sizeof(caml_data),\
\n                               caml_sections, sizeof(caml_sections),\
\n                               /* pooling */ 0,\
\n                               argv);\
\n}\
\n\
\nvoid caml_startup_pooled(char_os ** argv)\
\n{\
\n  caml_startup_code(caml_code, sizeof(caml_code),\
\n                    caml_data, sizeof(caml_data),\
\n                    caml_sections, sizeof(caml_sections),\
\n                    /* pooling */ 1,\
\n                    argv);\
\n}\
\n\
\nvalue caml_startup_pooled_exn(char_os ** argv)\
\n{\
\n  return caml_startup_code_exn(caml_code, sizeof(caml_code),\
\n                               caml_data, sizeof(caml_data),\
\n                               caml_sections, sizeof(caml_sections),\
\n                               /* pooling */ 1,\
\n                               argv);\
\n}\n"
       end;
       output_string outchan "\
\n#ifdef __cplusplus\
\n}\
\n#endif\n";
    );
  if not with_main && !Clflags.debug then
    output_cds_file ((Filename.chop_extension outfile) ^ ".cds")

(* Build a custom runtime *)

let build_custom_runtime prim_name exec_name =
  let runtime_lib =
    if not !Clflags.with_runtime
    then ""
    else "-lcamlrun" ^ !Clflags.runtime_variant in
  let debug_prefix_map =
    if Config.c_has_debug_prefix_map && not !Clflags.keep_camlprimc_file then
      let flag =
        [Printf.sprintf "-fdebug-prefix-map=%s=camlprim.c" prim_name]
      in
        if Ccomp.linker_is_flexlink then
          "-link" :: flag
        else
          flag
    else
      [] in
  let exitcode =
    (Clflags.std_include_flag "-I" ^ " " ^ Config.bytecomp_c_libraries)
  in
  Ccomp.call_linker Ccomp.Exe exec_name
    (debug_prefix_map @ [prim_name] @ List.rev !Clflags.ccobjs @ [runtime_lib])
    exitcode = 0

let append_bytecode bytecode_name exec_name =
  let oc = open_out_gen [Open_wronly; Open_append; Open_binary] 0 exec_name in
  let ic = open_in_bin bytecode_name in
  copy_file ic oc;
  close_in ic;
  close_out oc

(* Fix the name of the output file, if the C compiler changes it behind
   our back. *)

let fix_exec_name name =
  match Sys.os_type with
    "Win32" | "Cygwin" ->
      if String.contains name '.' then name else name ^ ".exe"
  | _ -> name

(* Main entry point (build a custom runtime if needed) *)

let link objfiles output_name =
  let objfiles =
    match
      !Clflags.nopervasives,
      !Clflags.output_c_object,
      !Clflags.output_complete_executable
    with
    | true, _, _         -> objfiles
    | false, true, false -> "stdlib.cma" :: objfiles
    | _                  -> "stdlib.cma" :: objfiles @ ["std_exit.cmo"]
  in
  let tolink = List.fold_right scan_file objfiles [] in
  let missing_modules =
    Ident.Map.filter (fun id _ -> not (Ident.is_predef id)) !missing_globals
  in
  begin
    match Ident.Map.bindings missing_modules with
    | [] -> ()
    | (id, cu_name) :: _ ->
        match !badly_ordered_dependencies with
        | [] ->
            raise (Error (Required_module_unavailable (Ident.name id, cu_name)))
        | l ->
            raise (Error (Wrong_link_order l))
  end;
  Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs; (* put user's libs last *)
  Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                   (* put user's opts first *)
  Clflags.dllibs := !lib_dllibs @ !Clflags.dllibs; (* put user's DLLs first *)
  if not !Clflags.custom_runtime then
    link_bytecode tolink output_name true
  else if not !Clflags.output_c_object then begin
    let bytecode_name = Filename.temp_file "camlcode" "" in
    let prim_name =
      if !Clflags.keep_camlprimc_file then
        output_name ^ ".camlprim.c"
      else
        Filename.temp_file "camlprim" ".c" in
    Misc.try_finally
      ~always:(fun () ->
          remove_file bytecode_name;
          if not !Clflags.keep_camlprimc_file then remove_file prim_name)
      (fun () ->
         link_bytecode ~final_name:output_name tolink bytecode_name false;
         let poc = open_out prim_name in
         (* note: builds will not be reproducible if the C code contains macros
            such as __FILE__. *)
         output_string poc "\
         #ifdef __cplusplus\n\
         extern \"C\" {\n\
         #endif\n\
         #ifdef _WIN64\n\
         #ifdef __MINGW32__\n\
         typedef long long value;\n\
         #else\n\
         typedef __int64 value;\n\
         #endif\n\
         #else\n\
         typedef long value;\n\
         #endif\n";
         Symtable.output_primitive_table poc;
         output_string poc "\
         #ifdef __cplusplus\n\
         }\n\
         #endif\n";
         close_out poc;
         let exec_name = fix_exec_name output_name in
         if not (build_custom_runtime prim_name exec_name)
         then raise(Error Custom_runtime);
         if not !Clflags.make_runtime then
           append_bytecode bytecode_name exec_name
      )
  end else begin
    let basename = Filename.remove_extension output_name in
    let c_file, stable_name =
      if !Clflags.output_complete_object
         && not (Filename.check_suffix output_name ".c")
      then Filename.temp_file "camlobj" ".c", Some "camlobj.c"
      else begin
        let f = basename ^ ".c" in
        if Sys.file_exists f then raise(Error(File_exists f));
        f, None
      end
    in
    let obj_file =
      if !Clflags.output_complete_object
      then (Filename.chop_extension c_file) ^ Config.ext_obj
      else basename ^ Config.ext_obj
    in
    let temps = ref [] in
    Misc.try_finally
      ~always:(fun () -> List.iter remove_file !temps)
      (fun () ->
         link_bytecode_as_c tolink c_file !Clflags.output_complete_executable;
         if !Clflags.output_complete_executable then begin
           temps := c_file :: !temps;
           if not (build_custom_runtime c_file output_name) then
             raise(Error Custom_runtime)
         end else if not (Filename.check_suffix output_name ".c") then begin
           temps := c_file :: !temps;
           if Ccomp.compile_file ~output:obj_file ?stable_name c_file <> 0 then
             raise(Error Custom_runtime);
           if not (Filename.check_suffix output_name Config.ext_obj) ||
              !Clflags.output_complete_object then begin
             temps := obj_file :: !temps;
             let mode, c_libs =
               if Filename.check_suffix output_name Config.ext_obj
               then Ccomp.Partial, ""
               else Ccomp.MainDll, Config.bytecomp_c_libraries
             in
             if not (
                 let runtime_lib =
                   if not !Clflags.with_runtime
                   then ""
                   else "-lcamlrun" ^ !Clflags.runtime_variant in
                 Ccomp.call_linker mode output_name
                   ([obj_file] @ List.rev !Clflags.ccobjs @ [runtime_lib])
                   c_libs = 0
               ) then raise (Error Custom_runtime);
           end
         end;
      )
  end

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Location.print_filename name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a bytecode object file"
        Location.print_filename name
  | Wrong_object_name name ->
      fprintf ppf "The output file %s has the wrong name. The extension implies\
                  \ an object file but the link step was requested" name
  | Symbol_error(name, err) ->
      fprintf ppf "Error while linking %a:@ %a" Location.print_filename name
      Symtable.report_error err
  | Inconsistent_import(intf, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ \
                 make inconsistent assumptions over interface %s@]"
        Location.print_filename file1
        Location.print_filename file2
        intf
  | Custom_runtime ->
      fprintf ppf "Error while building custom runtime system"
  | File_exists file ->
      fprintf ppf "Cannot overwrite existing file %a"
        Location.print_filename file
  | Cannot_open_dll file ->
      fprintf ppf "Error on dynamically loaded library: %a"
        Location.print_filename file
  | Required_module_unavailable (s, m) ->
      fprintf ppf "Module `%s' is unavailable (required by `%s')" s m
  | Camlheader (msg, header) ->
      fprintf ppf "System error while copying file %s: %s" header msg
  | Wrong_link_order l ->
      let depends_on ppf (dep, depending) =
        fprintf ppf "%s depends on %s" depending dep
      in
      fprintf ppf "@[<hov 2>Wrong link order: %a@]"
        (pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ",@ ") depends_on) l

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  lib_ccobjs := [];
  lib_ccopts := [];
  lib_dllibs := [];
  missing_globals := Ident.Map.empty;
  Consistbl.clear crc_interfaces;
  implementations_defined := [];
  debug_info := [];
  output_code_string_counter := 0
