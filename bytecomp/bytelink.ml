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

module Compunit = Symtable.Compunit

module Dep = struct
  type t = compunit * compunit
  let compare = compare
end

module DepSet = Set.Make (Dep)

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Wrong_object_name of filepath
  | Symbol_error of filepath * Symtable.error
  | Inconsistent_import of modname * filepath * filepath
  | Custom_runtime
  | File_exists of filepath
  | Cannot_open_dll of filepath
  | Camlheader of string * filepath
  | Link_error of Linkdeps.error
  | Needs_custom_runtime of string

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

let add_ccobjs obj_name origin l =
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
    end else if l.lib_custom then
      raise(Error(Needs_custom_runtime obj_name));
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

let required compunit =
  (Symtable.required_compunits compunit.cu_reloc
   @ compunit.cu_required_compunits)
  |> List.map (fun (Compunit i) -> i)

let provided compunit =
  List.filter_map (fun (rel, _pos) ->
  match rel with
    | Reloc_setcompunit (Compunit id) -> Some id
    | _ -> None) compunit.cu_reloc

let linkdeps_unit ldeps ~filename compunit =
  let requires = required compunit in
  (* [requires] contains pack submodules *)
  let provides = provided compunit in
  let Compunit compunit = compunit.cu_name in
  Linkdeps.add ldeps ~filename ~compunit ~requires ~provides

let scan_file ldeps obj_name tolink =
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
      linkdeps_unit ldeps ~filename:obj_name compunit;
      Link_object(file_name, compunit) :: tolink
    end
    else if buffer = cma_magic_number then begin
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      let pos_toc = input_binary_int ic in    (* Go to table of contents *)
      seek_in ic pos_toc;
      let toc = (input_value ic : library) in
      close_in ic;
      add_ccobjs obj_name (Filename.dirname file_name) toc;
      let required =
        List.fold_right
          (fun compunit reqd ->
             let Compunit name = compunit.cu_name in
            if compunit.cu_force_link
            || !Clflags.link_everything
            || Linkdeps.required ldeps name
            then begin
              linkdeps_unit ldeps ~filename:obj_name compunit;
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

let check_consistency file_name cu =
  try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc -> Consistbl.check crc_interfaces name crc file_name)
      cu.cu_imports
  with Consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_import(name, user, auth)))

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
  let code_block =
    Bigarray.Array1.create Bigarray.Char Bigarray.c_layout compunit.cu_codesize
  in
  match
    In_channel.really_input_bigarray inchan code_block 0 compunit.cu_codesize
  with
    | None -> raise End_of_file
    | Some () -> ();
  Symtable.patch_object code_block compunit.cu_reloc;
  if !Clflags.debug && compunit.cu_debug > 0 then begin
    seek_in inchan compunit.cu_debug;
    let debug_event_list : Instruct.debug_event list =
      Compression.input_value inchan in
    let debug_dirs : string list =
      Compression.input_value inchan in
    let file_path = Filename.dirname (Location.absolute_path file_name) in
    let debug_dirs =
      if List.mem file_path debug_dirs
      then debug_dirs
      else file_path :: debug_dirs in
    debug_info := (currpos_fun(), debug_event_list, debug_dirs) :: !debug_info
  end;
  output_fun code_block;
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
         let n = Compunit.name cu.cu_name in
         let name = file_name ^ "(" ^ n ^ ")" in
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

(* Transform a file name into an absolute file name *)

let make_absolute file =
  if not (Filename.is_relative file) then file
  else Location.rewrite_absolute_path
         (Filename.concat (Sys.getcwd()) file)

type launch_method =
| Shebang_bin_sh of string
| Shebang_runtime
| Executable

type runtime_launch_info = {
  buffer : string;
  bindir : string;
  launcher : launch_method;
  executable_offset : int
}

(* See https://www.in-ulm.de/~mascheck/various/shebang/#origin for a deep
   dive into shebangs.
   - Whitespace (space or horizontal tab) delimits the interpreter from an
     optional argument
   - The path clearly must not contain a linefeed
   - A maximum length of 125 (128 less the #! and the newline) is picked as a
     portable maximum (it's actually Linux's prior to kernel v5.1), rather than
     actually probing the maximum length in configure *)
let invalid_for_shebang_line path =
  let invalid_char = function ' ' | '\t' | '\n' -> true | _ -> false in
  String.length path > 125 || String.exists invalid_char path

(* The runtime-launch-info file consists of two "lines" followed by binary data.
   The file is _always_ LF-formatted, even on Windows. The sequence of bytes up
   to the first '\n' is interpreted:
     - "sh" - use a shebang-style launcher. If sh is needed, determine its
              location from [command -p -v sh]
     - "exe" - use the executable launcher contained in this runtime-launch-info
               file.
     - "/" ^ path - use a shebang-style launcher. If sh is needed, path is the
                    absolute location of sh. path must be valid for a shebang
                    line.
   The second "line" is interpreted as the next "\000\n"-terminated sequence and
   is the directory containing the default runtimes (ocamlrun, ocamlrund, etc.).
   The null terminator is used since '\n' is valid in a nefarious installation
   prefix but Posix forbids filenames including the nul character.
   The remainder of the file is then the executable launcher for bytecode
   programs (see stdlib/header{,nt}.c). *)

let read_runtime_launch_info file =
  let buffer =
    try
      In_channel.with_open_bin file In_channel.input_all
    with Sys_error msg -> raise (Error (Camlheader (msg, file)))
  in
  try
    let bindir_start = String.index buffer '\n' + 1 in
    let bindir_end = String.index_from buffer bindir_start '\000' in
    let bindir = String.sub buffer bindir_start (bindir_end - bindir_start) in
    let executable_offset = bindir_end + 2 in
    let launcher =
      let kind = String.sub buffer 0 (bindir_start - 1) in
      if kind = "exe" then
        Executable
      else if kind <> "" && (kind.[0] = '/' || kind = "sh") then
        Shebang_bin_sh kind
      else
        raise Not_found in
    if String.length buffer < executable_offset
       || buffer.[executable_offset - 1] <> '\n' then
      raise Not_found
    else
      {bindir; launcher; buffer; executable_offset}
  with Not_found ->
    raise (Error (Camlheader ("corrupt header", file)))

let find_bin_sh () =
  let output_file = Filename.temp_file "caml_bin_sh" "" in
  let result =
  try
    let cmd =
      Filename.quote_command ~stdout:output_file "command" ["-p"; "-v"; "sh"]
    in
    if !Clflags.verbose then
      Printf.eprintf "+ %s\n" cmd;
    if Sys.command cmd = 0 then
      In_channel.with_open_text output_file input_line
    else
      ""
  with Sys_error _
     | End_of_file -> ""
  in
  remove_file output_file;
  result

(* Writes the executable header to outchan and writes the RNTM section, if
   needed. Returns a toc_writer (i.e. Bytesections.init_record is always
   called) *)

let write_header outchan =
  let use_runtime, runtime =
    if String.length !Clflags.use_runtime > 0 then
      (true, make_absolute !Clflags.use_runtime)
    else
      (false, "ocamlrun" ^ !Clflags.runtime_variant)
  in
  (* Write the header *)
  let runtime_info =
    let header = "runtime-launch-info" in
    try read_runtime_launch_info (Load_path.find header)
    with Not_found -> raise (Error (File_not_found header))
  in
  let runtime =
    (* Historically, the native Windows ports are assumed to be finding
       ocamlrun using a PATH search. *)
    if use_runtime || Sys.win32 then
      runtime
    else
      Filename.concat runtime_info.bindir runtime
  in
  (* Determine which method will be used for launching the executable:
     Executable: concatenate the bytecode image to the executable stub
     Shebang_runtime: #! line with the required runtime
     Shebang_bin_sh: #! for a shell script calling exec *)
  let launcher =
    if runtime_info.launcher = Executable then
      Executable
    else
      if invalid_for_shebang_line runtime then
        match runtime_info.launcher with
        | Shebang_bin_sh sh ->
            let sh =
              if sh = "sh" then
                find_bin_sh ()
              else
                sh in
            if sh = "" || invalid_for_shebang_line sh then
              Executable
            else
              Shebang_bin_sh sh
        | _ ->
            Executable
      else
        Shebang_runtime
  in
  match launcher with
  | Shebang_runtime ->
      (* Use the runtime directly *)
      Printf.fprintf outchan "#!%s\n" runtime;
      Bytesections.init_record outchan
  | Shebang_bin_sh bin_sh ->
      (* exec the runtime using sh *)
      Printf.fprintf outchan "\
        #!%s\n\
        exec %s \"$0\" \"$@\"\n" bin_sh (Filename.quote runtime);
      Bytesections.init_record outchan
  | Executable ->
      (* Use the executable stub launcher *)
      let pos = runtime_info.executable_offset in
      let len = String.length runtime_info.buffer - pos in
      Out_channel.output_substring outchan runtime_info.buffer pos len;
      (* The runtime name needs recording in RNTM *)
      let toc_writer = Bytesections.init_record outchan in
      Printf.fprintf outchan "%s\000" runtime;
      Bytesections.record toc_writer RNTM;
      toc_writer

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
       let toc_writer =
         (* Write the header and set the path to the bytecode interpreter *)
         if standalone && !Clflags.with_runtime then
           write_header outchan
         else
           Bytesections.init_record outchan
       in
       (* The bytecode *)
       let start_code = pos_out outchan in
       Symtable.init();
       clear_crc_interfaces ();
       let sharedobjs = List.map Dll.extract_dll_name !Clflags.dllibs in
       let check_dlls = standalone && Config.target = Config.host in
       if check_dlls then begin
         (* Initialize the DLL machinery *)
         Dll.init_compile !Clflags.no_std_include;
         Dll.add_path (Load_path.get_path_list ());
         try Dll.open_dlls Dll.For_checking sharedobjs
         with Failure reason -> raise(Error(Cannot_open_dll reason))
       end;
       let output_fun buf =
         Out_channel.output_bigarray outchan buf 0 (Bigarray.Array1.dim buf)
       and currpos_fun () = pos_out outchan - start_code in
       List.iter (link_file output_fun currpos_fun) tolink;
       if check_dlls then Dll.close_all_dlls();
       (* The final STOP instruction *)
       output_byte outchan Opcodes.opSTOP;
       output_byte outchan 0; output_byte outchan 0; output_byte outchan 0;
       Bytesections.record toc_writer CODE;
       (* DLL stuff *)
       if standalone then begin
         (* The extra search path for DLLs *)
         output_string outchan (concat_null_terminated !Clflags.dllpaths);
         Bytesections.record toc_writer DLPT;
         (* The names of the DLLs *)
         output_string outchan (concat_null_terminated sharedobjs);
         Bytesections.record toc_writer DLLS
       end;
       (* The names of all primitives *)
       Symtable.output_primitive_names outchan;
       Bytesections.record toc_writer PRIM;
       (* The table of global data *)
       Emitcode.marshal_to_channel_with_possibly_32bit_compat
         ~filename:final_name ~kind:"bytecode executable"
         outchan (Symtable.initial_global_table());
       Bytesections.record toc_writer DATA;
       (* The map of global identifiers *)
       Symtable.output_global_map outchan;
       Bytesections.record toc_writer SYMB;
       (* CRCs for modules *)
       output_value outchan (extract_crc_interfaces());
       Bytesections.record toc_writer CRCS;
       (* Debug info *)
       if !Clflags.debug then begin
         output_debug_info outchan;
         Bytesections.record toc_writer DBUG
       end;
       (* The table of contents and the trailer *)
       Bytesections.write_toc_and_trailer toc_writer;
    )

(* Output a string as a C array of unsigned ints *)

let output_code_string_counter = ref 0

let output_code_string outchan code =
  let pos = ref 0 in
  let len = Bigarray.Array1.dim code in
  while !pos < len do
    let c1 = Char.code(Bigarray.Array1.get code !pos) in
    let c2 = Char.code(Bigarray.Array1.get code (!pos + 1)) in
    let c3 = Char.code(Bigarray.Array1.get code (!pos + 2)) in
    let c4 = Char.code(Bigarray.Array1.get code (!pos + 3)) in
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
       let toc_writer = Bytesections.init_record outchan in
       (* The map of global identifiers *)
       Symtable.output_global_map outchan;
       Bytesections.record toc_writer SYMB;
       (* Debug info *)
       output_debug_info outchan;
       Bytesections.record toc_writer DBUG;
       (* The table of contents and the trailer *)
       Bytesections.write_toc_and_trailer toc_writer;
    )

(* Output a bytecode executable as a C file *)

let link_bytecode_as_c tolink outfile with_main =
  let outchan = open_out outfile in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file outfile)
    (fun () ->
       (* The bytecode *)
       output_string outchan
{|#ifdef __cplusplus
extern "C" {
#endif

#define CAML_INTERNALS
#define CAMLDLLIMPORT
#define CAML_INTERNALS_NO_PRIM_DECLARATIONS

#include <caml/mlvalues.h>
#include <caml/startup.h>
#include <caml/sys.h>
#include <caml/misc.h>

static int caml_code[] = {
|};
       Symtable.init();
       clear_crc_interfaces ();
       let currpos = ref 0 in
       let output_fun code =
         output_code_string outchan code;
         currpos := !currpos + (Bigarray.Array1.dim code)
       and currpos_fun () = !currpos in
       List.iter (link_file output_fun currpos_fun) tolink;
       (* The final STOP instruction *)
       Printf.fprintf outchan "\n0x%x};\n" Opcodes.opSTOP;
       (* The table of global data *)
       output_string outchan {|
static char caml_data[] = {
|};
       output_data_string outchan
         (Marshal.to_string (Symtable.initial_global_table()) []);
       output_string outchan {|
};
|};
       (* The sections *)
       let sections : (string * Obj.t) array =
         [| Bytesections.Name.to_string SYMB,
            Symtable.data_global_map();
            Bytesections.Name.to_string CRCS,
            Obj.repr(extract_crc_interfaces()) |]
       in
       output_string outchan {|
static char caml_sections[] = {
|};
       output_data_string outchan
         (Marshal.to_string sections []);
       output_string outchan {|
};

|};
       (* The table of primitives *)
       Symtable.output_primitive_table outchan;
       (* The entry point *)
       if with_main then begin
         output_string outchan {|
int main_os(int argc, char_os **argv)
{
  caml_byte_program_mode = COMPLETE_EXE;
  caml_startup_code(caml_code, sizeof(caml_code),
                    caml_data, sizeof(caml_data),
                    caml_sections, sizeof(caml_sections),
                    /* pooling */ 0,
                    argv);
  caml_do_exit(0);
  return 0; /* not reached */
}
|}
       end else begin
         output_string outchan {|
void caml_startup(char_os ** argv)
{
  caml_startup_code(caml_code, sizeof(caml_code),
                    caml_data, sizeof(caml_data),
                    caml_sections, sizeof(caml_sections),
                    /* pooling */ 0,
                    argv);
}

value caml_startup_exn(char_os ** argv)
{
  return caml_startup_code_exn(caml_code, sizeof(caml_code),
                               caml_data, sizeof(caml_data),
                               caml_sections, sizeof(caml_sections),
                               /* pooling */ 0,
                               argv);
}

void caml_startup_pooled(char_os ** argv)
{
  caml_startup_code(caml_code, sizeof(caml_code),
                    caml_data, sizeof(caml_data),
                    caml_sections, sizeof(caml_sections),
                    /* pooling */ 1,
                    argv);
}

value caml_startup_pooled_exn(char_os ** argv)
{
  return caml_startup_code_exn(caml_code, sizeof(caml_code),
                               caml_data, sizeof(caml_data),
                               caml_sections, sizeof(caml_sections),
                               /* pooling */ 1,
                               argv);
}
|}
       end;
       output_string outchan {|
#ifdef __cplusplus
}
#endif
|};
    );
  if not with_main && !Clflags.debug then
    output_cds_file ((Filename.chop_extension outfile) ^ ".cds")

(* Build a custom runtime *)

let build_custom_runtime prim_name exec_name =
  let runtime_lib =
    if not !Clflags.with_runtime
    then ""
    else "-lcamlrun" ^ !Clflags.runtime_variant in
  let stable_name =
    if not !Clflags.keep_camlprimc_file then
      Some "camlprim.c"
    else
      None
  in
  let prims_obj = Filename.temp_file "camlprim" Config.ext_obj in
  let result =
    Ccomp.compile_file ~output:prims_obj ?stable_name prim_name = 0
    && Ccomp.call_linker Ccomp.Exe exec_name
        ([prims_obj] @ List.rev !Clflags.ccobjs @ [runtime_lib])
        (Clflags.std_include_flag "-I" ^ " " ^ Config.bytecomp_c_libraries) = 0
  in
  remove_file prims_obj;
  result

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
  let ldeps = Linkdeps.create ~complete:true in
  let tolink = List.fold_right (scan_file ldeps) objfiles [] in
  (match Linkdeps.check ldeps with
   | None -> ()
   | Some e -> raise (Error (Link_error e)));
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
         output_string poc
{|#ifdef __cplusplus
extern "C" {
#endif

#define CAML_INTERNALS_NO_PRIM_DECLARATIONS
#include <caml/mlvalues.h>

|};
         Symtable.output_primitive_table poc;
         output_string poc {|
#ifdef __cplusplus
}
#endif
|};
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

open Format_doc
module Style = Misc.Style

let report_error_doc ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a"
        Location.Doc.quoted_filename name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a bytecode object file"
        Location.Doc.quoted_filename name
  | Wrong_object_name name ->
      fprintf ppf "The output file %a has the wrong name. The extension implies\
                  \ an object file but the link step was requested"
        Style.inline_code name
  | Symbol_error(name, err) ->
      fprintf ppf "Error while linking %a:@ %a"
        Location.Doc.quoted_filename name
        Symtable.report_error_doc err
  | Inconsistent_import(intf, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ \
                 make inconsistent assumptions over interface %a@]"
        Location.Doc.quoted_filename file1
        Location.Doc.quoted_filename file2
        Style.inline_code intf
  | Custom_runtime ->
      fprintf ppf "Error while building custom runtime system"
  | File_exists file ->
      fprintf ppf "Cannot overwrite existing file %a"
        Location.Doc.quoted_filename file
  | Cannot_open_dll file ->
      fprintf ppf "Error on dynamically loaded library: %a"
        Location.Doc.filename file
  | Camlheader (msg, header) ->
      fprintf ppf "System error while copying file %a: %a"
        Style.inline_code header
        Style.inline_code msg
  | Link_error e ->
      Linkdeps.report_error_doc ~print_filename:Location.Doc.filename ppf e
  | Needs_custom_runtime obj_name ->
      fprintf ppf "%s links with C code, so cannot be linked with -use-prims \
                   or -use-runtime unless -noautolink is specified" obj_name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc

let reset () =
  lib_ccobjs := [];
  lib_ccopts := [];
  lib_dllibs := [];
  Consistbl.clear crc_interfaces;
  debug_info := [];
  output_code_string_counter := 0
