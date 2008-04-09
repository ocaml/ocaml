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

(* Compiling C files and building C libraries *)

let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  Sys.command cmdline

let run_command cmdline = ignore(command cmdline)

(* Build @responsefile to work around Windows limitations on 
   command-line length *)
let build_diversion lst =
  let (responsefile, oc) = Filename.open_temp_file "camlresp" "" in
  List.iter
    (fun f ->
      if f <> "" then begin
        output_string oc (Filename.quote f); output_char oc '\n'
      end)
    lst;
  close_out oc;
  at_exit (fun () -> Misc.remove_file responsefile);
  "@" ^ responsefile

let quote_files lst =
  let s =
    String.concat " "
      (List.map (fun f -> if f = "" then f else Filename.quote f) lst) in
  if Sys.os_type = "Win32" && String.length s >= 256
  then build_diversion lst
  else s

let quote_optfile = function
  | None -> ""
  | Some f -> Filename.quote f

let compile_file name =
     command
       (Printf.sprintf
         "%s -c %s %s %s %s"
         !Clflags.c_compiler
         (String.concat " " (List.rev !Clflags.ccopts))
         (quote_files
             (List.rev_map (fun dir -> "-I" ^ dir) !Clflags.include_dirs))
         (Clflags.std_include_flag "-I")
         (Filename.quote name))

let create_archive archive file_list =
  Misc.remove_file archive;
  let quoted_archive = Filename.quote archive in
  match Config.ccomp_type with
    "msvc" ->
      command(Printf.sprintf "link /lib /nologo /out:%s %s"
                             quoted_archive (quote_files file_list))
  | _ ->
      let r1 =
        command(Printf.sprintf "ar rc %s %s"
                quoted_archive (quote_files file_list)) in
      if r1 <> 0 || String.length Config.ranlib = 0
      then r1
      else command(Config.ranlib ^ " " ^ quoted_archive)

let expand_libname name =
  if String.length name < 2 || String.sub name 0 2 <> "-l"
  then name
  else begin
    let libname =
      "lib" ^ String.sub name 2 (String.length name - 2) ^ Config.ext_lib in
    try
      Misc.find_in_path !Config.load_path libname
    with Not_found ->
      libname
  end

(* Handling of msvc's /link options *)

let make_link_options optlist =
  let rec split linkopts otheropts = function
  | [] -> String.concat " " otheropts
	  ^ " /link /subsystem:console "
          ^ String.concat " " linkopts
  | opt :: rem ->
      if String.length opt >= 5 && String.sub opt 0 5 = "/link"
      then split (String.sub opt 5 (String.length opt - 5) :: linkopts)
                 otheropts rem
      else split linkopts (opt :: otheropts) rem
  in split [] [] optlist

(* Handling of Visual C++ 2005 manifest files *)

let merge_manifest exefile =
  let manfile = exefile ^ ".manifest" in
  if not (Sys.file_exists manfile) then 0 else begin
    let retcode =
      command (Printf.sprintf "mt -nologo -outputresource:%s -manifest %s"
                              (Filename.quote exefile)
                              (Filename.quote manfile)) in
    Misc.remove_file manfile;
    retcode
  end
