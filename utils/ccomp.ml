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

(* Compiling C files and building C libraries *)

let command cmdline =
  if !Clflags.verbose then begin
    prerr_string "+ ";
    prerr_string cmdline;
    prerr_newline()
  end;
  Sys.command cmdline

let compile_file name =
  command
   (Printf.sprintf
     "%s -c %s %s -I%s %s"
     !Clflags.c_compiler
     (String.concat " " (List.rev !Clflags.ccopts))
     (String.concat " "
       (List.map (fun dir -> "-I" ^ dir) 
                 (List.rev !Clflags.include_dirs)))
     Config.standard_library
     name)

let create_archive archive file_list =
  Misc.remove_file archive;
  match Config.system with
    "win32" ->
      command(Printf.sprintf "lib /nologo /debugtype:cv /out:%s %s"
                                 archive (String.concat " " file_list))
  | _ ->
      let r1 =
        command(Printf.sprintf "ar rc %s %s"
                                   archive (String.concat " " file_list)) in
      if r1 <> 0 or String.length Config.ranlib = 0
      then r1
      else command(Config.ranlib ^ " " ^ archive)
