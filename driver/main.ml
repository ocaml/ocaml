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

open Clflags

let process_file name =
  if Filename.check_suffix name ".ml" then begin
    Compile.implementation name;
    objfiles := (Filename.chop_suffix name ".ml" ^ ".cmo") :: !objfiles
  end
  else if Filename.check_suffix name ".mli" then
    Compile.interface name
  else if Filename.check_suffix name ".cmo" 
       or Filename.check_suffix name ".cma" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".o"
       or Filename.check_suffix name ".a" then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Compile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ".o")
    :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_number () =
  print_string "The Caml Special Light compiler, version ";
  print_string Config.version;
  print_newline()

let main () =
  try
    Arg.parse
      ["-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs);
       "-c", Arg.Unit(fun () -> compile_only := true);
       "-o", Arg.String(fun s -> exec_name := s; archive_name := s);
       "-i", Arg.Unit(fun () -> print_types := true);
       "-a", Arg.Unit(fun () -> make_archive := true);
       "-fast", Arg.Unit(fun () -> fast := true);
       "-nopervasives", Arg.Unit(fun () -> nopervasives := true);
       "-custom", Arg.Unit(fun () -> custom_runtime := true);
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts);
       "-cclib", Arg.String(fun s -> ccobjs := ("-l" ^ s) :: !ccobjs);
       "-l", Arg.String(fun s -> ccobjs := s :: !ccobjs);
       "-linkall", Arg.Unit(fun s -> link_everything := true);
       "-dlambda", Arg.Unit(fun () -> dump_lambda := true);
       "-dinstr", Arg.Unit(fun () -> dump_instr := true);
       "-v", Arg.Unit print_version_number;
       "-", Arg.String process_file]
      process_file;
    if !make_archive then begin
      Compile.init_path();
      Bytelibrarian.create_archive (List.rev !objfiles) !archive_name
    end
    else if not !compile_only & !objfiles <> [] then begin
      Compile.init_path();
      Bytelink.link (List.rev !objfiles)
    end;
    exit 0
  with x ->
    Format.set_formatter_output stderr;
    Errors.report_error x;
    exit 2

let _ = Printexc.catch main ()
