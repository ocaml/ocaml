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

open Config
open Clflags

let process_interface_file name =
  Compile.interface name

let process_implementation_file name =
  Compile.implementation name;
  objfiles := (Filename.chop_extension name ^ ".cmo") :: !objfiles

let process_file name =
  if Filename.check_suffix name ".ml"
  or Filename.check_suffix name ".mlt" then begin
    Compile.implementation name;
    objfiles := (Filename.chop_extension name ^ ".cmo") :: !objfiles
  end
  else if Filename.check_suffix name ".mli" then
    Compile.interface name
  else if Filename.check_suffix name ".cmo" 
       or Filename.check_suffix name ".cma" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       or Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Compile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
    :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_number () =
  print_string "The Objective Caml compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline()

let usage = "Usage: ocamlc <options> <files>\nOptions are:"

let main () =
  try
    Arg.parse [
       "-a", Arg.Set make_archive, " Build a library";
       "-c", Arg.Set compile_only, " Compile only (do not link)";
       "-cclib", Arg.String(fun s -> ccobjs := s :: !ccobjs),
             "<opt>  Pass option <opt> to the C linker";
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts),
             "<opt>  Pass option <opt> to the C compiler and linker";
       "-custom", Arg.Set custom_runtime, " Link in custom mode";
       "-g", Arg.Set debug, " Save debugging information";
       "-i", Arg.Set print_types, " Print the types";
       "-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs),
             "<dir>  Add <dir> to the list of include directories";
       "-impl", Arg.String process_implementation_file,
             "<file>  Compile <file> as a .ml file";
       "-intf", Arg.String process_interface_file,
             "<file>  Compile <file> as a .mli file";
       "-linkall", Arg.Set link_everything,
             " Link all modules, even unused ones";
       "-o", Arg.String(fun s -> exec_name := s;
                                 archive_name := s;
                                 object_name := s),
             "<file>  Set output file name to <file> (default a.out)";
       "-output-obj", Arg.Unit(fun () -> output_c_object := true;
                                         custom_runtime := true),
             "Output a C object file instead of an executable";
       "-pp", Arg.String(fun s -> preprocessor := Some s),
             "<command>  Pipe sources through preprocessor <command>";
       "-thread", Arg.Set thread_safe, " Use thread-safe standard library";
       "-unsafe", Arg.Set fast, " No bounds checking on array and string access";
       "-v", Arg.Unit print_version_number, " Print compiler version number";

       "-nopervasives", Arg.Set nopervasives, " (undocumented)";
       "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
       "-dlambda", Arg.Set dump_lambda, " (undocumented)";
       "-dinstr", Arg.Set dump_instr, " (undocumented)";

       "-", Arg.String process_file,
            "<file>  Treat <file> as a file name (even if it starts with `-')"
      ] process_file usage;
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
    Format.set_formatter_out_channel stderr;
    Errors.report_error x;
    exit 2

let _ = Printexc.catch main ()
