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
    Optcompile.implementation name;
    objfiles := (Filename.chop_suffix name ".ml" ^ ".cmx") :: !objfiles
  end
  else if Filename.check_suffix name ".mli" then
    Optcompile.interface name
  else if Filename.check_suffix name ".cmx" 
       or Filename.check_suffix name ".cmxa" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".o"
       or Filename.check_suffix name ".a" then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Optcompile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ".o")
    :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_number () =
  print_string "The Caml Special Light native-code compiler, version ";
  print_string Config.version;
  print_newline()

let main () =
  try
    Arg.parse
      ["-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs);
       "-c", Arg.Set compile_only;
       "-S", Arg.Set keep_asm_file;
       "-o", Arg.String(fun s -> exec_name := s; archive_name := s);
       "-i", Arg.Set print_types;
       "-a", Arg.Set make_archive;
       "-unsafe", Arg.Set fast;
       "-compact", Arg.Clear optimize_for_speed;
       "-nopervasives", Arg.Set nopervasives;
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts);
       "-cclib", Arg.String(fun s -> ccobjs := s :: !ccobjs);
       "-dlambda", Arg.Set dump_lambda;
       "-dcmm", Arg.Set dump_cmm;
       "-dsel", Arg.Set dump_selection;
       "-dlive", Arg.Unit(fun () -> dump_live := true;
                                    Printmach.print_live := true);
       "-dspill", Arg.Set dump_spill;
       "-dsplit", Arg.Set dump_split;
       "-dscheduling", Arg.Set dump_scheduling;
       "-dinterf", Arg.Set dump_interf;
       "-dprefer", Arg.Set dump_prefer;
       "-dalloc", Arg.Set dump_regalloc;
       "-dreload", Arg.Set dump_reload;
       "-dscheduling", Arg.Set dump_scheduling;
       "-dlinear", Arg.Set dump_linear;
       "-dstartup", Arg.Set keep_startup_file;
       "-v", Arg.Unit print_version_number;
       "-", Arg.String process_file]
      process_file;
    if !make_archive then begin
      Optcompile.init_path();
      Asmlibrarian.create_archive (List.rev !objfiles) !archive_name
    end
    else if not !compile_only & !objfiles <> [] then begin
      Optcompile.init_path();
      Asmlink.link (List.rev !objfiles)
    end;
    exit 0
  with x ->
    Format.set_formatter_output stderr;
    Opterrors.report_error x;
    exit 2

let _ = Printexc.catch main ()
