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
       "-c", Arg.Unit(fun () -> compile_only := true);
       "-S", Arg.Unit(fun s -> keep_asm_file := true);
       "-o", Arg.String(fun s -> exec_name := s; archive_name := s);
       "-i", Arg.Unit(fun () -> print_types := true);
       "-a", Arg.Unit(fun () -> make_archive := true);
       "-fast", Arg.Unit(fun () -> fast := true);
       "-compact", Arg.Unit(fun () -> optimize_for_speed := false);
       "-nopervasives", Arg.Unit(fun () -> nopervasives := true);
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts);
       "-cclib", Arg.String(fun s -> ccobjs := ("-l" ^ s) :: !ccobjs);
       "-l", Arg.String(fun s -> ccobjs := s :: !ccobjs);
       "-dlambda", Arg.Unit(fun () -> dump_lambda := true);
       "-dcmm", Arg.Unit(fun () -> dump_cmm := true);
       "-dsel", Arg.Unit(fun () -> dump_selection := true);
       "-dlive", Arg.Unit(fun () -> dump_live := true;
                                    Printmach.print_live := true);
       "-dspill", Arg.Unit(fun () -> dump_spill := true);
       "-dsplit", Arg.Unit(fun () -> dump_split := true);
       "-dinterf", Arg.Unit(fun () -> dump_interf := true);
       "-dprefer", Arg.Unit(fun () -> dump_prefer := true);
       "-dalloc", Arg.Unit(fun () -> dump_regalloc := true);
       "-dreload", Arg.Unit(fun () -> dump_reload := true);
       "-dlinear", Arg.Unit(fun () -> dump_linear := true);
       "-dstartup", Arg.Unit(fun () -> keep_startup_file := true);
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
