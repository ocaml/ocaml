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

open Config
open Clflags

let output_prefix name =
  let oname =
    match !output_name with
    | None -> name
    | Some n -> if !compile_only then (output_name := None; n) else name in
  Misc.chop_extension_if_any oname

let process_interface_file ppf name =
  Optcompile.interface ppf name (output_prefix name)

let process_implementation_file ppf name =
  let opref = output_prefix name in
  Optcompile.implementation ppf name opref;
  objfiles := (opref ^ ".cmx") :: !objfiles

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then 
    process_implementation_file ppf name
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Optcompile.interface ppf name opref;
    if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  end
  else if Filename.check_suffix name ".cmx" 
       || Filename.check_suffix name ".cmxa" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".cmi" && !make_package then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Optcompile.c_file name;
    ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
              :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_and_library () =
  print_string "The Objective Caml native-code compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let fatal err =
  prerr_endline err;
  exit 2

let extract_output = function
  | Some s -> s
  | None ->
      fatal "Please specify the name of the output file, using option -o"

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

let main () =
  native_code := true;
  let ppf = Format.err_formatter in
  try
    Arg.parse (Arch.command_line_options @ [
       "-a", Arg.Set make_archive, " Build a library";
       "-annot", Arg.Set annotations,
             " Save information in <filename>.annot";
       "-c", Arg.Set compile_only, " Compile only (do not link)";
       "-cc", Arg.String(fun s -> c_compiler := Some s),
             "<comp>  Use <comp> as the C compiler and linker";
       "-cclib", Arg.String(fun s ->
                              ccobjs := Misc.rev_split_words s @ !ccobjs),
             "<opt>  Pass option <opt> to the C linker";
       "-ccopt", Arg.String(fun s -> ccopts := s :: !ccopts),
             "<opt>  Pass option <opt> to the C compiler and linker";
       "-compact", Arg.Clear optimize_for_speed,
             " Optimize code size rather than speed";
       "-config", Arg.Unit show_config,
             " print configuration values and exit";
       "-dtypes", Arg.Set annotations,
             " (deprecated) same as -annot";
       "-for-pack", Arg.String (fun s -> for_package := Some s),
             "<ident>  Generate code that can later be `packed' with\n\
         \     ocamlopt -pack -o <ident>.cmx";
       "-g", Arg.Set debug,
             " Record debugging information for exception backtrace";
       "-i", Arg.Unit (fun () -> print_types := true; compile_only := true),
             " Print inferred interface";
       "-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs),
             "<dir>  Add <dir> to the list of include directories";
       "-impl", Arg.String (process_implementation_file ppf),
             "<file>  Compile <file> as a .ml file";
       "-inline", Arg.Int(fun n -> inline_threshold := n * 8),
             "<n>  Set aggressiveness of inlining to <n>";
       "-intf", Arg.String (process_interface_file ppf),
             "<file>  Compile <file> as a .mli file";
       "-intf-suffix", Arg.String (fun s -> Config.interface_suffix := s),
             "<file>  Suffix for interface files (default: .mli)";
       "-intf_suffix", Arg.String (fun s -> Config.interface_suffix := s),
             "<file>  (deprecated) same as -intf-suffix";
       "-labels", Arg.Clear classic, " Use commuting label mode";
       "-linkall", Arg.Set link_everything,
             " Link all modules, even unused ones";
       "-noassert", Arg.Set noassert, " Don't compile assertion checks";
       "-nocontract", Arg.Set nocontract, " Don't compile contract checks";
       "-noautolink", Arg.Set no_auto_link,
             " Don't automatically link C libraries specified in .cmxa files";
       "-nodynlink", Arg.Clear dlcode,
             " Enable optimizations for code that will not be dynlinked";
       "-nolabels", Arg.Set classic, " Ignore non-optional labels in types";
       "-nostdlib", Arg.Set no_std_include,
           " do not add standard directory to the list of include directories";
       "-o", Arg.String(fun s -> output_name := Some s),
             "<file>  Set output file name to <file>";
       "-output-obj", Arg.Unit(fun () -> output_c_object := true),
             " Output a C object file instead of an executable";
       "-p", Arg.Set gprofile,
             " Compile and link with profiling support for \"gprof\"\n\
         \     (not supported on all platforms)";
       "-pack", Arg.Set make_package,
              " Package the given .cmx files into one .cmx";
       "-pp", Arg.String(fun s -> preprocessor := Some s),
             "<command>  Pipe sources through preprocessor <command>";
       "-principal", Arg.Set principal,
             " Check principality of type inference";
       "-rectypes", Arg.Set recursive_types,
             " Allow arbitrary recursive types";
       "-shared", Arg.Unit (fun () -> shared := true; dlcode := true), 
             " Produce a dynlinkable plugin";
       "-S", Arg.Set keep_asm_file, " Keep intermediate assembly file";
       "-thread", Arg.Set use_threads,
             " Generate code that supports the system threads library";
       "-unsafe", Arg.Set fast,
             " No bounds checking on array and string access";
       "-v", Arg.Unit print_version_and_library,
             " Print compiler version and standard library location and exit";
       "-version", Arg.Unit print_version_string,
             " Print compiler version and exit";
       "-verbose", Arg.Set verbose, " Print calls to external commands";
       "-w", Arg.String (Warnings.parse_options false),
             "<flags>  Enable or disable warnings according to <flags>:\n\
         \032    C/c enable/disable suspicious comment\n\
         \032    D/d enable/disable deprecated features\n\
         \032    E/e enable/disable fragile match\n\
         \032    F/f enable/disable partially applied function\n\
         \032    L/l enable/disable labels omitted in application\n\
         \032    M/m enable/disable overriden methods\n\
         \032    P/p enable/disable partial match\n\
         \032    S/s enable/disable non-unit statement\n\
         \032    U/u enable/disable unused match case\n\
         \032    V/v enable/disable overriden instance variables\n\
         \032    Y/y enable/disable suspicious unused variables\n\
         \032    Z/z enable/disable all other unused variables\n\
         \032    X/x enable/disable all other warnings\n\
         \032    A/a enable/disable all warnings\n\
         \032    default setting is \"Aelz\"";
       "-warn-error" , Arg.String (Warnings.parse_options true),
        "<flags>  Treat the warnings of <flags> as errors, if they are\n\
         \     enabled.  See option -w for the list of flags.\n\
         \     Default setting is \"a\" (warnings are not errors)";
       "-where", Arg.Unit print_standard_library,
         " Print location of standard library and exit";

       "-nopervasives", Arg.Set nopervasives, " (undocumented)";
       "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
       "-dtypedtree", Arg.Set dump_typedtree, " (undocumented)";
       "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
       "-dlambda", Arg.Set dump_lambda, " (undocumented)";
       "-dcmm", Arg.Set dump_cmm, " (undocumented)";
       "-dsel", Arg.Set dump_selection, " (undocumented)";
       "-dcombine", Arg.Set dump_combine, " (undocumented)";
       "-dlive", Arg.Unit(fun () -> dump_live := true;
                                    Printmach.print_live := true),
             " (undocumented)";
       "-dspill", Arg.Set dump_spill, " (undocumented)";
       "-dsplit", Arg.Set dump_split, " (undocumented)";
       "-dinterf", Arg.Set dump_interf, " (undocumented)";
       "-dprefer", Arg.Set dump_prefer, " (undocumented)";
       "-dalloc", Arg.Set dump_regalloc, " (undocumented)";
       "-dreload", Arg.Set dump_reload, " (undocumented)";
       "-dscheduling", Arg.Set dump_scheduling, " (undocumented)";
       "-dlinear", Arg.Set dump_linear, " (undocumented)";
       "-dstartup", Arg.Set keep_startup_file, " (undocumented)";
       "-", Arg.String (process_file ppf),
            "<file>  Treat <file> as a file name (even if it starts with `-')"
      ]) (process_file ppf) usage;
    if
      List.length (List.filter (fun x -> !x)
		     [make_archive;make_package;shared;compile_only;output_c_object]) > 1
    then
      fatal "Please specify at most one of -pack, -a, -shared, -c, -output-obj";
    if !make_archive then begin
      Optcompile.init_path();
      let target = extract_output !output_name in
      Asmlibrarian.create_archive (List.rev !objfiles) target;
    end
    else if !make_package then begin
      Optcompile.init_path();
      let target = extract_output !output_name in
      Asmpackager.package_files ppf (List.rev !objfiles) target;
    end
    else if !shared then begin
      Optcompile.init_path();
      let target = extract_output !output_name in
      Asmlink.link_shared ppf (List.rev !objfiles) target;
    end
    else if not !compile_only && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll)
          then s
          else
            fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          default_output !output_name
      in
      Optcompile.init_path();
      Asmlink.link ppf (List.rev !objfiles) target
    end;
    exit 0
  with x ->
    Opterrors.report_error ppf x;
    exit 2

let _ = main ()
