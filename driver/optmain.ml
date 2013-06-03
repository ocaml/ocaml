(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

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

let cmxa_present = ref false;;

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then
    process_implementation_file ppf name
  else if Filename.check_suffix name !Config.interface_suffix then begin
    let opref = output_prefix name in
    Optcompile.interface ppf name opref;
    if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  end
  else if Filename.check_suffix name ".cmx" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ".cmxa" then begin
    cmxa_present := true;
    objfiles := name :: !objfiles
  end else if Filename.check_suffix name ".cmi" && !make_package then
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
  print_string "The OCaml native-code compiler, version ";
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

let readenv () = (* A copy is in main.ml *)
  try
    let s = Sys.getenv "OCAMLCOMPPARAM" in
    List.iter (fun s ->
      match Misc.split s '=' with
(* debugging *)
      | [ "g" ]
      | [ "g"; "1" ] -> Clflags.debug := true
      | [ "g"; "0" ] -> Clflags.debug := false
(* profiling *)
      | [ "p" ]
      | [ "p"; "1" ] -> Clflags.gprofile := true
      | [ "p"; "0" ] -> Clflags.gprofile := false
(* sources *)
      | [ "s" ]
      | [ "s"; "1" ] ->
        Clflags.keep_asm_file := true;
        Clflags.keep_startup_file := true
      | [ "s"; "0" ] ->
        Clflags.keep_asm_file := false;
        Clflags.keep_startup_file := false
(* warn-errors *)
      | [ "we" ] -> Warnings.parse_options true "A"
      | [ "we"; warnings ] -> Warnings.parse_options true warnings
(* warnings *)
      | [ "w" ] -> Warnings.parse_options false "A"
      | [ "w"; warnings ] -> Warnings.parse_options false warnings
(* warn-errors *)
      | [ "wwe" ] ->
        Warnings.parse_options false "A";
        Warnings.parse_options true "A"
      | [ "wwe"; warnings ] ->
        Warnings.parse_options false warnings;
        Warnings.parse_options true warnings
      | _ -> ()
    ) (Misc.split s ';')
  with Not_found -> ()

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

let ppf = Format.err_formatter

(* Error messages to standard error formatter *)
let anonymous filename =
  readenv(); process_file ppf filename;;
let impl filename =
  readenv(); process_implementation_file ppf filename;;
let intf filename =
  readenv(); process_interface_file ppf filename;;

let show_config () =
  Config.print_config stdout;
  exit 0;
;;

module Options = Main_args.Make_optcomp_options (struct
  let set r () = r := true
  let clear r () = r := false

  let _a = set make_archive
  let _absname = set Location.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = ccobjs := Misc.rev_split_words s @ !ccobjs
  let _ccopt s = ccopts := s :: !ccopts
  let _compact = clear optimize_for_speed
  let _config () = show_config ()
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I dir = include_dirs := dir :: !include_dirs
  let _impl = impl
  let _inline n = inline_threshold := n * 8
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _labels = clear classic
  let _linkall = set link_everything
  let _no_app_funct = clear applicative_functors
  let _noassert = set noassert
  let _noautolink = set no_auto_link
  let _nodynlink = clear dlcode
  let _nolabels = set classic
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _output_obj = set output_c_object
  let _p = set gprofile
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = ppx := s :: !ppx
  let _principal = set principal
  let _short_paths = clear real_paths
  let _rectypes = set recursive_types
  let _runtime_variant s = runtime_variant := s
  let _strict_sequence = set strict_sequence
  let _shared () = shared := true; dlcode := true
  let _S = set keep_asm_file
  let _thread = set use_threads
  let _unsafe = set fast
  let _v () = print_version_and_library ()
  let _version () = print_version_string ()
  let _vnum () = print_version_string ()
  let _verbose = set verbose
  let _w s = Warnings.parse_options false s
  let _warn_error s = Warnings.parse_options true s
  let _warn_help = Warnings.help_warnings
  let _where () = print_standard_library ()

  let _nopervasives = set nopervasives
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dclambda = set dump_clambda
  let _dcmm = set dump_cmm
  let _dsel = set dump_selection
  let _dcombine = set dump_combine
  let _dlive () = dump_live := true; Printmach.print_live := true
  let _dspill = set dump_spill
  let _dsplit = set dump_split
  let _dinterf = set dump_interf
  let _dprefer = set dump_prefer
  let _dalloc = set dump_regalloc
  let _dreload = set dump_reload
  let _dscheduling = set dump_scheduling
  let _dlinear = set dump_linear
  let _dstartup = set keep_startup_file

  let anonymous = anonymous
end);;

let main () =
  native_code := true;
  let ppf = Format.err_formatter in
  try
    Arg.parse (Arch.command_line_options @ Options.list) anonymous usage;
    readenv ();
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared;
                      compile_only; output_c_object]) > 1
    then
      fatal "Please specify at most one of -pack, -a, -shared, -c, -output-obj";
    if !make_archive then begin
      if !cmxa_present then
        fatal "Option -a cannot be used with .cmxa input files.";
      Optcompile.init_path();
      let target = extract_output !output_name in
      Asmlibrarian.create_archive (List.rev !objfiles) target;
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Optcompile.init_path();
      let target = extract_output !output_name in
      Asmpackager.package_files ppf (List.rev !objfiles) target;
      Warnings.check_fatal ();
    end
    else if !shared then begin
      Optcompile.init_path();
      let target = extract_output !output_name in
      Asmlink.link_shared ppf (List.rev !objfiles) target;
      Warnings.check_fatal ();
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
      Asmlink.link ppf (List.rev !objfiles) target;
      Warnings.check_fatal ();
    end;
    exit 0
  with x ->
    Opterrors.report_error ppf x;
    exit 2

let _ = main ()
