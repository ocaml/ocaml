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

let process_interface_file ppf name =
  Compile.interface ppf name

let process_implementation_file ppf name =
  Compile.implementation ppf name;
  objfiles := (Filename.chop_extension name ^ ".cmo") :: !objfiles

let process_file ppf name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then begin
    Compile.implementation ppf name;
    objfiles := (Filename.chop_extension name ^ ".cmo") :: !objfiles
  end
  else if Filename.check_suffix name !Config.interface_suffix then
    Compile.interface ppf name
  else if Filename.check_suffix name ".cmo"
       || Filename.check_suffix name ".cma" then
    objfiles := name :: !objfiles
  else if Filename.check_suffix name ext_obj
       || Filename.check_suffix name ext_lib then
    ccobjs := name :: !ccobjs
  else if Filename.check_suffix name ".c" then begin
    Compile.c_file name;
    match Sys.os_type with
    | "MacOS" -> ccobjs := (name ^ ".o") :: (name ^ ".x") :: !ccobjs
    | _ ->
       ccobjs := (Filename.chop_suffix (Filename.basename name) ".c" ^ ext_obj)
                 :: !ccobjs
  end
  else
    raise(Arg.Bad("don't know what to do with " ^ name))

let print_version_number () =
  print_string "The Objective Caml compiler, version ";
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let usage = "Usage: ocamlc <options> <files>\nOptions are:"

(* Error messages to standard error formatter *)
let anonymous = process_file Format.err_formatter;;
let impl = process_implementation_file Format.err_formatter;;
let intf = process_interface_file Format.err_formatter;;

module Options = Main_args.Make_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _c = set compile_only
  let _cc s = c_compiler := s; c_linker := s
  let _cclib s = ccobjs := s :: !ccobjs
  let _ccopt s = ccopts := s :: !ccopts
  let _custom = set custom_runtime
  let _g = set debug
  let _i = set print_types
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _o s = exec_name := s; archive_name := s; object_name := s
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _pp s = preprocessor := Some s
  let _rectypes = set recursive_types
  let _thread = set thread_safe
  let _unsafe = set fast
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v = print_version_number
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _dparsetree = set dump_parsetree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = anonymous
end)

let main () =
  try
    Arg.parse Options.list anonymous usage;
    if !make_archive then begin
      Compile.init_path();
      Bytelibrarian.create_archive (List.rev !objfiles) !archive_name
    end
    else if not !compile_only && !objfiles <> [] then begin
      Compile.init_path();
      Bytelink.link (List.rev !objfiles)
    end;
    exit 0
  with x ->
    Errors.report_error Format.err_formatter x;
    exit 2

let _ = Printexc.catch main ()
