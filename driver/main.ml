(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
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
  else if Filename.check_suffix name !Config.interface_suffix then
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

module Options = Main_args.Make_options (struct
  let set r () = r := true
  let _a = set make_archive
  let _c = set compile_only
  let _cclib s = ccobjs := s :: !ccobjs
  let _ccopt s = ccopts := s :: !ccopts
  let _custom = set custom_runtime
  let _g = set debug
  let _i = set print_types
  let _I s = include_dirs := s :: !include_dirs
  let _impl = process_implementation_file
  let _intf = process_interface_file
  let _intf_suffix s = Config.interface_suffix := s
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _noassert = set noassert
  let _o s = exec_name := s; archive_name := s; object_name := s
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _pp s = preprocessor := Some s
  let _thread = set thread_safe
  let _unsafe = set fast
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v = print_version_number
  let _w = Warnings.parse_options
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let anonymous = process_file
end)

let main () =
  try
    Arg.parse Options.list process_file usage;
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
