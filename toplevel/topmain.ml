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

open Clflags

let usage = "Usage: ocaml <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref []

let prepare ppf =
  Toploop.set_paths ();
  try
    let res = 
      List.for_all (Topdirs.load_file ppf) (List.rev !preload_objects) in
    !Toploop.toplevel_startup_hook ();
    res
  with x ->
    try Errors.report_error ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmo" || Filename.check_suffix name ".cma"
  then preload_objects := name :: !preload_objects
  else
    begin
      let newargs = Array.sub Sys.argv !Arg.current
                              (Array.length Sys.argv - !Arg.current)
      in
      if prepare ppf && Toploop.run_script ppf name newargs
      then exit 0
      else exit 2
    end

let print_version () =
  Printf.printf "The Objective Caml toplevel, version %s\n" Sys.ocaml_version;
  exit 0;
;;

let main () =
  Arg.parse [
     "-I", Arg.String(fun dir ->
       let dir = Misc.expand_directory Config.standard_library dir in
       include_dirs := dir :: !include_dirs),
           "<dir>  Add <dir> to the list of include directories";
     "-init", Arg.String (fun s -> init_file := Some s),
           "<file>  Load <file> instead of default init file";
     "-labels", Arg.Clear classic, " Labels commute (default)";
     "-noassert", Arg.Set noassert, " Do not compile assertion checks";
     "-nolabels", Arg.Set classic, " Ignore labels and do not commute";
     "-noprompt", Arg.Set noprompt, " Suppress all prompts";
     "-nostdlib", Arg.Set no_std_include,
           " do not add default directory to the list of include directories";
     "-principal", Arg.Set principal, " Check principality of type inference";
     "-rectypes", Arg.Set recursive_types, " Allow arbitrary recursive types";
     "-unsafe", Arg.Set fast, " No bound checking on array and string access";
     "-version", Arg.Unit print_version, " Print version and exit";
     "-w", Arg.String (Warnings.parse_options false),
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    C/c enable/disable suspicious comment\n\
       \032    D/d enable/disable deprecated features\n\
       \032    E/e enable/disable fragile match\n\
       \032    F/f enable/disable partially applied function\n\
       \032    L/l enable/disable labels omitted in application\n\
       \032    M/m enable/disable overriden method\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variable\n\
       \032    Y/y enable/disable suspicious unused variables\n\
       \032    Z/z enable/disable all other unused variables\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is \"Aelz\"";
     "-warn-error" , Arg.String (Warnings.parse_options true),
       "<flags>  Treat the warnings of <flags> as errors, if they are enabled.\n\
         \032    (see option -w for the list of flags)\n\
         \032    default setting is a (all warnings are non-fatal)";

     "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
     "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
     "-dlambda", Arg.Set dump_lambda, " (undocumented)";
     "-dinstr", Arg.Set dump_instr, " (undocumented)";
    ] file_argument usage;
  if not (prepare Format.err_formatter) then exit 2;
  Toploop.loop Format.std_formatter

