(***********************************************************************)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)
(* Original author: Nicolas Pouillard *)

let version = "ocamlbuild 0.1";;

type command_spec = Command.spec

open My_std
open Arg
open Format
open Command

let entry = ref None
let build_dir = ref "_build"
let include_dirs = ref []
let exclude_dirs = ref []
let nothing_should_be_rebuilt = ref false
let sterilize = ref true
let sterilization_script = ref "sterilize.sh"
let hygiene = ref true
let ignore_auto = ref true
let plugin = ref true
let just_plugin = ref false
let native_plugin = ref true
let make_links = ref true
let nostdlib = ref false
let use_menhir = ref false
let ocamlc = ref (A"ocamlc.opt")
let ocamlopt = ref (A"ocamlopt.opt")
let ocamldep = ref (A"ocamldep.opt")
let ocamldoc = ref (A"ocamldoc.opt")
let ocamlyacc = ref (A"ocamlyacc")
let ocamllex = ref (A"ocamllex")
let ocamlmklib = ref (A"ocamlmklib")
let ocamlrun = ref N
let program_to_execute = ref false
let must_clean = ref false
let ext_lib = ref "a"
let ext_obj = ref "o"
let ext_dll = ref "so"

let targets_internal = ref []
let ocaml_libs_internal = ref []
let ocaml_lflags_internal = ref []
let ocaml_cflags_internal = ref []
let ocaml_ppflags_internal = ref []
let ocaml_yaccflags_internal = ref []
let ocaml_lexflags_internal = ref []
let program_args_internal = ref []
let ignore_list_internal = ref []
let tags_internal = ref [["quiet"]]

let my_include_dirs = ref [[Filename.current_dir_name]]
let my_exclude_dirs = ref [[".svn"; "CVS"]]

let pwd = Sys.getcwd ()

let internal_log_file = ref None
let set_log_file file =
  internal_log_file := Some file;
  Log.log_file := lazy begin
    if !Log.level <= 0
    || ((!plugin || !just_plugin)
        && sys_file_exists (filename_concat pwd "myocamlbuild.ml")) then
      None
    else Some(filename_concat pwd file)
  end

let () = set_log_file "_log"

let dummy = "*invalid-dummy-string*";; (* Dummy string for delimiting the latest argument *)

let add_to rxs x =
  let xs = Lexers.comma_sep_strings (Lexing.from_string x) in
  rxs := xs :: !rxs
let add_to' rxs x =
  if x <> dummy then
    rxs := [x] :: !rxs
  else
    ()
let set_cmd rcmd = String (fun s -> rcmd := Sh s)
let spec =
  Arg.align
  [
   "-version", Unit (fun () -> print_endline version; raise Exit_OK), " Display the version";
   "-quiet", Unit (fun () -> Log.level := 0), " Make as quiet as possible";
   "-verbose", Int (fun i -> Log.level := i + 2), "<level> Set the verbosity level";
   "-log", String set_log_file, "<file> Set log file";
   "-no-log", Unit (fun () -> Log.log_file := lazy None), " No log file";
   "-clean", Set must_clean, " Remove build directory and other files, then exit"; 

   "-I", String (add_to' my_include_dirs), "<path> Add to include directories";
   "-Is", String (add_to my_include_dirs), "<path,...> (same as above, but accepts a comma-separated list)";
   "-X", String (add_to' my_exclude_dirs), "<path> Directory to ignore";
   "-Xs", String (add_to my_exclude_dirs), "<path,...> (idem)";

   "-lib", String (add_to' ocaml_libs_internal), "<flag> Link to this ocaml library";
   "-libs", String (add_to ocaml_libs_internal), "<flag,...> (idem)";
   "-lflag", String (add_to' ocaml_lflags_internal), "<flag> Add to ocamlc link flags";
   "-lflags", String (add_to ocaml_lflags_internal), "<flag,...> (idem)";
   "-cflag", String (add_to' ocaml_cflags_internal), "<flag> Add to ocamlc compile flags";
   "-cflags", String (add_to ocaml_cflags_internal), "<flag,...> (idem)";
   "-yaccflag", String (add_to' ocaml_yaccflags_internal), "<flag> Add to ocamlyacc flags";
   "-yaccflags", String (add_to ocaml_yaccflags_internal), "<flag,...> (idem)";
   "-lexflag", String (add_to' ocaml_lexflags_internal), "<flag> Add to ocamllex flags";
   "-lexflags", String (add_to ocaml_lexflags_internal), "<flag,...> (idem)";
   "-ppflag", String (add_to' ocaml_ppflags_internal), "<flag> Add to ocaml preprocessing flags";
   "-pp", String (add_to ocaml_ppflags_internal), "<flag,...> (idem)";
   "-tag", String (add_to' tags_internal), "<tag> Add to default tags";
   "-tags", String (add_to tags_internal), "<tag,...> (idem)";

   "-ignore", String (add_to ignore_list_internal), "<module,...> Don't try to build these modules";
   "-no-links", Clear make_links, " Don't make links of produced final targets";
   "-no-skip", Clear ignore_auto, " Don't skip modules that are requested by ocamldep but cannot be built";
   "-no-hygiene", Clear hygiene, " Don't apply sanity-check rules";
   "-no-plugin", Clear plugin, " Don't build myocamlbuild.ml";
   "-no-stdlib", Set nostdlib, " Don't ignore stdlib modules";
   "-just-plugin", Set just_plugin, " Just build myocamlbuild.ml";
   "-byte-plugin", Clear native_plugin, " Don't use a native plugin but bytecode";
   "-sterilization-script", Set_string sterilization_script, " Change the file name for the generated sterilization script";
   "-no-sterilize", Clear sterilize, " Do not generate sterilization script";
   "-nothing-should-be-rebuilt", Set nothing_should_be_rebuilt, " Fail if something needs to be rebuilt";
   "-classic-display", Set Log.classic_display, " Display executed commands the old-fashioned way";
   "-use-menhir", Unit(fun () -> use_menhir := true; ocamlyacc := A"menhir"),
                  " Use menhir instead of ocamlyacc";

   "-j", Set_int Command.jobs, "<N> Allow N jobs at once (0 for unlimited)";

   "-build-dir", Set_string build_dir, "<path> Set build directory";
   "-install-dir", Set_string Ocamlbuild_where.where, "<path> Set the install directory";
   "-where", Unit (fun () -> print_endline !Ocamlbuild_where.where; raise Exit_OK), " Display the install directory";

   "-ocamlc", set_cmd ocamlc, "<command> Set the OCaml bytecode compiler";
   "-ocamlopt", set_cmd ocamlopt, "<command> Set the OCaml native compiler";
   "-ocamldep", set_cmd ocamldep, "<command> Set the OCaml dependency tool";
   "-ocamlyacc", set_cmd ocamlyacc, "<command> Set the ocamlyacc tool";
   "-menhir", set_cmd ocamlyacc, "<command> Set the menhir tool (use it after -use-menhir)";
   "-ocamllex", set_cmd ocamllex, "<command> Set the ocamllex tool";
   (* Not set since we perhaps want to replace ocamlmklib *)
   (* "-ocamlmklib", set_cmd ocamlmklib, "<command> Set the ocamlmklib tool"; *)
   "-ocamlrun", set_cmd ocamlrun, "<command> Set the ocamlrun tool";

   "--", Rest (fun x -> program_to_execute := true; add_to' program_args_internal x),
   " Stop argument processing, remaining arguments are given to the user program";
  ]

let targets = ref []
let ocaml_libs = ref []
let ocaml_lflags = ref []
let ocaml_cflags = ref []
let ocaml_ppflags = ref []
let ocaml_yaccflags = ref []
let ocaml_lexflags = ref []
let program_args = ref []
let ignore_list = ref []
let tags = ref []

let init () =
  let anon_fun = add_to' targets_internal in
  let usage_msg = sprintf "Usage %s [options] <target>" Sys.argv.(0) in
  let argv' = Array.concat [Sys.argv; [|dummy|]] in
  parse_argv argv' spec anon_fun usage_msg;
  Shell.mkdir_p !build_dir;
  let reorder x y = x := (List.concat (List.rev !y)) in
  reorder targets targets_internal;
  reorder ocaml_libs ocaml_libs_internal;
  reorder ocaml_cflags ocaml_cflags_internal;
  reorder ocaml_lflags ocaml_lflags_internal;
  reorder ocaml_ppflags ocaml_ppflags_internal;
  reorder ocaml_yaccflags ocaml_yaccflags_internal;
  reorder ocaml_lexflags ocaml_lexflags_internal;
  reorder program_args program_args_internal;
  reorder tags tags_internal;
  reorder ignore_list ignore_list_internal;

  let dir_reorder my dir =
    let d = !dir in
    reorder dir my;
    dir := List.filter sys_file_exists (!dir @ d)
  in
  dir_reorder my_include_dirs include_dirs;
  dir_reorder my_exclude_dirs exclude_dirs;

  ignore_list := List.map String.capitalize !ignore_list
;;
