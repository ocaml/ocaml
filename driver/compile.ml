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

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree

(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)

let init_path () =
  let dirs =
    if !Clflags.thread_safe
    then "+threads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev (Config.standard_library :: exp_dirs);
  Env.reset_cache()

(* Return the initial environment in which compilation proceeds. *)

let initial_env () =
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"

(* Optionally preprocess a source file *)

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      let tmpfile = Filename.temp_file "camlpp" "" in
      let comm = Printf.sprintf "%s %s > %s" pp sourcefile tmpfile in
      if Ccomp.command comm <> 0 then begin
        remove_file tmpfile;
        Printf.eprintf "Preprocessing error\n";
        exit 2
      end;
      tmpfile

let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> remove_file inputfile

(* Parse a file or get a dumped syntax tree in it *)

exception Outdated_version

let parse_file inputfile parse_fun ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = String.create (String.length ast_magic) in
      really_input ic buffer 0 (String.length ast_magic);
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        fatal_error "Ocaml and preprocessor have incompatible versions"
    | _ -> false
  in
  let ast =
    try
      if is_ast_file then begin
        Location.input_name := input_value ic;
        input_value ic
      end else begin
        seek_in ic 0;
        Location.input_name := inputfile;
        parse_fun (Lexing.from_channel ic)
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  ast

(* Compile a .mli file *)

let interface ppf sourcefile =
  init_path();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = preprocess sourcefile in
  try
    let ast = parse_file inputfile Parse.interface ast_intf_magic_number in
    if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
    let sg = Typemod.transl_signature (initial_env()) ast in
    if !Clflags.print_types
      then fprintf std_formatter "%a@." Printtyp.signature sg;
    Warnings.check_fatal ();
    Env.save_signature sg modulename (prefixname ^ ".cmi");
    remove_preprocessed inputfile
  with e ->
    remove_preprocessed inputfile;
    raise e

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile =
  init_path();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  let inputfile = preprocess sourcefile in
  let objfile = prefixname ^ ".cmo" in
  let oc = open_out_bin objfile in
  let env = initial_env() in
  try
    parse_file inputfile Parse.implementation ast_impl_magic_number
    ++ print_if ppf Clflags.dump_parsetree Printast.implementation
    ++ Typemod.type_implementation sourcefile prefixname modulename env
    ++ Translmod.transl_implementation modulename
    ++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
    ++ Simplif.simplify_lambda
    ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
    ++ Bytegen.compile_implementation modulename
    ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
    ++ Emitcode.to_file oc modulename;
    Warnings.check_fatal ();
    remove_preprocessed inputfile;
    close_out oc;
  with x ->
    close_out oc;
    remove_file objfile;
    remove_preprocessed inputfile;
    raise x

let c_file name =
  if Ccomp.compile_file name <> 0 then exit 2
