(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The batch compiler *)

open Misc
open Config
open Format

(* Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory (unless the -nostdlib option is given).
 *)

let init_path () =
  let dirs =
    if !Clflags.use_threads then "+threads" :: !Clflags.include_dirs
    else if !Clflags.use_vmthreads then "+vmthreads" :: !Clflags.include_dirs
    else !Clflags.include_dirs in
  let exp_dirs =
    List.map (expand_directory Config.standard_library) dirs in
  load_path := "" :: List.rev_append exp_dirs (Clflags.std_include_dir ());
  Env.reset_cache ()

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#1775) *)
let initial_env () =
  Ident.reinit();
  try
    if !Clflags.nopervasives
    then Env.initial
    else Env.open_pers_signature "Pervasives" Env.initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"

(* Note: this function is duplicated in optcompile.ml *)
let check_unit_name ppf filename name =
  try
    begin match name.[0] with
    | 'A'..'Z' -> ()
    | _ ->
       Location.print_warning (Location.in_file filename) ppf
        (Warnings.Bad_module_name name);
       raise Exit;
    end;
    for i = 1 to String.length name - 1 do
      match name.[i] with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> ()
      | _ ->
         Location.print_warning (Location.in_file filename) ppf
           (Warnings.Bad_module_name name);
         raise Exit;
    done;
  with Exit -> ()
;;

(* Compile a .mli file *)

let interface ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  init_path ();
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
  check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  try
    let ast =
      Pparse.file ppf inputfile Parse.interface ast_intf_magic_number in
    if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
    let sg = Typemod.transl_signature (initial_env()) ast in
    if !Clflags.print_types then
      fprintf std_formatter "%a@." Printtyp.signature
                                   (Typemod.simplify_signature sg);
    ignore (Includemod.signatures (initial_env()) sg sg);
    Typecore.force_delayed_checks ();
    Warnings.check_fatal ();
    if not !Clflags.print_types then
      Env.save_signature sg modulename (outputprefix ^ ".cmi");
    Pparse.remove_preprocessed inputfile
  with e ->
    Pparse.remove_preprocessed_if_ast inputfile;
    raise e

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  init_path ();
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
  check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let env = initial_env() in
  if !Clflags.print_types then begin
    try ignore(
      Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ Typemod.type_implementation sourcefile outputprefix modulename env);
      Warnings.check_fatal ();
      Pparse.remove_preprocessed inputfile;
      Stypes.dump (outputprefix ^ ".annot");
    with x ->
      Pparse.remove_preprocessed_if_ast inputfile;
      Stypes.dump (outputprefix ^ ".annot");
      raise x
  end else begin
    let objfile = outputprefix ^ ".cmo" in
    let oc = open_out_bin objfile in
    try
      Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ Translmod.transl_implementation modulename
      ++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
      ++ Simplif.simplify_lambda
      ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
      ++ Bytegen.compile_implementation modulename
      ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
      ++ Emitcode.to_file oc modulename;
      Warnings.check_fatal ();
      close_out oc;
      Pparse.remove_preprocessed inputfile;
      Stypes.dump (outputprefix ^ ".annot");
    with x ->
      close_out oc;
      remove_file objfile;
      Pparse.remove_preprocessed_if_ast inputfile;
      Stypes.dump (outputprefix ^ ".annot");
      raise x
  end

let c_file name =
  Location.input_name := name;
  if Ccomp.compile_file name <> 0 then exit 2
