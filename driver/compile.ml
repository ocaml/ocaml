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

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

let interface ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  Compmisc.init_path false;
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
  check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let initial_env = Compmisc.initial_env () in
  try
    let ast =
      Pparse.file ppf inputfile Parse.interface ast_intf_magic_number in
    if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
    if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;
    let tsg = Typemod.transl_signature initial_env ast in
    if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
    let sg = tsg.sig_type in
    if !Clflags.print_types then
      Printtyp.wrap_printing_env initial_env (fun () ->
        fprintf std_formatter "%a@."
          Printtyp.signature (Typemod.simplify_signature sg));
    ignore (Includemod.signatures initial_env sg sg);
    Typecore.force_delayed_checks ();
    Warnings.check_fatal ();
    if not !Clflags.print_types then begin
      let sg = Env.save_signature sg modulename (outputprefix ^ ".cmi") in
      Typemod.save_signature modulename tsg outputprefix sourcefile
        initial_env sg ;
    end;
    Pparse.remove_preprocessed inputfile
  with e ->
    Pparse.remove_preprocessed inputfile;
    raise e

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x

let implementation ppf sourcefile outputprefix =
  Location.input_name := sourcefile;
  Compmisc.init_path false;
  let modulename =
    String.capitalize(Filename.basename(chop_extension_if_any sourcefile)) in
  check_unit_name ppf sourcefile modulename;
  Env.set_unit_name modulename;
  let inputfile = Pparse.preprocess sourcefile in
  let env = Compmisc.initial_env() in
  if !Clflags.print_types then begin
    try ignore(
      Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
           Printtyped.implementation_with_coercion);
      Warnings.check_fatal ();
      Pparse.remove_preprocessed inputfile;
      Stypes.dump (Some (outputprefix ^ ".annot"));
    with x ->
      Pparse.remove_preprocessed inputfile;
      Stypes.dump (Some (outputprefix ^ ".annot"));
      raise x
  end else begin
    let objfile = outputprefix ^ ".cmo" in
    let oc = open_out_bin objfile in
    try
      Pparse.file ppf inputfile Parse.implementation ast_impl_magic_number
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Typemod.type_implementation sourcefile outputprefix modulename env
      ++ print_if ppf Clflags.dump_typedtree
                  Printtyped.implementation_with_coercion
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
      Stypes.dump (Some (outputprefix ^ ".annot"));
    with x ->
      close_out oc;
      remove_file objfile;
      Pparse.remove_preprocessed inputfile;
      Stypes.dump (Some (outputprefix ^ ".annot"));
      raise x
  end

let c_file name =
  Location.input_name := name;
  if Ccomp.compile_file name <> 0 then exit 2
