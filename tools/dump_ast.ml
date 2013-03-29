(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                        Alain Frisch, LexiFi                         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let autoopen_modules = ["Parsetree"; "Asttypes"; "Longident"]

let autoopen = ref true
let hidelocs = ref true

let typ ty =
  let ty = Parse.core_type (Lexing.from_string ty) in
  let ty = Typetexp.transl_simple_type Env.initial false ty in
  ty.Typedtree.ctyp_type

let hide_loc ppf _ = Format.printf "<loc>"

let show_value ty v =
  let env = Env.initial in
  let env = if !autoopen then List.fold_right Env.open_pers_signature autoopen_modules env else env in
  let loc_printer_path = Path.Pident (Ident.create "#loc_printer") in
  if !hidelocs then Toploop.install_printer loc_printer_path (typ "Location.t") hide_loc;
  Format.printf "%a@." (Toploop.print_value env (Obj.repr v)) (typ ty);
  if !hidelocs then Toploop.remove_printer loc_printer_path

let dump_file fn =
  let ic = open_in fn in
  let buf = Lexing.from_channel ic in
  if Filename.check_suffix fn ".mli" then show_value "Parsetree.signature" (Parse.interface buf)
  else show_value "Parsetree.structure" (Parse.implementation buf);
  close_in ic

let dump_expr s =
  show_value "Parsetree.expression"
    (Parse.expression (Lexing.from_string s))

let dump_type s =
  show_value "Parsetree.core_type"
    (Parse.core_type (Lexing.from_string s))

let dump_pattern s =
  show_value "Parsetree.pattern"
    (Parse.pattern (Lexing.from_string s))

let args =
  let open Arg in
  [
   "-noopen", Clear autoopen,
   " Don't assume that default modules are opened";

   "-locs", Clear hidelocs,
   " Keep locations";

   "-e", String dump_expr,
   "<expr> Dump the AST for <expr>";

   "-t", String dump_type,
   "<type> Dump the AST for <type>";

   "-p", String dump_pattern,
   "<pattern> Dump the AST for <pattern>";

   "-w", Int Format.set_margin,
   "<width> Define the width (in characters) of the output"
  ]

let usage =
  "dump_ast <options> [file]\n"

let () =
  let dir = Filename.dirname Sys.argv.(0) in
  let c0 = Filename.concat dir in
  let c1 = Filename.concat Config.standard_library in
  if Sys.file_exists (c1 "compiler-libs/parsetree.cmi")
  then Config.load_path := [c1 "compiler-libs"; Config.standard_library]
  else if Sys.file_exists (c0 "../parsing/parsetree.cmi")
  then Config.load_path := [c0 "../parsing"; c0 "../stdlib"] (* Running from a source tree. *)
  else (prerr_endline "Cannot locate parsetree.cmi"; exit 2);
  Toploop.initialize_toplevel_env ();
  Toploop.max_printer_depth := max_int;
  Toploop.max_printer_steps := max_int;
  try Arg.parse (Arg.align args) dump_file usage
  with exn -> Errors.report_error Format.err_formatter exn
